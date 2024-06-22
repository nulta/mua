local Ast = require("compiler.ast")
local Defs = require("compiler.definitions")
local TokenType = Defs.TokenType


---@class Parser
---@field tokens TokenSequence
---@field tokenIndex number
---@field tree AbstractSyntaxTree
local Parser = {}


---@param tokens TokenSequence
---@param filename string?
---@return self
function Parser.new(tokens, filename)
    local self = setmetatable({}, { __index = Parser })
    self.tokens = tokens
    self.tokenIndex = 0
    self.tree = {filename = filename or "[?]"}
    return self
end


-- Helper functions --
do
    ---@return Token
    function Parser:current()
        return self.tokens[self.tokenIndex]
    end


    ---@return { line: number, column: number }
    function Parser:peekPosition()
        local token = self:peek()
        return {line = token.line, column = token.column}
    end


    ---@param index integer?
    ---@return Token
    function Parser:peek(index)
        index = index or 1
        return self.tokens[self.tokenIndex + index]
    end


    ---@return Token
    function Parser:next()
        self.tokenIndex = self.tokenIndex + 1
        local token = self.tokens[self.tokenIndex]
        return token
    end


    ---@param message string
    function Parser:error(message)
        local token = self:current()
        local filename = self.tree.filename
        local ln = token.line
        local col = token.column

        error(("%s:%d:%d: %s"):format(filename, ln, col, message))
    end


    ---@generic T
    ---@param condition T | nil
    ---@param message string?
    ---@return T
    function Parser:assert(condition, message)
        if not condition then
            self:error(message or ("unexcepted symbol near '%s'"):format(self:current().value))
        end

        return condition
    end


    ---@param ... TokenType | string
    ---@return Token?
    function Parser:match(...)
        local token = self:peek()
        local isSpecialToken = token.type == TokenType.KEYWORD or token.type == TokenType.SYMBOL

        for _, type in pairs({...}) do
            if (token.type == type) or (isSpecialToken and token.value == type) then
                return self:next()
            end
        end

        return nil
    end


    ---@param ... TokenType | string
    ---@return Token
    function Parser:expect(...)
        local match = self:match(...)
        if match then
            return match
        end

        ---@diagnostic disable: missing-return
        self:error(
            ("'%s' expected near '%s'"):format(table.concat({...}, "','"), self:current().value)
        )
    end


    --- Returns the current token index.
    ---@return integer
    function Parser:checkpoint()
        return self.tokenIndex
    end


    --- Rollback to a previous checkpoint.
    ---@param checkpoint integer
    ---@return nil
    function Parser:rollback(checkpoint)
        self.tokenIndex = checkpoint
    end
end


-- Parsing functions --
do
    --- Parse given tokens, then returns the AST.
    ---@return AbstractSyntaxTree
    function Parser:parse()
        local block = self:parseBlock()

        self:expect(TokenType.EOF)

        for _, node in ipairs(block) do
            table.insert(self.tree, node)
        end

        return self.tree
    end


    --- `block ::= {stat} [retstat]`
    ---@return StatNode[]
    function Parser:parseBlock()
        local block = {}

        -- {stat}
        local stat
        repeat
            stat = self:tryStat()
            table.insert(block, stat)
        until not stat

        -- [retstat]
        local retstat = self:tryReturnStat()
        table.insert(block, retstat)

        return block
    end


    --- `stat ::= (...)`
    ---@return StatNode?
    function Parser:tryStat()
        local position = self:peekPosition()

        -- functioncall
        local functionCall = self:tryFunctionCallExp()
        if functionCall then
            return Ast.FunctionCallStatNode:new({
                target = functionCall.target,
                method = functionCall.method,
                args = functionCall.args
            }, position)
        end

        -- varlist '=' explist
        local varlist = self:tryVarList()
        if varlist then
            self:expect("=")
            local explist = self:parseExpList()
            return Ast.VariableAssignmentStatNode:new({names = varlist, expressions = explist}, position)
        end

        -- label
        if self:match("::") then
            local label = self:expect(TokenType.NAME).value
            self:expect("::")
            return Ast.GotoLabelStatNode:new({name = label}, position)
        end

        -- break
        if self:match("break") then
            return Ast.BreakStatNode:new({}, position)
        end

        -- goto Name
        if self:match("goto") then
            local name = self:expect(TokenType.NAME).value
            return Ast.GotoStatNode:new({name = name}, position)
        end

        -- do block end
        if self:match("do") then
            local block = self:parseBlock()
            self:expect("end")

            return Ast.DoStatNode:new({block = block}, position)
        end

        -- while exp do block end
        if self:match("while") then
            local condition = self:parseExp()
            self:expect("do")
            local block = self:parseBlock()
            self:expect("end")

            return Ast.WhileStatNode:new({condition = condition, block = block}, position)
        end

        -- repeat block until exp
        if self:match("repeat") then
            local block = self:parseBlock()
            self:expect("until")
            local condition = self:parseExp()

            return Ast.RepeatStatNode:new({block = block, untilCondition = condition}, position)
        end

        -- if exp then block {elseif exp then block} [else block] end
        if self:match("if") then
            local ifNodes = {}
            local elseBlock = nil

            repeat
                local condition = self:parseExp()
                self:expect("then")
                local block = self:parseBlock()

                table.insert(ifNodes, {condition = condition, block = block})
            until not self:match("elseif")

            if self:match("else") then
                elseBlock = self:parseBlock()
            end

            self:expect("end")

            return Ast.IfStatNode:new({ifNodes = ifNodes, elseBlock = elseBlock}, position)
        end

        -- for Name `=` exp `,` exp [`,` exp] do block end |
        -- for namelist in explist do block end
        if self:match("for") then
            local name1 = self:expect(TokenType.NAME).value

            if self:match("=") then
                --> NumericForStat

                local startExpr = self:parseExp()
                self:expect(",")
                local endExpr = self:parseExp()

                local stepExpr = nil
                if self:match(",") then
                    stepExpr = self:parseExp()
                end

                self:expect("do")
                local block = self:parseBlock()
                self:expect("end")

                return Ast.NumericForStatNode:new({name = name1, startExpr = startExpr, endExpr = endExpr, stepExpr = stepExpr, block = block}, position)
            else
                --> IterativeForStat

                local namelist = {name1}
                while self:match(",") do
                    local name = self:expect(TokenType.NAME).value
                    table.insert(namelist, name)
                end

                self:expect("in")
                local explist = self:parseExpList()
                self:expect("do")
                local block = self:parseBlock()
                self:expect("end")

                return Ast.IterativeForStatNode:new({names = namelist, expressions = explist, block = block}, position)
            end
        end

        -- function funcname funcbody
        if self:match("function") then
            local funcname = self:parseFunctionName()
            local body = self:parseFuncBody()
            return Ast.FunctionDeclarationStatNode:new({name = funcname, parameters = body.parameters, block = body.block}, position)
        end

        -- local function Name funcbody | local namelist ['=' explist]
        if self:match("local") then
            if self:match("function") then
                local name = self:expect(TokenType.NAME).value
                local body = self:parseFuncBody()
                return Ast.LocalFunctionDeclarationStatNode:new({name = name, parameters = body.parameters, block = body.block}, position)
            else
                local namelist = self:parseNamelist()
                local explist = {}

                if self:match("=") then
                    explist = self:parseExpList()
                end

                return Ast.LocalVariableAssignmentStatNode:new({names = namelist, expressions = explist}, position)
            end
        end

        return nil
    end


    --- `retstat ::= return [explist]`
    ---@return ReturnStatNode?
    function Parser:tryReturnStat()
        local position = self:peekPosition()

        if self:match("return") then
            local explist = self:parseExpList(true)
            return Ast.ReturnStatNode:new({expressions = explist}, position)
        end

        return nil
    end


    --- namelist ::= Name {',' Name}
    --- parlist ::= namelist [',' '...'] | '...'
    ---@param allowEmpty boolean?
    ---@param allowVarargs boolean?
    ---@return string[]
    function Parser:parseNamelist(allowEmpty, allowVarargs)
        -- case 1: '...'
        if allowVarargs and self:match("...") then
            return {"..."}
        end

        -- case 2: [namelist] is empty
        if allowEmpty and self:peek().type ~= TokenType.NAME then
            return {}
        end

        -- Name
        local namelist = {}
        local name = self:expect(TokenType.NAME).value
        table.insert(namelist, name)

        -- {',' Name}
        while self:match(",") do
            -- '...'
            if allowVarargs and self:match("...") then
                table.insert(namelist, "...")
                break
            end

            -- Name
            local name = self:expect(TokenType.NAME).value
            table.insert(namelist, name)
        end

        return namelist
    end


    --- funcname ::= Name {'.' Name} [':' Name]
    ---@return string
    function Parser:parseFunctionName()
        -- Name
        local funcname = {self:expect(TokenType.NAME).value}

        -- {'.' Name}
        while self:match(".") do
            local name = self:expect(TokenType.NAME).value
            table.insert(funcname, ".")
            table.insert(funcname, name)
        end

        -- [':' Name]
        if self:match(":") then
            local name = self:expect(TokenType.NAME).value
            table.insert(funcname, ":")
            table.insert(funcname, name)
        end

        -- return as a concatnated string
        return table.concat(funcname)
    end


    function Parser:parseFuncBody()
        -- funcbody ::= '(' [parlist] ')' block end
        self:expect("(")
        local parlist = self:parseNamelist(true, true)
        self:expect(")")

        local block = self:parseBlock()
        self:expect("end")

        return {parameters = parlist, block = block}
    end
end


-- Expression parsing --
do
    function Parser:parseExp()
        local expression = self:tryExp()
        if not expression then
            self:error(("unexpected symbol near '%s'"):format(self:current().value))
        end

        return expression
    end

    --- `exp ::= primaryexp | (exp binop exp)`
    function Parser:tryExp()
        -- exp binop exp
        local binop = self:parseBinaryOpExp()
        if binop then
            return binop
        end

        -- primaryExp
        return self:tryPrimaryExp()
    end


    --- primaryexp ::= simpleexp | unop exp | prefixexp
    function Parser:tryPrimaryExp()
        -- simpleexp
        local simpleExp = self:trySimpleExp()
        if simpleExp then
            return simpleExp
        end

        -- unop exp
        local unaryOpExp = self:tryUnaryOpExp()
        if unaryOpExp then
            return unaryOpExp
        end

        -- prefixexp
        local prefixExp = self:tryPrefixExp()
        if prefixExp then
            return prefixExp
        end
    end


    --- simpleexp ::= nil | false | true | Numeral | LiteralString | '...'
    ---     | functiondef | tableconstructor
    function Parser:trySimpleExp()
        local position = self:peekPosition()

        -- nil
        if self:match("nil") then
            return Ast.NilLiteralExpNode:new({value = nil}, position)
        end

        -- true | false
        if self:match("true") or self:match("false") then
            local value = self:current().value == "true"
            return Ast.BooleanLiteralExpNode:new({value = value}, position)
        end

        -- Numeral
        if self:match(TokenType.NUMBER_LITERAL) then
            local value = self:current().value

            local number
            if value:match("^0o") then
                number = tonumber(value:sub(3):gsub("_", ""), 8)
            else
                number = tonumber(value:gsub("_", ""), 10)
            end
            number = self:assert(number, ("malformed number near '%s'"):format(value))

            return Ast.NumberLiteralExpNode:new({value = number}, position)
        end

        -- LiteralString
        local literalString = self:tryLiteralString()
        if literalString then
            return literalString
        end

        -- ...
        if self:match("...") then
            return Ast.NameExpNode:new({name = "..."}, position)
        end

        -- functiondef
        local functionDef = self:tryFunctionDef()
        if functionDef then
            return functionDef
        end

        -- tableconstructor
        local tableConstructor = self:tryTableConstructorExp()
        if tableConstructor then
            return tableConstructor
        end

        return nil
    end


    --- LiteralString
    function Parser:tryLiteralString()
        local position = self:peekPosition()
        if self:match(TokenType.STRING_LITERAL) then
            local value = self:current().value
            local isLongString = false

            -- Remove quotes
            if value:match("^['\"]") then
                -- "Short string"
                value = value:sub(2, -2)
            else
                -- [[Long string]]
                isLongString = true
                value = value:gsub("^%[=-%[\n?", "")
                value = value:gsub("%]=-%]$", "")
            end

            -- Long string does not process escape sequences
            if isLongString then
                return Ast.StringLiteralExpNode:new({value = value}, position)
            end

            -- Escape sequences processing
            local text = {}
            local j = 1
            local i = 1
            while i <= #value do
                local c = value:sub(i, i)
                i = i + 1
                if c ~= "\\" then goto CONTINUE end

                -- Prepend the previous string
                table.insert(text, value:sub(j, i - 2))

                local next = value:sub(i, i)
                local switches = {
                    ["a"] = "\a",
                    ["b"] = "\b",
                    ["f"] = "\f",
                    ["n"] = "\n",
                    ["r"] = "\r",
                    ["t"] = "\t",
                    ["v"] = "\v",
                    ['"'] = '"',
                    ["'"] = "'",
                    ["\n"] = "\n",
                    ["\\"] = "\\",
                }

                if switches[next] then
                    table.insert(text, switches[next])
                    i = i + 1
                elseif next == "x" then
                    -- \xHH
                    local hex = value:match("^%x%x", i+1)
                    self:assert(hex, "hexadecimal digit expected")
                    table.insert(text, string.char(tonumber(hex, 16)))
                    i = i + 3
                elseif next:match("%d") then
                    -- \ddd
                    local num = value:match("^%d%d?%d?", i)
                    table.insert(text, string.char(tonumber(num, 10)))
                    i = i + #num
                elseif next == "u" then
                    -- \u{HHH...}
                    local hex = value:match("^{%x+}", i+1)
                    self:assert(hex, "hexadecimal digit expected")

                    local hexNum = tonumber(hex:sub(2, -2), 16)
                    self:assert(hexNum <= 0x10FFFF, "UTF-8 value too large")

                    table.insert(text, utf8.char(hexNum))
                    i = i + #hex
                elseif next == "z" then
                    -- \z (skip whitespace)
                    while value:sub(i, i):match("%s") do
                        i = i + 1
                    end
                else
                    self:error(("invalid escape sequence near '\\%s'"):format(next))
                end

                j = i
                ::CONTINUE::
            end

            table.insert(text, value:sub(j, i - 1))
            value = table.concat(text)

            return Ast.StringLiteralExpNode:new({value = value}, position)
        end

        return nil
    end


    --- `functiondef ::= function funcbody`
    function Parser:tryFunctionDef()
        local position = self:peekPosition()

        if self:match("function") then
            local body = self:parseFuncBody()
            return Ast.FunctionDefExpNode:new({parameters = body.parameters, block = body.block}, position)
        end

        return nil
    end


    --- `explist ::= exp {',' exp}`
    function Parser:parseExpList(allowEmpty)
        local explist = {}

        -- exp
        local expression = self:tryExp()
        table.insert(explist, expression)

        -- [explist]?
        if not expression then
            if allowEmpty then
                return {}
            else
                self:error("expression expected")
            end
        end

        -- {',' exp}
        while self:match(",") do
            local expression = self:parseExp()
            table.insert(explist, expression)
        end

        return explist
    end


    --- `exp binop exp`
    function Parser:parseBinaryOpExp(minBindingPower)
        minBindingPower = minBindingPower or 0

        ---@type ExpNode
        local left = self:tryPrimaryExp()
        if not left then return nil end

        while true do
            local position = self:peekPosition()
            local token = self:peek()
            local operator = token.value

            local lbp, rbp = self:operatorBindingPower(token.value)
            if lbp <= minBindingPower then
                break
            end

            self:next()

            local right = self:parseBinaryOpExp(rbp)

            left = Ast.BinaryOpExpNode:new({left = left, operator = operator, right = right}, position)
        end

        return left
    end


    --- `unop exp`
    function Parser:tryUnaryOpExp()
        local unaryOperators = {
            ["not"] = true,
            ["-"] = true,
            ["#"] = true,
            ["~"] = true,
        }

        local position = self:peekPosition()
        local token = self:peek()
        local isSpecialToken = token.type == TokenType.KEYWORD or token.type == TokenType.SYMBOL
        local isUnaryOp = isSpecialToken and unaryOperators[token.value]

        if isUnaryOp then
            local operator = self:next().value
            local expression = self:parseExp()
            return Ast.UnaryOpExpNode:new({operator = operator, expression = expression}, position)
        end

        return nil
    end


    --- ```
    --- prefixexp ::= prefixexp_1 {prefixexp_2}
    ---
    --- prefixexp_1 ::= 
    ---     '(' exp ')'
    ---   | Name            (* var *)
    ---
    --- prefixexp_2 ::=
    ---     '[' exp ']'     (* var *)
    ---   | '.' Name        (* var *)
    ---   | ':' Name args   (* functioncall *)
    ---   | args            (* functioncall *)
    --- ```
    ---@return ExpNode | NameExpNode | TableIndexExpNode | FunctionCallExpNode | nil
    function Parser:tryPrefixExp()
        -- prefixexp_1
        local prefixexp
        do
            if self:match("(") then
                local expression = self:parseExp()
                self:expect(")")
                prefixexp = expression
            end

            if self:match(TokenType.NAME) then
                local name = self:current().value
                prefixexp = Ast.NameExpNode:new({name = name}, self:peekPosition())
            end

            if not prefixexp then
                return nil
            end
        end

        -- {prefixexp_2}
        while true do
            local position = self:peekPosition()

            if self:match("[") then
                -- '[' exp ']'
                local key = self:parseExp()
                self:expect("]")

                prefixexp = Ast.TableIndexExpNode:new({target = prefixexp, key = key}, position)
            elseif self:match(".") then
                -- '.' Name
                local key = self:expect(TokenType.NAME).value
                local keyNode = Ast.StringLiteralExpNode:new({value = key}, position)

                prefixexp = Ast.TableIndexExpNode:new({target = prefixexp, key = keyNode}, position)
            elseif self:match(":") then
                -- ':' Name args
                local method = self:expect(TokenType.NAME).value
                local args = self:tryArgsExp()

                prefixexp = Ast.FunctionCallExpNode:new({target = prefixexp, method = method, args = args}, position)
            else
                -- args
                local args = self:tryArgsExp()
                prefixexp = args and Ast.FunctionCallExpNode:new({target = prefixexp, args = args}, position) or prefixexp

                -- (no match)
                if not args then break end
            end
        end

        return prefixexp
    end


    --- `var ::= Name | prefixexp '[' exp ']'` | prefixexp '.' Name`
    --- but it's covered by prefixexp...
    function Parser:tryVarExp()
        local checkpoint = self:checkpoint()
        local prefixexp = self:tryPrefixExp()
        if not prefixexp then
            self:rollback(checkpoint)
            return nil
        end

        local node = Ast.NameExpNode:check(prefixexp) or Ast.TableIndexExpNode:check(prefixexp)
        if not node then
            self:rollback(checkpoint)
        end

        return node
    end


    --- `varlist ::= var {',' var}`
    function Parser:tryVarList()
        local varlist = {}

        -- var
        local var = self:tryVarExp()
        if not var then
            return nil
        end
        table.insert(varlist, var)

        -- {',' var}
        while self:match(",") do
            var = self:tryVarExp()
            if not var then
                self:error("variable expected")
            end
            table.insert(varlist, var)
        end

        return varlist
    end


    --- `functioncall ::= prefixexp args | prefixexp ':' Name args`
    --- but it's covered by prefixexp, too.
    function Parser:tryFunctionCallExp()
        local checkpoint = self:checkpoint()
        local prefixexp = self:tryPrefixExp()
        local node = prefixexp and Ast.FunctionCallExpNode:check(prefixexp) or nil

        if not node then
            self:rollback(checkpoint)
        end

        return node
    end


    --- args ::=  '(' [explist] ')' | tableconstructor | LiteralString
    function Parser:tryArgsExp()

        -- '(' [explist] ')'
        if self:match("(") then
            if self:match(")") then
                return {}
            end

            local explist = self:parseExpList()
            self:expect(")")
            return explist
        end

        -- tableconstructor
        local tableConstructor = self:tryTableConstructorExp()
        if tableConstructor then
            return {tableConstructor}
        end

        -- LiteralString
        local literalString = self:tryLiteralString()
        if literalString then
            return {literalString}
        end

        return nil
    end

    --- `tableconstructor ::= '{' [fieldlist] '}'`
    function Parser:tryTableConstructorExp()
        local position = self:peekPosition()

        -- '{' [fieldlist] '}'
        if self:match("{") then
            -- [fieldlist] is omitted
            if self:match("}") then
                return Ast.TableConstructorExpNode:new({fields = {}}, position)
            end

            -- fieldlist ::= field {fieldsep field} [fieldsep]
            local fields = {}

            -- field
            local field, listCount = self:parseFieldExp(1)
            table.insert(fields, field)

            -- fieldsep ::= ',' | ';'
            -- {fieldsep field} [fieldsep]
            while self:match(",", ";") do
                if self:peek().value == "}" then
                    break
                end
                field, listCount = self:parseFieldExp(listCount)
                table.insert(fields, field)
            end

            -- '}'
            self:expect("}")

            return Ast.TableConstructorExpNode:new({fields = fields}, position)
        end

        return nil
    end


    --- field ::= '[' exp ']' '=' exp | Name '=' exp | exp
    ---@param listCount integer
    ---@return {key: ExpNode?, value: ExpNode}, integer
    function Parser:parseFieldExp(listCount)
        local position = self:peekPosition()

        -- '[' exp ']' '=' exp
        if self:match("[") then
            local key = self:parseExp()
            self:expect("]")
            self:expect("=")
            local value = self:parseExp()

            return {key = key, value = value}, listCount
        end

        -- Name '=' exp
        if self:match(TokenType.NAME) then
            local key = Ast.StringLiteralExpNode:new({value = self:current().value}, position)
            self:expect("=")
            local value = self:parseExp()

            return {key = key, value = value}, listCount
        end

        -- exp
        local value = self:parseExp()
        local key = Ast.NumberLiteralExpNode:new({value = listCount}, position)
        return {key = key, value = value}, listCount + 1
    end


    --- Returns the left-binding power and right-binding power of a operator.
    ---@param symbol string
    ---@return integer, integer
    function Parser:operatorBindingPower(symbol)
        local bindingPower = {
            ["^"]  = {121, 120}, -- #1 (left associative)
            ["*"]  = {110}, -- #2
            ["/"]  = {110},
            ["//"] = {110},
            ["%"]  = {110},
            ["+"]  = {100},
            ["-"]  = {100},
            [".."] = {91, 90}, -- #3 (left associative)
            ["<<"] = {80}, -- #4
            [">>"] = {80},
            ["<"]  = {70}, -- #5
            [">"]  = {70},
            ["<="] = {70},
            [">="] = {70},
            ["=="] = {60}, -- #6
            ["~="] = {60},
            ["and"] = {30}, -- #7
            ["or"] = {20}, -- #8
        }

        local power = bindingPower[symbol] or {0, 0}
        local lbp = power[1]
        local rbp = power[2] or lbp + 1
        return lbp, rbp
    end
end


-- Memoize --
local MEMOIZE = true
if MEMOIZE then
    local memos = {}

    local function memoize(funcname)
        local memoizer = Parser[funcname]
        local memo = {}
        memos[funcname] = memo

        Parser[funcname] = function(self, argu)
            local key = self.tokenIndex .. tostring(argu)
            
            if not memo[key] then
                memo[key] = memoizer(self, argu)
            end
                
            return memo[key]
        end
    end


    memoize("tryStat")
    memoize("tryPrefixExp")
    memoize("tryVarExp")
    memoize("tryFunctionCallExp")
    memoize("tryExp")

    function Parser:clearMemo()
        for _, memo in pairs(memos) do
            for key, _ in pairs(memo) do
                memo[key] = nil
            end
        end
    end


    local _next = Parser.next
    ---@diagnostic disable-next-line: duplicate-set-field
    Parser.next = function(self, ...)
        self:clearMemo()
        return _next(self, ...)
    end


    local _rollback = Parser.rollback
    ---@diagnostic disable-next-line: duplicate-set-field
    Parser.rollback = function(self, ...)
        self:clearMemo()
        return _rollback(self, ...)
    end
end

return Parser