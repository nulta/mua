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
    self.tokenIndex = 1
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
        return self.tokens[self.tokenIndex]
    end


    ---@return Token
    function Parser:next()
        local token = self.tokens[self.tokenIndex]
        self.tokenIndex = self.tokenIndex + 1
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
    ---@param message string
    ---@return T
    function Parser:assert(condition, message)
        if not condition then
            self:error(message)
        end

        return condition
    end


    ---@param type TokenType | string
    ---@return Token?
    function Parser:match(type)
        local token = self:peek()
        local isSpecialToken = token.type == TokenType.KEYWORD or token.type == TokenType.SYMBOL

        if (token.type == type) or (isSpecialToken and token.value == type) then
            return self:next()
        end

        return nil
    end


    ---@param ... TokenType | string
    ---@return Token
    function Parser:expect(...)
        for _, type in ipairs({...}) do
            local match = self:match(type)
            if match then
                return match
            end
        end

        ---@diagnostic disable: missing-return
        self:error(("unexcepted symbol near '%s'"):format(type, self:current().value))
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
        local stat repeat
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

        -- varlist '=' explist | functioncall
        --!!! TODO

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
            local condition = self:parseExpression()
            self:expect("do")
            local block = self:parseBlock()
            self:expect("end")

            return Ast.WhileStatNode:new({condition = condition, block = block}, position)
        end

        -- repeat block until exp
        if self:match("repeat") then
            local block = self:parseBlock()
            self:expect("until")
            local condition = self:parseExpression()

            return Ast.RepeatStatNode:new({block = block, untilCondition = condition}, position)
        end

        -- if exp then block {elseif exp then block} [else block] end
        if self:match("if") then
            local ifNodes = {}
            local elseBlock = nil

            repeat
                local condition = self:parseExpression()
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

                local startExpr = self:parseExpression()
                self:expect(",")
                local endExpr = self:parseExpression()

                local stepExpr = nil
                if self:match(",") then
                    stepExpr = self:parseExpression()
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
                local explist = self:parseExpressionList()
                self:expect("do")
                local block = self:parseBlock()
                self:expect("end")
                
                return Ast.IterativeForStatNode:new({names = namelist, expressions = explist, block = block}, position)
            end
        end

        -- function funcname funcbody
        if self:match("function") then
            local funcname = self:parseFunctionName()

            -- funcbody ::= '(' [parlist] ')' block end
            self:expect("(")
            local parlist = self:parseNamelist(true, true)
            self:expect(")")

            local block = self:parseBlock()

            return Ast.FunctionDeclarationStatNode:new({name = funcname, parameters = parlist, block = block}, position)
        end

        -- local namelist ['=' explist]
        if self:match("local") then
            local namelist = self:parseNamelist()
            local explist = {}

            if self:match("=") then
                explist = self:parseExpressionList()
            end

            return Ast.LocalVariableAssignmentStatNode:new({names = namelist, expressions = explist}, position)
        end

        return nil
    end


    --- `retstat ::= return [explist]`
    ---@return ReturnStatNode?
    function Parser:tryReturnStat()
        local position = self:peekPosition()

        if self:match("return") then
            local explist = self:parseExpressionList(true)
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
end


-- Expression parsing --
do
    local unaryOperators = {
        ["not"] = true,
        ["-"] = true,
        ["#"] = true,
        ["~"] = true,
    }


    function Parser:parseExpression()

    end


    function Parser:tryUnaryOpExpression()
        local position = self:peekPosition()
        local token = self:peek()
        local isSpecialToken = token.type == TokenType.KEYWORD or token.type == TokenType.SYMBOL
        local isUnaryOp = isSpecialToken and unaryOperators[token.value]
        
        if isUnaryOp then
            local operator = self:next().value
            local expression = self:parseExpression()
            return Ast.UnaryOpExpressionNode:new({operator = operator, expression = expression}, position)
        end

        return nil
    end


    function Parser:tryLiteralExpression()
        local position = self:peekPosition()

        -- nil
        if self:match("nil") then
            return Ast.NilLiteralExpressionNode:new({value = nil}, position)
        end

        -- true | false
        if self:match("true") or self:match("false") then
            local value = self:current().value == "true"
            return Ast.BooleanLiteralExpressionNode:new({value = value}, position)
        end

        -- Numeral
        if self:match(TokenType.NUMBER_LITERAL) then
            local value = self:current().value

            local number
            if value:match("^0o") then
                number = tonumber(value:sub(3):gsub("_", ""), 8)
            else
                number = tonumber(value:gsub("_", ""))
            end
            number = self:assert(number, ("malformed number near '%s'"):format(value))

            return Ast.NumberLiteralExpressionNode:new({value = number}, position)
        end

        -- LiteralString
        if self:match(TokenType.STRING_LITERAL) then
            local value = self:current().value

            if value:match("^['\"]") then
                -- "Short string"
                value = value:sub(2, -2)
            else
                -- [[Long string]]
                value = value:gsub("^%[=-%[\n?", "")
                value = value:gsub("%]=-%]$", "")
            end

            return Ast.StringLiteralExpressionNode:new({value = value}, position)
        end

        -- ...
        if self:match("...") then
            return Ast.VarargLiteralExpressionNode:new({value = {kind = "..."}}, position)
        end

        return nil
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

return Parser