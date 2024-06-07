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

do -- Error handling --
    --- Throws a parser error
    ---@param message string
    function Parser:error(message)
        local token = self:currentToken()
        error(("%d:%d: Parser Error: %s"):format(token.line, token.column, message))
    end

    --- Asserts that a condition is true, otherwise throws an error
    ---@param condition any
    ---@param message string
    ---@param ... any
    function Parser:assertf(condition, message, ...)
        if not condition then
            self:error(message:format(...))
        end
    end

    --- Asserts that the current token is of a certain types
    ---@param token Token
    ---@param type TokenType
    ---@param value string
    ---@overload fun(self: self, token: Token, typeOrValue: TokenType | string)
    ---@overload fun(self: self, type: TokenType, value: string)
    ---@overload fun(self: self, typeOrValue: TokenType | string)
    function Parser:exceptToken(token, type, value)
        if _G.type(token) == "string" and not value then
            ---@diagnostic disable-next-line: cast-local-type
            token, type, value = self:currentToken(), token, type
        end

        if type and not value and not TokenType[type] then
            ---@diagnostic disable-next-line: cast-local-type
            value, type = type, nil
        end

        if type and token.type ~= type then
            self:error(("Expected %s, got %s"):format(type, token.type))
        end

        if value and token.value ~= value then
            self:error(("Expected %s, got %s"):format(value, token.value))
        end
    end
end

do -- Token sequence controller --
    --- Get the current token
    ---@return Token
    function Parser:currentToken()
        return self.tokens[self.tokenIndex]
    end

    --- Move to the next token and return it
    ---@return Token
    function Parser:advanceToken()
        self.tokenIndex = self.tokenIndex + 1
        return self.tokens[self.tokenIndex]
    end

    --- Peek the next token
    ---@param offset number?
    ---@return Token
    function Parser:nextToken(offset)
        offset = offset or 1
        return self.tokens[self.tokenIndex + offset]
    end

    --- Peek the previous token
    ---@return Token
    function Parser:previousToken()
        return self:nextToken(-1)
    end

    --- Check if the current token is the end of the file
    ---@return boolean
    function Parser:isEof()
        return self:currentToken().type == TokenType.EOF
    end

    --- Except the current token and move to the next one
    ---@param tokenTypeOrValue TokenType | string
    function Parser:skipExceptedToken(tokenTypeOrValue)
        self:exceptToken(tokenTypeOrValue)
        self:advanceToken()
    end

    --- Check if the current token is of a certain type.
    --- If the token is a KEYWORD or SYMBOL, it will also check the value
    ---@param tokenTypeOrValue TokenType | string
    function Parser:currentTokenIs(tokenTypeOrValue)
        local token = self:currentToken()
        if token.type == tokenTypeOrValue then
            return true
        elseif token.type == TokenType.KEYWORD or token.type == TokenType.SYMBOL then
            return token.value == tokenTypeOrValue
        else
            return false
        end
    end

    function Parser:getPosition()
        local token = self:currentToken()
        return {line = token.line, column = token.column}
    end
end

do -- Node controller --
end

do -- Parsing --
    ---@return AbstractSyntaxTree
    function Parser:parse()
        local nodes = self:parseStatementBlock(TokenType.EOF)
        for _, node in ipairs(nodes) do
            table.insert(self.tree, node)
        end

        return self.tree
    end


    ---@param ... TokenType | string  End conditions
    ---@return StatementNode[]
    function Parser:parseStatementBlock(...)
        local nodes = {}
        local endConditions = {...}

        local function endCondition()
            for _, condition in ipairs(endConditions) do
                if self:currentTokenIs(condition) then
                    return true
                end
            end
            return false
        end

        while not endCondition() do
            local statement = self:parseStatement()
            table.insert(nodes, statement)
        end
        self:advanceToken()

        return nodes
    end


    ---@return StatementNode
    function Parser:parseStatement()
        local token = self:currentToken()
        local value = token.value

        if token.type == TokenType.KEYWORD then
            local switches = {
                ["do"] = self.parseDoStmt,
                ["if"] = self.parseIfStmt,
                ["while"] = self.parseWhileStmt,
                ["repeat"] = self.parseRepeatStmt,
                ["for"] = self.parseForStmt,
                ["return"] = self.parseReturnStmt,
                ["break"] = self.parseBreakStmt,
                ["goto"] = self.parseGotoStmt,
                ["function"] = self.parseFunctionDeclearationStmt,
                ["local"] = function(_)
                    if self:nextToken().value == "function" then
                        return self:parseFunctionDeclearationStmt(true)
                    else
                        return self:parseVariableDeclearationStmt(true)
                    end
                end,
            }

            local parserFunction = switches[value]
            self:assertf(parserFunction, "Unexpected keyword '%s'", value)
            return parserFunction(self)
        else
            self:exceptToken(TokenType.NAME)
            return self:parseAssignmentStmt()
        end
    end


    ---@return DoStatementNode
    function Parser:parseDoStmt()
        local position = self:getPosition()

        self:skipExceptedToken("do")
        local block = self:parseStatementBlock("end")

        return Ast.DoStatementNode:new({block = block}, position)
    end


    ---@return IfStatementNode
    function Parser:parseIfStmt()
        local position = self:getPosition()

        ---@type _AtomicIf[]
        local ifNodes = {}

        self:skipExceptedToken("if")
        repeat
            local condition = self:parseExpression()
            self:skipExceptedToken("then")

            local block = self:parseStatementBlock("elseif", "else", "end")
            table.insert(ifNodes, {condition = condition, block = block})
        until self:previousToken().value ~= "elseif"

        local elseBlock = nil
        if self:previousToken().value == "else" then
            elseBlock = self:parseStatementBlock("end")
        end

        return Ast.IfStatementNode:new(
            {ifNodes = ifNodes, elseBlock = elseBlock},
            position
        )
    end


    ---@return WhileStatementNode
    function Parser:parseWhileStmt()
        local position = self:getPosition()

        self:skipExceptedToken("while")
        local condition = self:parseExpression()
        self:skipExceptedToken("do")
        local block = self:parseStatementBlock("end")

        return Ast.WhileStatementNode:new(
            {condition = condition, block = block},
            position
        )
    end
end

do -- Utilities --

    ---@param value string
    ---@return number
    function Parser:literalToNumber(value)
        -- 0x????, 0b????, 0o????
        if value:sub(1, 2) == "0x" then
            return tonumber(value:sub(3), 16)
        elseif value:sub(1, 2) == "0b" then
            return tonumber(value:sub(3), 2)
        elseif value:sub(1, 2) == "0o" then
            return tonumber(value:sub(3), 8)
        end

        -- otherwise, decimal
        local number = tonumber(value)
        self:assertf(number, "Invalid number literal")

        ---@type number
        return number
    end

    ---@param value string
    ---@return string
    function Parser:literalToString(value)
        local firstChar = value:sub(1, 1)
        
        -- case 1: "" or ''
        if firstChar == "\"" or firstChar == "'" then
            return value:sub(2, -2)
        end

        -- case 2: [[...]] or [=[...]=]
        if firstChar == "[" then
            local opener = value:match("^%[=*%[")
            local closer = opener:gsub("%[", "]")
            local content =  value:gsub("^" .. opener, ""):gsub(closer .. "$", "")
            return content
        end

        ---@diagnostic disable-next-line: return-type-mismatch
        return self:error("Invalid string literal")
    end
end

do -- Expression Parsing --
    ---@return ExpressionNode
    function Parser:parseExpression()
        return self:parseBinaryExpression(0)
    end

    ---@param minPrecedence number
    ---@return ExpressionNode
    function Parser:parseBinaryExpression(minPrecedence)
        local left = self:parseUnaryExpression()
        local token = self:currentToken()

        while token.type == TokenType.SYMBOL do
            local position = self:getPosition()

            local precedence = Defs.getPrecedence(token.value)
            if precedence < minPrecedence then
                break
            end

            self:advanceToken()
            local right = self:parseBinaryExpression(precedence + 1)
            left = Ast.BinaryExpressionNode:new({left = left, right = right, operator = token.value}, position)
            token = self:currentToken()
        end

        return left
    end

    ---@return ExpressionNode
    function Parser:parseUnaryExpression()
        local token = self:currentToken()
        if token.type == TokenType.SYMBOL then
            if token.value == "-" or token.value == "not" then
                local position = self:getPosition()

                self:advanceToken()
                local expression = self:parseUnaryExpression()
                return Ast.UnaryExpressionNode:new({expression = expression, operator = token.value}, position)
            end
        end

        return self:parsePrimaryExpression()
    end

    ---@return ExpressionNode
    function Parser:parsePrimaryExpression()
        local position = self:getPosition()
        local token = self:currentToken()

        if token.type == TokenType.NUMBER then
            local number = self:literalToNumber(token.value)
            self:advanceToken()
            return Ast.NumberLiteralNode:new({value = number}, position)
        end

        if token.type == TokenType.STRING then
            local string = self:literalToString(token.value)
            self:advanceToken()
            return Ast.StringLiteralNode:new({value = string}, position)
        end

        if token.type == TokenType.KEYWORD then
            if token.value == "nil" then
                self:advanceToken()
                return Ast.NilLiteralNode:new({value = nil}, position)
            end

            if token.value == "true" or token.value == "false" then
                self:advanceToken()
                return Ast.BooleanLiteralNode:new({value = (token.value == "true")}, position)
            end
        end

        if token.type == TokenType.NAME then
            local name = token.value
            self:advanceToken()

            if self:currentTokenIs("(") then
                return self:parseFunctionCallExpression(name)
            end

            return Ast.VariableNode:new({name = name}, position)
        end

        self:error("Unexpected token")
    end

    ---@param name string
    ---@return FunctionCallExpressionNode
    function Parser:parseFunctionCallExpression(name)
        local position = self:getPosition()

        self:skipExceptedToken("(")
        local arguments = self:parseExpressionList(")")

        return Ast.FunctionCallExpressionNode:new({name = name, arguments = arguments}, position)
    end

    ---@param endToken TokenType | string
    ---@return ExpressionNode[]
    function Parser:parseExpressionList(endToken)
        local expressions = {}

        while self:currentTokenIs(",") and not self:currentTokenIs(endToken) do
            local expression = self:parseExpression()
            table.insert(expressions, expression)
        end

        self:skipExceptedToken(endToken)
        return expressions
    end
end

return Parser