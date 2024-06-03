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

return Parser