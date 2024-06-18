-- Lua Lexer written in Lua

local Defs = require("compiler.definitions")
local TokenType = Defs.TokenType

local SymbolFirstChars = {}
for symbol in pairs(Defs.Symbols) do
    SymbolFirstChars[symbol:sub(1, 1)] = true
end


---@class Token
---@field type TokenType
---@field value string
---@field line number
---@field column number
---@field bytes number

---@alias TokenSequence Token[]


---@class Lexer
---@field source string
---@field line number
---@field column number
---@field bytes number
---@field tokens TokenSequence
---@field currentToken Token?
---@field filename string
local Lexer = {}


---@param source string
---@param filename string?
function Lexer.new(source, filename)
    local self = setmetatable({}, { __index = Lexer })
    self.source = source
    self.line = 1
    self.column = 1
    self.bytes = 0
    self.tokens = {}
    self.currentToken = nil
    self.filename = filename or "[?]"
    return self
end


---@overload fun(): string
---@overload fun(len: number): string
---@overload fun(offset: number, len: number): string
function Lexer:peek(offset, len)
    if not offset and not len then
        offset = 1
        len = 1
    end

    if not len then
        len = offset
        offset = 1
    end

    return self:sub(self.bytes + offset, self.bytes + offset + len - 1)
end


---@param len number?
---@return string
function Lexer:pop(len)
    len = len or 1
    self.bytes = self.bytes + len
    self.column = self.column + len
    return self:sub(self.bytes - len + 1, self.bytes)
end


function Lexer:isEof()
    return self:peek() == ""
end


function Lexer:sub(from, to)
    return self.source:sub(from, to)
end


---@param type TokenType
function Lexer:startToken(type)
    self:assertf(not self.currentToken, "internal error: unterminated token")

    self.currentToken = {
        type = type,
        value = "",
        line = self.line,
        column = self.column,
        bytes = self.bytes + 1,
    }
end

---@param type TokenType
function Lexer:switchToken(type)
    self:assertf(self.currentToken, "internal error: not started token")

    self.currentToken.type = type
end


function Lexer:getTokenValue()
    self:assertf(self.currentToken, "internal error: not started token")

    return self:sub(self.currentToken.bytes, self.bytes)
end


function Lexer:endToken()
    self:assertf(self.currentToken, "internal error: not started token")

    self.currentToken.value = self:getTokenValue()
    self.tokens[#self.tokens + 1] = self.currentToken
    self.currentToken = nil
end


function Lexer:insertEof()
    self:startToken(TokenType.EOF)
    self:endToken()
end


---@param message string
function Lexer:error(message)
    error(("%d:%d: Lexer error: %s"):format(self.line, self.column, message))
end


---@generic T
---@param condition T
---@param message string
---@param ... string | number
---@return T
function Lexer:assertf(condition, message, ...)
    return condition or self:error(message:format(...))
end


---@return TokenSequence
function Lexer:lex()
    while not self:isEof() do
        local char = self:peek()

        if char == "\n" then
            self.line = self.line + 1
            self.column = 1
            -- self:pop()
        end

        if char:match("%s") then
            -- Whitespaces
            self:pop()

        elseif char:match("%d") or (char == "." and self:peek(2,1):match("%d")) then
            self:lexNumber()

        elseif char == "\"" or char == "'" then
            self:lexString()

        elseif char == "[" and (self:peek(2) == "[[" or self:peek(2) == "[=") then
            self:lexString()

        elseif char == "-" and self:peek(2) == "--" then
            self:processComment()

        elseif SymbolFirstChars[char] then
            self:lexSymbol()

        else
            self:lexText()
        end
    end

    self:insertEof()

    return self.tokens
end


function Lexer:lexNumber()
    self:startToken(TokenType.NUMBER_LITERAL)

    local dot = false
    local exponent = false

    local isFirstChar = true
    local digits = "[0-9_]"
    local exponentSign = "[eE]"

    repeat
        -- 0x, 0b, 0o
        if isFirstChar then
            local prefix = self:peek(2):lower()

            if prefix == "0x" then
                digits = "[0-9a-fA-F_]"
                exponentSign = "[pP]"
                self:pop(2)

            elseif prefix == "0b" then
                digits = "[01_]"
                self:pop(2)

            elseif prefix == "0o" then
                digits = "[0-7_]"
                self:pop(2)
            end

            isFirstChar = false
        end

        local char = self:peek()

        -- 123, 3.14, .5, 100_00_000, ...
        if char:match(digits) then
            -- do nothing
        elseif char == "." then
            self:assertf(not dot, "malformed number near %s", char)
            dot = true
        elseif char:match(exponentSign) then
            self:assertf(not exponent, "malformed number near %s", char)
            exponent = true

            local nextChar = self:peek(2, 1)
            if nextChar:match("[+-]") then
                self:pop()
            end
        else
            break
        end

        self:pop()
    until self:isEof()

    self:endToken()
end


function Lexer:lexText()
    self:startToken(TokenType.NAME)

    repeat
        local char = self:peek()

        if SymbolFirstChars[char] or char:match("[%s~`!@#$%%^&*\\]") then
            break
        else
            self:pop()
        end
    until self:isEof()

    local value = self:getTokenValue()
    if Defs.LuaKeywords[value] then
        self:switchToken(TokenType.KEYWORD)
    end

    self:endToken()
end


function Lexer:lexSymbol()
    self:startToken(TokenType.SYMBOL)

    local peek3 = self:peek(3)
    local peek2 = self:peek(2)
    local peek1 = self:peek(1)

    if Defs.Symbols[peek3] then
        self:pop(3)
    elseif Defs.Symbols[peek2] then
        self:pop(2)
    elseif Defs.Symbols[peek1] then
        self:pop(1)
    else
        self:error("internal error: unknown symbol")
    end

    self:endToken()
end


function Lexer:lexString()
    self:startToken(TokenType.STRING_LITERAL)

    local delimiter = self:pop()
    local isLongString = delimiter == "["

    if isLongString then
        local eqCount = 0
        while self:peek() == "=" do
            eqCount = eqCount + 1
            self:pop()
        end

        self:assertf(self:pop() == "[", "internal error: malformed long string")
        delimiter = "]" .. ("="):rep(eqCount) .. "]"
    end

    while true do
        local char = self:pop()

        if isLongString then
            if char == "]" and self:peek(0, #delimiter) == delimiter then
                self:pop(#delimiter - 1)
                break
            end
        else
            if char == delimiter then
                break
            end

            if char == "\n" then
                self:error("unterminated string")
            end
        end

        if char == "\\" then
            self:pop()
        end

        if self:isEof() then
            self:error("unterminated string")
        end
    end

    self:endToken()
end


function Lexer:processComment()
    self:pop(2)
    if self:peek() == "[" then
        self:lexString()
        self.tokens[#self.tokens] = nil
    else
        while self:peek() ~= "\n" and not self:isEof() do
            self:pop()
        end
    end
end

return Lexer
