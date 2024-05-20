-- Lua Lexer written in Lua

---@enum TokenType
local TokenType = {
    KEYWORD = "KEYWORD",
    NAME = "NAME",
    SYMBOL = "SYMBOL",
    NUMBER_LITERAL = "NUMBER_LITERAL",
    STRING_LITERAL = "STRING_LITERAL",
    EOF = "EOF",
}


-- Vanilla Lua keywords (always keywords)
local LuaKeywords = {
    ["and"] = true,
    ["break"] = true,
    ["do"] = true,
    ["else"] = true,
    ["elseif"] = true,
    ["end"] = true,
    ["false"] = true,
    ["for"] = true,
    ["function"] = true,
    ["goto"] = true,
    ["if"] = true,
    ["in"] = true,
    ["local"] = true,
    ["nil"] = true,
    ["not"] = true,
    ["or"] = true,
    ["repeat"] = true,
    ["return"] = true,
    ["then"] = true,
    ["true"] = true,
    ["until"] = true,
    ["while"] = true,
}


-- Lua and mua symbols
local Symbols = {
    ["..."] = true,
    [".."] = true,
    ["=="] = true,
    ["~="] = true,
    ["<="] = true,
    [">="] = true,
    ["<<"] = true,
    [">>"] = true,
    ["//"] = true,
    ["::"] = true,
    ["+"] = true,
    ["-"] = true,
    ["*"] = true,
    ["/"] = true,
    ["^"] = true,
    ["%"] = true,
    ["<"] = true,
    [">"] = true,
    ["="] = true,
    ["~"] = true,
    ["#"] = true,
    [","] = true,
    [":"] = true,
    [";"] = true,
    ["{"] = true,
    ["}"] = true,
    ["["] = true,
    ["]"] = true,
    ["("] = true,
    [")"] = true,
    ["."] = true,

    -- Mua
    -- ["??"] = true,
    -- ["?."] = true,
    -- ["?["] = true,
    -- ["?:"] = true,
    -- ["@"] = true,
    -- ["`"] = true,
    -- ["|"] = true,
    -- ["!"] = true,
}


local SymbolFirstChars = {}
for symbol in pairs(Symbols) do
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
local Lexer = {}


---@param source string
function Lexer.new(source)
    local self = setmetatable({}, { __index = Lexer })
    self.source = source
    self.line = 1
    self.column = 1
    self.bytes = 0
    self.tokens = {}
    self.currentToken = nil
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


---@param type TokenType
function Lexer:insertBlankToken(type)
    self:startToken(type)
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


function Lexer:lex()
    while not self:isEof() do
        local char = self:peek()

        if char == "\n" then
            self.line = self.line + 1
            self:pop()
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

        elseif SymbolFirstChars[char] then
            self:lexSymbol()

        else
            self:lexText()
        end
    end

    self:insertBlankToken(TokenType.EOF)
end


function Lexer:lexNumber()
    self:startToken(TokenType.NUMBER_LITERAL)

    local dot = false
    local exponent = false

    repeat
        ::continue::

        local char = self:peek()
        local isFirstChar = self.bytes + 1 == self.currentToken.bytes

        -- 0x, 0b, 0o
        if isFirstChar then
            ---@diagnostic disable-next-line: redefined-local
            local char = self:peek(2)
            if char:match("0[xXbBoO]") then
                self:pop(2)
                goto continue
            end
        end

        -- 123, 3.14, .5, 100_00_000, ...
        if char:match("%d") or char == "_" then
            -- do nothing
        elseif char == "." then
            self:assertf(not dot, "malformed number literal near %s", char)
            dot = true
        elseif char:match("[eE]") then
            self:assertf(not exponent, "malformed number literal near %s", char)
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
    if LuaKeywords[value] then
        self:switchToken(TokenType.KEYWORD)
    end

    self:endToken()
end


function Lexer:lexSymbol()
    self:startToken(TokenType.SYMBOL)

    local peek3 = self:peek(3)
    local peek2 = self:peek(2)
    local peek1 = self:peek(1)

    if Symbols[peek3] then
        self:pop(3)
    elseif Symbols[peek2] then
        self:pop(2)
    elseif Symbols[peek1] then
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


-- Test code
if 1 then
    local function testLexer(code)
        print("")
        local lex = Lexer.new(code)
        lex:lex()
        for _, token in ipairs(lex.tokens) do
            print(token.bytes, token.line .. ":" .. token.column, token.type, token.value)
        end
    end

    testLexer("for i=1, 100 do print(i or 3 .. 10) end")
    testLexer("for k,v in pairs(t) do print(k,v+v) end")
    testLexer[[
        local a = 10
        local b = 20
        local c=a+b
        print(c)
        function foo(x)
            print("Hello, World!" .. x .. '!')
        end
    ]]

    testLexer[====[
        [[Hello worl
        d hello wo
        rld]]

        [=[Hi!!
    hi
hi!]=];

    ]====]

    testLexer("nil")
end

return Lexer
