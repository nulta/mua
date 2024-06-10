
local Defs = {}

--- Token types, outputted by the lexer
---@enum TokenType
---@class _TokenType
Defs.TokenType = {
    KEYWORD = "KEYWORD",
    NAME = "NAME",
    SYMBOL = "SYMBOL",
    NUMBER_LITERAL = "NUMBER_LITERAL",
    STRING_LITERAL = "STRING_LITERAL",
    EOF = "EOF",
}


--- Vanilla Lua keywords (always keywords)
Defs.LuaKeywords = {
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


--- Lua symbols
Defs.LuaSymbols = {
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
}

Defs.MuaSymbols = {
    ["??"] = true,
    ["?."] = true,
    ["?["] = true,
    ["?:"] = true,
    ["@"] = true,
    ["`"] = true,
    ["|"] = true,
    ["!"] = true,
}

Defs.Symbols = {}
for k,_ in pairs(Defs.LuaSymbols) do
    Defs.Symbols[k] = true
end
for k,_ in pairs(Defs.MuaSymbols) do
    Defs.Symbols[k] = true
end

--- Returns the left-binding power and right-binding power of a operator.
---@param symbol string
---@return integer, integer
function Defs.getPrecedence(symbol)
    if symbol == "^" then
        return 121, 120
    elseif symbol == "*" or symbol == "/" or symbol == "//" or symbol == "%" then
        return 110, 111
    elseif symbol == "+" or symbol == "-" then
        return 100, 101
    elseif symbol == ".." then
        return 91, 90
    elseif symbol == "<<" or symbol == ">>" then
        return 80, 81
    elseif symbol == "<" or symbol == ">" or symbol == "<=" or symbol == ">=" then
        return 70, 71
    elseif symbol == "==" or symbol == "~=" then
        return 60, 61
    elseif symbol == "and" then
        return 30, 31
    elseif symbol == "or" then
        return 20, 21
    else
        return 0, 0
    end
end

return Defs
