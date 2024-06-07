
local Defs = {}

--- Token types, outputted by the lexer
---@enum TokenType
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

function Defs.getPrecedence(symbol)
    if symbol == "^" then
        return 12
    elseif symbol == "*" or symbol == "/" or symbol == "//" or symbol == "%" then
        return 11
    elseif symbol == "+" or symbol == "-" then
        return 10
    elseif symbol == ".." then
        return 9
    elseif symbol == "<<" or symbol == ">>" then
        return 8
    elseif symbol == "<" or symbol == ">" or symbol == "<=" or symbol == ">=" then
        return 7
    elseif symbol == "==" or symbol == "~=" then
        return 6
    elseif symbol == "and" then
        return 3
    elseif symbol == "or" then
        return 2
    else
        return -1
    end
end

return Defs
