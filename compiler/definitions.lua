
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

Defs.MuaKeywords = {
    ["continue"] = true,
    ["fn"] = true,
}

Defs.Keywords = {}

for k,_ in pairs(Defs.LuaKeywords) do
    Defs.Keywords[k] = true
end
for k,_ in pairs(Defs.MuaKeywords) do
    Defs.Keywords[k] = true
end


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
}

Defs.Symbols = {}
for k,_ in pairs(Defs.LuaSymbols) do
    Defs.Symbols[k] = true
end
for k,_ in pairs(Defs.MuaSymbols) do
    Defs.Symbols[k] = true
end

return Defs
