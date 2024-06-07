
print("")
print("=== Test: compiler.lexer ===")
print("")
local Lexer = require("compiler.lexer")
do
    local function testLexer(code)
        print("")
        local tokens = Lexer.new(code):lex()
        for _, token in ipairs(tokens) do
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

local Parser = require("compiler.parser")
print("")
print("=== Test: compiler.parser ===")
print("")
do
    local function testParser(code)
        print("")
        local tokens = Lexer.new(code):lex()
        local ast = Parser.new(tokens):parse()
        if _G["p"] then
            -- Luvit extension
            for k,v in ipairs(ast) do _G["p"](k) _G["p"](v) end
        else
            for k,v in ipairs(ast) do print(k, v) end
        end
    end

    local function exceptError(code)
        print("")
        local tokens = Lexer.new(code):lex()
        local ok, err = pcall(function() Parser.new(tokens):parse() end)
        if not ok then
            print("Caught excepted error", err)
        else
            error("Excepted error, but the code was executed with no error")
        end
    end

    testParser("do do end end")
    exceptError("do do end end end")
    exceptError("do do do end")

end