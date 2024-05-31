
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