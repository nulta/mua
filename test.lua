
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
    local counts = 0

    local function testParser(code)
        counts = counts + 1
        print("")
        print("")
        print("[ Test #" .. counts .. " ]")

        local tokens = Lexer.new(code):lex()

        -- print("[Tokens]")
        -- for _, token in ipairs(tokens) do
        --     print(token.bytes, token.line .. ":" .. token.column, token.type, token.value)
        -- end

        local ast = Parser.new(tokens):parse()

        local function printAst(ast, key, ident)
            if key == "position" then return end
            if type(key) == "number" then key = nil end

            ident = ident or 0
            key = key and (key .. ": ") or ""

            if type(ast) == "table" then
                if not next(ast) then
                    return print(("    "):rep(ident) .. key .. (ast.type and ast.type .. " " or "") .. "{}")
                end

                print(("    "):rep(ident) .. key .. (ast.type and ast.type .. " " or "") .. "{")

                if ast.left then
                    printAst(ast.left, "L", ident + 1)
                end
                if ast.operator then
                    printAst(ast.operator, "O", ident + 1)
                end
                if ast.right then
                    printAst(ast.right, "R", ident + 1)
                end

                for k,v in pairs(ast) do
                    if k ~= "left" and k ~= "operator" and k ~= "right" then
                        printAst(v, k, ident + 1)
                    end
                end

                print(("    "):rep(ident) .. "}")
            elseif type(ast) == "string" then
                print(("    "):rep(ident) .. key .. '"' .. ast .. '"')
            else
                print(("    "):rep(ident) .. key .. tostring(ast))
            end
        end

        printAst(ast)
    end

    local function exceptError(code)
        counts = counts + 1
        print("")
        print("")
        print("[ Test #" .. counts .. " ]")

        local tokens = Lexer.new(code):lex()
        local ok, err = pcall(function() Parser.new(tokens):parse() end)
        if not ok then
            print("Caught excepted error")
            print(err)
        else
            error("Excepted error, but the code was executed with no error")
        end
    end

    testParser("do do end end")
    exceptError("do do end end end")
    exceptError("do do do end")

    testParser("a = 1 + 1")
    testParser("a = 1+2*3")
    testParser("a = 1+2*3/4+5-6+7^8^9")

    testParser([[a = "hello" .. "world"]])
    testParser([===[a = "hello" .. [[hello]] .. [=[world]=] ]===])

    testParser("a, bb, ccc = aaa, 123, 'qwer'")

    testParser("hello_world(1,2,3,10+20*30)")
    testParser("hello_world(self, create_world())")
end
