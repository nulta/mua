local VERBOSE = false


print("")
print("=== Test: compiler.lexer ===")
local Lexer = require("compiler.lexer")
do
    local counts = 0
    local function testLexer(code)
        counts = counts + 1
        print("")
        print("[ Test #" .. counts .. " ]")

        local tokens = Lexer.new(code):lex()

        if VERBOSE then
            for _, token in ipairs(tokens) do
                print(token.bytes, token.line .. ":" .. token.column, token.type, token.value)
            end
        else
            if not code:find("\n") then
                print("  " .. code)
            end
            print("> " .. #tokens .. " tokens")
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

    testLexer("true -- this is a comment 123456")

    testLexer("3 3.0 3.1416 314.16e-2 0.31416E1 0xff 0x0.1E 0xA23p-4 0X1.921FB54442D18P+1")
    testLexer("3_000_000 10_00_____00 10234E5_ 0x_4_404305")

    testLexer([===[
        -- this is a singleline comment
        123
        --[[this is a
        multiline
        comment]]--
        456
    ]===])

end


local Parser = require("compiler.parser")
print("")
print("=== Test: compiler.parser ===")
print("")
do
    local counts = 0

    local function testParser(code, filename)
        counts = counts + 1
        print("")
        print("")
        print("[ Test #" .. counts .. " ]")
        if not code:find("\n") then
            print("  " .. code)
        elseif filename then
            print(": " .. filename)
        end

        local tokens = Lexer.new(code, filename):lex()

        if VERBOSE then
            print("[Tokens]")
            for _, token in ipairs(tokens) do
                print(token.bytes, token.line .. ":" .. token.column, token.type, token.value)
            end
        end

        local ast = Parser.new(tokens, filename):parse()

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

        if VERBOSE then
            printAst(ast, "AST")
        end
    end

    local function exceptError(code)
        counts = counts + 1
        print("")
        print("")
        print("[ Test #" .. counts .. " ]")
        if not code:find("\n") then
            print("> " .. code)
        end

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
    testParser("hello_world()")
    testParser("hello_world(self, create_world())")

    testParser([==[
debug = require "debug"

assert(x == 'a\0a' and string.len(x) == 3)

assert('\n\"\'\\' == [[

"'\]])
    ]==])

    testParser([=[
local function exceptError(code)
counts = counts + 1
print("")  --[[this is blockwide
  commen
ts]]--
print("")
print("[ Test #" .. counts .. " ]")
if not code:find("\n") then
    print("> " .. code)
end
local tokens = Lexer.new(code):lex()
local ok, err = pcall(function() Parser.new(tokens):parse() end)
if not ok then
    print("Caught excepted error")
    print(err)
else
    error("Excepted error, but the code was executed with no error")
end
end
    ]=])

    testParser([=[
        local bindingPower = {
            ["^"]  = {121, 120}, -- #1 (left associative)
            ["*"]  = {110}, -- #2
            ["/"]  = {110},
            ["//"] = {110},
            ["%"]  = {110},
            ["+"]  = {100},
            ["-"]  = {100},
            [".."] = {91, 90}, -- #3 (left associative)
            ["<<"] = {80}, -- #4
            [">>"] = {80},
            ["<"]  = {70}, -- #5
            [">"]  = {70},
            ["<="] = {70},
            [">="] = {70},
            ["=="] = {60}, -- #6
            ["~="] = {60},
            ["and"] = {30}, -- #7
            ["or"] = {20}, -- #8
        }
    ]=])

    testParser(io.open("docs/testsuite_literals.lua"):read("*a"), "docs/testsuite_literals.lua")
end


print("")
print("=== Test: compiler.retranslator ===")
print("")
do
    local Retrans = require("compiler.retranslator")

    local counts = 0

    local function newTest(code, filename)
        counts = counts + 1
        print("")
        print("")
        print("[ Test #" .. counts .. " ]")
        if not code:find("\n") then
            print("  " .. code)
        elseif filename then
            print(": " .. filename)
        end
    end

    local function retranslate(code, filename)
        local tokens = Lexer.new(code, filename):lex()
        local ast = Parser.new(tokens, filename):parse()
        local retrans = Retrans:retranslate(ast)
        return retrans
    end

    local function testRetrans(code, filename)
        newTest(code, filename)

        -- equality test on repeated retranslation
        local retransed = retranslate(code, filename)
        print("\n[Retrans]")
        if not VERBOSE and #retransed > 100 then
            print("  " .. retransed:sub(1, 100) .. " ...")
        else
            print("  " .. retransed)
        end

        assert(retranslate(retransed) == retransed, "retrans(retrans) should be equal with the retrans: " .. retranslate(retransed))

        -- virtual environment
        local fenv1 = {
            print = function() end,
            os = {clock = os.clock, time = os.time},
            debug = {traceback = debug.traceback},
        }
        fenv1._G = fenv1
        fenv1.dostring = function(x) return load(x, nil, nil, fenv1)() end
        fenv1.load = function(x, y, z) return load(x, y, z, fenv1) end
        setmetatable(fenv1, {__index = _G})

        local fenv2 = {
            print = function() end,
            os = {clock = os.clock, time = os.time},
            debug = {traceback = debug.traceback},
        }
        fenv2._G = fenv2
        fenv2.dostring = function(x) return load(x, nil, nil, fenv2)() end
        fenv2.load = function(x, y, z) return load(x, y, z, fenv2) end
        setmetatable(fenv2, {__index = _G})

        -- return value equality test with the original code
        local originalFunc, failOriginal = load(code, nil, nil, fenv1)
        local retransFunc, failRetrans = load(retranslate(code), nil, nil, fenv2)

        if originalFunc then
            assert(retransFunc, "load(retranslate) should not error: " .. (failRetrans or "?"))

            local okO, originalVal = pcall(originalFunc)
            local okR, retransVal = pcall(retransFunc)
            if okO ~= okR then retransFunc() end
            assert(okO == okR, "Original function and retranslated function should return equal status: " .. tostring(originalVal) .. " ~= " .. tostring(retransVal))

            if not okO then
                -- remove original string from error message
                originalVal = originalVal:gsub("%[string \".+\"%]:%d+:", "[string]")
                retransVal = retransVal:gsub("%[string \".+\"%]:%d+:", "[string]")
            end

            if type(originalVal) == "table" then
                originalVal = table.concat(originalVal, ",")
                retransVal = table.concat(retransVal, ",")
            end

            assert(originalVal == retransVal, "Original function and retranslated function should return equal value: " .. tostring(originalVal) .. " ~= " .. tostring(retransVal))
            print("\n[Return]\n  " .. tostring(retransVal))
        else
            assert(failOriginal == failRetrans, "load(retranslate) error should be equal with errOriginal: " .. tostring(failOriginal) .. " ~= " .. tostring(failRetrans))
        end
    end

    local function testFile(filename)
        local code = io.open(filename)
        assert(code)
        testRetrans(code:read("*a"), filename)
        code:close()
    end


    testRetrans("do do end end")
    testRetrans("return 1 + 1")
    testRetrans("return 1+2*3")
    testRetrans("return 1+2*3/4+5-6+7^8^9")
    testRetrans([[a = "hello" .. "world"]])
    testRetrans([===[a = "hello" .. [[hello]] .. [=[world]=] ]===])
    testRetrans("a, bb, ccc = aaa, 123, 'qwer'")
    testRetrans("hello_world(1,2,3,10+20*30)")
    testRetrans("hello_world()")
    testRetrans("return math.sin(1)")
    testRetrans("hello_world(self, create_world())")


    testRetrans([==[
debug = require "debug"

assert(x == 'a\0a' and string.len(x) == 3)

assert('\n\"\'\\' == [[

"'\]])
    ]==])

    testRetrans([[if code() then print("> " .. code) end]])

    testRetrans([=[
local function exceptError(code)
counts = counts + 1
print("")  --[[this is blockwide
  commen
ts]]--
print("")
print("[ Test #" .. counts .. " ]")
if not code:find("\n") then
    print("> " .. code)
end
local tokens = Lexer.new(code):lex()
local ok, err = pcall(function() Parser.new(tokens):parse() end)
if not ok then
    print("Caught excepted error")
    print(err)
else
    error("Excepted error, but the code was executed with no error")
end
end

return true
    ]=])

    testRetrans([=[
        local bindingPower = {
            ["^"]  = {121, 120}, -- #1 (left associative)
            ["*"]  = {110}, -- #2
            ["/"]  = {110},
            ["//"] = {110},
            ["%"]  = {110},
            ["+"]  = {100},
            ["-"]  = {100},
            [".."] = {91, 90}, -- #3 (left associative)
            ["<<"] = {80}, -- #4
            [">>"] = {80},
            ["<"]  = {70}, -- #5
            [">"]  = {70},
            ["<="] = {70},
            [">="] = {70},
            ["=="] = {60}, -- #6
            ["~="] = {60},
            ["and"] = {30}, -- #7
            ["or"] = {20}, -- #8
        }

        local t = {}
        for k, v in pairs(bindingPower) do
            table.insert(t, k .. table.concat(v, ";"))
        end
        return table.concat(t, "//")
    ]=])

    testRetrans("return (1+6)/3*6^4-(55*4+32+(2^7+5)*43)+2^2^(0+2)")

    testRetrans("a = nothing ;(a or math.sin)(1) return math.sin(2);")

    testRetrans("a = function(b, ...) end")

    testFile("compiler/ast.lua")
    testFile("compiler/definitions.lua")
    testFile("compiler/lexer.lua")
    testFile("compiler/parser.lua")
    testFile("compiler/retranslator.lua")

    testFile("docs/testsuite_literals.lua")
end

print("")
print("")
print("== All tests passed ==")
print("")
