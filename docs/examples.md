# Examples of mua features

## new keywords & symbols
```
:   ->  =>  ?

fn  continue
const   pure    virtual
```

## local variable declaration inside if-statement
```lua
if obj = getObject(id) then obj:doSomething() end

-- compiles into:

do
    local obj = getObject(id)
    if obj then obj:doSomething() end
end
```

## short fn-functions
```lua
local a = ("hello?"):sort(fn(x, y) => x < y)
local b = ("hello?"):gsub(fn(x) => x:upper() == x)

local func = fn(x) => doSomething(x + 1)
local func2 = fn(x, y)
    local xx = x * x
    return xx + y
end

-- to:

local a = ("hello?"):sort(function(x, y) return x < y end)
local b = ("hello?"):gsub(function(x) return x:upper() == x end)

local func = function(x) return doSomething(x + 1) end
local func2 = function(x, y)
    local xx = x * x
    return xx + y
end
```

## *virtual* metamethods on function, string, boolean, etc
```lua
local condition = true
local a = condition:whether(10, 100)

-- to:

local condition = true
local a = condition and 10 or 100
```

## global variable declarations must use _G
```lua
-- Below code makes compile error:
--MyGlobal = {}
--function MyFunction() end

_G.MyGlobal = {}
function _G.MyFunction() end
```

## local const declarations
```lua
-- const keyword is considered local constant
const asdf = true
--asdf = false   -- compile error
```

## global const declarations
```lua
-- ALL_CAPS global variables are considered global constant
_G.MY_CONSTANT = 1234
```

## continue statement
```lua
for k,v in pairs(something) do
    if not v:isAlive() then continue end
    v:doSomething()
end
```

## Types!

### Type annotations
```lua
local a: string
local b: string = ""
local c: string? = nil

-- implict: _G.asdf is table
_G.asdf = {}
```

### non-impliable variable declarations must have type annotations
```lua
--local x  -- compile error

local x: string
x = ""
```

### non-nullable variables cannot be used before the initialization
```lua
-- OK:
local x: number
if condition then
    x = 123
else
    x = 1010
end
print(x)


-- ERROR:
local x: number
if condition then
    x = 123
end
print(x)
```

### function type annotations
```lua
-- f1: Fn<><>
local function f1() end

-- f2: Fn<number, number><>
local function f2(x: number, y: number)
    print("f2:", x + y)
end

-- f3: Fn<><number, number>
local function f3()
    return 13, 26
end

-- f4: Fn<number><number>
local function f4(x: number) -> number
    return x + 1
end

-- f5: Fn<Vararg<number>><string, Vararg<number>>
local function f5(...: number) -> string, number...
    return select(1, ...), ...
end
```

### generic functions and virtual functions
```lua
-- from default virtual functions

-- virtual table.map<K,V,T>:
--     PureMethodFn< table<K,V>, fn<V,K><T> >< SELF<K,T> >
virtual pure function table<K,V>:map<T>(func: fn<V,K><T>) -> SELF<K,T>
    -- self: table<K,V>
    local newTbl = {}

    for k, v in pairs(self) do
        newTbl[k] = func(v, k)
    end

    return newTbl
end

virtual pure function Array<V>:map<T>(func: fn<V,integer><R>) -> Array<T>
    return self:map(func)
end

virtual pure function Fn<A, _:any, ...><>:curry()
```

## pure functions!

```lua
-- pure functions:
-- 1. must not manipulate parameters (including self)
-- 2. must not read nor manipulate upvalues outside the function scope
-- 3. must not call other non-pure functions
-- 
-- pure functions could be 

local pure function add(a: number, b: number) -> number
    return a + b
end 
```

## function overloading
```lua
function foo(bar: number) -> number
    return bar + 1
end

function foo(bar: string) -> string
    return bar + "1"
end

print(foo(1))
```