
---@type metatable
local fnMeta = {}

---@class Fn
---@overload fun(tbl: table): Fn.FiniteIterator
local Fn = setmetatable({}, fnMeta)


-- Fn.Iterator
do
    ---@generic T
    ---@class Fn.Iterator<T>
    ---@overload fun(): any?
    Fn.Iterator = {}

    --- Initialize new Iterator.
    ---@generic T
    ---@param next fun(self: Fn.Iterator<T>): T
    ---@return Fn.Iterator
    ---@nodiscard
    function Fn.Iterator.new(next)
        local meta = {
            __call = next,
            __index = Fn.Iterator
        }

        return setmetatable({}, meta)
    end

    --- Returns null iterator. null iterator always returns nil.
    ---@return Fn.FiniteIterator
    function Fn.Iterator.null()
        return Fn.FiniteIterator.new(function() return nil end)
    end

    --- Maps iterator with given function.
    ---@generic T, U
    ---@param func fun(val: T): U
    ---@return Fn.Iterator
    ---@nodiscard
    function Fn.Iterator:map(func)
        return self.new(function()
            local x = self()
            if x == nil then return nil end
            return func(x)
        end)
    end

    --- Returns first item on iterator which passes given filter function.
    ---@generic T
    ---@param filter fun(val: T): boolean
    ---@return T?
    ---@nodiscard
    function Fn.Iterator:find(filter)
        for val in self do
            if filter(val) then return val end
        end
        return nil
    end

    --- Filters out values from iterator which given filter function returns false.
    ---@generic T
    ---@param filter fun(val: T): boolean
    ---@return Fn.Iterator
    ---@nodiscard
    function Fn.Iterator:filter(filter)
        return self.new(function()
            return self:find(filter)
        end)
    end

    --- Filters out duplicate values from iterator.
    ---@return Fn.Iterator
    ---@nodiscard
    function Fn.Iterator:unique()
        local values = {}
        return self:filter(function(x)
            if values[x] then return false end
            values[x] = true
            return true
        end)
    end

    --- Limits the count of items to be a maximum of given integer.
    ---@generic T
    ---@param limit integer
    ---@return Fn.FiniteIterator
    ---@nodiscard
    function Fn.Iterator:limit(limit)
        local count = 0
        return Fn.FiniteIterator.new(function()
            if count >= limit then return nil end
            count = count + 1
            return self()
        end)
    end

    --- Iterate on the item while given function returns true.
    ---@generic T
    ---@param test fun(T): boolean
    ---@return Fn.Iterator
    ---@nodiscard
    function Fn.Iterator:stopIf(test)
        return self.new(function()
            local val = self()
            if not test(val) then return nil end
            return val
        end)
    end

    --- Skips first n items from iterator.
    ---@generic T
    ---@param amount integer
    ---@return Fn.Iterator
    ---@nodiscard
    function Fn.Iterator:skip(amount)
        for _=1, amount do
            local x = self()
            if x == nil then
                return self:null()
            end
        end
        return self
    end

    --- Skips item while given function returns true with item.
    ---@generic T
    ---@param test fun(T): boolean
    ---@return Fn.Iterator
    ---@nodiscard
    function Fn.Iterator:skipWhile(test)
        local item = self:find(function(x) return not test(x) end)
        local first = true

        return self.new(function()
            if first then
                first = false
                return item
            else
                return self()
            end
        end)
    end

    --- Returns true if the iterator produces at least one item which is not false.
    ---@return boolean
    ---@nodiscard
    function Fn.Iterator:some()
        for x in self do
            if x ~= false then return true end
        end
        return false
    end

    --- Returns iterator and some data which can be used in place of ipairs().
    ---@return function
    ---@return Fn.Iterator
    ---@return integer
    function Fn.Iterator:ipairs()
        return function(next, count)
            local v = next()
            if v then
                return count + 1, v
            else
                return nil, nil
            end
        end, self, 0
    end
end


-- Fn.FiniteIterator
do
    ---@class Fn.FiniteIterator<T>: Fn.Iterator<T>
    Fn.FiniteIterator = setmetatable({}, {__index = Fn.Iterator})

    --- Initialize new FiniteIterator.
    ---@generic T
    ---@param next fun(self: Fn.FiniteIterator<T>): T
    ---@return Fn.FiniteIterator
    ---@nodiscard
    function Fn.FiniteIterator.new(next)
        local meta = {
            __call = next,
            __index = Fn.FiniteIterator
        }

        return setmetatable({}, meta)
    end

    --- Returns count of the iterations.
    ---@return integer
    ---@nodiscard
    function Fn.FiniteIterator:count()
        local count = 0
        for _ in self do
            count = count + 1
        end
        return count
    end

    --- Reduces an array to a single value, iterating through elements and accumulating a result.
    ---@generic T
    ---@param func fun(accumulated: T, current: T): T
    ---@param initialValue T?
    ---@return T?
    ---@nodiscard
    function Fn.FiniteIterator:reduce(func, initialValue)
        local accumulated = initialValue                     -- initialValue
        if accumulated == nil then accumulated = self() end  -- no initialValue?
        if accumulated == nil then return nil end            -- and iterator immediately died?

        for current in self do
            accumulated = func(accumulated, current)
        end

        return accumulated
    end

    --- Returns true if every items that the iterator produces are not false, **or if the iterator is empty.**
    ---@return boolean
    ---@nodiscard
    function Fn.FiniteIterator:every()
        for x in self do
            if x == false then return false end
        end
        return true
    end

    --- Returns the sum of items.
    ---@return number?
    ---@nodiscard
    function Fn.FiniteIterator:sum()
        return self:reduce(Fn:op("+"), 0)
    end

    --- Cast the iterator to a table.
    ---@generic T
    ---@return {[integer]: T}
    ---@nodiscard
    function Fn.FiniteIterator:table()
        local tbl = {}
        for x in self do
            table.insert(tbl, x)
        end
        return tbl
    end

    --- Cast the iterator to a string.
    ---@generic T
    ---@param seperator string?
    ---@return string
    ---@nodiscard
    function Fn.FiniteIterator:concat(seperator)
        return table.concat(self:table(), seperator)
    end

    ---Executes given function once for each elements of iterator, in order.
    ---@param func function
    ---@return nil
    function Fn.FiniteIterator:forEach(func)
        for x in self do
            func(x)
        end
    end

    --- Returns reversed iterator. It uses self:table() under the hood.
    ---@return Fn.FiniteIterator
    ---@nodiscard
    function Fn.FiniteIterator:reversed()
        local tbl = self:table()
        return Fn:range(#tbl, 1, -1):map( Fn:op(".", tbl) )
    end

    --- Returns sorted iterator. It uses self:table() under the hood.
    ---@return Fn.FiniteIterator
    ---@nodiscard
    function Fn.FiniteIterator:sorted(func)
        local tbl = self:table()
        table.sort(tbl, func)
        return Fn(tbl)
    end

    --- Zip multiple iterators together to create a new iterator.
    --- It's return value is a list of values from each iterator.
    --- The new iterator will stop when any of the given iterators stop.
    ---@param ... Fn.Iterator
    ---@return Fn.FiniteIterator
    function Fn.FiniteIterator:zip(...)
        local iterators = {self, ...}
        return Fn.FiniteIterator.new(function()
            local values = {}
            for _, iterator in ipairs(iterators) do
                local value = iterator()
                table.insert(values, value)
                if value == nil then return nil end
            end
            return values
        end)
    end

    --- Append multiple iterators together to create a new iterator.
    ---@param ... Fn.FiniteIterator
    ---@return Fn.FiniteIterator
    function Fn.FiniteIterator:append(...)
        local iterators = {self, ...}
        local current = 1
        return Fn.FiniteIterator.new(function()
            local value = iterators[current]()
            if value == nil then
                current = current + 1
                if current > #iterators then return nil end
                return iterators[current]()
            end
            return value
        end)
    end

    --- Prepend multiple iterators before this iterator to create a new iterator.
    ---@param ... Fn.FiniteIterator
    ---@return Fn.FiniteIterator
    function Fn.FiniteIterator:prepend(...)
        return (...):append(select(2, ...)):append(self)
    end

    ---@class Fn.InfiniteIterator<T>: Fn.Iterator<T>
    Fn.InfiniteIterator = setmetatable({}, {__index = Fn.Iterator})

    --- Initialize new InfiniteIterator.
    ---@generic T
    ---@param next fun(self: Fn.InfiniteIterator<T>): T
    ---@return Fn.InfiniteIterator
    ---@nodiscard
    function Fn.InfiniteIterator.new(next)
        local meta = {
            __call = next,
            __index = Fn.InfiniteIterator
        }

        return setmetatable({}, meta)
    end
end

-- Fn:range, Fn:counter
do
    --- Returns new FiniteIterator which creates numbers. Default value for `from` is 1.
    ---@param from number Default is 1. Inclusive.
    ---@param to number Required. Inclusive.
    ---@param step number? Default is 1.
    ---@return Fn.FiniteIterator
    ---@overload fun(self, to: number, _: nil, step: number): Fn.FiniteIterator
    ---@overload fun(self, to: number): Fn.FiniteIterator
    function Fn:range(from, to, step)
        assert(from, "bad argument #2 to Fn.range (number expected, got nil or false)")
        assert(step ~= 0, "bad argument #4 to Fn.range (step must not be 0)")

        step = step or 1
        if not to then
            to = from
            from = 1
        end

        local current = from
        return Fn.FiniteIterator.new(function()
            if 0 < step then
                if to < current then return nil end
            else
                if current < to then return nil end
            end

            local val = current
            current = current + step
            return val
        end)
    end

    --- Returns new InfiniteIterator which creates numbers.
    ---@param from number? Default to 1.
    ---@param step number? Default to 1.
    ---@return Fn.InfiniteIterator
    function Fn:counter(from, step)
        from = from or 1
        step = step or 1

        local current = from
        return Fn.InfiniteIterator.new(function()
            local val = current
            current = current + step
            return val
        end)
    end

    --- Returns new InfiniteIterator which repeats through given list.
    --- Return a null iterator if `#list == 0`.
    ---@param list table<integer, any>
    ---@return Fn.InfiniteIterator
    function Fn:repeater(list)
        if #list == 0 then
            ---@type Fn.InfiniteIterator
            return Fn.Iterator.null()
        end

        local current = 0
        return Fn.InfiniteIterator.new(function()
            current = (current) % #list + 1
            return list[current]
        end)
    end

    --- Returns new InfiniteIterator which creates random numbers.
    --- - `Fn:urandom()`: Returns random float iterator within the range `[0, 1)`.
    --- - `Fn:urandom(m)`: Returns random integer iterator within the range `[1, m]`.
    --- - `Fn:urandom(m, n)`: Returns random integer iterator within the range `[m, n]`.
    ---@param m integer?
    ---@param n integer?
    ---@param randFunc? fun(m: integer?, n: integer?): number RNG function to use instead of math.random.
    ---@return Fn.InfiniteIterator
    function Fn:urandom(m, n, randFunc)
        randFunc = randFunc or math.random
        -- return Fn.InfiniteIterator.new( Fn:bindSeal(randFunc, m, n) )
        return Fn.InfiniteIterator.new( function() return randFunc(m,n) end )
    end

end

-- Fn:op("+")
do
    ---@alias Fn.opSymbol
    ---| "+"
    ---| "-"
    ---| "*"
    ---| "/"
    ---| "%"
    ---| ".."
    ---| "^"
    ---| "=="
    ---| "~="
    ---| "<="
    ---| ">="
    ---| "<"
    ---| ">"
    ---| "and"
    ---| "or"
    ---| "."

    ---@alias Fn.opSymbolUnary
    ---| "not"
    ---| "neg"

    local operators = {
        ["+"]  = function(a, b) return a + b end,
        ["-"]  = function(a, b) return a - b end,
        ["*"]  = function(a, b) return a * b end,
        ["/"]  = function(a, b) return a / b end,
        ["%"]  = function(a, b) return a % b end,
        [".."] = function(a, b) return a .. b end,
        ["^"]  = function(a, b) return a ^ b end,
        ["=="] = function(a, b) return a == b end,
        ["~="] = function(a, b) return a ~= b end,
        ["<="] = function(a, b) return a <= b end,
        [">="] = function(a, b) return a >= b end,
        ["<"]  = function(a, b) return a < b end,
        [">"]  = function(a, b) return a > b end,
        ["and"] = function(a, b) return a and b end,
        ["or"] = function(a, b) return a or b end,
        ["."] = function(a, b) return a[b] end,
        ["not"] = function(a) return not a end,
        ["neg"] = function(a) return -a end,
    }

    --- Returns operator as a function, with filled parameter
    ---@overload fun(self, symbol: Fn.opSymbol                ): fun(a: any, b: any): any
    ---@overload fun(self, symbol: Fn.opSymbol, a: any        ): fun(b: any): any
    ---@overload fun(self, symbol: Fn.opSymbol, _: nil, b: any): fun(a: any): any
    ---@overload fun(self, symbol: Fn.opSymbolUnary           ): fun(a: any): any
    function Fn:op(symbol, a, b)
        local opfunc = operators[symbol]
        if not opfunc then error("bad argument #2 to Fn.op (opSymbol expected)") end

        if a == nil and b == nil then
            return opfunc
        elseif a == nil then
            return function(x) return opfunc(x, b) end
        elseif b == nil then
            return function(x) return opfunc(a, x) end
        else
            local c = opfunc(a, b)
            return function() return c end
        end
    end

    --- Returns negated function.
    ---@param func fun(any...): boolean
    ---@return fun(any...): boolean
    function Fn:negate(func)
        return function(...)
            return not func(...)
        end
    end

    --- Returns function with prefilled arguments.
    ---@generic T1, T2, R
    ---@param func fun(a: T1, ...: T2): R
    ---@param a T1
    ---@param ... T2?
    ---@return fun(...: T2): R
    function Fn:bind(func, a,...)
        local args1 = table.pack(a, ...)
        return function(...)
            local args2 = table.pack(...)
            local argsTbl = {}
            argsTbl.n = args1.n + args2.n

            for i=1, args1.n do argsTbl[i] = args1[i] end
            for j=1, args2.n do argsTbl[j + args1.n] = args2[j] end

            return func(unpack(argsTbl, 1, argsTbl.n))
        end
    end

    --- Returns new function with prefilled arguments, which accepts no other arguments.
    ---@generic T, R
    ---@param func fun(...: T): R
    ---@param ... T
    ---@return fun(): R
    function Fn:bindSeal(func, ...)
        local args = table.pack(...)
        return function()
            return func(unpack(args, 1, args.n))
        end
    end


end

-- Fn(arg)
do
    --- Cast the given argument into matching Fn type.
    --- - table -> Fn.FiniteIterator
    --- - string -> Fn.Utf8String
    ---@param arg table
    function fnMeta:__call(arg)
        if type(arg) == "table" then
            return Fn:range(#arg):map(function(i) return arg[i] end)
        end

        error("bad argument #2 to fnMeta.__call (table expected)")
    end
end

return Fn
