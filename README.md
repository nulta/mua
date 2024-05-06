# Mua - Mostly Sweet Lua

Mua is a sweet and type-safe dialect to Lua. It is compiled to native Lua 5.1.

## Features
### Type-safeness
- 99% safe when used alone
- `function(x: number) -> number return x+1 end`
### Syntatic improvements
- `fn(x) x+1 end`
  - omitted type is inferred as `function<@T><@T+1>`
- `global` keyword: No more implict global variable declarations
  - variable declarations must use `local` or `global`
- 
### Semantic sugars
- `virtual` functions can be added on `table, function, thread`
  - e.g. `print:bind("[log]")` compiles to `_fn_bind(print, "[log]")`
### More safety
- `pure` functions
- `mut` arguments (for table and userdata which is mutable inside parameter)
### etc
- `table` extends to `Array<@T>` `Record<@K, @V>`
