```c
/* ORDER RESERVED */
static const char *const luaX_tokens [] = {
    "and", "break", "do", "else", "elseif",
    "end", "false", "for", "function", "goto", "if",
    "in", "local", "nil", "not", "or", "repeat",
    "return", "then", "true", "until", "while",
    "//", "..", "...", "==", ">=", "<=", "~=",
    "<<", ">>", "::", "<eof>",
    "<number>", "<integer>", "<name>", "<string>"
};

// From Lua 5.4 llex.c
```


## Features
### Type-safeness
- 99% safe when used alone
- `function(x: number) -> number return x+1 end`
- Enforced nil-safety
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
