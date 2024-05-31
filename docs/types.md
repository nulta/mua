# Types

## Extension Types
- nil
- boolean
- number
    - Integer
        - PosInteger
        - NegInteger
        - `0`
    - `(not exhausitive)`
- string
- userdata
- function
    - Fn<@R, @P...>
        - PureFn<@R, @P...>
        - AsyncFn<@R, @P...>
        - `(not exhausitive)`
- thread
- table
    - Array<@T>
    - Record<@K, @V>
    - `(not exhausitive)`

## Virtual Types
- Never
- Vararg
    - Vararg<@T1>
    - Vararg<@T1, @T2>
    - Vararg<@T1, @T2, @T3>
    - Vararg<@T1, @T2, @T3, @T4>
    - and so on...
