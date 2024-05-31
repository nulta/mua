- continue
- short functions: `fn(x) x+3 end`
    - returns last value
- `if` statement can be used as expression
    - returns last value (like Rust)
- `if` can have assignment inside exp
    - `if wep = ply?:GetWeapon() then ... end`
- function overloading
- ~~generic overloading?~~
    - `type Asdf<@T>` `type Asdf<number>`
- 

problems
- nil safety, how?
    - nullable type? like `number?` ?
    - enum? like `Maybe<number>` ?