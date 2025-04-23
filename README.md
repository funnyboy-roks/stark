# stark

A stack-based compiled programming language

## TODO

- [x] Compilation
- [ ] Loops
- [ ] Conditionals (see [conditionals.st](./examples/conditional.st))
    - [ ] `if` or `then`
    - [ ] `else`
    - [ ] `switch`? 
- [ ] Non-decimal integer literals
- [ ] Full suite of numbers: i8, i16, i32, i64, u8, u16, u32, u64
- [ ] Miette + thiserror for better errors
- [ ] Type system
    - [ ] Somehow validate that functions are using the correct args
    - [ ] Function overloading of sorts (maybe just for native functions,
      i.e., print)- both `10 print` and `"hello" print` should work
- [ ] Functions
- [ ] Macros
- [ ] Modules
    - [ ] Imports
    - [ ] Namespaces
- [x] Linking with libc
- [x] extern functions
- [ ] Better CLI
- [ ] structs
- [ ] pointers
    - [ ] Typed pointers
        - [ ] Smarter pointer increments (like C)
    - [ ] Fat pointers for things like strings
- [x] c-strings (null-terminated strings)
    - `c"hello"` -> `"hello", 0`
- [ ] Auto drop (and other global directives):
    - `@auto_drop` at top of file or something
