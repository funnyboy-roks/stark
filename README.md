# stark

A stack-based compiled programming language

## TODO

- [x] Compilation
- [ ] Loops
- [ ] Type system
    - [ ] Somehow validate that functions are using the correct args
    - [ ] Function overloading of sorts (maybe just for native functions,
      i.e., print)- both `10 print` and `"hello" print` should work
- [ ] Functions
- [ ] Macros
- [ ] Imports
- [x] Linking with libc
- [x] extern functions
- [ ] Better CLI
- [ ] structs
- [ ] pointers
- [x] c-strings (null-terminated strings)
    - `c"hello"` -> `"hello", 0`
- [ ] Auto drop (and other global directives):
    - `@auto_drop` at top of file or something
