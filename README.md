# stark

A stack-based compiled programming language

## Examples

```zig
extern fn printf(*i8 ...) -> (i64);

"hello world" printf
```

```zig
// 5 4 3 2 1
5 while dup 0 = ! {
    dup c"%d\n" printf(2) drop
    1 -
} drop
```

## TODO

- [x] Compilation
- [x] Loops
    - [ ] Labels: `'x` `while'x` (I like the idea of having this "tick" syntax like rust)
    - [ ] break: `break` or `break'x` -- still need to check that the stack has not changed
- [x] Conditionals (see [conditionals.st](./examples/conditional.st))
    - [x] `then`
    - [x] `else`
    - [ ] `switch`? 
- [ ] Error handling
    - [ ] Report good type errors
    - [ ] Continue after error and report all at once
- [ ] Non-decimal integer literals
- [x] Full suite of numbers: i8, i16, i32, i64, u8, u16, u32, u64
- [x] Miette + thiserror for better errors
- [x] Type system
    - [x] Somehow validate that functions are using the correct args
- [ ] Functions
    - [x] Extern functions
        - `extern fn strlen(*i8) -> (i64);`
        - [ ] Ability to change the linker symbol for this function- not
          sure on the syntax yet:
          ```zig
          extern fn strlen(*i8) -> (i32) @extern("strlen");
          extern("strlen") fn strlen(*i8) -> (i32);
          extern fn strlen(*i8) -> (i32) @ "strlen";
          ```
    - [x] User-defined functions
        - `fn double(i64) -> (i64) { dup + }`
        - type check function: stack = args, compile body, assert(stack = results)
        - [ ] Need to see if we can clean up the generated assembly
        - [ ] Ideally use linux calling convention so we can export the
          functions for use from other langauges
- [ ] Macros
- [ ] Modules
    - [ ] Imports
    - [ ] Namespaces
- [x] Linking with libc
- [ ] Better CLI
- [ ] structs
- [ ] pointers
    - [x] Typed pointers
        - [ ] Smarter pointer increments (like C)
    - [ ] Fat pointers for things like strings
- [x] c-strings (null-terminated strings)
    - `c"hello"` -> `"hello", 0`
- [ ] Auto drop (and other global directives):
    - `@auto_drop` at top of file or something
