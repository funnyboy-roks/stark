extern fn printf(ptr ...) -> (i64);
extern fn hello() -> ();

c"hello from stark!\n" printf drop
hello
