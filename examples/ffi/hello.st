extern fn printf(*i8 ...) -> (i64);
extern fn hello() -> ();

c"hello from stark!\n" printf drop
hello
