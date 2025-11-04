extern fn printf(*i8 ...) -> (i64);
extern("printf") fn printf2(*i8 ...) -> (i64);
extern fn hello;

fn main {
    c"hello from stark!\n" printf drop
    c"hello from stark with custom linker name!\n" printf2 drop
    hello
}
