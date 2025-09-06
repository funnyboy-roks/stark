extern fn printf(*i8 ...) -> (i64);
extern fn hello() -> ();

fn main {
    c"hello from stark!\n" printf drop
    hello
}
