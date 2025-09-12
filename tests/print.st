extern fn printf(*i8 ...) -> (i32);

fn main {
    c"hello world\n" printf drop
    c"hello world 2\n" printf drop
    2 c"hello world %d\n" printf(2) drop
}
