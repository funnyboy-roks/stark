extern fn printf(*i8) -> (i32);

fn main {
    c"hello world" printf drop
}
