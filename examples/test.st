extern fn printf(*i8 ...) -> (i32);

fn foo(u32) {
    drop
}

fn foo() -> (u32) {
    0
}

fn main {
    5 10
    dup2 c"hello %d %d\n" printf(3) drop
    drop drop
}
