extern fn printf(*i8 ...) -> (i64);

fn main {
    0 1
    while dup 1000 < {
        dup(2) drop c"%d\n" printf(2) drop
        dup(2) drop +
        swap
    }
    drop drop
}
