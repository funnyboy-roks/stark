extern fn printf(*i8 ...) -> (i64);

fn main {
    0 1
    while dup 1000 < {
        dup2 drop c"%d\n" printf(2) drop
        dup2 drop +
        swap
    }
    drop drop
}
