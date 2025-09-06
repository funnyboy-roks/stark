extern fn printf(*i8 ...) -> (i64);

fn main {
    // prints 5 4 3 2 1
    5 while dup 0 != {
        dup c"%ld\n" printf(2) drop
        1 -
    } drop

    5 while dup 0 != {
        dup c"%ld\n" printf(2) drop
        1 -
    } drop

    // 0 5 for {
    //     // loops 0..5
    // }
}
