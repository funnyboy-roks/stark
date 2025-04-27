extern fn printf(ptr ...) -> (i32);

fn quadruple(i64) -> (i64) {
    double double
}

fn double(i64) -> (i64) {
    dup c"hello %d\n" printf(2) drop
    dup +
}

5 double c"5 * 2 = %d\n" printf(2) drop
5 double double c"5 * 4 = %d\n" printf(2) drop
5 quadruple c"5 * 4 = %d\n" printf(2) drop
