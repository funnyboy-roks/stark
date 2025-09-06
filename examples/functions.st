extern fn printf(*i8 ...) -> (i32);

fn quadruple(i64) -> (i64) {
    double double
}

fn double(i64) -> (i64) {
    dup c"hello %d\n" printf(2) drop
    dup +
}

fn main {
    5 double c"5 * 2 = %d\n" printf(2) drop
    5 double double c"5 * 4 = %d\n" printf(2) drop
    5 quadruple c"5 * 4 = %d\n" printf(2) drop
}
