extern fn printf(*i8 ...) -> (i32);

fn fizzbuzz(i64) {
    dup 15 % 0 = then {
        c"fizzbuzz\n" printf drop
    } else dup 3 % 0 = then {
        c"fizz\n" printf drop
    } else dup 5 % 0 = then {
        c"buzz\n" printf drop
    } else {
        dup c"%d\n" printf(2) drop
    }
    drop
}

fn main {
    0 while dup 50 = ! {
        dup fizzbuzz
        1 +
    } drop
}
