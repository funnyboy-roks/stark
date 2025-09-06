extern fn printf(*i8 ...) -> (i64);

fn blackbox {}

fn main {
    5 2 + 7 = blackbox then {
        c"hello" printf
    } else {
        c"world" printf
    } drop
    c"\n" printf drop

    5 2 + 8 = then {
        c"hello" printf
    } else 1 2 = then {
        c"world" printf
    } else {
        c"goodbye" printf
    } drop
    c"\n" printf drop

    // Switch

    // 5 2 + switch {
    //     7 -> "hello" printf,
    //     8 -> {
    //         "world" printf
    //     }
    //     _ -> "die" printf,
    // }
}
