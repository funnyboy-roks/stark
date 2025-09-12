extern fn printf(*i8 ...);

fn blackbox {}
fn assert(*i8 bool) {
    swap then {
        c"Assertion succeeded: %s  \x1b[32m✔️\x1b[0m\n" printf(2)
    } else {
        c"Assertion failed:    %s  \x1b[31m❌\x1b[0m\n" printf(2)
    }
}

fn main {
    c"\033[33mInteger -> Float:\033[0m\n" printf
    42              blackbox cast(f32)  42.0  = c"42        cast(f32)  == 42.0 " assert
    42              blackbox cast(f64)  42.0  = c"42        cast(f64)  == 42.0 " assert

    42    cast(i8)  blackbox cast(f32)  42.0  = c"42(i8)    cast(f32)  == 42.0 " assert
    42    cast(i8)  blackbox cast(f64)  42.0  = c"42(i8)    cast(f64)  == 42.0 " assert
    42    cast(u8)  blackbox cast(f32)  42.0  = c"42(u8)    cast(f32)  == 42.0 " assert
    42    cast(u8)  blackbox cast(f64)  42.0  = c"42(u8)    cast(f64)  == 42.0 " assert

    42    cast(i16) blackbox cast(f32)  42.0  = c"42(i16)   cast(f32)  == 42.0 " assert
    42    cast(i16) blackbox cast(f64)  42.0  = c"42(i16)   cast(f64)  == 42.0 " assert
    42    cast(u16) blackbox cast(f32)  42.0  = c"42(u16)   cast(f32)  == 42.0 " assert
    42    cast(u16) blackbox cast(f64)  42.0  = c"42(u16)   cast(f64)  == 42.0 " assert

    42    cast(i32) blackbox cast(f32)  42.0  = c"42(i32)   cast(f32)  == 42.0 " assert
    42    cast(i32) blackbox cast(f64)  42.0  = c"42(i32)   cast(f64)  == 42.0 " assert
    42    cast(u32) blackbox cast(f32)  42.0  = c"42(u32)   cast(f32)  == 42.0 " assert
    42    cast(u32) blackbox cast(f64)  42.0  = c"42(u32)   cast(f64)  == 42.0 " assert

    42    cast(i64) blackbox cast(f64)  42.0  = c"42(i64)   cast(f64)  == 42.0 " assert
    42    cast(i64) blackbox cast(f64)  42.0  = c"42(i64)   cast(f64)  == 42.0 " assert
    42    cast(u64) blackbox cast(f64)  42.0  = c"42(u64)   cast(f64)  == 42.0 " assert
    42    cast(u64) blackbox cast(f64)  42.0  = c"42(u64)   cast(f64)  == 42.0 " assert

    c"\033[33mFloat -> Integer:\033[0m\n" printf
    3.14            blackbox cast(i8)   3     = c"3.14      cast(i8)   == 3    " assert
    3.14            blackbox cast(u8)   3     = c"3.14      cast(u8)   == 3    " assert
    3.14            blackbox cast(i16)  3     = c"3.14      cast(i16)  == 3    " assert
    3.14            blackbox cast(u16)  3     = c"3.14      cast(u16)  == 3    " assert
    3.14            blackbox cast(i32)  3     = c"3.14      cast(i32)  == 3    " assert
    3.14            blackbox cast(u32)  3     = c"3.14      cast(u32)  == 3    " assert
    3.14            blackbox cast(i64)  3     = c"3.14      cast(i64)  == 3    " assert
    3.14            blackbox cast(u64)  3     = c"3.14      cast(u64)  == 3    " assert

    3.14  cast(f32) blackbox cast(i8)   3     = c"3.14(f32) cast(i8)   == 3    " assert
    3.14  cast(f32) blackbox cast(u8)   3     = c"3.14(f32) cast(u8)   == 3    " assert
    3.14  cast(f32) blackbox cast(i16)  3     = c"3.14(f32) cast(i16)  == 3    " assert
    3.14  cast(f32) blackbox cast(u16)  3     = c"3.14(f32) cast(u16)  == 3    " assert
    3.14  cast(f32) blackbox cast(i32)  3     = c"3.14(f32) cast(i32)  == 3    " assert
    3.14  cast(f32) blackbox cast(u32)  3     = c"3.14(f32) cast(u32)  == 3    " assert
    3.14  cast(f32) blackbox cast(i64)  3     = c"3.14(f32) cast(i64)  == 3    " assert
    3.14  cast(f32) blackbox cast(u64)  3     = c"3.14(f32) cast(u64)  == 3    " assert

    3.14  cast(f64) blackbox cast(i8)   3     = c"3.14(f64) cast(i8)   == 3    " assert
    3.14  cast(f64) blackbox cast(u8)   3     = c"3.14(f64) cast(u8)   == 3    " assert
    3.14  cast(f64) blackbox cast(i16)  3     = c"3.14(f64) cast(i16)  == 3    " assert
    3.14  cast(f64) blackbox cast(u16)  3     = c"3.14(f64) cast(u16)  == 3    " assert
    3.14  cast(f64) blackbox cast(i32)  3     = c"3.14(f64) cast(i32)  == 3    " assert
    3.14  cast(f64) blackbox cast(u32)  3     = c"3.14(f64) cast(u32)  == 3    " assert
    3.14  cast(f64) blackbox cast(i64)  3     = c"3.14(f64) cast(i64)  == 3    " assert
    3.14  cast(f64) blackbox cast(u64)  3     = c"3.14(f64) cast(u64)  == 3    " assert

    c"\033[33mInteger <-> Bool:\033[0m\n" printf
    1               blackbox cast(bool) true  = c"1         cast(bool) == true " assert
    0               blackbox cast(bool) false = c"0         cast(bool) == false" assert
    true            blackbox cast(i32) 1      = c"true      cast(i32)  == 1    " assert
    false           blackbox cast(i32) 0      = c"false     cast(i32)  == 0    " assert





    c"\033[33mInteger -> Float (without blackbox):\033[0m\n" printf
    42              cast(f32)  42.0  = c"42        cast(f32)  == 42.0 " assert
    42              cast(f64)  42.0  = c"42        cast(f64)  == 42.0 " assert

    42    cast(i8)  cast(f32)  42.0  = c"42(i8)    cast(f32)  == 42.0 " assert
    42    cast(i8)  cast(f64)  42.0  = c"42(i8)    cast(f64)  == 42.0 " assert
    42    cast(u8)  cast(f32)  42.0  = c"42(u8)    cast(f32)  == 42.0 " assert
    42    cast(u8)  cast(f64)  42.0  = c"42(u8)    cast(f64)  == 42.0 " assert

    42    cast(i16) cast(f32)  42.0  = c"42(i16)   cast(f32)  == 42.0 " assert
    42    cast(i16) cast(f64)  42.0  = c"42(i16)   cast(f64)  == 42.0 " assert
    42    cast(u16) cast(f32)  42.0  = c"42(u16)   cast(f32)  == 42.0 " assert
    42    cast(u16) cast(f64)  42.0  = c"42(u16)   cast(f64)  == 42.0 " assert

    42    cast(i32) cast(f32)  42.0  = c"42(i32)   cast(f32)  == 42.0 " assert
    42    cast(i32) cast(f64)  42.0  = c"42(i32)   cast(f64)  == 42.0 " assert
    42    cast(u32) cast(f32)  42.0  = c"42(u32)   cast(f32)  == 42.0 " assert
    42    cast(u32) cast(f64)  42.0  = c"42(u32)   cast(f64)  == 42.0 " assert

    42    cast(i64) cast(f64)  42.0  = c"42(i64)   cast(f64)  == 42.0 " assert
    42    cast(i64) cast(f64)  42.0  = c"42(i64)   cast(f64)  == 42.0 " assert
    42    cast(u64) cast(f64)  42.0  = c"42(u64)   cast(f64)  == 42.0 " assert
    42    cast(u64) cast(f64)  42.0  = c"42(u64)   cast(f64)  == 42.0 " assert

    c"\033[33mFloat -> Integer (without blackbox):\033[0m\n" printf
    3.14            cast(i8)   3     = c"3.14      cast(i8)   == 3    " assert
    3.14            cast(u8)   3     = c"3.14      cast(u8)   == 3    " assert
    3.14            cast(i16)  3     = c"3.14      cast(i16)  == 3    " assert
    3.14            cast(u16)  3     = c"3.14      cast(u16)  == 3    " assert
    3.14            cast(i32)  3     = c"3.14      cast(i32)  == 3    " assert
    3.14            cast(u32)  3     = c"3.14      cast(u32)  == 3    " assert
    3.14            cast(i64)  3     = c"3.14      cast(i64)  == 3    " assert
    3.14            cast(u64)  3     = c"3.14      cast(u64)  == 3    " assert

    3.14  cast(f32) cast(i8)   3     = c"3.14(f32) cast(i8)   == 3    " assert
    3.14  cast(f32) cast(u8)   3     = c"3.14(f32) cast(u8)   == 3    " assert
    3.14  cast(f32) cast(i16)  3     = c"3.14(f32) cast(i16)  == 3    " assert
    3.14  cast(f32) cast(u16)  3     = c"3.14(f32) cast(u16)  == 3    " assert
    3.14  cast(f32) cast(i32)  3     = c"3.14(f32) cast(i32)  == 3    " assert
    3.14  cast(f32) cast(u32)  3     = c"3.14(f32) cast(u32)  == 3    " assert
    3.14  cast(f32) cast(i64)  3     = c"3.14(f32) cast(i64)  == 3    " assert
    3.14  cast(f32) cast(u64)  3     = c"3.14(f32) cast(u64)  == 3    " assert

    3.14  cast(f64) cast(i8)   3     = c"3.14(f64) cast(i8)   == 3    " assert
    3.14  cast(f64) cast(u8)   3     = c"3.14(f64) cast(u8)   == 3    " assert
    3.14  cast(f64) cast(i16)  3     = c"3.14(f64) cast(i16)  == 3    " assert
    3.14  cast(f64) cast(u16)  3     = c"3.14(f64) cast(u16)  == 3    " assert
    3.14  cast(f64) cast(i32)  3     = c"3.14(f64) cast(i32)  == 3    " assert
    3.14  cast(f64) cast(u32)  3     = c"3.14(f64) cast(u32)  == 3    " assert
    3.14  cast(f64) cast(i64)  3     = c"3.14(f64) cast(i64)  == 3    " assert
    3.14  cast(f64) cast(u64)  3     = c"3.14(f64) cast(u64)  == 3    " assert

    c"\033[33mInteger <-> Bool (without blackbox):\033[0m\n" printf
    1               cast(bool) true  = c"1         cast(bool) == true " assert
    0               cast(bool) false = c"0         cast(bool) == false" assert
    true            cast(i32) 1      = c"true      cast(i32)  == 1    " assert
    false           cast(i32) 0      = c"false     cast(i32)  == 0    " assert
}
