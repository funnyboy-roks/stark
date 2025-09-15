extern fn malloc(u64) -> (*void);
extern fn printf(*i8 ...);

fn strlen(*i8) -> (u64) {
    0
    while dup(2) + load 0 != {
        1 +
    }
    swap drop
}

fn main {
    c"hello" dup strlen swap c"strlen(\"%s\") = %d\n" printf(3)
    c"world" dup strlen swap c"strlen(\"%s\") = %d\n" printf(3)
    c"rust" dup strlen swap c"strlen(\"%s\") = %d\n" printf(3)
    c"hello world, how are you doing on this fine day?" dup strlen swap c"strlen(\"%s\") = %d\n" printf(3)
    c"" dup strlen swap c"strlen(\"%s\") = %d\n" printf(3)

    /*sizeof(i8)*/ 1 6 * malloc cast(*i8)
    dup 0 + c"h" load store
    dup 1 + c"e" load store
    dup 2 + c"l" load store
    dup 3 + c"l" load store
    dup 4 + c"o" load store
    dup 5 + c""  load store
    dup c"result = '%s'\n" printf(2)

    cast(*u32)
    // rust -> 72, 75, 73, 74 -> 0x74737572
    dup 0x74737572 cast(u32) store
    cast(*i8)
    dup 4 + c"" load store
    c"result = '%s'\n" printf(2)
}
