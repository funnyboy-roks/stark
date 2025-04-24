extern fn printf(ptr ...) -> (i64);
// Hrmm...  if vs then?

// More traditional
5 2 + 7 = then {
    c"hello" printf
} else {
    c"world" printf
} drop
c"\n" printf drop

// Less confusing since the predicate comes _before_
5 2 + 7 = then {
    c"hello" printf
} else 1 2 = then {
    c"world" printf
} else {
    c"goodbye" printf
} drop
c"\n" printf drop

// Maybe _EITHER_ :eyes:

// Switch

// 5 2 + switch {
//     7 -> "hello" printf,
//     8 -> {
//         "world" printf
//     }
//     _ -> "die" printf,
// }
