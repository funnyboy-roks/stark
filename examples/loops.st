extern fn printf(ptr ...) -> (i64);

// prints 5 4 3 2 1
5 while dup 0 = ! {
    dup "%d\n" printf(2) drop
    1 -
}

// 0 5 for {
//     // loops 0..5
// }
