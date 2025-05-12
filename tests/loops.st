extern fn printf(*i8 ...) -> (i64);

5 while dup 0 = ! {
    dup c"%ld\n" printf(2) drop
    1 -
} drop

5 while dup bool {
    dup c"%ld\n" printf(2) drop
    1 -
} drop
