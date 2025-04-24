extern fn printf(ptr ...) -> (i64);

0 1
while dup 1000 < {
    dup2 + c"%d\n" printf(2) drop
    // a b a
    dup2 drop +
    dup c"[DBG] %d\n" printf(2) drop
    swap
}
drop drop
