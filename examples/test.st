extern fn printf(*i8 ...) -> (i32);

5 10
dup2 c"hello %d %d\n" printf(3) drop
drop drop
