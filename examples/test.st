extern fn exit(u32) -> ();
extern fn printf(ptr ...) -> (i32);
extern fn strlen(ptr) -> (i32);

-1 u64 dup + dup c"%lu\n" printf(3) drop
511 i8 i32 dup c"%u %d\n" printf(3) drop

0 u32
c"hello world" strlen c"hello world\n%ld\n" printf(2) drop
exit
