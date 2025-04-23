extern fn exit(i64) -> ();
extern fn printf(ptr ...) -> (i64);
extern fn strlen(ptr) -> (i64);

69 c"hello world" strlen(1) c"hello world\n%ld\n%ld\n" printf(3) drop
