extern fn exit(i64) -> ();
extern fn printf(ptr ...) -> (i64);
extern fn strlen(ptr) -> (i64);

c"hello world" strlen 11 = ! c"hello world\n%ld\n" printf(2) drop
