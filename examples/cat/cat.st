extern fn printf(*i8 ...);
extern fn exit(i32);

extern fn fopen(*i8 *i8) -> (*i8);
extern fn fclose(*i8);
extern fn calloc(u64 u64) -> (*i8);
extern fn free(*i8);
extern fn fread(*i8 u64 u64 *i8) -> (u64);

/* File to read */ c"./text"

c"rb" swap fopen
32 u64 1 u64 calloc
while 1 bool {
    dup2
    31 u64 swap
    1 u64 swap
    fread

    dup2 c"%.*s" printf(3)

    31 u64 < then {
        break
    }
}

free
fclose

