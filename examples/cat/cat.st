extern fn printf(*i8 ...);
extern fn exit(i32);

extern fn fopen(*i8 *i8) -> (*i8);
extern fn fclose(*i8);
extern fn calloc(u64 u64) -> (*i8);
extern fn free(*i8);
extern fn fread(*i8 u64 u64 *i8) -> (u64);

fn main {
    // FILE *i8 bytes

    /* File to read */ c"./text"

    c"rb" swap fopen
    32 1 calloc
    while true {
        dup(2)
        31 swap
        1 swap
        fread

        dup(2) c"%.*s" printf(3)

        31 < then {
            break
        }
    }

    free
    fclose
}
