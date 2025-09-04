extern fn printf(*i8 ...) -> (i64);

fn main {
    373242
    while dup 1 = ! {
        dup c"step: %d\n" printf(2) drop
        dup 2 % 1 = then {
            3 * 1 +
            dup c"odd -> %d\n" printf(2) drop
        } else {
            2 /
            dup c"even -> %d\n" printf(2) drop
        }
    }
    drop
}
