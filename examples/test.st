extern fn printf(*i8 ...);

fn print(*i8) { printf }

fn main {
    c"hi" print
}
