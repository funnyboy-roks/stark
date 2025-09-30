mod mod_test
mod libc

fn main {
    c"hello from main through libc::printf\n" libc::printf
    mod_test::hello
}
