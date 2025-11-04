extern fn printf(*i8 ...);

// A function that can not be optimised through
fn blackbox {}

fn swap_test {
    // NOTE: printf ordering is to ensure the top of the stack is on the right
    1 2 blackbox swap c"    1 2 swap => Expected: 2 1, Actual: %2$d %1$d\n" printf(3)
    2 1 blackbox swap c"    2 1 swap => Expected: 1 2, Actual: %2$d %1$d\n" printf(3)
}

fn drop_test {
    // NOTE: printf ordering is to ensure the top of the stack is on the right
    1 2   blackbox drop c"    1 2   drop => Expected: 1,   Actual: %d\n" printf(2)
    1 2 3 blackbox drop c"    1 2 3 drop => Expected: 1 2, Actual: %2$d %1d\n" printf(3)
}

fn dup_test {
    1 2 blackbox dup c"    1 2 dup => Expected: 1 2 2, Actual: %3$d %2$d %1$d\n" printf(4)
    1   blackbox dup c"    1   dup => Expected: 1 1,   Actual: %2$d %1$d\n" printf(3)
}

fn dup2_test {
    1 2   blackbox dup(2) c"    1 2   dup(2) => Expected: 1 2 1 2,   Actual: %4$d %3$d %2$d %1$d\n" printf(5)
    1 2 3 blackbox dup(2) c"    1 2 3 dup(2) => Expected: 1 2 3 2 3, Actual: %5$d %4$d %3$d %2$d %1$d\n" printf(6)
}

fn not_test {
    false blackbox ! c"    false ! => Expected: true  (1), Actual: %d\n" printf(2)
    true  blackbox ! c"    true  ! => Expected: false (0), Actual: %d\n" printf(2)
}

fn and_bool_test {
    false false blackbox & c"    (false false &) => Expected: 0, Actual: %d\n" printf(2)
    false true  blackbox & c"    (false true  &) => Expected: 0, Actual: %d\n" printf(2)
    true  false blackbox & c"    (true  false &) => Expected: 0, Actual: %d\n" printf(2)
    true  true  blackbox & c"    (true  true  &) => Expected: 1, Actual: %d\n" printf(2)
}

fn or_bool_test {
    false false blackbox | c"    (false false |) => Expected: 0, Actual: %d\n" printf(2)
    false true  blackbox | c"    (false true  |) => Expected: 1, Actual: %d\n" printf(2)
    true  false blackbox | c"    (true  false |) => Expected: 1, Actual: %d\n" printf(2)
    true  true  blackbox | c"    (true  true  |) => Expected: 1, Actual: %d\n" printf(2)
}

fn xor_bool_test {
    false false blackbox ^ c"    (false false ^) => Expected: 0, Actual: %d\n" printf(2)
    false true  blackbox ^ c"    (false true  ^) => Expected: 1, Actual: %d\n" printf(2)
    true  false blackbox ^ c"    (true  false ^) => Expected: 1, Actual: %d\n" printf(2)
    true  true  blackbox ^ c"    (true  true  ^) => Expected: 0, Actual: %d\n" printf(2)
}

fn and_test {
    1 2 blackbox & c"    (1 2 &) => Expected: 0, Actual: %d\n" printf(2)
    1 1 blackbox & c"    (1 1 &) => Expected: 1, Actual: %d\n" printf(2)
    2 1 blackbox & c"    (2 1 &) => Expected: 0, Actual: %d\n" printf(2)
    2 3 blackbox & c"    (2 3 &) => Expected: 2, Actual: %d\n" printf(2)
    3 5 blackbox & c"    (3 5 &) => Expected: 1, Actual: %d\n" printf(2)
}

fn or_test {
    1 2 blackbox | c"    (1 2 |) => Expected: 3, Actual: %d\n" printf(2)
    1 1 blackbox | c"    (1 1 |) => Expected: 1, Actual: %d\n" printf(2)
    2 1 blackbox | c"    (2 1 |) => Expected: 3, Actual: %d\n" printf(2)
    2 3 blackbox | c"    (2 3 |) => Expected: 3, Actual: %d\n" printf(2)
    3 5 blackbox | c"    (3 5 |) => Expected: 7, Actual: %d\n" printf(2)
}

fn xor_test {
    1 2 blackbox ^ c"    (1 2 ^) => Expected: 3, Actual: %d\n" printf(2)
    1 1 blackbox ^ c"    (1 1 ^) => Expected: 0, Actual: %d\n" printf(2)
    2 1 blackbox ^ c"    (2 1 ^) => Expected: 3, Actual: %d\n" printf(2)
    2 3 blackbox ^ c"    (2 3 ^) => Expected: 1, Actual: %d\n" printf(2)
    3 5 blackbox ^ c"    (3 5 ^) => Expected: 6, Actual: %d\n" printf(2)
}

fn bitnot_test {
    0xfffffff0 cast(u32) blackbox ~ c"    (0xfffffff0 ~) => Expected: 0x0000000f, Actual: 0x%08x\n" printf(2)
    0xffffff0f cast(u32) blackbox ~ c"    (0xffffff0f ~) => Expected: 0x000000f0, Actual: 0x%08x\n" printf(2)
    0xfffff0ff cast(u32) blackbox ~ c"    (0xfffff0ff ~) => Expected: 0x00000f00, Actual: 0x%08x\n" printf(2)
    0xffff0fff cast(u32) blackbox ~ c"    (0xffff0fff ~) => Expected: 0x0000f000, Actual: 0x%08x\n" printf(2)
    0xfff0ffff cast(u32) blackbox ~ c"    (0xfff0ffff ~) => Expected: 0x000f0000, Actual: 0x%08x\n" printf(2)
    0xff0fffff cast(u32) blackbox ~ c"    (0xff0fffff ~) => Expected: 0x00f00000, Actual: 0x%08x\n" printf(2)
    0xf0ffffff cast(u32) blackbox ~ c"    (0xf0ffffff ~) => Expected: 0x0f000000, Actual: 0x%08x\n" printf(2)
    0x0fffffff cast(u32) blackbox ~ c"    (0x0fffffff ~) => Expected: 0xf0000000, Actual: 0x%08x\n" printf(2)
    0x00000000 cast(u32) blackbox ~ c"    (0x00000000 ~) => Expected: 0xffffffff, Actual: 0x%08x\n" printf(2)
    0xffffffff cast(u32) blackbox ~ c"    (0xffffffff ~) => Expected: 0x00000000, Actual: 0x%08x\n" printf(2)
    0x55555555 cast(u32) blackbox ~ c"    (0x55555555 ~) => Expected: 0xaaaaaaaa, Actual: 0x%08x\n" printf(2)
    0xaaaaaaaa cast(u32) blackbox ~ c"    (0xaaaaaaaa ~) => Expected: 0x55555555, Actual: 0x%08x\n" printf(2)
}

fn shl_test {
    1 1 blackbox << c"    (1 1 <<) => Expected: 2,   Actual: %d\n" printf(2)
    1 2 blackbox << c"    (1 2 <<) => Expected: 4,   Actual: %d\n" printf(2)
    2 2 blackbox << c"    (2 2 <<) => Expected: 8,   Actual: %d\n" printf(2)
    3 6 blackbox << c"    (3 6 <<) => Expected: 192, Actual: %d\n" printf(2)
}

fn shr_test {
    1   1 blackbox >> c"    (1   1 >>) => Expected: 0,   Actual: %d\n" printf(2)
    2   1 blackbox >> c"    (2   1 >>) => Expected: 1,   Actual: %d\n" printf(2)
    8   2 blackbox >> c"    (8   2 >>) => Expected: 2,   Actual: %d\n" printf(2)
    264 1 blackbox >> c"    (264 1 >>) => Expected: 132, Actual: %d\n" printf(2)
}

fn equal_test {
    1 2 blackbox = c"    (1 2 =) => Expected: 0, Actual: %d\n" printf(2)
    1 1 blackbox = c"    (1 1 =) => Expected: 1, Actual: %d\n" printf(2)
}

fn equal_test_float {
    1.0 2.0 blackbox = c"    (1.0 2.0 =) => Expected: 0, Actual: %d\n" printf(2)
    1.0 1.0 blackbox = c"    (1.0 1.0 =) => Expected: 1, Actual: %d\n" printf(2)
}

fn not_equal_test {
    1 2 blackbox != c"    (1 2 !=) => Expected: 1, Actual: %d\n" printf(2)
    1 1 blackbox != c"    (1 1 !=) => Expected: 0, Actual: %d\n" printf(2)
}

fn not_equal_test_float {
    1.0 2.0 blackbox != c"    (1.0 2.0 !=) => Expected: 1, Actual: %d\n" printf(2)
    1.0 1.0 blackbox != c"    (1.0 1.0 !=) => Expected: 0, Actual: %d\n" printf(2)
}

fn lt_test {
    1 2 blackbox < c"    (1 2 <) => Expected: 1, Actual: %d\n" printf(2)
    1 1 blackbox < c"    (1 1 <) => Expected: 0, Actual: %d\n" printf(2)
    2 1 blackbox < c"    (2 1 <) => Expected: 0, Actual: %d\n" printf(2)
}

fn lt_test_float {
    1.0 2.0 blackbox < c"    (1.0 2.0 <) => Expected: 1, Actual: %d\n" printf(2)
    1.0 1.0 blackbox < c"    (1.0 1.0 <) => Expected: 0, Actual: %d\n" printf(2)
    2.0 1.0 blackbox < c"    (2.0 1.0 <) => Expected: 0, Actual: %d\n" printf(2)
}

fn lte_test {
    1 2 blackbox <= c"    (1 2 <=) => Expected: 1, Actual: %d\n" printf(2)
    1 1 blackbox <= c"    (1 1 <=) => Expected: 1, Actual: %d\n" printf(2)
    2 1 blackbox <= c"    (2 1 <=) => Expected: 0, Actual: %d\n" printf(2)
}

fn lte_test_float {
    1.0 2.0 blackbox <= c"    (1.0 2.0 <=) => Expected: 1, Actual: %d\n" printf(2)
    1.0 1.0 blackbox <= c"    (1.0 1.0 <=) => Expected: 1, Actual: %d\n" printf(2)
    2.0 1.0 blackbox <= c"    (2.0 1.0 <=) => Expected: 0, Actual: %d\n" printf(2)
}

fn gt_test {
    1 2 blackbox > c"    (1 2 >) => Expected: 0, Actual: %d\n" printf(2)
    1 1 blackbox > c"    (1 1 >) => Expected: 0, Actual: %d\n" printf(2)
    2 1 blackbox > c"    (2 1 >) => Expected: 1, Actual: %d\n" printf(2)
}

fn gt_test_float {
    1.0 2.0 blackbox > c"    (1.0 2.0 >) => Expected: 0, Actual: %d\n" printf(2)
    1.0 1.0 blackbox > c"    (1.0 1.0 >) => Expected: 0, Actual: %d\n" printf(2)
    2.0 1.0 blackbox > c"    (2.0 1.0 >) => Expected: 1, Actual: %d\n" printf(2)
}

fn gte_test {
    1 2 blackbox >= c"    (1 2 >=) => Expected: 0, Actual: %d\n" printf(2)
    1 1 blackbox >= c"    (1 1 >=) => Expected: 1, Actual: %d\n" printf(2)
    2 1 blackbox >= c"    (2 1 >=) => Expected: 1, Actual: %d\n" printf(2)
}

fn gte_test_float {
    1.0 2.0 blackbox >= c"    (1.0 2.0 >=) => Expected: 0, Actual: %d\n" printf(2)
    1.0 1.0 blackbox >= c"    (1.0 1.0 >=) => Expected: 1, Actual: %d\n" printf(2)
    2.0 1.0 blackbox >= c"    (2.0 1.0 >=) => Expected: 1, Actual: %d\n" printf(2)
}

fn main() {
    c"swap_test\n"            printf swap_test
    c"drop_test\n"            printf drop_test
    c"dup_test\n"             printf dup_test
    c"dup2_test\n"            printf dup2_test
    c"not_test\n"             printf not_test

    c"and_bool_test\n"        printf and_bool_test
    c"or_bool_test\n"         printf or_bool_test
    c"xor_bool_test\n"        printf xor_bool_test

    c"and_test\n"             printf and_test
    c"or_test\n"              printf or_test
    c"xor_test\n"             printf xor_test

    c"bitnot_test\n"          printf bitnot_test
    c"shl_test\n"             printf shl_test
    c"shr_test\n"             printf shr_test

    c"equal_test\n"           printf equal_test
    c"equal_test_float\n"     printf equal_test_float
    c"not_equal_test\n"       printf not_equal_test
    c"not_equal_test_float\n" printf not_equal_test_float

    c"lt_test\n"              printf lt_test
    c"lt_test_float\n"        printf lt_test_float
    c"lte_test\n"             printf lte_test
    c"lte_test_float\n"       printf lte_test_float
    c"gt_test\n"              printf gt_test
    c"gt_test_float\n"        printf gt_test_float
    c"gte_test\n"             printf gte_test
    c"gte_test_float\n"       printf gte_test_float
}
