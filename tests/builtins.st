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
