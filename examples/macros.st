extern fn printf(*i8 ...);

macro #constant 420

macro #print_num {
    c"%d\n" printf(2)
}

macro #print($str: literal) {
    $str c"%s\n" printf(2)
}

fn main {
    #print(c"hello world")
    69 #print_num
    #constant #print_num
}
