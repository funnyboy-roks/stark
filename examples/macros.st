extern fn printf(*i8 ...);

macro #constant 420
macro #echo($num: literal) $num

macro #plus2 { 2 + }

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

    40 #plus2 #print_num

    #echo(400) #print_num
}
