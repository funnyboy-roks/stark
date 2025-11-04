#!/bin/sh

set -xe

TARGETS="
collatz
conditional
fib
fizzbuzz
functions
loops
pointers
test
cat/cat
ffi/hello
raylib/japan
"

if [ -n "$1" ]; then
    make "$1"
else
    init_dir=$(pwd)
    for target in $TARGETS; do
        if [ -n $(dirname $target) ]; then
            cd $(dirname $target)
        fi
        make -B $(basename $target)
        cd $init_dir
    done
fi
