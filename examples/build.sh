#!/bin/sh

set -xe

build() {
    exe=${1%.st}
    asm="$exe.s"
    obj="$exe.o"
    echo "Compiling '$exe'"

    cargo r -- -o "$asm" "$1"
    fasm "$asm" "$obj"
    gcc -no-pie -o "$exe" "$obj"
}

if [ -n "$1" ]; then
    build "$1"
else
    for file in $(find -maxdepth 1 -iname "*.st"); do
        build $file
    done
fi
