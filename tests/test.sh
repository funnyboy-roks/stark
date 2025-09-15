#!/bin/sh

set -e

GREEN='\033[32m'
RED='\033[31m'
RESET='\033[0m'

fail() {
    echo -e $1 >&2
    exit 1
}

build() {
    asm="$1.s"
    obj="$1.o"

    cargo r -q -- "$1.st" -o "$asm"
    fasm "$asm" "$obj"
    gcc -no-pie -o "$1" "$obj"
}

run() {
    name=$(basename $1)
    stdout=$(mktemp stark-test-$name-stdout.XXXXXXXXXX -t)
    stderr=$(mktemp stark-test-$name-stderr.XXXXXXXXXX -t)
    ./$1 >$stdout 2>$stderr
    # STDOUT:
    # asdf
    # asdf
    # asdf
    # asdf
    #
    # STDERR:
    # asdkljf
    # lkj
    # lkj
    #
    # STATUS: 0
    printf "STDOUT:\n%s\n\nSTDERR:\n%s\n\nSTATUS: %d\n" "$(cat $stdout | awk '{ print "    " $0 }')" "$(cat $stderr | awk '{ print "    " $0 }')" $?
    rm $stdout $stderr
}

record() {
    name=$(basename $1)
    echo "Recording '$name'"
    build $1 2>&1 | awk "{ print \"[Compile '$name'] \" \$0 }"
    run $1 > ./$name-out.txt
}

test_single() {
    name=$(basename $1)
    echo "Testing '$name'"
    build $1 2>&1 | awk "{ print \"[Compile '$name'] \" \$0 }"
    tmp=$(mktemp)
    run $1 > $tmp
    dif=$(diff ./$name-out.txt $tmp --color=always -u)
    diffst=$?
    if [ $diffst -eq 0 ]; then
        echo -e "[Testing '$name'] ${GREEN}Test passed! ✔️$RESET"
    else
        echo "$dif" 2>&1 | awk "{ print \"[Testing '$name'] \" \$0 }"
        echo "[Testing '$name'] ${RED}Test failed! ❌$RESET"
        return 1
    fi
    return 0
}

test_all() {
    count=0
    for file in $(find . -iname '*.st' -type f); do
        set +e
        test_single "${file%.st}"
        local st=$?
        echo
        set -e
        if [ $st -ne 0 ]; then
            let count+=1
        fi
    done
    if [ $count -ne 0 ]; then
        fail "${RED}$count test failures $RESET"
    else
        echo -e "${GREEN}All Tests Passed!$RESET"
    fi
}

case "$1" in
    "run")
        if [ ! -n "$2" ]; then
            fail "Usage: $0 $1 <file.st>"
        fi
        shift
        exe=${1%.st}
        build "$exe" 2>&1 | awk "{ print \"[Compile '$exe'] \" \$0 }"
        ./$exe       2>&1 | awk "{ print \"[Run '$exe'] \" \$0 }"
    ;;
    "record")
        if [ ! -n "$2" ]; then
            for file in $(find . -iname '*.st' -type f); do
                record "${file%.st}"
                echo
            done
        else
            record "${2%.st}"
        fi
    ;;
    "test")
        if [ ! -n "$2" ]; then
            test_all
        else
            set +e
            test_single "${2%.st}"
            st=$?
            echo $st
            if [ $st -ne 0 ]; then
                fail "Test failed!"
            else
                fail "Test passed!"
            fi
            set -e
        fi
    ;;
    "")
        test_all
    ;;
    *)
        fail "Unknown subcommand '$1'"
    ;;
esac
