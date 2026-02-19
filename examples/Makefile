all: test loops fib hello conditional functions collatz fizzbuzz

%: %.s
	gcc -ggdb -no-pie -o $@ $^

# %.o: %.s
# 	fasm $< $@

%.s: %.st
	cargo r -- $< -o $@
