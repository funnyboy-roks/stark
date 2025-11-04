RAYLIB_VERSION=5.5
RAYLIB_ZIPPED=raylib-$(RAYLIB_VERSION)_linux_amd64.tar.gz

japan: japan.o libraylib.a
	gcc -L. -ggdb -no-pie -o japan japan.o -l:libraylib.a -lm

libraylib.a: $(RAYLIB_ZIPPED)
	tar xf $(RAYLIB_ZIPPED) raylib-$(RAYLIB_VERSION)_linux_amd64/lib/libraylib.a --strip-components=2
	touch libraylib.a # Update the date so that make doesn't unzip every time

$(RAYLIB_ZIPPED): 
	wget -O $(RAYLIB_ZIPPED) https://github.com/raysan5/raylib/releases/download/$(RAYLIB_VERSION)/raylib-$(RAYLIB_VERSION)_linux_amd64.tar.gz

japan.o: japan.s
	fasm japan.s japan.o

japan.s: japan.st
	cargo r -- japan.st -o japan.s
