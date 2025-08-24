extern fn printf(*i8 ...) -> ();
extern fn exit(i32) -> ();

extern fn InitWindow(i32 i32 *i8) -> ();
extern fn WindowShouldClose() -> (bool);
extern fn BeginDrawing() -> ();
extern fn EndDrawing() -> ();
extern fn ClearBackground(u32) -> ();
extern fn DrawCircle(i32 i32 f32 u32) -> ();
extern fn CloseWindow() -> ();

c"hello world" 600 i32 900 i32 InitWindow

while WindowShouldClose ! {
    BeginDrawing
        0xffffffff u32 ClearBackground
        0xff3729e6 u32 200.0 300 i32 450 i32 DrawCircle
    EndDrawing
}

CloseWindow
