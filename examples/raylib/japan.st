extern fn printf(*i8 ...);
extern fn exit(i32);

extern fn InitWindow(i32 i32 *i8);
extern fn WindowShouldClose -> (bool);
extern fn BeginDrawing;
extern fn EndDrawing;
extern fn ClearBackground(u32);
extern fn DrawCircle(i32 i32 f32 u32);
extern fn CloseWindow;

fn main {
    c"hello world" 600 900 InitWindow

    while WindowShouldClose ! {
        BeginDrawing
            0xffffffff cast(u64) ClearBackground
            0xff3729e6 200.0 300 450 DrawCircle
        EndDrawing
    }

    CloseWindow
}
