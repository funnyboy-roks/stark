extern fn printf(*i8 ...);
extern fn exit(i32);

extern("InitWindow")        fn init_window(i32 i32 *i8);
extern("WindowShouldClose") fn window_should_close -> (bool);
extern("BeginDrawing")      fn begin_drawing;
extern("EndDrawing")        fn end_drawing;
extern("ClearBackground")   fn clear_background(u32);
extern("DrawCircle")        fn draw_circle(i32 i32 f32 u32);
extern("CloseWindow")       fn close_window;


fn main {
    c"hello world" 600 900 init_window

    while window_should_close ! {
        begin_drawing
            0xffffffff clear_background
            0xff3729e6 200.0 300 450 draw_circle
        end_drawing
    }

    close_window
}
