#include <raylib.h>

int main(void) {
    InitWindow(900, 600, (char *)0);
    float x = 199.;
    x += 1.;
    while (!WindowShouldClose()) {
        BeginDrawing();
            ClearBackground(RAYWHITE);
            DrawCircle(450, 300, x, RED);
        EndDrawing();
    }
    CloseWindow();
    return 0;
}
