#include <stdbool.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#define FACE_SIZE 50

static const int delta_x[4] = { 1, 0, -1, 0 };
static const int delta_y[4] = { 0, 1, 0, -1 };

static inline int floordiv(int x) {
    return (x / FACE_SIZE) - (x < 0);
}

static inline int floormod(int x) {
    if (x < 0) {
        return (FACE_SIZE - (-x % FACE_SIZE)) % FACE_SIZE;
    } else {
        return x % FACE_SIZE;
    }
}

enum dir {
    dir_right,
    dir_down,
    dir_left,
    dir_up,
};

enum face {
    face_right,
    face_down,
    face_left,
    face_up,
    face_front,
    face_back,
};

struct relative_space {
    int y[6], x[6];
};

struct cube_face {
    uint8_t bits[((FACE_SIZE * FACE_SIZE) + 7) / 8];
    struct relative_space space;
};

struct direction {
    struct direction *next;

    bool is_rotate;
    int delta;
};

struct cube_map {
    struct cube_face *faces[4][4];
    struct direction *dirs;

    int start_y, start_x;
};

static bool has_face(struct cube_map *cubemap, int y, int x) {
    if (x < 0 || x >= 4 * FACE_SIZE || y < 0 || y >= 4 * FACE_SIZE) {
        return false;
    }
    return cubemap->faces[floordiv(y)][floordiv(x)] != NULL;
}

static void cubemap_assignment(struct cube_map *cubemap, struct relative_space *space, int *faces, int y, int x) {
    if (space->x[faces[face_front]] >= 0) return;

    space->x[faces[face_front]] = x;
    space->y[faces[face_front]] = y;

    if (has_face(cubemap, y * FACE_SIZE, (x - 1) * FACE_SIZE)) {
        int new_faces[6];
        new_faces[face_front] = faces[face_left];
        new_faces[face_left] = faces[face_back];
        new_faces[face_back] = faces[face_right];
        new_faces[face_right] = faces[face_front];
        new_faces[face_up] = faces[face_up];
        new_faces[face_down] = faces[face_down];
        cubemap_assignment(cubemap, space, new_faces, y, x - 1);
    }

    if (has_face(cubemap, y * FACE_SIZE, (x + 1) * FACE_SIZE)) {
        int new_faces[6];
        new_faces[face_front] = faces[face_right];
        new_faces[face_right] = faces[face_back];
        new_faces[face_back] = faces[face_left];
        new_faces[face_left] = faces[face_front];
        new_faces[face_up] = faces[face_up];
        new_faces[face_down] = faces[face_down];
        cubemap_assignment(cubemap, space, new_faces, y, x + 1);
    }

    if (has_face(cubemap, (y - 1) * FACE_SIZE, x * FACE_SIZE)) {
        int new_faces[6];
        new_faces[face_front] = faces[face_up];
        new_faces[face_up] = faces[face_back];
        new_faces[face_back] = faces[face_down];
        new_faces[face_down] = faces[face_front];
        new_faces[face_left] = faces[face_left];
        new_faces[face_right] = faces[face_right];
        cubemap_assignment(cubemap, space, new_faces, y - 1, x);
    }

    if (has_face(cubemap, (y + 1) * FACE_SIZE, x * FACE_SIZE)) {
        int new_faces[6];
        new_faces[face_front] = faces[face_down];
        new_faces[face_down] = faces[face_back];
        new_faces[face_back] = faces[face_up];
        new_faces[face_up] = faces[face_front];
        new_faces[face_left] = faces[face_left];
        new_faces[face_right] = faces[face_right];
        cubemap_assignment(cubemap, space, new_faces, y + 1, x);
    }
}

static void make_cube_assignment(struct cube_map *cubemap, int y, int x) {
    int faces[6];
    struct cube_face *face = cubemap->faces[y][x];
    for (int i = 0; i < 6; i++) {
        faces[i] = i;
        face->space.x[i] = -1;
    }
    cubemap_assignment(cubemap, &face->space, faces, y, x);
}

static void set_bit(struct cube_map *cubemap, int y, int x) {
    int face_x = floordiv(x);
    int face_y = floordiv(y);
    int local_x = floormod(x);
    int local_y = floormod(y);

    int index = (local_y * FACE_SIZE) + local_x;
    cubemap->faces[face_y][face_x]->bits[index >> 3] |= (1 << (index & 7));
}

static bool get_bit(struct cube_map *cubemap, int y, int x) {
    int face_x = floordiv(x);
    int face_y = floordiv(y);
    int local_x = floormod(x);
    int local_y = floormod(y);

    if (cubemap->faces[face_y][face_x] == NULL) {
        return false;
    }

    int index = (local_y * FACE_SIZE) + local_x;
    return (cubemap->faces[face_y][face_x]->bits[index >> 3] & (1 << (index & 7))) != 0;
}

static void push_direction(struct cube_map *out, bool is_rotate, int delta) {
    struct direction *dir = malloc(sizeof(struct direction));
    dir->next = out->dirs;
    dir->is_rotate = is_rotate;
    dir->delta = delta;
    out->dirs = dir;
}

static void parse_input(const char *filename, struct cube_map *out) {
    for (int y = 0; y < 4; y++) {
        for (int x = 0; x < 4; x++) {
            out->faces[y][x] = NULL;
        }
    }
    out->dirs = NULL;
    out->start_x = 0;
    out->start_y = 0;

    int y = 0;
    int x = 0;
    FILE *f = fopen(filename, "r");

    for (;;) {
        char c = fgetc(f);
        if (c == '\n') {
            if (x == 0) {
                break;
            }
            x = 0;
            ++y;
        } else {
            if (c != ' ') {
                int face_x = floordiv(x);
                int face_y = floordiv(y);

                if (out->faces[face_y][face_x] == NULL) {
                    out->faces[face_y][face_x] = malloc(sizeof(struct cube_face));
                    memset(out->faces[face_y][face_x], 0, sizeof(struct cube_face));
                }

                if (c == '#') {
                    set_bit(out, y, x);
                }
            }
            ++x;
        }
    }

    int accumulator = 0;
    for (;;) {
        char c = fgetc(f);
        if (c == '\n') break;

        if (c == 'R' || c == 'L') {
            if (accumulator != 0) {
                push_direction(out, false, accumulator);
                accumulator = 0;
            }
            push_direction(out, true, c == 'R' ? 1 : -1);
        } else {
            accumulator *= 10;
            accumulator += c - '0';
        }
    }
    if (accumulator != 0) {
        push_direction(out, false, accumulator);
    }

    fclose(f);

    // reverse direction list so directions are processed in-order
    struct direction *old = out->dirs;
    struct direction *new = NULL;
    while (old != NULL) {
        struct direction *next = old->next;
        old->next = new;
        new = old;
        old = next;
    }
    out->dirs = new;

    for (int y = 0; y < 4; y++) {
        for (int x = 0; x < 4; x++) {
            if (out->faces[y][x] != NULL) {
                make_cube_assignment(out, y, x);
            }
        }
    }

    for (int y = 0; y < 4; y++) {
        for (int x = 0; x < 4; x++) {
            if (out->faces[y][x] != NULL) {
                out->start_x = x * FACE_SIZE;
                out->start_y = y * FACE_SIZE;
                return;
            }
        }
    }
}

static int part_1(struct cube_map *cubemap) {
    struct direction *dirs = cubemap->dirs;
    int x = cubemap->start_x;
    int y = cubemap->start_y;
    int facing = 0;
    while (dirs != NULL) {
        if (dirs->is_rotate) {
            facing = (facing + dirs->delta + 4) % 4;
        } else {
            int dx = delta_x[facing];
            int dy = delta_y[facing];
            for (int i = 0; i < dirs->delta; i++) {
                int old_x = x;
                int old_y = y;
                x += dx;
                y += dy;
                if (!has_face(cubemap, y, x)) {
                    do {
                        x -= dx;
                        y -= dy;
                    } while (has_face(cubemap, y, x));
                    x += dx;
                    y += dy;
                }
                if (get_bit(cubemap, y, x)) {
                    x = old_x;
                    y = old_y;
                    break;
                }
            }
        }
        dirs = dirs->next;
    }
    return (1000 * (y + 1)) + (4 * (x + 1)) + facing;
}

static int part_2(struct cube_map *cubemap) {
    struct direction *dirs = cubemap->dirs;
    int x = cubemap->start_x;
    int y = cubemap->start_y;
    int facing = 0;
    while (dirs != NULL) {
        if (dirs->is_rotate) {
            facing = (facing + dirs->delta + 4) % 4;
        } else {
            for (int n = 0; n < dirs->delta; n++) {
                int old_x = x;
                int old_y = y;
                int rot_change = 0;
                int new_x = x + delta_x[facing];
                int new_y = y + delta_y[facing];
                int face_x = floordiv(x);
                int face_y = floordiv(y);
                if (face_x == floordiv(new_x) && face_y == floordiv(new_y)) {
                    x = new_x;
                    y = new_y;
                } else {
                    struct relative_space *src_space = &cubemap->faces[face_y][face_x]->space;
                    int next_x = src_space->x[facing];
                    int next_y = src_space->y[facing];
                    struct relative_space *dst_space = &cubemap->faces[next_y][next_x]->space;
                    if (dst_space->x[face_left] == face_x && dst_space->y[face_left] == face_y) {
                        rot_change = 0;
                    } else if (dst_space->x[face_up] == face_x && dst_space->y[face_up] == face_y) {
                        rot_change = 1;
                    } else if (dst_space->x[face_right] == face_x && dst_space->y[face_right] == face_y) {
                        rot_change = 2;
                    } else if (dst_space->x[face_down] == face_x && dst_space->y[face_down] == face_y) {
                        rot_change = 3;
                    }
                    rot_change = (rot_change - facing + 4) % 4;
                    new_x = floormod(new_x);
                    new_y = floormod(new_y);
                    for (int i = 0; i < rot_change; i++) {
                        int temp = (FACE_SIZE - 1) - new_y;
                        new_y = new_x;
                        new_x = temp;
                    }
                    new_x += next_x * FACE_SIZE;
                    new_y += next_y * FACE_SIZE;
                    x = new_x;
                    y = new_y;
                }

                if (get_bit(cubemap, y, x)) {
                    x = old_x;
                    y = old_y;
                    break;
                }
                facing = (facing + rot_change) % 4;
            }
        }
        dirs = dirs->next;
    }
    return (1000 * (y + 1)) + (4 * (x + 1)) + facing;
}

int main(void) {
    struct cube_map cubemap;
    parse_input("input", &cubemap);
    printf("part 1: %d\n", part_1(&cubemap));
    printf("part 2: %d\n", part_2(&cubemap));
    for (int y = 0; y < 4; y++) {
        for (int x = 0; x < 4; x++) {
            free(cubemap.faces[y][x]);
        }
    }
    while (cubemap.dirs != NULL) {
        struct direction *next = cubemap.dirs->next;
        free(cubemap.dirs);
        cubemap.dirs = next;
    }
    return EXIT_SUCCESS;
}
