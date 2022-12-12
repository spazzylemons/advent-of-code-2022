#include <stdbool.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

struct coordinate {
    int x, y;
};

struct grid {
    uint8_t *points;
    int *distance;
    int width, height;
    struct coordinate start;
    struct coordinate end;
};

static uint8_t grid_at(const struct grid *grid, int y, int x) {
    return grid->points[y * grid->width + x];
}

static int dist_at(const struct grid *grid, int y, int x) {
    return grid->distance[y * grid->width + x];
}

static void dist_set(const struct grid *grid, int y, int x, int value) {
    grid->distance[y * grid->width + x] = value;
}

struct priority_queue {
    const struct grid *grid;
    struct coordinate *nodes;
    int size, capacity;
};

#define INITIAL_CAPACITY 8

static void queue_init(struct priority_queue *queue, const struct grid *grid) {
    queue->grid = grid;
    queue->nodes = malloc(sizeof(struct coordinate) * INITIAL_CAPACITY);
    queue->size = 0;
    queue->capacity = INITIAL_CAPACITY;
}

static bool queue_compare(const struct priority_queue *queue, int a, int b) {
    int ha = dist_at(queue->grid, queue->nodes[a].y, queue->nodes[a].x);
    int hb = dist_at(queue->grid, queue->nodes[b].y, queue->nodes[b].x);
    return ha < hb;
}

static void queue_swap(const struct priority_queue *queue, int a, int b) {
    struct coordinate temp = queue->nodes[a];
    queue->nodes[a] = queue->nodes[b];
    queue->nodes[b] = temp;
}

static void sift_up(struct priority_queue *queue, int index) {
    while (index > 0) {
        int parent = (index - 1) >> 1;
        if (queue_compare(queue, parent, index)) {
            break;
        }
        queue_swap(queue, parent, index);
        index = parent;
    }
}

static void sift_down(struct priority_queue *queue, int index) {
    for (;;) {
        int best = index;
        int left_child = (index << 1) + 1;
        int right_child = (index << 1) + 2;
        if (left_child < queue->size && queue_compare(queue, left_child, best)) {
            best = left_child;
        }
        if (right_child < queue->size && queue_compare(queue, right_child, best)) {
            best = right_child;
        }
        if (best == index) return;
        queue_swap(queue, index, best);
        index = best;
    }
}

static void queue_push(struct priority_queue *queue, int x, int y) {
    // check capacity
    if (queue->size == queue->capacity) {
        queue->capacity *= 2;
        queue->nodes = realloc(queue->nodes, sizeof(struct coordinate) * queue->capacity);
    }
    // insert into queue
    queue->nodes[queue->size].x = x;
    queue->nodes[queue->size].y = y;
    // sift into place
    sift_up(queue, queue->size++);
}

static bool queue_pop(struct priority_queue *queue, struct coordinate *out) {
    // if empty, return false
    if (queue->size == 0) return false;
    // get from top of heap
    *out = queue->nodes[0];
    // move node at end to top
    queue->nodes[0] = queue->nodes[--queue->size];
    // sift into place
    sift_down(queue, 0);
    return true;
}

static void queue_update(struct priority_queue *queue, int y, int x, int old_priority) {
    for (int i = 0; i < queue->size; i++) {
        if (queue->nodes[i].x == x && queue->nodes[i].y == y) {
            int new_priority = dist_at(queue->grid, y, x);
            if (old_priority < new_priority) {
                sift_down(queue, i);
            } else if (new_priority < old_priority) {
                sift_up(queue, i);
            }
            return;
        }
    }
}

static void load_grid(const char *filename, struct grid *out) {
    FILE *f = fopen(filename, "r");
    int width = 0, height = 0;

    // scan through once to get the dimensions
    for (;;) {
        int c = fgetc(f);
        if (c == EOF) break;

        if (c == '\n') {
            // new line, add one to height
            ++height;
        } else if (height == 0) {
            // add one to width if not yet known
            ++width;
        }
    }
    // reset file pointer
    fseek(f, 0, SEEK_SET);
    // allocate grid
    out->points = malloc(width * height);
    out->distance = malloc(width * height * sizeof(int));
    out->width = width;
    out->height = height;
    // for gcc's sake
    out->start.x = 0;
    out->start.y = 0;
    out->end.x = 0;
    out->end.y = 0;
    // read grid
    uint8_t *ptr = out->points;
    for (int y = 0; y < height; y++) {
        for (int x = 0; x < width; x++) {
            // read a character
            char c = fgetc(f);
            if (c == 'S') {
                *ptr++ = 0;
                out->start.x = x;
                out->start.y = y;
            } else if (c == 'E') {
                *ptr++ = 25;
                out->end.x = x;
                out->end.y = y;
            } else {
                *ptr++ = c - 'a';
            }
        }
        // skip new line
        fseek(f, 1, SEEK_CUR);
    }
    fclose(f);
}

static int hill_climb(const struct grid *grid, bool any_start) {
    struct priority_queue queue;
    queue_init(&queue, grid);

    for (int y = 0; y < grid->height; y++) {
        for (int x = 0; x < grid->width; x++) {
            if (any_start ? (grid_at(grid, y, x) == 0) : (x == grid->start.x && y == grid->start.y)) {
                dist_set(grid, y, x, 0);
            } else {
                dist_set(grid, y, x, 1000000);
            }
            queue_push(&queue, x, y);
        }
    }

    struct coordinate u, neighbors[4];
    while (queue_pop(&queue, &u)) {
        int num_neighbors = 0;
        if (u.x + 1 < grid->width) {
            neighbors[num_neighbors].x = u.x + 1;
            neighbors[num_neighbors].y = u.y;
            ++num_neighbors;
        }
        if (u.y + 1 < grid->height) {
            neighbors[num_neighbors].x = u.x;
            neighbors[num_neighbors].y = u.y + 1;
            ++num_neighbors;
        }
        if (u.x > 0) {
            neighbors[num_neighbors].x = u.x - 1;
            neighbors[num_neighbors].y = u.y;
            ++num_neighbors;
        }
        if (u.y > 0) {
            neighbors[num_neighbors].x = u.x;
            neighbors[num_neighbors].y = u.y - 1;
            ++num_neighbors;
        }
        for (int i = 0; i < num_neighbors; i++) {
            struct coordinate v = neighbors[i];
            int alt = 1 + dist_at(grid, u.y, u.x);
            int old = dist_at(grid, v.y, v.x);
            if (grid_at(grid, v.y, v.x) - grid_at(grid, u.y, u.x) < 2 && alt < old) {
                dist_set(grid, v.y, v.x, alt);
                queue_update(&queue, v.y, v.x, old);
            }
        }
    }
    free(queue.nodes);

    return dist_at(grid, grid->end.y, grid->end.x);
}

int main(void) {
    struct grid grid;
    load_grid("input", &grid);
    printf("part 1: %d\n", hill_climb(&grid, false));
    printf("part 2: %d\n", hill_climb(&grid, true));
    free(grid.points);
    free(grid.distance);
    return EXIT_SUCCESS;
}
