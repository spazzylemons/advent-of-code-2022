#include <stdbool.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#define TUNNEL_WIDTH 7

/**
 * Stores tunnel rows as bytes.
 */
struct bit_vector {
    int height;
    uint8_t *bits;
};

/**
 * Reduce malloc() calls by partially reusing bit arrays
 */
#define MAX_SAVED_BITS 100

static uint8_t *saved_bits[MAX_SAVED_BITS] = {};

static void new_vector(struct bit_vector *vec, int height) {
    vec->height = height;
    if (height < MAX_SAVED_BITS && saved_bits[height] != NULL) {
        vec->bits = saved_bits[height];
        saved_bits[height] = NULL;
    } else {
        vec->bits = malloc(height);
    }
    memset(vec->bits, 0, height);
}

static void free_vector(struct bit_vector *vec) {
    if (vec->height < MAX_SAVED_BITS && saved_bits[vec->height] == NULL) {
        saved_bits[vec->height] = vec->bits;
    } else {
        free(vec->bits);
    }
}

static bool in_bounds(struct bit_vector *vec, int x, int y) {
    return x >= 0 && x < TUNNEL_WIDTH && y >= 0 && y < vec->height;
}

static bool get_bit(struct bit_vector *vec, int x, int y) {
    return (vec->bits[y] & (1 << x)) != 0;
}

static void set_bit(struct bit_vector *vec, int x, int y) {
    vec->bits[y] |= (1 << x);
}

static const bool rocks[5][4][4] = {
    {
        { 1, 1, 1, 1 },
        { 0, 0, 0, 0 },
        { 0, 0, 0, 0 },
        { 0, 0, 0, 0 },
    },

    {
        { 0, 1, 0, 0 },
        { 1, 1, 1, 0 },
        { 0, 1, 0, 0 },
        { 0, 0, 0, 0 },
    },

    {
        { 1, 1, 1, 0 },
        { 0, 0, 1, 0 },
        { 0, 0, 1, 0 },
        { 0, 0, 0, 0 },
    },

    {
        { 1, 0, 0, 0 },
        { 1, 0, 0, 0 },
        { 1, 0, 0, 0 },
        { 1, 0, 0, 0 },
    },

    {
        { 1, 1, 0, 0 },
        { 1, 1, 0, 0 },
        { 0, 0, 0, 0 },
        { 0, 0, 0, 0 },
    },
};

static bool check_collision(int rock, int x, int y, struct bit_vector *tile_array) {
    for (int h = 0; h < 4; h++) {
        int check_y = y + h;
        for (int w = 0; w < 4; w++) {
            int check_x = x + w;
            if (rocks[rock][h][w] && (!in_bounds(tile_array, check_x, check_y) || get_bit(tile_array, check_x, check_y))) {
                return true;
            }
        }
    }
    return false;
}

static int stamp_rock(int rock, int x, int y, struct bit_vector *tile_array) {
    int highest_point = -1;
    for (int h = 0; h < 4; h++) {
        int check_y = y + h;
        for (int w = 0; w < 4; w++) {
            int check_x = x + w;
            if (rocks[rock][h][w] && in_bounds(tile_array, check_x, check_y)) {
                if (check_y + 1 > highest_point) {
                    highest_point = check_y + 1;
                }
                set_bit(tile_array, check_x, check_y);
            }
        }
    }
    return highest_point;
}

struct reachable_stack {
    struct reachable_stack *next;
    int x;
    int y;
};

static struct reachable_stack *allocated_stacks = NULL;

static struct reachable_stack *push_stack(struct reachable_stack *stack, int x, int y) {
    struct reachable_stack *new;
    if (allocated_stacks == NULL) {
        new = malloc(sizeof(struct reachable_stack));
    } else {
        new = allocated_stacks;
        allocated_stacks = allocated_stacks->next;
    }
    new->next = stack;
    new->x = x;
    new->y = y;
    return new;
}

static struct reachable_stack *pop_stack(struct reachable_stack *stack) {
    struct reachable_stack *result = stack->next;
    stack->next = allocated_stacks;
    allocated_stacks = stack;
    return result;
}

static int find_lowest_reachable(int highest_point, struct bit_vector *tile_array) {
    struct bit_vector visited;
    new_vector(&visited, highest_point + 1);
    memcpy(visited.bits, tile_array->bits, highest_point + 1);

    int lowest_point = highest_point;

    struct reachable_stack *stack = push_stack(NULL, 3, highest_point);

    while (stack != NULL) {
        int x = stack->x;
        int y = stack->y;
        stack = pop_stack(stack);

        if (y <= highest_point && in_bounds(tile_array, x, y) && !get_bit(&visited, x, y)) {
            if (y < lowest_point) {
                lowest_point = y;
            }
            set_bit(&visited, x, y);
            stack = push_stack(stack, x - 1, y);
            stack = push_stack(stack, x + 1, y);
            stack = push_stack(stack, x, y - 1);
            stack = push_stack(stack, x, y + 1);
        }
    }

    free_vector(&visited);
    return lowest_point;
}

static void shift_array_down(struct bit_vector *tile_array, int amount, int highest_point) {
    memmove(&tile_array->bits[0], &tile_array->bits[amount], highest_point);
}

#define TOTAL_TO_CHECK 1000000000000

static char *get_directions(int *size, const char *filename) {
    FILE *f = fopen(filename, "r");
    fseek(f, 0, SEEK_END);
    *size = ftell(f) - 1;
    char *result = malloc(*size);
    fseek(f, 0, SEEK_SET);
    fread(result, *size, 1, f);
    fclose(f);
    for (int i = 0; i < *size; i++) {
        if (result[i] == '>') {
            result[i] = 1;
        } else {
            result[i] = -1;
        }
    }
    return result;
}

struct state_chain {
    struct state_chain *next;
    struct bit_vector grid;

    int blocks;
    int height;
};

static void free_seen_states(struct state_chain **seen_states, int size) {
    for (int i = 0; i < size; i++) {
        struct state_chain *chain = seen_states[i];
        while (chain != NULL) {
            struct state_chain *next = chain->next;
            free_vector(&chain->grid);
            free(chain);
            chain = next;
        }
    }
    free(seen_states);
}

int main(int argc, char *argv[]) {
    int num_directions;
    char *directions = get_directions(&num_directions, argc > 1 ? argv[1] : "input");

    int highest_point = 0;
    long long accumulated_height = 0;
    struct bit_vector tile_array;

    int direction_index = 0;
    int rock_index = 0;

    new_vector(&tile_array, 1000);

    int seen_states_alloc_size = sizeof(struct state_chain *) * num_directions * 5;
    struct state_chain **seen_states = malloc(seen_states_alloc_size);
    memset(seen_states, 0, seen_states_alloc_size);

    long long rock_count = 0;
    while (rock_count++ < TOTAL_TO_CHECK) {
        int x = 2;
        int y = highest_point + 3;
        for (;;) {
            int direction = directions[direction_index];
            direction_index = (direction_index + 1) % num_directions;
            int new_x = x + direction;

            if (!check_collision(rock_index, new_x, y, &tile_array)) {
                x = new_x;
            }

            if (check_collision(rock_index, x, y - 1, &tile_array)) {
                int lowest_point = find_lowest_reachable(highest_point, &tile_array);
                accumulated_height += (long long) lowest_point;
                shift_array_down(&tile_array, lowest_point, highest_point);
                highest_point -= lowest_point;
                y -= lowest_point;
                int new_highest_point = stamp_rock(rock_index, x, y, &tile_array);
                if (new_highest_point > highest_point) {
                    highest_point = new_highest_point;
                }
                break;
            }
            --y;
        }
        rock_index = (rock_index + 1) % 5;

        if (seen_states != NULL) {
            int chain_key = (5 * direction_index) + rock_index;
            struct state_chain *chain = seen_states[chain_key];
            /* search chain for matching entry */
            while (chain != NULL) {
                if (chain->grid.height == highest_point && !memcmp(chain->grid.bits, tile_array.bits, highest_point)) {
                    break;
                }
                chain = chain->next;
            }
            if (chain != NULL) {
                /* if an entry was found, use it to skip cycles */
                long long blocks_in_state = rock_count - chain->blocks;
                long long height_in_state = accumulated_height - chain->height;
                long long repeat_count = (TOTAL_TO_CHECK - rock_count) / blocks_in_state;
                accumulated_height += repeat_count * height_in_state;
                rock_count += repeat_count * blocks_in_state;
                free_seen_states(seen_states, num_directions * 5);
                seen_states = NULL;
            } else {
                /* otherwise, add an entry to the chain */
                chain = malloc(sizeof(struct state_chain));
                chain->blocks = rock_count;
                chain->height = accumulated_height;
                new_vector(&chain->grid, highest_point);
                memcpy(chain->grid.bits, tile_array.bits, highest_point);
                chain->next = seen_states[chain_key];
                seen_states[chain_key] = chain;
            }
        }
    }

    accumulated_height += (long long) highest_point;
    printf("%lld\n", accumulated_height);

    while (allocated_stacks != NULL) {
        struct reachable_stack *next = allocated_stacks->next;
        free(allocated_stacks);
        allocated_stacks = next;
    }

    if (seen_states) {
        free_seen_states(seen_states, num_directions * 5);
    }

    free_vector(&tile_array);
    free(directions);

    for (int index = 0; index < MAX_SAVED_BITS; index++) {
        free(saved_bits[index]);
    }

    return 0;
}
