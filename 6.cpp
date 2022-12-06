#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

template<size_t N>
struct Solver {
    unsigned char stream[N], current;
    size_t counts[256] = { 0 };
    size_t stream_index = 0;
    size_t index = 0;
    size_t num_ones = 0;

    void read(unsigned char c) {
        if (stream_index < index) {
            decrement_count(stream[stream_index]);
        }
        stream[stream_index] = c;
        increment_count(stream[stream_index]);
        stream_index = (stream_index + 1) % N;
        ++index;
    }

    void increment_count(unsigned char i) {
        if (counts[i] == 1) {
            --num_ones;
        } else if (counts[i] == 0) {
            ++num_ones;
        }
        ++counts[i];
    }

    void decrement_count(unsigned char i) {
        if (counts[i] == 1) {
            --num_ones;
        } else if (counts[i] == 2) {
            ++num_ones;
        }
        --counts[i];
    }

    bool is_good(void) {
        return num_ones == N;
    }
};

int main(void) {
    FILE *file = fopen("input", "r");
    Solver<4> part1;
    Solver<14> part2;
    unsigned char c;
    bool found_part_1 = false;
    bool found_part_2 = false;
    while (fread(&c, 1, 1, file)) {
        if (!found_part_1) {
            part1.read(c);
            if (part1.is_good()) {
                printf("part 1: %zu\n", part1.index);
                found_part_1 = true;
            }
        }

        if (!found_part_2) {
            part2.read(c);
            if (part2.is_good()) {
                printf("part 2: %zu\n", part2.index);
                found_part_2 = true;
            }
        }

        if (found_part_1 && found_part_2) break;
    }
    fclose(file);
    return 0;
}
