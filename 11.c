#include <inttypes.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

struct item {
    struct item *next;
    uint64_t worry_level;
};

struct monkey {
    struct item *items;
    uint64_t(*op_type)(uint64_t, uint64_t);
    uint64_t op_value;
    uint64_t factor;
    int inspections;
    int true_throw;
    int false_throw;
};

static uint64_t op_add(uint64_t a, uint64_t b) {
    return a + b;
}

static uint64_t op_mul(uint64_t a, uint64_t b) {
    return a * b;
}

static struct monkey *parse_monkeys(FILE *file, int *num_monkeys_out) {
    char buf[256], *line;
    struct monkey *monkeys = NULL;
    int num_monkeys = 0;

    while (fgets(buf, sizeof(buf), file)) {
        monkeys = realloc(monkeys, sizeof(struct monkey) * (num_monkeys + 1));
        struct monkey *monkey = &monkeys[num_monkeys++];
        monkey->inspections = 0;

        // parse starting items
        monkey->items = NULL;
        line = fgets(buf, sizeof(buf), file) + strlen("  Starting items: ");
        for (;;) {
            struct item *item = malloc(sizeof(struct item));
            item->next = monkey->items;
            monkey->items = item;
            sscanf(line, "%" SCNu64, &item->worry_level);

            if ((line = strstr(line, ","))) {
                line += 2;
            } else break;
        }
        // parse operation
        line = fgets(buf, sizeof(buf), file) + strlen("  Operation: new = old ");
        monkey->op_type = line[0] == '+' ? op_add : op_mul;
        line += 2;
        monkey->op_value = atoi(line);
        // parse divisible test
        line = fgets(buf, sizeof(buf), file) + strlen("  Test: divisible by ");
        monkey->factor = atoi(line);
        // parse throws
        line = fgets(buf, sizeof(buf), file) + strlen("    If true: throw to monkey ");
        monkey->true_throw = atoi(line);
        line = fgets(buf, sizeof(buf), file) + strlen("    If false: throw to monkey ");
        monkey->false_throw = atoi(line);
        // skip unused line
        fgets(buf, sizeof(buf), file);
    }

    *num_monkeys_out = num_monkeys;
    return monkeys;
}

static int sort_monkeys(const void *a, const void *b) {
    const struct monkey *ma = a;
    const struct monkey *mb = b;
    return mb->inspections - ma->inspections;
}

static long long monkey_business(int num_rounds, uint64_t worry_division) {
    FILE *f = fopen("input", "r");
    int num_monkeys;
    struct monkey *monkeys = parse_monkeys(f, &num_monkeys);
    fclose(f);

    uint64_t max_worry_level = 1;
    for (int i = 0; i < num_monkeys; i++) {
        max_worry_level *= monkeys[i].factor;
    }

    for (int count = 0; count < num_rounds; count++) {
        for (int i = 0; i < num_monkeys; i++) {
            struct monkey *monkey = &monkeys[i];
            while (monkey->items != NULL) {
                ++monkey->inspections;

                struct item *item = monkey->items;
                monkey->items = item->next;

                uint64_t other_value = monkey->op_value;
                if (other_value == 0) other_value = item->worry_level;
                item->worry_level = monkey->op_type(item->worry_level, other_value);
                item->worry_level /= worry_division;
                item->worry_level %= max_worry_level;
                struct monkey *throw_to;
                if (item->worry_level % monkey->factor == 0) {
                    throw_to = &monkeys[monkey->true_throw];
                } else {
                    throw_to = &monkeys[monkey->false_throw];
                }
                item->next = throw_to->items;
                throw_to->items = item;
            }
        }
    }

    qsort(monkeys, num_monkeys, sizeof(struct monkey), sort_monkeys);
    long long best_1 = monkeys[0].inspections;
    long long best_2 = monkeys[1].inspections;

    for (int i = 0; i < num_monkeys; i++) {
        struct item *item = monkeys[i].items;
        while (item != NULL) {
            struct item *next = item->next;
            free(item);
            item = next;
        }
    }
    free(monkeys);

    return best_1 * best_2;
}

int main(void) {
    long long part_1 = monkey_business(20, 3);
    long long part_2 = monkey_business(10000, 1);

    printf("part 1: %lld\n", part_1);
    printf("part 2: %lld\n", part_2);

    return 0;
}
