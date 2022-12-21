#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct monkey {
    struct monkey *next;

    char name[4];

    long long value;
    long long (*op)(long long, long long);

    union {
        struct monkey *lhs;
        char lhs_name[4];
    };

    union {
        struct monkey *rhs;
        char rhs_name[4];
    };
};

static struct monkey *free_monkeys = NULL;
static struct monkey *used_monkeys = NULL;

static struct monkey *allocate_monkey(void) {
    struct monkey *result;
    if (free_monkeys != NULL) {
        result = free_monkeys;
        free_monkeys = free_monkeys->next;
    } else {
        result = malloc(sizeof(struct monkey));
    }
    result->next = used_monkeys;
    used_monkeys = result;
    return result;
}

static void reclaim_monkeys(void) {
    while (used_monkeys != NULL) {
        struct monkey *next = used_monkeys->next;
        used_monkeys->next = free_monkeys;
        free_monkeys = used_monkeys;
        used_monkeys = next;
    }
}

static struct monkey *make_monkey(struct monkey *lhs, struct monkey *rhs, long long (*op)(long long, long long)) {
    struct monkey *result = allocate_monkey();
    memset(result->name, 0, 4);
    result->lhs = lhs;
    result->rhs = rhs;
    result->op = op;
    return result;
}

static long long op_add(long long a, long long b) {
    return a + b;
}

static long long op_sub(long long a, long long b) {
    return a - b;
}

static long long op_mul(long long a, long long b) {
    return a * b;
}

static long long op_div(long long a, long long b) {
    return a / b;
}

static long long op_humn(
    __attribute__((__unused__)) long long a,
    __attribute__((__unused__)) long long b) {
    __builtin_unreachable();
}

static struct monkey *parse_monkey(const char *line) {
    struct monkey *result = allocate_monkey();
    memcpy(result->name, &line[0], 4);
    if (strlen(line) == 17) {
        memcpy(result->lhs_name, &line[6], 4);
        memcpy(result->rhs_name, &line[13], 4);
        switch (line[11]) {
            case '+':
                result->op = op_add;
                break;
            case '-':
                result->op = op_sub;
                break;
            case '*':
                result->op = op_mul;
                break;
            case '/':
                result->op = op_div;
                break;
        }
    } else {
        result->op = NULL;
        result->value = atoll(&line[5]);
    }
    return result;
}

struct monkey_hash {
    struct monkey_hash *next;
    struct monkey *monkey;
};

static void free_hash(struct monkey_hash **hashes) {
    for (int i = 0; i < 26; i++) {
        struct monkey_hash *hash = hashes[i];
        while (hash != NULL) {
            struct monkey_hash *next = hash->next;
            free(hash);
            hash = next;
        }
    }
}

static struct monkey *find_monkey(struct monkey_hash **hashes, const char *name) {
    struct monkey_hash *hash = hashes[name[0] - 'a'];
    for (;;) {
        if (!memcmp(hash->monkey->name, name, 4)) {
            return hash->monkey;
        }
        hash = hash->next;
    }
}

static void parse_input(const char *filename, struct monkey_hash **hashes) {
    FILE *f = fopen(filename, "r");
    char buf[32], *line;
    memset(hashes, 0, sizeof(struct monkey_hash *) * 26);
    while ((line = fgets(buf, sizeof(buf), f))) {
        line[strlen(line) - 1] = '\0';
        struct monkey *monkey = parse_monkey(line);
        struct monkey_hash *hash = malloc(sizeof(struct monkey_hash));
        hash->monkey = monkey;
        hash->next = hashes[monkey->name[0] - 'a'];
        hashes[monkey->name[0] - 'a'] = hash;
    }
    fclose(f);
    for (int i = 0; i < 26; i++) {
        struct monkey_hash *hash = hashes[i];
        while (hash != NULL) {
            if (hash->monkey->op != NULL) {
                hash->monkey->lhs = find_monkey(hashes, hash->monkey->lhs_name);
                hash->monkey->rhs = find_monkey(hashes, hash->monkey->rhs_name);
            }
            hash = hash->next;
        }
    }
}

static void calculate(struct monkey *monkey) {
    if (monkey->op != NULL && memcmp(monkey->name, "humn", 4)) {
        calculate(monkey->lhs);
        calculate(monkey->rhs);
        if (monkey->lhs->op == NULL && monkey->rhs->op == NULL) {
            monkey->value = monkey->op(monkey->lhs->value, monkey->rhs->value);
            monkey->op = NULL;
        }
    }
}

static void part_1(const char *filename) {
    struct monkey_hash *hashes[26];
    parse_input(filename, hashes);

    struct monkey *root = find_monkey(hashes, "root");
    free_hash(hashes);

    calculate(root);

    printf("part 1: %lld\n", root->value);
    reclaim_monkeys();
}

static void part_2(const char *filename) {
    struct monkey_hash *hashes[26];
    parse_input(filename, hashes);

    struct monkey *root = find_monkey(hashes, "root");
    struct monkey *humn = find_monkey(hashes, "humn");
    free_hash(hashes);

    humn->op = op_humn;

    struct monkey *lhs = root->lhs;
    struct monkey *rhs = root->rhs;

    for (;;) {
        calculate(lhs);
        calculate(rhs);

        if (lhs == humn && rhs->op == NULL) {
            break;
        } else if (lhs->op == op_add && lhs->lhs->op == NULL && lhs->rhs->op != NULL) {
            rhs = make_monkey(rhs, lhs->lhs, op_sub);
            lhs = lhs->rhs;
        } else if (lhs->op == op_add && lhs->lhs->op != NULL && lhs->rhs->op == NULL) {
            rhs = make_monkey(rhs, lhs->rhs, op_sub);
            lhs = lhs->lhs;
        } else if (lhs->op == op_sub && lhs->lhs->op == NULL && lhs->rhs->op != NULL) {
            struct monkey *inverter = allocate_monkey();
            memset(inverter->name, 0, 4);
            inverter->op = NULL;
            inverter->value = -1;
            lhs->rhs = make_monkey(lhs->rhs, inverter, op_mul);
            lhs->op = op_add;
        } else if (lhs->op == op_sub && lhs->lhs->op != NULL && lhs->rhs->op == NULL) {
            rhs = make_monkey(rhs, lhs->rhs, op_add);
            lhs = lhs->lhs;
        } else if (lhs->op == op_mul && lhs->lhs->op == NULL && lhs->rhs->op != NULL) {
            rhs = make_monkey(rhs, lhs->lhs, op_div);
            lhs = lhs->rhs;
        } else if (lhs->op == op_mul && lhs->lhs->op != NULL && lhs->rhs->op == NULL) {
            rhs = make_monkey(rhs, lhs->rhs, op_div);
            lhs = lhs->lhs;
        } else if (lhs->op == op_div && lhs->lhs->op != NULL && lhs->rhs->op == NULL) {
            rhs = make_monkey(rhs, lhs->rhs, op_mul);
            lhs = lhs->lhs;
        }
    }

    printf("part 2: %lld\n", rhs->value);
    reclaim_monkeys();
}

int main(int argc, char *argv[]) {
    const char *filename = argc > 1 ? argv[1] : "input";

    part_1(filename);
    part_2(filename);

    while (free_monkeys != NULL) {
        struct monkey *next = free_monkeys->next;
        free(free_monkeys);
        free_monkeys = next;
    }

    return EXIT_SUCCESS;
}
