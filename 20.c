#include <stdio.h>
#include <stdlib.h>

struct node {
    struct node *prev;
    struct node *next;
    long long value;
};

static struct node *parse_input(const char *filename, size_t *size, long long multiply) {
    FILE *f = fopen(filename, "r");
    char buf[64], *line;

    struct node *tail = NULL;
    *size = 0;
    while ((line = fgets(buf, sizeof(buf), f))) {
        struct node *new = malloc(sizeof(struct node));
        new->prev = tail;
        new->value = atoll(line) * multiply;
        tail = new;
        ++*size;
    }

    struct node *a = tail;
    struct node *b = tail->prev;
    while (b != NULL) {
        b->next = a;
        a = a->prev;
        b = b->prev;
    }
    a->prev = tail;
    tail->next = a;

    fclose(f);

    return a;
}

static struct node **capture_ordering(struct node *list, size_t size) {
    struct node **result = malloc(sizeof(struct node *) * size);

    for (size_t i = 0; i < size; i++) {
        result[i] = list;
        list = list->next;
    }

    return result;
}

static void extract_from_list(struct node *list) {
    list->prev->next = list->next;
    list->next->prev = list->prev;
}

static void mix(struct node **ordering, long long size) {
    for (size_t i = 0; i < size; i++) {
        struct node *list = ordering[i];
        long long amt = list->value % (size - 1);
        if (amt > (size / 2)) {
            amt -= size - 1;
        }
        if (amt > 0) {
            struct node *insertion = list;
            for (long long x = 0; x < amt; x++) {
                insertion = insertion->next;
            }
            extract_from_list(list);
            insertion->next->prev = list;
            list->next = insertion->next;
            list->prev = insertion;
            insertion->next = list;
        } else if (amt < 0) {
            struct node *insertion = list;
            for (long long x = 0; x < -amt; x++) {
                insertion = insertion->prev;
            }
            extract_from_list(list);
            insertion->prev->next = list;
            list->prev = insertion->prev;
            list->next = insertion;
            insertion->prev = list;
        }
    }
}

static long long mix_and_calculate(const char *filename, long long multiply, int repeat) {
    size_t size;
    struct node *list = parse_input(filename, &size, multiply);
    struct node **ordering = capture_ordering(list, size);
    long long result = 0;

    for (int i = 0; i < repeat; i++) {
        mix(ordering, size);
    }

    while (list->value != 0) {
        list = list->next;
    }

    for (int i = 0; i < 3; i++) {
        for (int j = 0; j < 1000; j++) {
            list = list->next;
        }
        result += list->value;
    }

    struct node *ptr = list;
    do {
        struct node *next = ptr->next;
        free(ptr);
        ptr = next;
    } while (ptr != list);
    free(ordering);

    return result;
}

int main(int argc, char *argv[]) {
    const char *filename = argc > 1 ? argv[1] : "input";
    printf("part 1: %lld\n", mix_and_calculate(filename, 1, 1));
    printf("part 2: %lld\n", mix_and_calculate(filename, 811589153, 10));
}
