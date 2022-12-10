#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct stack {
    struct stack *next;
    char c;
};

static struct stack *push_stack(struct stack *stack, char c) {
    struct stack *new = malloc(sizeof(struct stack));
    new->next = stack;
    new->c = c;
    return new;
}

static struct stack *pop_stack(struct stack *stack, char *out) {
    if (stack == NULL) {
        printf("what the hell are you doing\n");
    }
    struct stack *result = stack->next;
    *out = stack->c;
    free(stack);
    return result;
}

static struct stack *reverse_stack(struct stack *stack) {
    struct stack *old = stack;
    struct stack *new = NULL;
    while (old != NULL) {
        struct stack *old_next = old->next;
        old->next = new;
        new = old;
        old = old_next;
    }
    return new;
}

static void free_stack(struct stack *stack) {
    while (stack != NULL) {
        struct stack *next = stack->next;
        free(stack);
        stack = next;
    }
}

static void solver(bool do_reverse) {
    FILE *f = fopen("input", "rb");
    struct stack **stacks = NULL;
    int num_stacks;
    char buffer[256], *line;

    while ((line = fgets(buffer, sizeof(buffer), f)) && strstr(line, "[")) {
        if (stacks == NULL) {
            num_stacks = strlen(line) / 4;
            stacks = malloc(sizeof(struct stack *) * num_stacks);
            for (int i = 0; i < num_stacks; i++) {
                stacks[i] = NULL;
            }
        }
        for (int i = 0; i < num_stacks; i++) {
            char c = line[((i * 4) + 1)];
            if (c != ' ') {
                stacks[i] = push_stack(stacks[i], c);
            }
        }
    }

    for (int i = 0; i < num_stacks; i++) {
        stacks[i] = reverse_stack(stacks[i]);
    }

    // ignore line we won't use
    fgets(buffer, sizeof(buffer), f);

    while ((line = fgets(buffer, sizeof(buffer), f))) {
        int count, src, dst;
        sscanf(line, "move %d from %d to %d", &count, &src, &dst);
        --src;
        --dst;

        struct stack *saved = NULL;
        for (int i = 0; i < count; i++) {
            char c;
            stacks[src] = pop_stack(stacks[src], &c);
            saved = push_stack(saved, c);
        }

        if (do_reverse) {
            saved = reverse_stack(saved);
        }

        for (int i = 0; i < count; i++) {
            char c;
            saved = pop_stack(saved, &c);
            stacks[dst] = push_stack(stacks[dst], c);
        }
    }

    for (int i = 0; i < num_stacks; i++) {
        char c;
        free_stack(pop_stack(stacks[i], &c));
        printf("%c", c);
    }
    printf("\n");
    fclose(f);
    free(stacks);
}

int main(void) {
    printf("part 1: ");
    solver(true);
    printf("part 2: ");
    solver(false);
}
