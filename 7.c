#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define PART_1_THRESHOLD 100000
#define DISK_SIZE 70000000
#define REQUIRED_SPACE 30000000

// 1 to include root directory
static int num_dirs = 1;
static int *dir_sizes;
static int dir_sizes_index = 0;

struct file {
    int size;
    char *name;
    struct file *next;
    struct file *children;
    struct file *parent;
};

static struct file *get_child(struct file *directory, const char *name, int size) {
    for (struct file *child = directory->children; child != NULL; child = child->next) {
        if (!strcmp(child->name, name)) {
            return child;
        }
    }

    struct file *new_file = malloc(sizeof(struct file));

    new_file->size = size;

    new_file->name = malloc(strlen(name) + 1);
    strcpy(new_file->name, name);

    new_file->next = directory->children;
    directory->children = new_file;

    new_file->children = NULL;
    new_file->parent = directory;

    if (size == 0) {
        ++num_dirs;
    }

    return new_file;
}

static int tree_walk(struct file *file) {
    int sum = file->size;

    struct file *next;
    for (struct file *child = file->children; child != NULL; child = next) {
        // get next before calling as the recursive call frees the child
        next = child->next;
        sum += tree_walk(child);
    }

    if (file->size == 0) {
        dir_sizes[dir_sizes_index++] = sum;
    }

    free(file->name);
    free(file);

    return sum;
}

int main(void) {
    FILE *f = fopen("input", "r");
    char buffer[256], *line;
    struct file *root = malloc(sizeof(struct file));
    root->size = 0;
    root->name = NULL;
    root->next = NULL;
    root->children = NULL;
    root->parent = NULL;
    struct file *current = root;

    while ((line = fgets(buffer, sizeof(buffer), f))) {
        char *ptr = line, *start = line, *parts[3];
        int index = 0;
        while (*ptr) {
            if (*ptr == ' ' || *ptr == '\n') {
                *ptr = '\0';
                parts[index++] = start;
                start = ptr + 1;
            }
            ++ptr;
        }

        if (!strcmp(parts[0], "$")) {
            if (!strcmp(parts[1], "cd")) {
                if (!strcmp(parts[2], "..")) {
                    current = current->parent;
                } else if (!strcmp(parts[2], "/")) {
                    current = root;
                } else {
                    current = get_child(current, parts[2], 0);
                }
            }
        } else if (!strcmp(parts[0], "dir")) {
            get_child(current, parts[1], 0);
        } else {
            get_child(current, parts[1], atoi(parts[0]));
        }
    }
    dir_sizes = malloc(sizeof(int) * num_dirs);
    int root_size = tree_walk(root);
    int part_1 = 0;
    int part_2 = DISK_SIZE;
    for (int i = 0; i < num_dirs; i++) {
        int entry = dir_sizes[i];

        if (entry <= PART_1_THRESHOLD) {
            part_1 += entry;
        }

        int free_space = DISK_SIZE - (root_size - entry);
        if (free_space >= REQUIRED_SPACE && entry < part_2) {
            part_2 = entry;
        }
    }
    printf("part 1: %d\n", part_1);
    printf("part 2: %d\n", part_2);
    free(dir_sizes);
    fclose(f);
    return EXIT_SUCCESS;
}
