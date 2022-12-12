from mpl_toolkits.mplot3d import Axes3D
import matplotlib.pyplot as plt
import matplotlib as mpl
from matplotlib import cm
import numpy as np

from queue import PriorityQueue

def char_to_height(i):
    if i == 'S':
        return 0
    elif i == 'E':
        return 25
    else:
        return ord(i) - 97

def load_grid(filename):
    with open(filename, 'r') as file:
        lines = file.read().splitlines()
        grid = np.array([[char_to_height(i) for i in line] for line in lines], dtype=np.int32)
        for y, line in enumerate(lines):
            for x, char in enumerate(line):
                if char == 'S':
                    start = y, x
                elif char == 'E':
                    end = y, x
    return grid, start, end

def hill_climb(grid, start, end, any_start):
    q = PriorityQueue()
    in_queue = set()
    dist = np.empty(grid.shape, dtype=np.int32)

    for y in range(grid.shape[0]):
        for x in range(grid.shape[1]):
            if grid[y, x] == 0 if any_start else (y, x) == start:
                dist[y, x] = 0
                q.put((0, (y, x)))
                in_queue.add((y, x))
            else:
                dist[y, x] = 1000000

    while not q.empty():
        old_dist, u = q.get()
        in_queue.remove(u)
        if old_dist != dist[u]:
            continue
        neighbors = []
        if u[0] + 1 < grid.shape[0]:
            neighbors.append((u[0] + 1, u[1]))
        if u[1] + 1 < grid.shape[1]:
            neighbors.append((u[0], u[1] + 1))
        if u[0] > 0:
            neighbors.append((u[0] - 1, u[1]))
        if u[1] > 0:
            neighbors.append((u[0], u[1] - 1))
        for v in neighbors:
            alt = dist[u] + 1
            old = dist[v]
            if grid[v] - grid[u] < 2 and alt < old:
                if v not in in_queue:
                    in_queue.add(v)
                    q.put((alt, v))
                    dist[v] = alt

    for y in range(grid.shape[0]):
        for x in range(grid.shape[1]):
            if dist[y, x] == 1000000:
                dist[y, x] = -1

    return dist

def show_visual(z, dist):
    # colormap to pick from
    scalar_map = cm.ScalarMappable(norm=mpl.colors.Normalize(vmin = 0.0, vmax=1.0), cmap=plt.get_cmap('summer'))
    # generate colors
    max_dist = np.max(dist)
    colors = np.zeros((z.shape[0], z.shape[1], 3))
    # value 0 is black, other values are white for now
    for y in range(z.shape[0]):
        for x in range(z.shape[1]):
            if dist[y, x] != -1:
                colors[y,x,:] = scalar_map.to_rgba(dist[y, x] / max_dist)[:3]
    x, y = range(z.shape[1]), range(z.shape[0])
    xpos, ypos = np.meshgrid(x, y)
    fig = plt.figure()
    ax = fig.add_subplot(111, projection='3d')
    ax.plot_surface(xpos, ypos, z, facecolors=colors, shade=False)
    ax.set_box_aspect([z.shape[1], z.shape[0], 25])
    plt.show()

z, start, end = load_grid('input')
dist_1 = hill_climb(z, start, end, False)
dist_2 = hill_climb(z, start, end, True)
show_visual(z, dist_1)
show_visual(z, dist_2)
