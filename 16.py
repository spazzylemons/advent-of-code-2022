import re

from functools import cache
import itertools

LINE = re.compile(r'^Valve (..) has flow rate=(.+?); tunnels? leads? to valves? (.+)$')

edges = {}
rates = {}

with open('input', 'r') as file:
    for line in file.read().splitlines():
        match = LINE.match(line)
        edges[match[1]] = {i: 1 for i in match[3].split(', ')}
        rates[match[1]] = int(match[2])

# make graph complete
for node in edges:
    for u, v in itertools.permutations(edges[node], 2):
        new_weight = edges[node][v] + edges[node][u]
        if u not in edges[v]:
            edges[v][u] = new_weight
        elif new_weight < edges[v][u]:
            edges[v][u] = new_weight
        if v not in edges[u]:
            edges[u][v] = new_weight
        elif new_weight < edges[u][v]:
            edges[u][v] = new_weight

# remove edges that connect to broken valves
for node in edges:
    edges[node] = {k: edges[node][k] for k in edges[node] if rates[k]}

TOTAL_MINUTES = 30

@cache
def calculate_pressure(sequence):
    result = 0
    for minute, valve in sequence:
        result += (TOTAL_MINUTES - minute) * rates[valve]
    return result

@cache
def naive_approach(current, minute, currently_open):
    best_sequence = ()
    best_pressure = 0
    for edge in edges[current]:
        if edge not in currently_open:
            weight = edges[current][edge]
            new_minute = minute + weight + 1
            if new_minute < TOTAL_MINUTES:
                new_currently_open = tuple(sorted(currently_open + (edge,)))
                sequence = ((new_minute, edge),) + naive_approach(edge, new_minute, new_currently_open)
                pressure = calculate_pressure(sequence)
                if pressure > best_pressure:
                    best_pressure = pressure
                    best_sequence = sequence
    return best_sequence

@cache
def naive_approach_two(states, currently_open):
    best_sequence = ()
    best_pressure = 0
    state_options = ([], [])
    for i, state in enumerate(states):
        minute, current = state
        for edge in edges[current]:
            if edge not in currently_open:
                weight = edges[current][edge]
                new_minute = minute + weight + 1
                if new_minute < TOTAL_MINUTES:
                    state_options[i].append((new_minute, edge))
    for state_1, state_2 in itertools.product(*state_options):
        new_states = tuple(sorted((state_1, state_2)))
        if new_states[1][1] == new_states[0][1]:
            new_states = new_states[:1]
        new_currently_open = tuple(sorted(set(currently_open + tuple(i[1] for i in new_states))))
        sequence = new_states + naive_approach_two(new_states, new_currently_open)
        pressure = calculate_pressure(sequence)
        if pressure > best_pressure:
            best_pressure = pressure
            best_sequence = sequence
    return best_sequence

sequence = naive_approach('AA', 0, ())
print(sequence)
print(calculate_pressure(sequence))
TOTAL_MINUTES -= 4
calculate_pressure.cache_clear()
sequence = naive_approach_two(((0, 'AA'), (0, 'AA')), ())
print(sequence)
print(calculate_pressure(sequence))
