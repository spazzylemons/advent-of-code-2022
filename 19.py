from dataclasses import dataclass
from multiprocessing import Pool

@dataclass
class Blueprint:
    index: int
    ore_robot_cost_ore: int
    clay_robot_cost_ore: int
    obs_robot_cost_ore: int
    obs_robot_cost_clay: int
    geode_robot_cost_ore: int
    geode_robot_cost_obs: int

def parse_line(line):
    x = ''
    for i in line:
        if not (48 <= ord(i) <= 57):
            x += ' '
        else:
            x += i
    return Blueprint(*(int(i) for i in x.split(' ') if len(i)))

def parse_input(filename):
    with open(filename, 'r') as file:
        for line in file.read().splitlines():
            yield parse_line(line)

MINUTES = 24

def maximize(cache, blueprint, minute, ore, clay, obs, ore_robot, clay_robot, obs_robot, geode_robot):
    key = (minute, ore, clay, obs, ore_robot, clay_robot, obs_robot, geode_robot)
    if key in cache:
        return cache[key]
    # state = state + ((ore, clay, obs, ore_robot, clay_robot, obs_robot, geode_robot),)
    best_result = 0
    if minute < MINUTES:
        max_ore_cost = max(blueprint.ore_robot_cost_ore, blueprint.clay_robot_cost_ore, blueprint.obs_robot_cost_ore, blueprint.geode_robot_cost_ore)
        if ore >= blueprint.geode_robot_cost_ore and obs >= blueprint.geode_robot_cost_obs:
            x = maximize(
                cache,
                blueprint,
                minute + 1,
                ore + ore_robot - blueprint.geode_robot_cost_ore,
                clay + clay_robot,
                obs + obs_robot - blueprint.geode_robot_cost_obs,
                ore_robot,
                clay_robot,
                obs_robot,
                geode_robot + 1)
            if x > best_result:
                best_result = x
        elif ore >= max_ore_cost and clay >= blueprint.obs_robot_cost_clay and obs_robot < blueprint.geode_robot_cost_obs:
            x = maximize(
                cache,
                blueprint,
                minute + 1,
                ore + ore_robot - blueprint.obs_robot_cost_ore,
                clay + clay_robot - blueprint.obs_robot_cost_clay,
                obs + obs_robot,
                ore_robot,
                clay_robot,
                obs_robot + 1,
                geode_robot)
            if x > best_result:
                best_result = x
        else:
            if ore >= blueprint.obs_robot_cost_ore and clay >= blueprint.obs_robot_cost_clay and obs_robot < blueprint.geode_robot_cost_obs:
                x = maximize(
                    cache,
                    blueprint,
                    minute + 1,
                    ore + ore_robot - blueprint.obs_robot_cost_ore,
                    clay + clay_robot - blueprint.obs_robot_cost_clay,
                    obs + obs_robot,
                    ore_robot,
                    clay_robot,
                    obs_robot + 1,
                    geode_robot)
                if x > best_result:
                    best_result = x
            if ore >= blueprint.ore_robot_cost_ore and ore_robot < max_ore_cost:
                x = maximize(
                    cache,
                    blueprint,
                    minute + 1,
                    ore + ore_robot - blueprint.ore_robot_cost_ore,
                    clay + clay_robot,
                    obs + obs_robot,
                    ore_robot + 1,
                    clay_robot,
                    obs_robot,
                    geode_robot)
                if x > best_result:
                    best_result = x
            if ore >= blueprint.clay_robot_cost_ore and clay_robot < blueprint.obs_robot_cost_clay:
                x = maximize(
                    cache,
                    blueprint,
                    minute + 1,
                    ore + ore_robot - blueprint.clay_robot_cost_ore,
                    clay + clay_robot,
                    obs + obs_robot,
                    ore_robot,
                    clay_robot + 1,
                    obs_robot,
                    geode_robot)
                if x > best_result:
                    best_result = x
            x = maximize(
                cache,
                blueprint,
                minute + 1,
                ore + ore_robot,
                clay + clay_robot,
                obs + obs_robot,
                ore_robot,
                clay_robot,
                obs_robot,
                geode_robot)
            if x > best_result:
                best_result = x
    best_result += geode_robot
    cache[key] = best_result
    return best_result
def maximize_one(blueprint):
    cache = {}
    return (maximize(cache, blueprint, 1, 0, 0, 0, 1, 0, 0, 0), blueprint.index)

blueprints = list(parse_input('sample'))
result = 0
with Pool(4) as p:
    for i, index in p.map(maximize_one, blueprints):
        print((i, index))
        result += i * index
print(result)
result = 1
MINUTES = 32
with Pool(3) as p:
    for i, _ in p.map(maximize_one, blueprints[:3]):
        result *= i
print(result)
