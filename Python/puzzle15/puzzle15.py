"""15 Puzzle solver."""

import random, cProfile, pickle, sys
import matplotlib.pyplot as plt
from queue import PriorityQueue

# Feel free to change start directly
start = \
    '1324' \
    '5678' \
    '9ABC' \
    'DFE '

# Change goal only via goal_new(state) method.
# Otherwise proxies must be manually rebuilt by calling build_proxy_goals().
goal = \
    '1234' \
    '5678' \
    '9ABC' \
    'DEF '

# -----------------------------------------------------------------------------
# Solver

VERBOSE      = False
SEARCH_DEPTH = 15
full_path    = []
explored     = set()

def solve(start, mode=0):
    """Solve 15-puzzle: search for moves sequence between start and goal states.
    Modes: 0 - default; 1 - fast."""
    global goal, goal_proxy, full_path, explored
    
    if not explored:  # initial call
        if goal_proxy is None:
            build_proxy_goals()
        if not VERBOSE:
            print('Searching... ', end='')
            sys.stdout.flush()
        if mode == 1:  # fast search
            path = greedy_path_search(start, goal)[1::2]
            if path:
                full_path.append(' '.join(path))
                return finalize()
            else:
                return finalize(False)

    # default search
    res = shortest_path_search(start, goal)
    if type(res) == list:  # path is found, or deadend was hit
        if res:
            full_path.append(' '.join(res[1::2]))
            return finalize()
        else:
            return finalize(False)
    
    # search reached depth limit, frontier returned
    frontier = res
    states = [x[-1] for _, x in frontier.queue]
    
    # check if any state is in proximity of a goal
    for i in range(1, 12):  # look from closest proximity outward
        hits = set(states) & set(goal_proxy[i])
        if hits:  # guaranteed to find path within next search
            if VERBOSE: print('solve: Encountered proxy')
            s = hits.pop()
            j = states.index(s)
            full_path.append(' '.join(frontier.queue[j][1][1::2]))
            full_path.append(' '.join(shortest_path_search(s, goal)[1::2]))
            return finalize()
    
    # search most promising states first, according to utility function
    if VERBOSE: print('solve: Using heuristic')
    Qstates = sorted(states, key=U)
    for s in Qstates:
        j = states.index(s)
        full_path.append(' '.join(frontier.queue[j][1][1::2]))
        path = solve(s)
        if path:    # success
            return path
        else:       # search failed, backtrack
            if VERBOSE: print('solve: Backtracking')
            del(full_path[-1])
    return finalize(False)

def shortest_path_search(start, goal, depth=SEARCH_DEPTH):
    """Find the shortest path from start to goal."""
    global successors, explored
    if start == goal:
        return [start]
    frontier = PriorityQueue()
    frontier.put((0, [start]))
    plen, plen_prev = 0, 0
    if VERBOSE: print('\rsearch: Path length:', plen, end='')
    while not frontier.empty():
        plen = len(frontier.queue[0][1][1::2])
        if plen > plen_prev:
            plen_prev = plen
            if VERBOSE: print('\rsearch: Path length:', plen, end='')
        if plen == depth:  # stop if depth is reached
            if VERBOSE: print()
            return frontier
        p = frontier.get()[1]
        s = p[-1]
        if s == goal:  # path found
            if VERBOSE: print()
            explored.clear()
            return p
        explored.add(s)
        for s2, a in successors(s).items():
            if s2 not in explored:
                p2 = p + [a, s2]
                priority = len(p2)
                frontier.put((priority, p2))
    if VERBOSE: print()
    explored.clear()
    return []  # failed to find path

def greedy_path_search(start, goal):
    """Greedily search path from start to goal."""
    global goal_proxy, successors
    if start == goal:
        return [start]
    frontier = PriorityQueue()
    frontier.put((0, [start]))
    explored = set()
    while not frontier.empty():
        p = frontier.get()[1]
        s = p[-1]
        if s == goal:  # path found
            return p
        # check if state is in proximity of a goal
        for i in range(1, 12):  # look from closest proximity outward
            if s in goal_proxy[i]:
                return p + shortest_path_search(s, goal)[1:]  # switch to optimal search
        # continue greedy search
        explored.add(s)
        for s2, a in successors(s).items():
            if s2 not in explored:
                p2 = p + [a, s2]
                priority = U(s2)
                frontier.put((priority, p2))
    return []  # failed to find path

def successors(state):
    """Return a dict of {state: action} pairs of legal actions."""
    i = state.index(' ')
    res = {}
    x, y = i%4, i//4
    for x2, y2 in ((x+1, y), (x, y+1), (x-1, y), (x, y-1)):
        if 0 <= x2 < 4 and 0 <= y2 < 4:
            j = y2*4 + x2
            state2 = str_swap(state, i, j)
            res[state2] = state2[i]
    return res

def str_swap(s, i, j):
    """Swaps i-th and j-th characters of s."""
    if i < j:
        return ''.join([s[:i], s[j], s[i+1:j], s[i], s[j+1:]])
    else:
        return ''.join([s[:j], s[i], s[j+1:i], s[j], s[i+1:]])

def U(state):
    """State utility function. Sum of distances of elems to their goal positions."""
    global goal
    sum_of_distances = 0
    for i in range(16):
        d = abs(state.index(goal[i]) - i)
        sum_of_distances += d//4 + d%4
    return sum_of_distances

def finalize(found=True):
    """Print results, reset global variables and return final path."""
    global full_path, explored
    print('done')
    if found:
        path = ' '.join(full_path)
        print('Path:', path)
        print('Path len:', len(path.split()))
        full_path = []
        explored.clear()
        return path
    else:
        print('Path not found')
        full_path = []
        explored.clear()
        return ''


# -----------------------------------------------------------------------------
# Visualization

def show(state):
    """Print out state."""
    for i in range(0, 16, 4):
        row = state[i:i+4]
        for item in row:
            print('{:>3}'.format(item), end='')
        print()

def plot(state):
    """Plot state."""
    # data
    num = {a:b for a,b in zip(' 123456789ABCDEF', [0] + list(range(4, 19)))}
    x, y = zip(*[(b, -a) for a in range(4) for b in range(4) for _ in range(num[state[a*4+b]])])
    # plot
    fig = plt.figure(figsize=(4, 4))
    fig.set_tight_layout(True)
    ax = fig.add_subplot(111)
    ax.tick_params(axis='x', bottom='off', labelbottom='off')
    ax.tick_params(axis='y', left='off', labelleft='off')
    ax.hist2d(x, y, bins=4)
    plt.show()
   
def plot_path(path, start, goal_=None):
    """Export path states as images."""
    global goal
    if goal_ is None:
        goal_ = goal
    # data
    num = {a:b for a,b in zip(' 123456789ABCDEF', [0] + list(range(4, 19)))}
    x, y = zip(*[(b, -a) for a in range(4) for b in range(4) for _ in range(num[start[a*4+b]])])
    # plot
    fig = plt.figure(figsize=(4, 4))
    fig.set_tight_layout(True)
    ax = fig.add_subplot(111)
    ax.tick_params(axis='x', bottom='off', labelbottom='off')
    ax.tick_params(axis='y', left='off', labelleft='off')
    ax.clear()
    ax.hist2d(x, y, bins=4)
    plt.savefig('puzzle_000.png')
    # frames
    state = start
    blank = state.index(' ')
    for n, item in enumerate(path.split(), 1):
        print('\rWriting frames... puzzle_%03d.png' % n, end='')
        sys.stdout.flush()
        i = state.index(item)
        state = str_swap(state, i, blank)
        blank = state.index(' ')
        x, y = zip(*[(b, -a) for a in range(4) for b in range(4) for _ in range(num[state[a*4+b]])])
        ax.clear()
        ax.hist2d(x, y, bins=4)
        plt.savefig('puzzle_%03d.png' % n)
    print('\rWriting frames... done' ' '*20)


# -----------------------------------------------------------------------------
# Utils

def shuffle(n, state=None):
    """Produce a solvable random state by shuffling goal."""
    global goal
    if state is None:
        state = goal
    for _ in range(n):
        state = random.choice(list(successors(state)))
    return state

def goal_new(state):
    """Change default goal."""
    global goal
    goal = state
    build_proxy_goals()

def build_proxy_goals():
    """Build and export a library of states that are in proximity of a goal."""
    global goal, goal_proxy
    print('Building proxy goals... ', end='')
    sys.stdout.flush()
    blank = goal.index(' ')
    i, j = 0, 0  # indexes of elements to swap
    while i == blank: i += 1
    while j == blank or j == i: j += 1
    s = str_swap(goal, i, j)  # odd swap parity guarantees there's no solution
    goal_proxy = {}
    for k in range(SEARCH_DEPTH):
        goal_proxy[k] = set([x[-1] for _, x in shortest_path_search(goal, s, depth=k).queue])
        explored.clear()
    save(goal_proxy, 'goal_proxy.pickle')
    print('done')

def save(item, fname):
    """Export item to file."""
    with open(fname, 'wb') as f:
        pickle.dump(item, f, pickle.HIGHEST_PROTOCOL)

def load(fname):
    """Import item from file."""
    try:
        with open(fname, 'rb') as f:
            return pickle.load(f)
    except:
        return None


# -----------------------------------------------------------------------------
# Autorun

goal_proxy = load('goal_proxy.pickle')
if not goal_proxy or goal not in goal_proxy[0]:  # proxies are for different goal
    goal_new(goal)


# -----------------------------------------------------------------------------
# Tests

layouts = [ \
    '1234' \
    '5678' \
    '9ABC' \
    'DFE ',

    '1234' \
    '5 78' \
    '9ABC' \
    'DFE6',

    '1234' \
    '5C78' \
    '9AB ' \
    'DFE6',

    '1 34' \
    '5278' \
    '9ABC' \
    'DFE6']

def test_successors():
    assert successors(layouts[0]) == \
        {'123456789ABCDF E': 'E', '123456789AB DFEC': 'C'}
    assert successors(layouts[1]) == \
        {'123457 89ABCDFE6': '7', '12345A789 BCDFE6': 'A',
         '1234 5789ABCDFE6': '5', '1 3452789ABCDFE6': '2'}
    assert successors(layouts[2]) == \
        {'12345C789AB6DFE ': '6', '12345C789A BDFE6': 'B',
        '12345C7 9AB8DFE6': '8'}
    assert successors(layouts[3]) == \
        {'13 452789ABCDFE6': '3', '12345 789ABCDFE6': '2',
         ' 13452789ABCDFE6': '1'}
    return 'successors passes'

def test_path(path, start, _goal=None):
    """Start at start and execute all steps along the path."""
    global goal
    if _goal is None:
        _goal = goal
    state = start
    show(state)
    print()
    blank = state.index(' ')
    for a in path.split():
        i = state.index(a)
        if i < blank:
            state = str_swap(state, i, blank)
        else:
            state = str_swap(state, blank, i)
        blank = state.index(' ')
        show(state)
        print()
    if state == _goal:
        print('Goal reached')
    else:
        print('Failed to reach the goal')

def test():
    test_successors()
    return 'tests pass'

# print(test())
# cProfile.run('solve(start)', sort='tottime')

## Example usage
# a = shuffle(1000)   # get random state
# show(a)             # show state in console
# plot(a)             # plot state
# p = solve(a)        # find solution (slow, shorter path)
# p = solve(a,1)      # find solution (fast, longer path)
# test_path(p, a)     # test solution in console
# plot_path(p, a)     # export solution frames
