from functools import reduce
from operator  import mul

def tictactoe_states(size=3):
    """Returns the number of states in tictactoe game on board of size x size.
    Mirror states like [xo] and [ox] are counted once, so games with identical
    moves but x's and o's swapped are considered the same. Winning conditions
    are ignored"""
    positions = size**2 # number of positions
    states = 0
    crosses = 0
    zeros = 0
    while crosses + zeros < positions:
        states += state_permutations(crosses, zeros, positions)
        crosses += 1
        states += state_permutations(crosses, zeros, positions)
        zeros += 1
    if crosses + zeros == positions:
        states += state_permutations(crosses, zeros, positions)
    return states

def state_permutations(crosses, zeros, positions):
    """Returns the number of all possible placements of fixed amount of crosses
    and zeros on the board"""
    crossesPerms = nCk(positions, crosses)
    zerosPerms = nCk(positions - crosses, zeros)
    return crossesPerms * zerosPerms

def nCk(n, k):
    """Returns the number of combinations of size k from n elements"""
    numer = int(reduce(mul, (n - i for i in range(k)), 1))
    denom = int(reduce(mul, (i + 1 for i in range(k)), 1))
    return numer//denom
