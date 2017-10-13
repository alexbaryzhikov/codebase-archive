import random
from collections import Counter

def simulate_stpete():
    """Toss a coin. Each heads doubles the bank, first tail finishes the game."""
    bank = 2
    toss = random.choice(('h', 't'))
    while toss == 'h':
        bank <<= 1
        toss = random.choice(('h', 't'))
    return bank

print(Counter(simulate_stpete() for _ in range(10**5)))
