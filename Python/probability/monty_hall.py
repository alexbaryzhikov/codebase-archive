import random
from collections import Counter

def simulate_monty(strategy, doors=(1, 2, 3)):
    """Randomly place a car, and given a strategy of 'switch' or 'stick', return True iff
    the strategy wins."""
    car    = random.choice(doors)
    pick   = random.choice(doors)
    opened = random.choice([d for d in doors if d != car and d != pick])
    if strategy == 'switch':
        pick = next(d for d in doors if d != pick and d != opened)
    return (pick == car)

print(Counter(simulate_monty('switch') for _ in range(10**5)))
print(Counter(simulate_monty('stick') for _ in range(10**5)))
