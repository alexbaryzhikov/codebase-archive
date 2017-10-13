from collections import Counter, deque
import random
import matplotlib.pyplot as plt
from prob_intro import ProbDist


## The board: a list of the names of the 40 squares
## As specified by https://projecteuler.net/problem=84
board = """GO   A1 CC1 A2  T1 R1 B1  CH1 B2 B3
           JAIL C1 U1  C2  C3 R2 D1  CC2 D2 D3
           FP   E1 CH2 E2  E3 R3 F1  F2  U2 F3
           G2J  G1 G2  CC3 G3 R4 CH3 H1  T2 H2""".split()

def monopoly(steps):
    """Simulate given number of steps of Monopoly game,
    yielding the number of the current square after each step."""
    goto(0) # start at GO
    CC_deck = Deck('GO JAIL' + 14 * ' ?')
    CH_deck = Deck('GO JAIL C1 E3 H2 R1 R R U -3' + 6 * ' ?')
    doubles = 0
    jail = board.index('JAIL')
    for _ in range(steps):
        d1, d2 = random.randint(1, 6), random.randint(1, 6)
        goto(here + d1 + d2)
        doubles = (doubles + 1) if (d1 == d2) else 0
        if doubles == 3 or board[here] == 'G2J':
            goto(jail)
        elif board[here].startswith('CC'):
            do_card(CC_deck)
        elif board[here].startswith('CH'):
            do_card(CH_deck)
        yield here

def goto(square):
    """Update the global variable 'here' to be square."""
    global here
    here = square % len(board)

def Deck(names):
    """Make a shuffle deck of cards, given a space-delimited string."""
    cards = names.split()
    random.shuffle(cards)
    return deque(cards)

def do_card(deck):
    """Take the top card from deck and do what it says."""
    global here
    card = deck[0]          # The top card
    deck.rotate(-1)         # Move top card to bottom of deck
    if card == 'R' or card == 'U':
        while not board[here].startswith(card):
            goto(here + 1)  # Advance to next railroad or utility
    elif card == '-3':
        goto(here - 3)      # Go back 3 spaces
    elif card != '?':
        goto(board.index(card)) # Go to destination named on card

results = list(monopoly(400000))

spaces_stats = list(0 for _ in range(40))
for v in results:
    spaces_stats[v] += 1

## Graph the results
plt.figure(figsize=(14,7))
plt.bar(range(40), spaces_stats)
avg = len(results) / 40
plt.plot([-0.5, 39.5], [avg, avg], 'r--');
plt.xticks(range(40), board)
plt.savefig('monopoly')

## Show the results as a ProbDist
print(ProbDist(Counter(board[i] for i in results)))
