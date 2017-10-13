"""
======================================================================================

Probability, Paradox, and the Reasonable Person Principle

http://nbviewer.jupyter.org/url/norvig.com/ipython/ProbabilityParadox.ipynb


Vocabulary:

P(E, D)         The probability of event E, given probability distribution D describing the
                complete sample space of possible outcomes.

Outcome         What actually happens (like a die rolling a 6). Also called an atomic event.

Event           A description of possibly several atomic events (like rolling an even number).

Sample space    The set of possible outcomes.

Probability distribution
                A mapping from every possible outcome to a number in the range 0 to 1 saying how
                likely the outcome is.

Uniform distribution
                A probability distribution in which every outcome is equally probable.

======================================================================================
"""

from fractions import Fraction
import random
from IPython.display import HTML
import matplotlib.pyplot as plt
from collections import Counter

def P(predicate, dist):
    """The probability that 'predicate' is true, given the probability distribution 'dist'."""
    return sum(dist[e] for e in dist if predicate(e))

class ProbDist(dict):
    """A Probability Distribution; an {outcome: prob} mapping where probabilities sum to 1."""
    def __init__(self, mapping=(), **kwargs):
            self.update(mapping, **kwargs)
            total = sum(self.values())
            if isinstance(total, int):
                    total = Fraction(total, 1)
            for key in self: # make probabilities sum to 1
                    self[key] = self[key] / total

    def __and__(self, predicate): # call this method by writing 'probdist & predicate'
            """A new ProbDist, restricted to the outcomes of this ProbDist for which the
            predicate is true."""
            return ProbDist({e:self[e] for e in self if predicate(e)})

def Uniform(outcomes): return ProbDist({e:1 for e in outcomes})

def joint(A, B, sep=''):
        """The joint distribution of two independent probability distributions.
        Result is all entries of the form {a+sep+b: P(a)*P(b)}"""
        return ProbDist({a + sep + b: A[a] * B[b] for a in A for b in B})

#-------------------------------------------------------------------------------------
#
# Child Paradoxes
#

## Child Problem 1: Older child is a boy. What is the probability both are boys?

two_kids = Uniform({'BG', 'BB', 'GB', 'GG'})

def two_boys(outcome): return outcome.count('B') == 2

def older_is_a_boy(outcome): return outcome.startswith('B')

# print(P(two_boys, two_kids & older_is_a_boy)) # 1/2


## Child Problem 2: At least one is a boy. What is the probability both are boys?

def at_least_one_boy(outcome): return 'B' in outcome

# print(P(two_boys, two_kids & at_least_one_boy)) # 1/3


## Child Problem 2: Another interpretation.

two_kids_b = Uniform({'BB/b?', 'BB/?b', 'BG/b?', 'BG/?g', 'GB/g?', 'GB/?b', 'GG/g?', 'GG/?g'})

def observed_boy(outcome): return 'b' in outcome

# print(P(two_boys, two_kids_b & observed_boy)) # 1/2


## Child Problem 3: One is a boy born on Tuesday. What's the probability both are boys?

one_kid_w  = joint(Uniform('BG'), Uniform('1234567'))
two_kids_w = joint(one_kid_w, one_kid_w)

# print(len(two_kids_w))
# print(random.sample(list(two_kids_w), 8))
# print(P(at_least_one_boy, two_kids))   # 3/4
# print(P(at_least_one_boy, two_kids_w)) # 3/4
# print(P(two_boys, two_kids))   # 1/4
# print(P(two_boys, two_kids_w)) # 1/4
# print(P(two_boys, two_kids & at_least_one_boy))   # 1/3
# print(P(two_boys, two_kids_w & at_least_one_boy)) # 1/3

def at_least_one_boy_tues(outcome): return 'B3' in outcome

# print(P(two_boys, two_kids_w & at_least_one_boy_tues)) # 13/27

## Child Problem 3: Another interpretation.

def observed_boy_tues(outcome): return 'b3' in outcome

two_kids_wb = Uniform({children + '/' + observation
                       for children in two_kids_w
                       for observation in (children[:2].lower()+'??', '??'+children[-2:].lower())})

# print(random.sample(list(two_kids_wb), 5))

# print(P(two_boys, two_kids_wb & observed_boy_tues))


#-------------------------------------------------------------------------------------
#
# The Sleeping Beauty Paradox
#
# Sleeping Beauty volunteers to undergo the following experiment and is told all of the following
# details: On Sunday she will be put to sleep. Then a fair coin will be tossed, to determine which
# experimental procedure to undertake:
#
# Heads: Beauty will be awakened and interviewed on Monday only.
# Tails: Beauty will be awakened and interviewed on Monday and Tuesday only.
#
# In all cases she is put back to sleep with an amnesia-inducing drug that makes her forget that
# awakening and sleep until the next one. In any case, she will be awakened on Wednesday without
# interview and the experiment ends. Any time Beauty is awakened and interviewed, she is asked,
# "What is your belief now for the proposition that the coin landed heads?"
#

beauty = Uniform({'heads/Monday/interviewed', 'heads/Tuesday/sleep',
                  'tails/Monday/interviewed', 'tails/Tuesday/interviewed'})

def t(property):
    """Return a predicate that is true of all outcomes that have 'property' as a substring."""
    return lambda outcome: property in outcome

# print(P(t('heads'), beauty & t('interviewed')))


#-------------------------------------------------------------------------------------
#
# The Monty Hall Paradox
#
# Suppose you're on a game show, and you're given the choice of three doors: Behind one door is
# a car; behind the others, goats. You pick a door, say No. 1, and the host, who knows what's
# behind the doors, opens another door, say No. 3, which has a goat. He then says to you, "Do you
# want to switch your choice to door No. 2?" Is it to your advantage to switch your choice?
#

monty  = Uniform({'Car1/Lo/Pick1/Open2', 'Car1/Hi/Pick1/Open3',
                  'Car2/Lo/Pick1/Open3', 'Car2/Hi/Pick1/Open3',
                  'Car3/Lo/Pick1/Open2', 'Car3/Hi/Pick1/Open2'})

# print(P(t("Car1"), monty & t("Open3")))
# print(P(t("Car2"), monty & t("Open3")))


#-------------------------------------------------------------------------------------
#
# Reasoning with non-Uniform Probability Distributions
#

DK = ProbDist(GG=121801., GB=126840.,
              BG=127123., BB=135138.)

## Child Problem 1 in DK
# print(P(two_boys, DK & older_is_a_boy))

## Child Problem 2 in DK
# print(P(two_boys, DK & at_least_one_boy))

## Child Problem 4. One is a boy born on Feb. 29. What is the probability both are boys?
sexes      = ProbDist(B=51.5, G=48.5)   # Probability distribution over sexes
days       = ProbDist(L=1, N=4*365)     # Probability distribution over Leap days and Non-leap days
child      = joint(sexes, days)         # Probability distribution for one child family
two_kids_L = joint(child, child)        # Probability distribution for two-child family

# print(P(two_boys, two_kids_L & t('BL')))


#-------------------------------------------------------------------------------------
#
# The St. Petersburg Paradox
#
# A casino offers a game of chance for a single player in which a fair coin is tossed at each
# stage. The pot starts at 2 dollars and is doubled every time a head appears. The first time a
# tail appears, the game ends and the player wins whatever is in the pot. Thus the player wins
# 2 dollars if a tail appears on the first toss, 4 dollars if a head appears on the first toss
# and a tail on the second, etc. What is the expected value of this game to the player?

## Response 1: Limited Resources

def st_pete(limit):
    """Return the probability distribution for the St. Petersburg Paradox with a limited bank."""
    P = {}    # The probability distribution
    pot = 2   # Amount of money in the pot
    pr = 1/2. # Probability that you end up with the amount in pot
    while pot < limit:
        P[pot]  = pr
        pot     = pot * 2
        pr      = pr / 2
    P[limit] = pr * 2   # pr * 2 because you get limit for heads or tails
    return ProbDist(P)

StP = st_pete(limit=10**8)

def EV(P):
    """The expected value of a probability distribution."""
    return sum(P[v] * v for v in P)

# print(EV(StP))

## Response 2: Value of Money

def util(dollars, enough=1000): 
    """The value of money: only half as valuable after you already have enough."""
    if dollars < enough:
        return dollars
    else:
        additional = dollars-enough
        return enough + util(additional / 2, enough * 2)

# X = list(range(1000, 1000000, 10000))
# fig = plt.figure()
# plt.plot(X, [util(x) for x in X])
# fig.show()

def EU(P, U):
    """The expected utility of a probability distribution, given a utility function."""
    return sum(P[e] * U(e) for e in P)

# print(EU(StP, util))


#-------------------------------------------------------------------------------------
#
# Understanding St. Petersburg through Simulation
#

def flip(): return random.choice(('head', 'tail'))

def simulate_st_pete(limit=10**9):
    """Simulate one round of the St. Petersburg game, and return the payoff."""
    pot = 2
    while flip() == 'head':
        pot <<= 1
        if pot > limit:
            return limit
    return pot

# random.seed(123456)
# c = Counter(simulate_st_pete() for _ in range(100000))
# results = ProbDist(c)
# print(EU(results, util), float(EV(results)))

## Plot the running average of repeated rounds

def running_averages(iterable):
    """For each element in the iterable, yield the mean of all elements seen so far."""
    total, n = 0, 0
    for x in iterable:
        total, n = total + x, n + 1
        yield total / n

def plot_running_averages(fn, n):
    """Plot the running average of calling the function n times."""
    plt.plot(list(running_averages(fn() for _ in range(n))))

# random.seed('running')
# fig = plt.figure()
# plt.grid(True)
# for i in range(10):
#     plot_running_averages(simulate_st_pete, 100000)
# fig.show()


#-------------------------------------------------------------------------------------
#
# The Ellsburg Paradox
#
# An urn contains 33 red balls and 66 other balls that are either black or yellow. You don't know
# the mix of black and yellow, just that they total 66. A single ball is drawn at random. You are
# given a choice between these two gambles:
#
# R: Win $100 for a red ball.
# B: Win $100 for a black ball.
#
# You are also given a choice between these two gambles:
#
# RY: Win $100 for a red or yellow ball.
# BY: Win $100 for a black or yellow ball.
#

def ellsburg():
    fig = plt.figure()
    show('R', 'r')
    show('B', 'k')
    show('RY', 'r--')
    show('BY', 'k--')
    plt.xlabel('Number of black balls')
    plt.ylabel('Expected value of each gamble')
    fig.show()

blacks = list(range(67))
urns   = [Counter(R=33, B=b, Y=66-b) for b in blacks]

def show(colors, line):
    scores = [score(colors, urn) for urn in urns]
    plt.plot(blacks, scores, line)

def score(colors, urn): return sum(urn[c] for c in colors)

# ellsburg()

def avgscore(colors, urns):
    return sum(score(colors, urn) for urn in urns) / len(urns)

def compare(urns):
    for colors in ('R', 'B', 'RY', 'BY'):
        print(colors.ljust(2), avgscore(colors, urns))

# compare(urns)
# print()
# compare(urns[:33] + 2 * urns[33:])
# print()
# compare(2 * urns[:33] + urns[33:])

def ellsburg2():
    fig = plt.figure()
    show2('R', 'r')
    show2('B', 'k')
    show2('RY', 'r--')
    show2('BY', 'k--')
    plt.xlabel('Different combinations of two urns')
    plt.ylabel('Expected value of each gamble')
    fig.show()

def show2(colors, line):
    urnpairs = [(u1, u2) for u1 in urns for u2 in urns]
    urnpairs.sort(key=lambda urns: avgscore('B', urns))
    X = list(range(len(urnpairs)))
    plt.plot(X, [avgscore(colors, urns) for urns in urnpairs], line)

# ellsburg2()


#-------------------------------------------------------------------------------------
#
# Simpson's Paradox
#

## Good and bad outcomes for kidney stone reatments A and B,
## each in two cases: [small_stones, large_stones]
A = dict(small=Counter(good=81.,  bad=6.),  large=Counter(good=192., bad=71.))
B = dict(small=Counter(good=234., bad=36.), large=Counter(good=55., bad=25.))

def success(case): return ProbDist(case)['good']

# print(success(A['small']), success(B['small']))
# print(success(A['large']), success(B['large']))
# print(success(A['small'] + A['large']))
# print(success(B['small'] + B['large']))

## Batting averages for two baseball players
Jeter   = {1995: Counter(hit=12,  out=36),  1996: Counter(hit=183, out=399)}
Justice = {1995: Counter(hit=104, out=307), 1996: Counter(hit=45,  out=95)}

def BA(case): "Batting average"; return round(float(ProbDist(case)['hit']), 3)

# print(BA(Jeter[1995]), BA(Justice[1995]))
# print(BA(Jeter[1996]), BA(Justice[1996]))
# print()
# print(BA(Jeter[1995] + Jeter[1996]))
# print(BA(Justice[1995] + Justice[1996]))

