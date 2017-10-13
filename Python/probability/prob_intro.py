"""
======================================================================================

A Concrete Introduction to Probability
by Peter Norvig

http://nbviewer.jupyter.org/url/norvig.com/ipython/Probability.ipynb


Vocabulary:

Experiment      An occurrence with an uncertain outcome that we can observe. 
                For example, rolling a die.

Outcome         The result of an experiment; one particular state of the world.
                What Laplace calls a "case." 
                For example: 4.

Sample Space    The set of all possible outcomes for the experiment. 
                For example, {1, 2, 3, 4, 5, 6}.

Event           A subset of possible outcomes that together have some property we are
                interested in. 
                For example, the event "even die roll" is the set of outcomes {2, 4, 6}.

Probability     As Laplace said, the probability of an event with respect to a sample space is
                the number of favorable cases (outcomes from the sample space that are in the
                event) divided by the total number of cases in the sample space. (This assumes
                that all outcomes in the sample space are equally likely.) Since it is a ratio,
                probability will always be a number between 0 (representing an impossible event)
                and 1 (representing a certain event). 
                For example, the probability of an even die roll is 3/6 = 1/2.

Frequency       A number describing how often an outcome occurs. Can be a count like 121801, or
                a ratio like 0.515.

Distribution    A mapping from outcome to frequency for each outcome in a sample space.

Probability Distribution:
                A distribution that has been normalized so that the sum of the frequencies is 1.

======================================================================================
"""

import random
import itertools as it
import matplotlib.pyplot as plt
import numpy as np
from fractions import Fraction
from functools import reduce
from operator  import mul
from random import gauss, triangular, choice, vonmisesvariate, uniform
from statistics import mean
from numpy import arange
from collections import Counter
from mpl_toolkits.mplot3d.axes3d import Axes3D


#-------------------------------------------------------------------------------------
# 
# Code for P
#

def P(event, space):
    """The probability of an event, given a sample of equiprobable outcomes."""
    return Fraction(len(event & space), len(space))

## Die roll
D    = {1, 2, 3, 4, 5, 6}
even = {   2,    4,    6}
# print(P(even, D))


#-------------------------------------------------------------------------------------
#
# Urn problems
# 
# 
# An urn contains 23 balls: 8 white, 6 blue, and 9 red. We select six balls at random
# (each possible selection is equally likely). What is the probability of each of these
# possible outcomes:
# 
# 1. all balls are red
# 2. 3 are blue, 2 are white, and 1 is red
# 3. exactly 4 balls are white
#

def cross(A, B):
    """The set of ways of concatenating one item from collection A with one from B."""
    return {a + b for a in A for b in B}

urn = cross('W', '12345678') | cross('B', '123456') | cross('R', '123456789')
# print(urn)
# print(len(urn))

def combos(items, n):
    """All combinations of n items; each combo as a concatenated str."""
    return {' '.join(combo) for combo in it.combinations(items, n)}

U6 = combos(urn, 6)
# print(len(U6))
# print(random.sample(U6, 10))

def choose(n, k):
    """Number of ways to choose k items from a list of n items."""
    return int(reduce(mul, (Fraction(n-i, i+1) for i in range(k)), 1))

# print(choose(23, 6))


## Urn Problem 1: what's the probability of selecting 6 red balls?

red6 = {s for s in U6 if s.count('R') == 6}
# print(P(red6, U6))
# assert P(red6, U6) == Fraction(choose(9, 6), len(U6))


## Urn Problem 2: what is the probability of 3 blue, 2 white, and 1 red?

b3w2r1 = {s for s in U6 if s.count('B') == 3 and s.count('W') == 2 and s.count('R') == 1}
# print(P(b3w2r1, U6))
# assert P(b3w2r1, U6) == Fraction(choose(6, 3) * choose(8, 2) * choose(9, 1), len(U6))


## Urn Problem 3: what's the probability of exactly 4 white balls?

w4 = {s for s in U6 if s.count('W') == 4}
# print(P(w4, U6))
# assert P(w4, U6) == Fraction(choose(8, 4) * choose(15, 2), len(U6))


#-------------------------------------------------------------------------------------
#
# Revised version of P, with more general events
#

def even(n): return n % 2 == 0

def P(event, space):
    """The probability of an event, given a sample space of equiprobable outcomes.
    event can be either a set of outcomes, or a predicate (true for outcomes in the event)."""
    if is_predicate(event):
        event = such_that(event, space)
    return Fraction(len(event & space), len(space))

is_predicate = callable

def such_that(predicate, collection):
    """The subset of elements in the collection for which the predicate is true."""
    return {e for e in collection if predicate(e)}

# print(such_that(even, D))
# print(P(even, D))

D12 = {i for i in range(1,13)}
# print(such_that(even, D12))
# print(P(even, D12))

D3 = {(d1, d2, d3) for d1 in D for d2 in D for d3 in D}

def prime_sum(outcome): return is_prime(sum(outcome))

def is_prime(n): return n > 1 and not any(n % i == 0 for i in range(2, n))

# print(P(prime_sum, D3))


#-------------------------------------------------------------------------------------
#
# Card problems
#

suits = 'SHDC'
ranks = 'A23456789TJQK'
deck  = cross(ranks, suits)

# print(len(deck))

Hands = combos(deck, 5)

assert len(Hands) == choose(52, 5)

# print(random.sample(Hands, 5))

def flush(hand):
    return any(hand.count(suit) == 5 for suit in suits)

# print(P(flush, Hands))

def four_kind(hand):
    return any(hand.count(rank) == 4 for rank in ranks)

# print(P(four_kind, Hands))


#-------------------------------------------------------------------------------------
#
# Fermat and Pascal: gambling, triangles and the birth of probability
#

def win_unfinished_game(Hneeds, Tneeds):
    """The probability that H will win the unfinished game, given the number of points needed
    by H and T to win."""
    def Hwins(outcome): return outcome.count('h') >= Hneeds
    return P(Hwins, continuations(Hneeds, Tneeds))

def continuations(Hneeds, Tneeds):
    """All continuations of a game where H needs 'Hneeds' poins to win and T needs 'Tneeds'."""
    rounds = ['ht' for _ in range(Hneeds + Tneeds - 1)]
    return set(it.product(*rounds))

# print(continuations(2, 3))
# print(win_unfinished_game(2, 3))


#-------------------------------------------------------------------------------------
#
# Non-equiprobable outcomes: probability distributions
#

class ProbDist(dict):
    """A Probability Distribution; an {outcome: probability} mapping."""
    def __init__(self, mapping=(), **kwargs):
        self.update(mapping, **kwargs)
        # Make probabilities sum to 1.0; assert no negative probabilities
        total = sum(self.values())
        for outcome in self:
            self[outcome] = self[outcome] / total
            assert self[outcome] >= 0

def P(event, space):
    """The probability of an event, given a sample space of equiprobable outcomes.
    event: a collection of outcomes, or a predicate that is true of outcomes in the event.
    space: a set of outcomes or a probability distribution of {outcome: frequency} pairs."""
    if is_predicate(event):
        event = such_that(event, space)
    if isinstance(space, ProbDist):
        return sum(space[o] for o in space if o in event)
    else:
        return Fraction(len(event & space), len(space))

def such_that(predicate, space):
    """The outcomes in the sample space for which the predicate is true.
    If space is a set, return a subset {outcome, ...};
    if space is a ProbDist, return a ProbDist {outcome: frequency, ...};
    in both cases only with outcomes where predicate(element) is true."""
    if isinstance(space, ProbDist):
        return ProbDist({o:space[o] for o in space if predicate(o)})
    else:
        return {o for o in space if predicate(o)}

DK = ProbDist(GG=121801, GB=126840, BG=127123, BB=135138)
# print(DK)

def first_girl(outcome):    return outcome[0] == 'G'
def first_boy(outcome):     return outcome[0] == 'B'
def second_girl(outcome):   return outcome[1] == 'G'
def second_boy(outcome):    return outcome[1] == 'B'
def two_girls(outcome):     return outcome    == 'GG'

# print(P(first_girl, DK))
# print(P(second_girl, DK))
# print(P(second_girl, such_that(first_girl, DK)), P(second_girl, such_that(first_boy, DK)))
# print(P(second_boy, such_that(first_girl, DK)), P(second_boy, such_that(first_boy, DK)))


#-------------------------------------------------------------------------------------
#
# More urn problems: M&Ms and Bayes
#
# The blue M&M was introduced in 1995. Before then, the color mix in a bag of plain M&Ms was
# (30% Brown, 20% Yellow, 20% Red, 10% Green, 10% Orange, 10% Tan). Afterward it was (24% Blue,
# 20% Green, 16% Orange, 14% Yellow, 13% Red, 13% Brown). A friend of mine has two bags of M&Ms,
# and he tells me that one is from 1994 and one from 1996. He won't tell me which is which, but
# he gives me one M&M from each bag. One is yellow and one is green. What is the probability that
# the yellow M&M came from the 1994 bag?
#

bag94 = ProbDist(brown=30, yellow=20, red=20, green=10, orange=10, tan=10)
bag96 = ProbDist(blue=24, green=20, orange=16, yellow=14, red=13, brown=13)

def joint(A, B, sep=''):
    """The joint distribution of two independent probability distributions.
    Result is all entries of the form {a+sep+b: P(a)*P(b)}"""
    return ProbDist({a + sep + b: A[a] * B[b] for a in A for b in B})

MM = joint(bag94, bag96, ' ')
# print(MM)

def yellow_and_green(outcome): return 'yellow' in outcome and 'green' in outcome

# print(such_that(yellow_and_green, MM))

def yellow94(outcome): return outcome.startswith('yellow')

# print(P(yellow94, such_that(yellow_and_green, MM)))


## Now solve it the Bayes's way.

def bayes(prior_prob_A, prob_E_given_A, marginal_prob_E):
    """We want to know the probability of an outcome A (hypothesis) given evidence E.
    We have a prior belief that an outcome A has a probability of P(A).

    prior_prob_A:   P(A)    initial est. of probability of A independent of E
    prob_EifA:      P(E|A)  probability of E given A
    prob_E:         P(E)    = sum(P(E|H) * P(H) for H in sample space), marginal probability of E.
    Return          P(A|E)  = P(E|A) * P(A) / P(E), probability of A given E."""

    return prior_prob_A * prob_E_given_A / marginal_prob_E

## A: first M&M from 94 bag, second from 96 bag
## B: first M&M from 96 bag, second from 94 bag
P_A = P_B = 0.5

## E: first M&M yellow, second green
P_EifA = bag94['yellow'] * bag96['green']
P_EifB = bag96['yellow'] * bag94['green']
P_E = P_EifA * P_A + P_EifB * P_B # marginal probability of E

# print(bayes(P_A, P_EifA, P_E))

## Assert that both methods give the same result
# assert P(yellow94, such_that(yellow_and_green, MM)) == bayes(P_A, P_EifA, P_E)


#-------------------------------------------------------------------------------------
#
# Newton's answer to a problem by Pepys
#
#
# Which of the following three propositions has the greatest chance of success?
#
# 1. Six fair dice are tossed independently and at least one “6” appears.
# 2. Twelve fair dice are tossed independently and at least two “6”s appear.
# 3. Eighteen fair dice are tossed independently and at least three “6”s appear.
#

die = ProbDist({'6' : 1/6, '-' : 5/6})

def dice(n, die):
    """Joint probability from tossing n dice."""
    if n == 1:
        return die
    else:
        return joint(die, dice(n-1, die))

# print(dice(3, die))

def at_least(k, result): return lambda s: s.count(result) >= k

# print(P(at_least(1, '6'), dice(6, die)))
# print(P(at_least(2, '6'), dice(12, die)))
# print(P(at_least(3, '6'), dice(18, die)))


#=====================================================================================
#
# Simulation
#
#=====================================================================================


#-------------------------------------------------------------------------------------
#
# The Central Limit Theorem / strength in numbers theorem
#
# Let's take 5 random variables reprsenting the per-game scores of 5 basketball players,
# and then sum them together to form the team score. Each random variable/player is represented
# as a function; calling the function returns a single sample from the distribution.
#

def SC(): return posint(gauss(15.1, 3) + 3 * triangular(1, 4, 13)) # 30.1
def KT(): return posint(gauss(10.2, 3) + 3 * triangular(1, 3.5, 9)) # 22.1
def DG(): return posint(vonmisesvariate(30, 2) * 3.08) # 14.0
def HB(): return posint(gauss(6.7, 1.5) if choice((True, False)) else gauss(16.7, 2.5)) # 11.7
def OT(): return posint(triangular(5, 17, 25) + uniform(0, 30) + gauss(6, 3)) # 37.0

def posint(x): """Positive integer"""; return max(0, int(round(x)))

def repeated_hist(rv, bins=10, k=100000):
    """Repeat rv() k times and make a histogram of the results."""
    samples = [rv() for _ in range(k)]
    fig = plt.figure()
    plt.hist(samples, bins=bins)
    fig.show()
    return mean(samples)

# print(repeated_hist(SC, bins=range(60)))
# print(repeated_hist(KT, bins=range(60)))
# print(repeated_hist(DG, bins=range(60)))
# print(repeated_hist(HB, bins=range(60)))
# print(repeated_hist(OT, bins=range(60)))

## Now we define the team score to be the sum of the five players, and look at the distribution.
def GSW(): return SC() + KT() + DG() + HB() + OT()

# print(repeated_hist(GSW, bins=range(70, 160, 2)))


#=====================================================================================
#
# Continuous Sample Spaces
#
#=====================================================================================


#-------------------------------------------------------------------------------------
#
# The hot new game show problem: simulation
#
# Two players go on a hot new game show called Higher Number Wins. The two go into separate
# booths, and each presses a button, and a random number between zero and one appears on a
# screen. (At this point, neither knows the other’s number, but they do know the numbers are
# chosen from a standard uniform distribution.) They can choose to keep that first number, or
# to press the button again to discard the first number and get a second random number, which
# they must keep. Then, they come out of their booths and see the final number for each player
# on the wall. The lavish grand prize — a case full of gold bullion — is awarded to the player
# who kept the higher number. Which number is the optimal cutoff for players to discard their
# first number and choose another? Put another way, within which range should they choose to
# keep the first number, and within which range should they reject it and try their luck with
# a second number?
#

def number(cutoff):
    """Play the game with given cutoff, returning the first or second random number."""
    first = random.random()
    return first if first > cutoff else random.random()

def Pwin(A, B, trials=30000):
    """The probability that cutoff A wins against cutoff B."""
    Awins = sum(number(A) > number(B) for _ in range(trials))
    return Awins/trials

def top(N, cutoffs):
    """Return the N best cutoffs and the number of opponent cutoffs they beat."""
    winners = Counter(A if Pwin(A, B) > 0.5 else B for (A, B) in it.combinations(cutoffs, 2))
    return winners.most_common(N)

# print(top(5, arange(0.50, 0.99, 0.01)))


#-------------------------------------------------------------------------------------
#
# The hot new game show problem: simulation
#

def Phigher(A, B):
    """Probability that a sample from [A..1] is higher than one from [B..1]."""
    if A <= B:
        return (1 - B) / (2 * (1 - A))
    else:
        return 1 - Phigher(B, A)

def Pwin(A, B):
    """With what probability does cutoff A win against cutoff B?"""
    return ((1-A) * (1-B) * Phigher(A, B)  # both above cutoff
            + A * B       * Phigher(0, 0)  # both below cutoff
            + (1-A) * B   * Phigher(A, 0)  # A above, B below
            + A * (1-B)   * Phigher(0, B)) # A below, B above

def test():
    assert Phigher(0.5, 0.5) == Phigher(0.7, 0.7) == Phigher(0, 0) == 0.5
    assert Pwin(0.5, 0.5) == Pwin(0.7, 0.7) == 0.5
    assert Phigher(.6, .5) == 0.6
    assert Phigher(.5, .6) == 0.4
    return 'ok'

# print(test())

# print(top(1, arange(0.50, 0.99, 0.01)))
# print(top(1, arange(0.500, 0.700, 0.001)))
# print(top(1, arange(0.61700, 0.61900, 0.00001)))


#-------------------------------------------------------------------------------------
#
# The hot new game show problem: best strategy for player A
#

def map2(fn, A, B):
    """Map fn to corresponding elements of 2D arrays A and B."""
    return [list(map(fn, Arow, Brow)) for (Arow, Brow) in zip(A, B)]

## 3D plot of Pwin(A, B) for values of A and B between 0 and 1
cutoffs = arange(0.00, 1.00, 0.02)
A, B = np.meshgrid(cutoffs, cutoffs)
fig = plt.figure(figsize=(10, 10))
ax = fig.add_subplot(1, 1, 1, projection='3d')
ax.set_xlabel('A')
ax.set_ylabel('B')
ax.set_zlabel('Pwin(A, B)')
ax.plot_surface(A, B, map2(Pwin, A, B));
fig.show()

## Maximum Pwin(A, B)

cutoffs = (set(arange(0.00,    1.00,   0.01)) |
           set(arange(0.500,   0.700,  0.001)) |
           set(arange(0.61700, 0.61900, 0.00001)))

print(max([Pwin(A, B), A, B] for A in cutoffs for B in cutoffs))

## If player B gets to respond with a cutoff, with full knowledge of A's choice, what cutoff
## should A choose to maximize Pwin(A, B)?
print(max(min([Pwin(A, B), A, B] for B in cutoffs) for A in cutoffs))

## What if B chooses a cutoff first, and then A responds?
print(min(max([Pwin(A, B), A, B] for A in cutoffs) for B in cutoffs))

