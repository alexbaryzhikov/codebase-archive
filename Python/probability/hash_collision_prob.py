from fractions import Fraction
from functools import reduce
from operator import mul

def collision_prob(n, k):
    """We have n buckets and a hash function that maps input numbers to buckets
    with uniform probability. Return the chance of at least one collision on k inputs."""
    return 1.0 - float(reduce(mul, (Fraction(n-i, n) for i in range(1, k)), 1))
