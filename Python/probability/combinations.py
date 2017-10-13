from fractions import Fraction
from functools import reduce
from operator  import mul

def nCk(n, k):
    '''Returns the number of combinations of size k from n elements'''
    return int(reduce(mul, (Fraction(n - i, i + 1) for i in range(k)), 1))

def nPk(n, k):
    '''Returns the number of permutations of size k from n elements'''
    return int(reduce(mul, (n - i for i in range(k)), 1))

