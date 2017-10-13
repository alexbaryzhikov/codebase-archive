from fractions import Fraction
from functools import reduce
from operator  import mul
import math, scipy.integrate


# ---------------------------------------------------------------------------
# Combinatorics

def nCk(n, k):
    """Returns the number of combinations of size k from n elements"""
    return int(reduce(mul, (Fraction(n - i, i + 1) for i in range(k)), 1))

def nPk(n, k):
    """Returns the number of permutations of size k from n elements"""
    return int(reduce(mul, (n - i for i in range(k)), 1))

def product(str1, str2):
    """Returns cartesian product of input strings."""
    return [a + b for a in str1 for b in str2]


# ---------------------------------------------------------------------------
# Probability

def bayes(probA, probBifA, probB):
    """We have a belief that an outcome A has a probability of P(A).
    B is a new evidence of A.  We need to correct our initial belief
    concerning A, considering B.
    
    probA:      P(A)    initial est. of probability of A independent of B
    probBifA:   P(B|A)  est. of probability of B given A
    probB:      P(B)    est. of probability of B
    
    Return       P(A|B)  probability of A given B"""
    return probA*probBifA/probB

def gaussian(x, m, sd):
    """Returns the value of normal distribution function at point x.
    x       point (outcome);
    m, sd   mean, standart deviation."""
    return math.e**(-(x-m)**2/(2*sd**2))/(sd*math.sqrt(2*math.pi))

def gaussian_area(m, sd, x):
    """Returns the area of normal distribution from (m - x*sd) to (m + x*sd).
    I.e. the chance of an outcome to fall withing x*std range around the mean."""
    return scipy.integrate.quad(gaussian, m - x*sd, m + x*sd, (m, sd))[0]


# ---------------------------------------------------------------------------
# Binary system and two's complement number representation

def twos_comp(x, nbits=32):
    """Converts x to nbits twos complement number, int32 by default."""
    mask = (1<<nbits)-1
    if x >= 0:
        if x > (mask>>1):
            print("Warning: {} is too big for {}-bit representation.".format(x, nbits))
        return x&mask
    else:
        if abs(x) > (1<<nbits-1):
            print("Warning: {} is too big for {}-bit representation.".format(x, nbits))
        return ((abs(x)^mask)+1)&mask

def twos_comp_str(x, nbits=32):
    """Returns human-readable string of twos complement binary."""
    _tmp = twos_comp(x, nbits)
    _bin = []
    for i in range(nbits):
        _bin.append(str((_tmp>>i)&1))
        if not (i+1)%4: _bin.append(' ')
    _bin.reverse()
    return ''.join(_bin).strip()

# binding for convenience
tcs = twos_comp_str

def pow_of_two():
    for i in range(17):
        print('2^'+str(i),':',1<<i)

def hex_bin():
    for i in range(16):
        print('{:x} : {:04b}'.format(i, i))


# ---------------------------------------------------------------------------
# Iterators

def ipairs(lst):
    """Assumes lst is a list. Returns a stream of (index, element) pairs."""
    return zip(range(len(lst)), lst)

def pairs(dct):
    """Assumes dct is a dictionary. Returns a stream of (key, value) pairs."""
    return zip(dct.keys(), dct.values())
