"""
2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any
remainder.

What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
"""

from functools import reduce
from operator import mul

def is_even_div(x):
     return all(not(x % p) for p in range(2, 21))

primes = [2, 3, 5, 7, 11, 13, 17, 19]

def prime_factors(x):
    res = []
    i = 0
    while x > 1:
        if not(x % primes[i]):
            res.append(primes[i])
            x /= primes[i]
            i = 0
        else:
            i += 1
    return res

def factors_collection(x):
    res = []
    for i in range(2, x+1):
        pf_i = prime_factors(i)
        for j in pf_i:
            if pf_i.count(j) > res.count(j):
                res.extend([j] * (pf_i.count(j) - res.count(j)))
    return res

a = reduce(mul, factors_collection(20), 1)
assert is_even_div(a)
print(a)
