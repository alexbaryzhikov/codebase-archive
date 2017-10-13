import sys
import time
import random
import copy

def quick_sort(L):
    t_start = time.clock()
    qsort(L, 0, len(L)-1)
    t_finish = time.clock()
    is_sorted(L)
    print('  Time: '+str(round(t_finish-t_start, 3))+'s\n')

def qsort(L, lo, hi):
    if (hi-lo) < 5:
        ## run isort if partition is small enough
        isort(L, lo, hi)
    else:
        ## divide and conquer
        left, right = partition(L, lo, hi)
        qsort(L, lo, right)
        qsort(L, left, hi)
    
def partition(L, lo, hi):
    ## select pivot
    mid = lo + (hi-lo)//2
    pivot = median3([L[lo], L[mid], L[hi]])
    while True:
        while L[lo] < pivot: lo += 1
        while pivot < L[hi]: hi -= 1
        if lo >= hi: return lo, hi
        L[lo], L[hi] = L[hi], L[lo]
        lo += 1; hi -= 1

################ Auxiliary ################

def isort(L, lo, hi):
    for i in range(lo+1, hi+1):
        tmp = L[i]
        j = i
        while j > lo and tmp < L[j-1]:
            L[j] = L[j-1]
            j -= 1
        L[j] = tmp

def median3(L):
    if L[0] > L[1]: L[0], L[1] = L[1], L[0]
    if L[1] > L[2]: L[1], L[2] = L[2], L[1]
    if L[0] > L[1]: return L[0]
    return L[1]

def write_list(L):
    sys.stdout.write(str(L)+'\r')
    sys.stdout.flush()
    time.sleep(0.2)

def is_sorted(L):
    for i in range(1, len(L)):
        assert L[i-1] <= L[i]
    print('  List is sorted.')

################ Tests ################

def rnd_batch_test(n_tests, max_val, list_size):
    for i in range(n_tests):
        a = [random.randrange(max_val) for _ in range(list_size)]
        quick_sort(a)
        
def power_list(n_lists, max_val, list_size):
    """Generates a list of lists, then
    yields deep copies of it."""
    def rnd_list():
        return [random.randrange(max_val) for _ in range(list_size)]
    res = []
    for _ in range(n_lists):
        res.append(rnd_list())
    while True:
        yield copy.deepcopy(res)

def batch_test(plist):
    ## create power_list generator to non-destructively feed it to batch_test
    print('Copying lists... ', end='')
    L = next(plist)
    print('Done\n')
    for v in L:
        quick_sort(v)
