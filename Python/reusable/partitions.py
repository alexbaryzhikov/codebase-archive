'''
Generate every possible way to separate list of items into groups.
'''

def partitions_gen(n, p, p1 = 1):
    '''n - number of elements, p - number of partitions,
    p1 - first partition size constraint.
    Yields partitioning templates (e.g. [3, 1, 1], where values are
    number of elements in partition)'''
    assert 0 < p <= n   # range of possible partitions number
    assert p1 <= n-p+1  # first partition upper limit
    if p == 1:
        yield [n]
    else:
        for i in range(p1, n//p+1):
            for nums in partitions_gen(n-i, p-1, i):
                yield nums+[i]

def combinations_gen(items, p):
    '''items - list of elements, p - partitioning template (e.g. [2, 3])
    Yields all combinations of elements for a given way of partitioning'''
    assert len(items) == sum(p) # same number of elements in list and partitions
    if not p:
        yield []
    else:
        ## generate partitions of the same size in batches
        n = p.count(p[0])
        for batch, rest in batches_gen(items, p[0], n):
            for b_rest in combinations_gen(rest, p[n:]):
                yield batch+b_rest

def all_partitions_gen(items):
    '''Main function. Yields all possible partitions of list of items.'''
    n = len(items)
    for k in range(1, n+1):
        for p in partitions_gen(n, k):
            for c in combinations_gen(items, p):
                yield c

############################## Auxiliary ##############################

def bit_combinations(n, k):
    '''Yields bit-coded k-combinations of n elements'''
    assert 0 <= k <= n   # range of combination size
    if k == 0:
        yield 0
    elif k == 1:
        for i in range(n):
            yield 1<<i
    else:
        for i in range(n-k+1):
            for j in bit_combinations(n-i-1, k-1):
                yield (1<<i)+(j<<i+1)

def batches_gen(items, k, n):
    '''items - list of items, k - size of partition, n - batch size.
    Yields batches of k-length partitions, n partitions in each batch,
    and list of elements excluded from current batch'''
    assert (0 <= n) & (0 <= k)
    assert k*n <= len(items) 
    if n == 0:
        yield [], items
    else:
        for c in bit_combinations(len(items), k):
            reserve = []        # won't go to recursion to avoid duplicates
            for item in items:
                if c&1: break
                reserve.append(item)
                c >>= 1
            parts = [[], []]    # [0] goes to recursion, [1] goes to batch
            for item in items[len(reserve):]:
                parts[c&1].append(item)
                c >>= 1
            if len(parts[0]) < k*(n-1):
                break # stop if there's not enough items for the partial batch
            for p, rest in batches_gen(parts[0], k, n-1):
                yield [parts[1]]+p, reserve+rest

############################## Tests ##############################

def test_partitions_gen(n, p):
    '''Prints all p-partitions of n elements'''
    for res in partitions_gen(n, p):
        for j in res:
            print(str(j), end = ' ')
        print()

def batch_test_partitions_gen(n):
    '''Prints all possible partitions of n elements'''
    for p in range(1, n+1):
        for res in partitions_gen(n, p):
            for j in res:
                print(str(j), end = ' ')
            print()

def test_bit_combinations(n, k):
    '''Prints all possible k-combination of n elements in bit-encoded form'''
    for v in bit_combinations(n, k):
        print('{:b}'.format(v), end = ' ')
    print()

def batch_test_bit_combinations(n):
    for i in range(1, n+1):
        for j in range(i+1):
            print('n={} k={}: '.format(i, j)); test_bit_combinations(i, j)

def test_combinations_gen(items, p):
    '''Prints all combinations of items for a p way of partitioning'''
    print('items {}  partitions {}'.format(items, p))
    for l in combinations_gen(items, p):
        for v in l:
            print(str(v), end = '  ')
        print()

def test_batches_gen(items, k, n):
    for l, r in batches_gen(items, k, n):
        for v in l:
            print(str(v), end = '  ')
        print('- '+str(r))

def test_all_partitions_gen(items):
    '''Prints all possible ways to partition items'''
    for l in all_partitions_gen(items):
        for v in l:
            print(str(v), end = '  ')
        print()

## Uncomment next line and run this file to see
## visualization of the partitioning algorithm

# test_all_partitions_gen([1, 2, 3, 4])
