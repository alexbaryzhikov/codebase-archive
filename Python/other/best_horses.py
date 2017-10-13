'''Hippodrome has 5 tracks. Pick 5 fastest horses out of 20 without using timer.'''

import numpy as np

class Namespace: pass

def get_best_5():

    def sort5(list_of_5):
        if len(list_of_5) > 5: raise ValueError('list is too long.')
        Namespace.total_runs += 1
        return sorted(list_of_5, key=lambda x: horses_time[x])

    def merge(list1, list2, best5=False):
        res = []
        a, b = list1.copy(), list2.copy()
        while True:
            if   not a: res.extend(b); break
            elif not b: res.extend(a); break
            elif len(a)+len(b) == 4: res.extend(sort5(a+b)); break
            if   len(a) == 1: tmp = a+b[:3]
            elif len(b) == 1: tmp = a[:3]+b
            else: tmp = a[:2]+b[:2]
            tmp = sort5(tmp)
            res.extend(tmp[:2])
            if best5 and len(res) > 4: return res[:5]
            if tmp[0] in a: a.remove(tmp[0])
            else: b.remove(tmp[0])
            if tmp[1] in a: a.remove(tmp[1])
            else: b.remove(tmp[1])
        return res

    np.random.seed()
    horses = [i for i in range(20)]
    horses_time = {i : j for i, j in zip(range(20), np.random.randint(100, size=20))}
    # print(horses_time)
    Namespace.total_runs = 0

    step1 = []
    for i in range(0, 16, 5):
        tmp = horses[i:i+5]
        step1.append(sort5(tmp))

    step2 = []
    step2.append(merge(step1[0], step1[1]))
    step2.append(merge(step1[2], step1[3]))

    step3 = merge(step2[0][:5], step2[1][:5], True)
    # print(step3)

    return(Namespace.total_runs)

def stats(n):
    all = []
    for _ in range(n): all.append(get_best_5())
    runs = {}
    for i in all:
        if i in runs:
            runs[i] += 1
        else:
            runs[i] = 1

    print(runs)
    for i in runs:
        print(i, ':', runs[i]/len(all))
