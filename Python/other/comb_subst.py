s = '*//*/'
subst = {'*':['у', 'при'], '/':['под', 'на']}

for i in range(2**len(s)):
    res = []
    for l in s:
        res.append(subst[l][i&1])
        i >>= 1
    print('(в е ({} А ({} К Р))) ({} ({} (к П А) Т) ({} Е Т)) !'.format(*res))
