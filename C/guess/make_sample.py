import random

def rnd(x):
    return 1 if abs(random.gauss(0, 1)) < x else 0

# items = [1,1,1,0]*100
# items = [1,1,1,1,0]*100
# items = [1,1,0,0,1,0,1]*200
items = [1,1,1,1,0,1,0]*200
# items = (rnd(0.7) for _ in range(1000))
# items = (random.randint(0, 1) for _ in range(1000))

with open('sample.txt', 'w') as f:
    for i in items:
        f.write(str(i))
f.close()
