import cProfile
import numpy as np

def dot(a, b):
  sum=0
  for i in range(len(a)):
      sum += a[i]*b[i]
  return sum

a = np.arange(256, dtype=np.float32)
b = np.arange(256, dtype=np.float32)

cProfile.run('dot(a, b)', sort='tottime')

