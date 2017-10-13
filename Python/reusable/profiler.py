from accelerate import profiler
import numpy as np

def dot(a, b):
  sum=0
  for i in range(len(a)):
      sum += a[i]*b[i]
  return sum

a = np.arange(256, dtype=np.float32)
b = np.arange(256, dtype=np.float32)

p = profiler.Profile(signatures=False)
p.enable()
dot(a, b)
p.disable()
p.print_stats()
