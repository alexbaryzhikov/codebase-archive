import numpy as np
from scipy.spatial import Delaunay
import matplotlib.pyplot as plt

SCREEN_W   = 1200
SCREEN_H   = 1200
NUM_POINTS = 100
RND_SEED   = 0

def generate_points(n):
    """Returns an array of randomly generated points."""
    points_x = np.random.randint(0, SCREEN_W, n)
    points_y = -1 * np.random.randint(0, SCREEN_H, n)
    return np.asarray(list(zip(points_x, points_y)))

np.random.seed(RND_SEED)
points = generate_points(NUM_POINTS)
tri = Delaunay(points)
fig = plt.figure(figsize=(12,12))
plt.triplot(points[:,0], points[:,1], tri.simplices.copy())
plt.xlim(0, SCREEN_W)
plt.ylim(-SCREEN_H, 0)
plt.savefig('triangulation.png')
plt.close(fig)
