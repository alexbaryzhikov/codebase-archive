"""
=========================================================================================
Racing Duel

Players have cars with maximum speed of 100 km/h. They can sacrifice their speed to make
the track of the opponent longer by 1 km per 1 km/h of speed. A player's score is the
difference between finish times.

What is the optimal strategy to maximize winning chances and the score?

=========================================================================================
"""

import numpy as np
from numpy import arange
from mpl_toolkits.mplot3d.axes3d import Axes3D
import matplotlib.pyplot as plt


def score(A, B):
    """Compute the outcome of the race for player A, given A and B to be speeds of the cars."""
    assert 10 <= A <= 100 and 10 <= B <= 100  # you can't get too slow or too fast
    trackA = 100 - B
    trackB = 100 - A
    tA = trackA/A
    tB = trackB/B
    return tB - tA


# Graph the space of possible scores for A
speeds = arange(10, 101)
A, B = np.meshgrid(speeds, speeds)
S = np.array([list(map(score, Arow, Brow)) for (Arow, Brow) in zip(A, B)])

fig = plt.figure(figsize=(10, 10))
ax = fig.add_subplot(1, 1, 1, projection='3d')
ax.set_xlabel('Speed of A')
ax.set_ylabel('Speed of B')
ax.set_zlabel('Race Score for A')
ax.plot_surface(A, B, S)
plt.savefig('racing_duel.png')
plt.close(fig)

# Maximum score for A
print("Max score for A:",
      max([score(A, B), A, B] for A in speeds for B in speeds))

# If player B gets to respond with a speed choice, with full knowledge of A's choice, what
# speed should A choose to maximize the score?
print("Max score for A if B responds:",
      max(min([score(A, B), A, B] for B in speeds) for A in speeds))

# What if B chooses a speed first, and then A responds?
print("Max score for B if A responds:",
      min(max([score(A, B), A, B] for A in speeds) for B in speeds))
