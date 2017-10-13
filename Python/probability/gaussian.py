import numpy as np
import math, pylab, scipy.integrate

def gaussian(x, m, sd):
    return math.e**(-(x-m)**2/(2*sd**2))/(sd*math.sqrt(2*math.pi))

def plot_gaussian(m, sd):
    """Plot gaussian distribution: m - median, sd - std."""
    x_list = np.linspace(m-4.0*sd, m+4.0*sd, 101)
    vals = np.array([gaussian(i, m, sd) for i in x_list])
    pylab.figure('plot')
    pylab.plot(x_list, vals)
    pylab.ylim(0, max(vals)*1.1)
    pylab.title('Gaussian distribution, m = ' + str(m) + ', std = ' + str(sd))
    pylab.savefig('gaussian')

def gaussian_area(m, sd, x):
    """Return the area of gaussian distribution from (m - x*sd) to (m + x*sd).
    I.e. the chance of an outcome to fall withing x*std range around the mean."""
    return scipy.integrate.quad(gaussian, m - x*sd, m + x*sd, (m, sd))[0]
