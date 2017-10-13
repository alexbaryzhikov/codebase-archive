import pylab as plt
import math

mySamples = []
myLinear = []
myQuadratic = []
myCubic = []
myExponential = []
myFactorial = []

for i in range(30):
    mySamples.append(i)
    myLinear.append(i)
    myQuadratic.append(i**2)
    myCubic.append(i**3)
    myExponential.append(2**i)
    myFactorial.append(math.factorial(i))
    
plt.figure('lin_quad')
plt.clf()
plt.title('Linear vs quadratic')
plt.xlabel('samples')
plt.ylabel('function')
plt.subplot('121')
plt.ylim(0, 1000)
plt.plot(mySamples, myLinear, 'b-', label = 'linear', linewidth = 3.0)
plt.subplot('122')
plt.ylim(0, 1000)
plt.plot(mySamples, myQuadratic, 'r+', label = 'quadratic')
plt.legend(loc = 'upper left')

plt.figure('cub_exp')
plt.clf()
plt.title('Cubic vs exponential vs factorial')
plt.xlabel('samples')
plt.ylabel('function')
plt.plot(mySamples, myCubic, 'g--', label = 'cubic', linewidth = 1.0)
plt.plot(mySamples, myExponential, 'r', label = 'exponential', linewidth = 1.0)
plt.plot(mySamples, myFactorial, 'b', label = 'factorial', linewidth = 1.0)
plt.yscale('log')
plt.legend(loc = 'upper left')
