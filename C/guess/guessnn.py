"""
Goal:
    Create a deep neural net agent to play guess game.

Net topology:
    [inputs] --> [sigmoids] --> [output]
"""

import numpy as np
import itertools as it
import math
import time
import sys

ITER_LIMIT = 50000
VERBOSE = False

class FFnet:
    def __init__(self, units):
        """Initialize NN.
        units   List of layer sizes starting from input layer.
                Minimum number of layers = 3, which means: 1 input, 1 hidden, 1 output."""

        assert len(units) > 2             # at least 1 hidden layer
        assert all(v > 0 for v in units)  # non-zero number of units in each layer

        self.units   = units
        self.nlayers = len(units)
        # The last weight of each unit is for bias, which itself is always 1
        self.weights = [np.ones((units[0], 1))] + \
                       [np.random.randn(i, j + 1) * 0.1 for i, j in zip(units[1:], units[:-1])]

    def learn(self, inputs, targets, eps=0.1):
        """Learn weights by back propagating error.
        inputs      Array of input patterns.
        targets     Array of target output patterns, in correspondence with input patterns.
        eps         Learning rate."""

        inputs  = np.asarray(inputs, dtype=float)
        targets = np.asarray(targets, dtype=float)
        assert inputs.shape[1]  == self.units[0]    # number of inputs == input layer size
        assert targets.shape[1] == self.units[-1]   # number of outputs == output layer size
        assert len(inputs)      == len(targets)     # same number of input and output patterns

        if VERBOSE:
            print("\n" + "-" * 80)
            print("Initial weights:")
            print(self)
        print("Learning... ", end='')
        sys.stdout.flush()

        t_start       = time.clock()
        outputs       = self.units_array()
        error_signals = self.units_array()
        returns       = np.zeros(len(targets), dtype=bool)
        it            = 0

        while it < ITER_LIMIT:
            it += 1
            w_deltas = [np.zeros((self.units[0], 1))] + \
                       [np.zeros((i, j + 1)) for i, j in zip(self.units[1:], self.units[:-1])]
            # Sweep through all patterns
            for c in range(len(inputs)):
                # Forward pass
                out = self.evaluate(inputs[c], outputs)
                returns[c] = all([valid(t, o) for t, o in zip(targets[c], out)])
                # Backward pass
                for i in range(self.units[-1]):  # output units
                    t = targets[c, i]
                    o = outputs[-1][i]
                    error_signals[-1][i] = output_error_signal(t, o)
                for i in range(self.nlayers - 2, 0, -1):
                    e = error_signals[i + 1]
                    for j in range(self.units[i]):  # hidden units
                        w = vec([self.weights[i + 1][k, j] for k in range(self.units[i + 1])])
                        o = outputs[i][j]
                        error_signals[i][j] = hidden_error_signal(e, w, o)
                # Accumulate weights deltas from each pattern
                for i in range(1, self.nlayers):
                    o_prev = np.append(outputs[i - 1], [1.0])
                    for j in range(self.units[i]):
                        e = error_signals[i][j]
                        w_deltas[i][j] += eps * e * o_prev
            if all(returns):  # learning complete
                break
            # Weights correction
            for i in range(1, self.nlayers):
                for j in range(self.units[i]):
                    self.weights[i][j] += w_deltas[i][j]

        dt = time.clock() - t_start
        if all(returns):
            print("success")
        else:
            print("failed")
        print("Iterations: {}, time: {:.2f}s".format(it, dt))
        print("Valid outputs: {}%".format((sum(returns) / float(len(returns))) * 100.0))
        if VERBOSE:
            print(returns)
            print("Final weights:")
            print(self)

    def evaluate(self, inputs, outputs=None):
        """Returns a list of outputs for the given list of inputs."""

        assert len(inputs) == self.units[0]  # correct number of inputs

        if outputs is None:  # create new outputs structure if none is given
            outputs = self.units_array()
        outputs[0] = vec(inputs)
        for i in range(len(outputs) - 1):       # previous layer
            j = i + 1                           # current layer
            for k in range(len(outputs[j])):    # current unit
                o = vec(np.append(outputs[i], [1.0]))  # append bias as extra output
                w = vec(self.weights[j][k])
                outputs[j][k] = sigmoid(o, w)
        return outputs[-1][:, 0]

    def units_array(self):
        """Returns a list of layers, where each layer is a vector of floats."""
        return [np.zeros((n, 1)) for n in self.units]

    def __str__(self):
        """Str description of NN, including each unit's weight vector."""
        s = "Layer 0\n    inputs: {}".format(self.units[0])
        for i in range(1, self.nlayers):
            s += "\nLayer {}".format(i)
            for j in range(self.units[i]):
                s += "\n    unit {}: {}".format(j, self.weights[i][j])
        return s


def sigmoid(inputs, weights):
    """Logistic unit function.
    Returns a real-valued output that is a smooth and bounded function of unit's total input."""
    z = float(np.dot(weights.T, inputs))
    return 1 / (1 + math.e**(-z))


def sigmoid_derivative(output):
    """Returns value of the derivative of sigmoid function."""
    return output * (1 - output)


def output_error_signal(target, output):
    """Returns error signal for an output node."""
    return (target - output) * sigmoid_derivative(output)


def hidden_error_signal(error_signals, weights, output):
    """Returns error signal for a hidden unit.
    error_signals   Error signals vector from downstream layer.
    weights         Weights vector from this unit to downstream layer.
    output          Output value of this unit."""
    return sigmoid_derivative(output) * float(np.dot(error_signals.T, weights))


def vec(x):
    """Converts list or 1d array to transposable vector."""
    if type(x) is np.ndarray and x.ndim == 2 and x.shape[1] == 1:  # already a proper vector
        return x
    return np.array(x, ndmin=2).T


def valid(t, o):
    """Returns True if output 'o' is valid with respect to target output 't'."""
    return o >= 0.9 if t == 1.0 else o <= 0.1


# -------------------------------------------------------------------------------------------
# Testing

def rotations(items, n):
    """Generate inputs and targets tuple from items for NN with n input units.
    rotations([1,0,0], 3) -> [[1,0,0], [0,0,1], [0,1,0]], [[1], [0], [0]]"""
    items2 = items*2
    inp = [tuple(items2[i:i+n]) for i in range(len(items))]
    tgt = [items2[i+n] for i in range(len(items))]
    # Keep track of target frequencies for duplicate inputs
    d = {}
    for i in range(len(inp)):
        try:
            d[inp[i]][tgt[i]] += 1
        except KeyError:
            d[inp[i]] = {0:0, 1:1} if tgt[i] else {0:1, 1:0}
    inp, tgt = [], []
    # Decide which target to choose
    for k in d:
        inp.append(list(k))
        tgt.append([0] if d[k][0] > d[k][1] else [1])
    return inp, tgt

def test(net, items):
    print('Testing...')
    n = net.units[0]
    cases = [valid(items[i+n], net.evaluate(items[i:i+n])[0]) for i in range(len(items)-n)]
    perc = (sum(cases) / float(len(cases))) * 100.0
    print('Valid outputs: {:.1f}%'.format(perc))
    return perc

def test_batch(psize, net_layout=[3, 3, 1], eps=0.5):
    assert psize > net_layout[0]
    res = {}
    for pattern in it.product((1,0), repeat=psize):
        print('-'*50)
        print('Pattern:', pattern)
        a = train(pattern, net_layout, eps)
        res[pattern] = test(a, pattern*10)
    print('\nAverage rate {:.1f}%'.format(sum(res.values()) / float(len(res))))
    print('Worst case {} with rate {:.1f}%'.format(*min(res.items(), key=lambda x: x[1])))
    return res

def train(pattern, net_layout=[3, 3, 1], eps=0.5):
    inp, tgt = rotations(pattern, net_layout[0])
    a = FFnet(net_layout)
    a.learn(inp, tgt, eps)
    return a


# -----------------------------------------
# test_batch(4)

# # -----------------------------------------
# a = train([1,0,1,1,0,0,1,1,1])
# test(a, [1,0,1,1,0,0,1,1,1]*10)
