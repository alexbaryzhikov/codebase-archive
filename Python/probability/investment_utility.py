from math import log

def Psucc(investment, payoff):
    """Given the investment required and potential payoff,
    return the threshold probability of succes.
    P * u(payoff - investmet) + (1 - P) * u(-investment) = u(0)"""
    return (u(0) - u(-investment))/(u(payoff - investment) - u(-investment))

def u(x):
    """Utility function. 'x' is a change in asset value in dollars."""
    A = 25      # slope
    B = 1000000 # max investment
    return A * (log (x + B) - log(B))
