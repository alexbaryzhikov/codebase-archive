import random

def calcBayes(priorA, probBifA, probB):
    """We have a belief that an outcome A has a probability of P(A).
    B is a new evidence of A.  We need to correct our initial belief
    concerning A, considering B.
    
    priorA:      P(A)    initial est. of probability of A independent of B
    probBifA:    P(B|A)  est. of probability of B given A
    probB:       P(B)    est. of probability of B
    
    Return       P(A|B)  probability of A given B"""
    return priorA*probBifA/probB

## Let's assume we have three dies A, B and C that have probabilities to roll "6"
## 1/5, 1/6 and 1/7 respectively.  We randomly pick one die and start rolling it,
## trying to determine the probability of it being of type A.

priorA      = 1/3                   # hypothesis: we picked A with priorA certainty
prob6ifA    = 1/5                   # chance to roll "6" given A was picked
prob6       = (1/5 + 1/6 + 1/7)/3   # marginal probability to roll "6", independent of hypotheses

## suppose we really picked die C
prob6_this_pick = 1/7

## The hypothesis is that we picked A with priorA certainty.
## Let's roll A and revise the propability that our hypothesis is true.
numRolls = 500
postA = priorA

for i in range(numRolls + 1):
    if i%(numRolls//10) == 0: # report estimate of P(A)
        print('After', i, 'rolls. P(A) =', round(postA, 4))
    isSix = random.random() <= prob6_this_pick
    if isSix:
        postA = calcBayes(postA, prob6ifA, prob6)
    else:
        postA = calcBayes(postA, 1 - prob6ifA, 1 - prob6)
