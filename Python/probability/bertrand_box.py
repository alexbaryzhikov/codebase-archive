'''
Bertrand's box paradox is a paradox of elementary probability theory, first posed by Joseph Bertrand
in his 1889 work "Calcul des probabilités".

There are three boxes:

a box containing two gold coins,
a box containing two silver coins,
a box containing one gold coin and a silver coin.

The 'paradox' is in the probability, after choosing a box at random and withdrawing one coin at
random, if that happens to be a gold coin, of the next coin also being a gold coin.
'''

import math, random
import numpy as np

def simulate(precision):
    
    def grab_coin():
        ## g - gold coin, s - silver coin
        boxes   = [['g']*2, ['g']+['s'], ['s']*2]
        coin    = ''
        
        while coin != 'g':
            box         = random.choice(boxes).copy()
            choice      = random.randrange(0, len(box))
            coin        = box.pop(choice)
            other_coin  = random.choice(box)

        if other_coin == 'g':
            return True
        return False

    def monte_carlo(experiment, sample_size):
        passed = 0
        for _ in range(sample_size):
            passed += experiment()
        return passed/sample_size

    def get_samples(num_trials, sample_size):
        results = []
        for _ in range(num_trials):
            result = monte_carlo(grab_coin, sample_size)
            results.append(result)
        return results
    
    random.seed()

    sample_size = 1000
    numTrials   = 100
    stdev       = precision
    
    ## guarantee precision with 95% confidence
    while stdev >= precision/1.96:
        samples     = get_samples(numTrials, sample_size)
        est         = np.mean(samples) 
        stdev       = np.std(samples)
        interval    = round(stdev*1.96, 6)
        print('Estimate = '+str(est)+' +- '+str(interval)+', sample size = '+str(sample_size)+'.')
        sample_size *= 2

    ## take one big sample
    # sample_size *= 1000
    # sample      = monte_carlo(grab_coin, sample_size)
    # est         = np.mean(sample)
    # print('Estimate = '+str(est)+', sample size = '+str(sample_size)+'.')

    return est

simulate(0.01)
