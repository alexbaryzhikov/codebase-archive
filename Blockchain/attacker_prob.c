/*
We consider the scenario of an attacker trying to generate an alternate chain faster than the
honest chain. The race between the honest chain and an attacker chain can be characterized as
a Binomial Random Walk. The success event is the honest chain being extended by one block,
increasing its lead by +1, and the failure event is the attacker's chain being extended by one
block, reducing the gap by -1.

The probability of an attacker catching up from a given deficit is analogous to a Gambler's Ruin
problem. Suppose a gambler with unlimited credit starts at a deficit and plays potentially an
infinite number of trials to try to reach breakeven. We can calculate the probability he ever
reaches breakeven, or that an attacker ever catches up with the honest chain, as follows:

    p       probability an honest node finds the next block
    q       probability the attacker finds the next block
    q_z     probability the attacker will ever catch up from z blocks behind

    q_z = { 1 if p<=q, (q/p)^z if p>q }

Given our assumption that p > q, the probability drops exponentially as the number of blocks
the attacker has to catch up with increases.

We now consider how long the recipient of a new transaction needs to wait before being
sufficiently certain the sender can't change the transaction. We assume the sender is an
attacker who wants to make the recipient believe he paid him for a while, then switch it to
pay back to himself after some time has passed. The receiver will be alerted when that happens,
but the sender hopes it will be too late.

The recipient waits until the transaction has been added to a block and z blocks have been
linked after it. He doesn't know the exact amount of progress the attacker has made, but
assuming the honest blocks took the average expected time per block, the attacker's potential
progress will be a Poisson distribution with expected value:

    lambda = z*(q/p)

To get the probability the attacker could still catch up now, we multiply the Poisson density for
each amount of progress he could have made by the probability he could catch up from that point:

    sum(lambda^k * e^-lambda / k! * { (q/p)^(z-k) if k<=z, 1 if k>z } for k in [0 ... inf))

Rearranging to avoid summing the infinite tail of the distribution...

    1 - sum(lambda^k * e^-lambda / k! * (1 - (q/p)^(z-k)) for k in [0 ... z])
*/

#include <stdio.h>
#include <math.h>

/* Attacker success probability */
double AttackerSuccesProbability(double q, int z) {
    double p, lambda, sum, poisson;
    int i, k;

    p = 1.0 - q;
    lambda = z * (q / p);
    sum = 1.0;

    for (k = 0; k <= z; k++) {
        poisson = exp(-lambda);
        for (i = 1; i <= k; i++)
            poisson *= lambda / i;
        sum -= poisson * (1 - pow(q / p, z - k));
    }

    return sum;
}

int main(int argc, char const *argv[]) {
    int z;

    printf("q = 0.1\n");
    for (z = 0; z < 11; z++)
        printf("z = %d\t\tP = %.7f\n", z, AttackerSuccesProbability(0.1, z));

    printf("\nq = 0.3\n");
    for (z = 0; z < 51; z += 5)
        printf("z = %d\t\tP = %.7f\n", z, AttackerSuccesProbability(0.3, z));

    return 0;
}
