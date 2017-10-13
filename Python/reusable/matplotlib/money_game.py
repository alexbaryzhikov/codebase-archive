"""
Money Exchange Model

Initially, all agents are given the same amount of money.  After simulation starts, $1 is
repeatedly transferred from one randomly selected agent to another.  If the selected agent does
not have enough money to pay, the transaction does not take place, and simulation continues with
another pair of agents.  The transfers of money are supposed to represent payments from one agent
to another for certain products and services.  However, we do not keep track of the goods offered
in exchange for money and only keep track of money balances of all agents.  The random character
of money transfers is supposed to reflect the wide variety of products and connections in modern
economy.

Original idea: http://www2.physics.umd.edu/~yakovenk/econophysics/animation.html
"""

import numpy as np
import matplotlib.pyplot as plt
import matplotlib.animation as animation
from matplotlib import gridspec

def model_init():
    "Initialize model."
    agents = 500
    initial_money = 10
    sims = 500
    batches = 300
    batch_size = 200
    data_init = np.ones([agents * sims], dtype=np.int) * initial_money
    data_i = np.arange(agents)
    binsno = 80
    return agents, sims, batches, batch_size, data_init, data_i, binsno

def simulate(agents, sims, batches, batch_size, data_init, data_i):
    "Run simulations and accumulate results."
    data_accum = np.zeros((agents * sims, batches), dtype=np.int)
    data = data_init.copy()
    data_accum[:, 0] = data
    for i in range(1, batches):
        print("Batch: {}".format(i), end="\r")
        for _ in range(batch_size):
            for j in range(sims):
                pair = np.random.choice(data_i, 2, replace=False) + j * agents
                np.random.shuffle(pair)
                if data[pair[0]]:
                    data[pair] += [-1, 1]
        data_accum[:, i] = data
    return data_accum

def get_entropy(data):
    d = data[np.nonzero(data)]
    P = d / np.sum(d)
    return -np.sum(P * np.log(P))

def animate(frameno, data, binsno, patches, line, entropy):
    "Update histogram."
    n, _ = np.histogram(data[:, frameno], np.arange(binsno))
    for rect, h in zip(patches, n):
        rect.set_height(h)
    entropy.append(get_entropy(n))
    line.set_data(np.arange(len(entropy)), entropy)
    patches.append(line)
    return patches

def model_plot(agents, sims, batches, batch_size, data_accum, binsno):
    "Plot model."
    fig = plt.figure(figsize=(10, 7))
    fig.set_tight_layout(True)
    gs = gridspec.GridSpec(2, 1, height_ratios=[3, 1])

    # Set up money distribution plot
    ax0 = plt.subplot(gs[0])
    n, bins, patches = ax0.hist([], np.arange(binsno))
    ax0.set_title('Distribution of Money')
    ax0.set_ylabel('Number of Agents')
    ax0.set_xlabel('Money')
    ax0.set_xlim(0, binsno)
    ax0.set_ylim(0, agents * sims // 10)

    # Set up entropy plot
    entropy = []
    ax1 = plt.subplot(gs[1])
    line, = ax1.plot([], [], color='red')
    ax1.set_ylabel('Entropy')
    ax1.set_xlabel('Number of Exchanges (x{})'.format(batch_size))
    ax1.set_xlim(0, batches)
    ax1.set_ylim(0, 5)

    Writer = animation.writers['ffmpeg']
    writer = Writer(fps=15, bitrate=1800)
    ani = animation.FuncAnimation(fig, animate, fargs=(data_accum, binsno, patches, line, entropy), \
        blit=True, interval=10, frames=batches, repeat=False)
    ani.save('money_game.mp4', writer=writer)
    # plt.show()


# agents, sims, batches, batch_size, data_init, data_i, binsno = model_init()
# data_accum = simulate(agents, sims, batches, batch_size, data_init, data_i)
# model_plot(agents, sims, batches, batch_size, data_accum, binsno) 

