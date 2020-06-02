import numpy as np
import matplotlib.pyplot as plt
import matplotlib.animation as animation
from logistic import logictic_sequence

title = "Logistic sequence: x' = rx(1 - x)"

def make_plot(ys):
    xs = np.arange(len(ys)) + 1
    fig = plt.figure(figsize=(12.8, 7.2))
    fig.set_tight_layout(True)
    ax = fig.subplots()
    ax.set(
        xlabel='Iterations', 
        ylabel='Population', 
        title='{}\nr = ?'.format(title), 
        xlim=(0, len(ys)),
        ylim=(0, 1), 
        xticks=np.arange(0, len(ys) + 1, 5),
        yticks=np.arange(0.0, 1.0, 0.1)
    )
    ax.grid()
    line, = ax.plot(xs, ys)
    return fig, ax, line

def update_plot(data, ax, line):
    r, ys = data
    ax.set_title('{}\nr = {:0.3f}'.format(title, r))
    xs = np.arange(len(ys)) + 1
    line.set_data(xs, ys)

def render_plots(r_start, r_end, r_step, x_init, iters):

    def data_gen():
        for r in np.arange(r_start, r_end, r_step):
            yield r, logictic_sequence(r, x_init).take(iters)

    fig, ax, line = make_plot(logictic_sequence(r_start, x_init).take(iters))
    n = (r_end - r_start) / r_step
    ani = animation.FuncAnimation(fig, update_plot, data_gen, fargs=(ax, line), save_count=n, repeat=False, blit=False)
    writer = animation.FFMpegWriter(fps=30, bitrate=3600, metadata=dict(artist="Alexey Baryzhikov"))
    ani.save('plot.mp4', writer=writer)

if __name__ == '__main__':
    # make_plot(logictic_sequence(2.7, 0.2).take(50))[0].savefig('plot.png')
    render_plots(r_start=2.5, r_end=4.0, r_step=0.002, x_init=0.2, iters=100)
