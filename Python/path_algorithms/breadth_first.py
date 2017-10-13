from tkinter import *
import numpy as np
from queue import Queue


G = {}

class MyCanvas(Canvas):
    def __init__(self, master, shape):
        self.cwidth = 50
        self.shape = shape
        Canvas.__init__(self, master, width=shape[0]*self.cwidth, height=shape[1]*self.cwidth)
        self.pack()
        self.bind("<Button-1>", self.on_mclick)
        self.bind("<Button-3>", self.on_mclick)
        self.tiles = {}
        self.labels = {}
        for y in range(0, self.shape[1]):
            for x in range(0, self.shape[0]):
                x_, y_ = x*self.cwidth, y*self.cwidth
                tile = self.create_rectangle(x_+1, y_+1, x_+self.cwidth, y_+self.cwidth, \
                    fill='white', outline='')
                self.tiles[(x, y)] = tile
                label = self.create_text((x_+self.cwidth//2, y_+self.cwidth//2), \
                    fill='black', text='')
                self.labels[(x, y)] = label

    def on_mclick(self, event):
        start, goal = G['pathf'].start, G['pathf'].goal
        G['pathf'].reset()
        self.reset()
        x, y = event.x//self.cwidth, event.y//self.cwidth
        if event.num == 1:
            G['pathf'].set_start((x, y))
            if goal:
                G['pathf'].set_goal(goal)
                update()
        elif event.num == 3:
            G['pathf'].set_goal((x, y))
            if start:
                G['pathf'].set_start(start)
                update()

    def reset(self):
        for y in range(0, self.shape[1]):
            for x in range(0, self.shape[0]):
                self.itemconfig(self.tiles[(x, y)], fill = 'white')
                self.itemconfig(self.labels[(x, y)], text = '')

class Grid:
    def __init__(self, x, y):
        self.nodes = np.zeros((x, y), dtype=np.int)
    
    def neighbors(self, node):
        res = [(node[0]+x, node[1]+y) for x in range(-1, 2) for y in range(-1,2) \
               if  (node[0]+x >= 0) and (node[0]+x < self.nodes.shape[0]) \
               and (node[1]+y >= 0) and (node[1]+y < self.nodes.shape[1]) \
               and (x != 0 or y != 0)]
        return res

class Pathfinder:
    def __init__(self):
        self.reset()
    
    def reset(self):
        self.start = None
        self.goal = None
        self.frontier = Queue()
        self.came_from = {}
        self.done = False

    def set_start(self, node):
        self.start = node
        self.frontier.put(node)
        self.came_from[node] = None
        G['c'].itemconfig(G['c'].tiles[node], fill='#ef7373')

    def set_goal(self, node):
        self.goal = node
        G['c'].itemconfig(G['c'].tiles[node], fill='#785bef')

    def expand(self):
        current = self.frontier.get()
        if current == self.goal: self.get_path(); return
        for next_node in G['grid'].neighbors(current):
            if next_node not in self.came_from:
                self.frontier.put(next_node)
                self.came_from[next_node] = current
                text = '{} {}'.format(current[0]-next_node[0], current[1]-next_node[1])
                G['c'].itemconfig(G['c'].labels[next_node], text=text)
        ## coloring pass
        for x in range(G['grid'].nodes.shape[0]):
            for y in range(G['grid'].nodes.shape[1]):
                if (x, y) == self.start or (x, y) == self.goal: pass
                elif (x, y) in self.frontier.queue:
                    G['c'].itemconfig(G['c'].tiles[(x, y)], fill='#62aac9')
                elif (x, y) in self.came_from.keys():
                    G['c'].itemconfig(G['c'].tiles[(x, y)], fill='#bee2c8')

    def get_path(self):
        current = self.goal
        path = []
        while current != self.start:
            path.append(current)
            current = self.came_from[current]
        path.reverse()
        ## coloring pass
        for node in path:
            G['c'].itemconfig(G['c'].tiles[node], fill='#82ef5b')
        self.done = True


def update():
    if not G['pathf'].done:
        G['pathf'].expand()
        root.after(10, update)

def main():
    global root
    root = Tk()
    root.title('Breadth first search')
    root.resizable(0, 0)

    G['grid'] = Grid(15, 15)
    G['pathf'] = Pathfinder()
    G['c'] = MyCanvas(root, G['grid'].nodes.shape)

    mainloop()

if __name__ == '__main__': main()
