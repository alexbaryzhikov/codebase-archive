from tkinter import *
import numpy as np
from queue import PriorityQueue

G = {}
MOVE_COST = 10
MOVE_COST_DIAG = 14

class MyCanvas(Canvas):
    def __init__(self, master, shape):
        self.cwidth = 40
        self.shape = shape
        Canvas.__init__(self, master, width=shape[0]*self.cwidth, height=shape[1]*self.cwidth)
        self.pack()
        self.bind("<Button-1>", self.on_mclick)
        self.bind("<Button-2>", self.on_mclick)
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
                    fill='black', text=G['grid'].nodes[(x, y)], font='Arial 8')
                self.labels[(x, y)] = label
        for node in G['grid'].walls:
            self.itemconfig(self.tiles[node], fill = 'gray')

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
        elif event.num == 2:
            if (x, y) in G['grid'].walls:
                G['grid'].walls.remove((x, y))
            else:
                G['grid'].walls.append((x, y))
            self.reset()

    def reset(self):
        for y in range(0, self.shape[1]):
            for x in range(0, self.shape[0]):
                self.itemconfig(self.tiles[(x, y)], fill = 'white')
                self.itemconfig(self.labels[(x, y)], text = G['grid'].nodes[(x, y)])
        for node in G['grid'].walls:
            self.itemconfig(self.tiles[node], fill = 'gray')

class Grid:
    def __init__(self, x, y):
        np.random.seed()
        self.nodes = np.random.randint(200, size=(x, y))
        self.walls = []
        ## create walls
        for i in range(10):
            self.walls.append((i, 4))
        for i in range(4, 21):
            self.walls.append((9, i))

    def neighbors(self, node):
        res = [(node[0]+x, node[1]+y) for x in range(-1, 2) for y in range(-1,2) \
               if  (node[0]+x >= 0) and (node[0]+x < self.nodes.shape[0]) \
               and (node[1]+y >= 0) and (node[1]+y < self.nodes.shape[1]) \
               and (x != 0 or y != 0)]
        res = [node for node in res if node not in self.walls]
        return res

    def cost(self, node_from, node_to):
        move_cost = 0
        if node_from[0] == node_to[0] or node_from[1] == node_to[1]:
            move_cost = MOVE_COST
        else:
            move_cost = MOVE_COST_DIAG
        return move_cost+self.nodes[node_to]

class Pathfinder:
    def __init__(self):
        self.reset()
    
    def reset(self):
        self.start = None
        self.goal = None
        self.frontier = PriorityQueue()
        self.came_from = {}
        self.cost_so_far = {}
        self.explored = {}
        self.done = False

    def set_start(self, node):
        self.start = node
        self.frontier.put((0, node))
        self.came_from[node] = None
        self.cost_so_far[node] = 0
        G['c'].itemconfig(G['c'].tiles[node], fill='#ef7373')

    def set_goal(self, node):
        self.goal = node
        G['c'].itemconfig(G['c'].tiles[node], fill='#785bef')

    def expand(self):
        while True: # pop queue until unexplored node
            if self.frontier.empty(): return # there is no path
            current = self.frontier.get()[1]
            if current not in self.explored.keys(): break
        if current == self.goal: self.get_path(); return # path found
        self.explored[current] = True # mark node as explored
        for next_node in G['grid'].neighbors(current):
            if next_node in self.explored.keys(): continue # skip all explored nodes
            new_cost = self.cost_so_far[current] + G['grid'].cost(current, next_node)
            if next_node not in self.cost_so_far or new_cost < self.cost_so_far[next_node]:
                self.cost_so_far[next_node] = new_cost
                priority = new_cost
                self.frontier.put((priority, next_node))
                self.came_from[next_node] = current
                ## update label
                text = '{}\n{}'.format(int(G['grid'].nodes[next_node]), int(self.cost_so_far[next_node]))
                G['c'].itemconfig(G['c'].labels[next_node], text=text)
        ## coloring pass
        for x in range(G['grid'].nodes.shape[0]):
            for y in range(G['grid'].nodes.shape[1]):
                if (x, y) == self.start or (x, y) == self.goal: pass
                elif (x, y) in [item[1] for item in self.frontier.queue]:
                    G['c'].itemconfig(G['c'].tiles[(x, y)], fill='#62aac9')
                elif (x, y) in self.cost_so_far.keys():
                    G['c'].itemconfig(G['c'].tiles[(x, y)], fill='#bee2c8')

    def get_path(self):
        current = self.goal
        path = []
        while current != self.start:
            path.append(current)
            current = self.came_from[current]
        path.reverse()
        ## coloring pass
        for node in path[:-1]:
            G['c'].itemconfig(G['c'].tiles[node], fill='#82ef5b')
        self.done = True

def update():
    if not G['pathf'].done:
        G['pathf'].expand()
        root.after(10, update)

def main():
    global root
    root = Tk()
    root.title('Dijkstra search')
    root.resizable(0, 0)

    G['grid'] = Grid(30, 30)
    G['pathf'] = Pathfinder()
    G['c'] = MyCanvas(root, G['grid'].nodes.shape)

    mainloop()

if __name__ == '__main__': main()
