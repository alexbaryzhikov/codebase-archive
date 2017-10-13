from tkinter import *

class MyCanvas(Canvas):
    def __init__(self, master):
        Canvas.__init__(self, master, width=300, height=300)
        self.pack()
        self.bind("<Button-1>", self._onClick)
        self.mouseClicked = False
        self.tiles = []
        for y in range(5, 256, 50):
            for x in range(5, 256, 50):
                tmp = self.create_rectangle(x, y, x+45, y+45, fill='white', outline='')
                self.tiles.append(tmp)

    def _onClick(self, point):
        print('Clicked')
        self.mouseClicked = True

def update():
    c.update()
    tmp = c.find_withtag("current")
    if tmp:
        c.itemconfig(tmp[0], fill='red')
        c.update()
    root.after(10, update)

root = Tk()
root.title('Grid')
root.resizable(0, 0)
c = MyCanvas(root)

update()
mainloop()
