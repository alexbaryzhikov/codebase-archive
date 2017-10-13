from tkinter import *
import time

root = Tk()
root.title('Man on the Moon')

margin = Frame(root, width=50, height=10)
margin.pack()

e = Entry(root, width=50)
e.pack()
e.focus_set()

def callback():
    print(e.get())

b = Button(root, text="get", width=10, command=callback)
b.pack()

var = IntVar()

c = Checkbutton(root, text="Expand", variable=var)
c.pack()

mainloop()
