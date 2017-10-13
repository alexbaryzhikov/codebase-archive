from tkinter import *
import time

root = Tk()
c = Canvas(root, width=500, height=500)
c.pack()
c.create_text(25, 0, anchor=NW, text='man on the moon')
c.create_line(100, 400, 340, 8)
c.create_rectangle(100, 100, 350, 200, fill='white')
c.create_oval(200, 300, 250, 350, fill='red', outline='')
triangle = c.create_polygon([150, 150, 200, 300, 450, 100], fill='green')
root.update()
time.sleep(2)
c.delete(triangle)
root.mainloop()
