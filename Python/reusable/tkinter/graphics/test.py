from graphics import *
import time

def main():
    win = GraphWin("test", 500, 500)
    win.setBackground(color_rgb(0, 0, 0))
    win.plot(100, 100, color_rgb(255, 255, 255))
    time.sleep(2)
    win.delete('all')
    win.getMouse() # Pause to view result
    win.close()    # Close window when done

main()
