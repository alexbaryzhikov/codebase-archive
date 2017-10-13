import pygame, time
from pygame.locals import *
from config import *
import config as G

def load():
    pygame.init()
    G.screen = pygame.display.set_mode([SCREEN_W, SCREEN_H], SCREEN_MODE)
    pygame.display.set_caption('Pygame')

def events(events_queue):
    for event in events_queue:
        if event.type == QUIT or (event.type == KEYDOWN and event.key == K_ESCAPE):
            return False
    return True

def update(dt):
    pass

def draw():
    G.screen.fill(BG_COLOR)
    pygame.display.flip()

def main():
    load()
    t = time.clock()  # init clock
    while events(pygame.event.get()):
        t_last, t = t, time.clock()
        update(t - t_last)
        draw()
    pygame.quit()

if __name__ == '__main__': main()
