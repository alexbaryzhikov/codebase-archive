import pygame, time, math, random
from pygame.locals import *
from config import *
import config as G

# -----------------------------------------------------------------------------
# Objects

class Circle:
    def __init__(self, pos):
        self.pos = pos
        self.r =   0

    def update(self):
        self.r = 0
        for ripple in G.ripples:
            d = euclidean(self.pos, ripple.pos) - ripple.r
            self.r += ripple.mag * sigmoid_deriv(d, ripple.mag)

def sigmoid_deriv(x, mag):
    sigm = (math.tanh(x*mag/RELAX) + 1) / 2
    return sigm * (1 - sigm)

def euclidean(pos1, pos2=[SCREEN_W//2, SCREEN_H//2]):
    return math.sqrt((pos1[0]-pos2[0])**2 + (pos1[1]-pos2[1])**2)

class Ripple:
    def __init__(self, pos, mag):
        self.pos   = pos
        self.mag   = mag
        self.r     = 0

    def update(self, dt):
        self.r += SPEED * dt
        self.mag *= DECAY

# -----------------------------------------------------------------------------
# Engine callbacks


def load():
    pygame.init()
    G.screen = pygame.display.set_mode([SCREEN_W, SCREEN_H], SCREEN_MODE)
    pygame.display.set_caption('Circles')
    G.circles = [Circle([x, y]) for x in range(0, SCREEN_W+1, 20)
                                for y in range(0, SCREEN_H+1, 20)]
    G.ripples = []
    G.frame  = 0

def events(events_queue):
    for event in events_queue:
        if event.type == QUIT or (event.type == KEYDOWN and event.key == K_ESCAPE):
            return False
        if event.type == KEYDOWN and event.key == 32:
            x = random.randint(-int(SCREEN_W*0.1), int(SCREEN_W*1.1))
            y = random.randint(-int(SCREEN_H*0.1), int(SCREEN_H*1.1))
            mag = random.randint(MAG_MIN, MAG_MAX)
            G.ripples.append(Ripple([x, y], mag))
    return True

def update(dt):
    for x in G.ripples: x.update(0.05)  # 0.05 = 20 fps
    G.ripples = [x for x in G.ripples if x.mag > 5]
    for x in G.circles: x.update()
    G.frame += 1

def draw():
    G.screen.fill(TERRACOTTA)
    for c in G.circles:
        if c.r > 1:
            pygame.draw.circle(G.screen, TURQUOISE, c.pos, round(c.r))
    pygame.display.flip()
    if RENDER:
        pygame.image.save(G.screen, 'frames/frame%04d.png' % (G.frame-1))

def main():
    load()
    t = time.clock()  # init clock
    while events(pygame.event.get()):
        t_last, t = t, time.clock()
        update(t - t_last)
        draw()
    pygame.quit()

if __name__ == '__main__': main()
