import  pygame
from    pygame.locals import *
import  time
import  random
from    config import *
import  config as G
from    fps import FPS


'''
=============================
load
=============================
'''
def load():
    random.seed()
    pygame.init()
    pygame.display.set_caption('Unnamed')
    G.screen_w      = SCREEN_W
    G.screen_h      = SCREEN_H
    G.screen_mode   = SCREEN_MODE
    G.screen        = pygame.display.set_mode([G.screen_w, G.screen_h], G.screen_mode)
    G.screen.fill(BG_COLOR)
    pygame.display.flip()
    G.background    = G.screen.copy()
    G.dirty_rects   = []
    G.fps           = FPS()


'''
=============================
events
=============================
'''
def events(events_queue):
    for event in events_queue:

        if event.type == QUIT or (event.type == KEYDOWN and event.key == K_ESCAPE):
            return False

        if event.type == KEYDOWN:
            if event.__dict__['key'] == 32: # space
                pass
    return True


'''
=============================
update
=============================
'''
def update(dt):
    if DRAW_FPS: G.fps.update(dt)
    

'''
=============================
draw
=============================
'''
def draw(screen):
    if DRAW_FPS: G.fps.draw(screen)
    pygame.display.update(G.dirty_rects)


'''
=============================
main
=============================
'''
def main():
    load()
    # init clock
    t, t_last = 0.0, 0.0
    time.clock()

    while events(pygame.event.get()): # main loop
        t_loop = time.clock()-t                             # clean loop time
        if t_loop < 1/MAX_FPS: time.sleep(1/MAX_FPS-t_loop) # fps guardian
        t_last = t; t = time.clock()                        # dt
        update(t-t_last)                                    # updates
        draw(G.screen)                                      # draws

    pygame.quit()


if __name__ == '__main__': main()
