def update(dt):
    ''' Main cycle'''

    # This limits the while loop to a max of 60 times per second.
    # Leave this out and we will use all CPU we can.
    clock.tick(60)
     
    for event in pygame.event.get(): # User did something
        if event.type == pygame.QUIT: # If user clicked close
            return False
 
    # Clear the screen and set the screen background
    screen.fill(0x000000)
 
    pygame.draw.line(screen, 0x005500, [0, 0], [50, 30])
    pygame.draw.lines(screen, 0x0055DD, False, [[0, 80], [50, 90], [200, 80], [220, 30]], 5)
    pygame.draw.aaline(screen, GREEN, [0, 50], [50, 80], True)

    # Draw a rectangle outline
    pygame.draw.rect(screen, 0xAA3300, [75, 10, 50, 20], 2)
     
    # Draw a solid rectangle
    pygame.draw.rect(screen, 0x7122a5, [150, 10, 50, 20])
     
    # Draw an ellipse outline, using a rectangle as the outside boundaries
    pygame.draw.ellipse(screen, 0x375a8e, [320, 250, 150, 150], 1) 

    # Draw an solid ellipse, using a rectangle as the outside boundaries
    pygame.draw.ellipse(screen, 0xce7b37, [300, 250, 100, 120]) 
 
    # This draws a triangle using the polygon command
    pygame.draw.polygon(screen, 0x3792ce, [[100, 100], [0, 200], [200, 200]])
  
    # Draw an arc as part of an ellipse. 
    # Use radians to determine what angle to draw.
    pygame.draw.arc(screen, 0xfffb35, [210, 75, 150, 125], 0, pi/2, 5)
    pygame.draw.arc(screen, GREEN,[210, 75, 150, 125], pi/2, pi, 5)
    pygame.draw.arc(screen, BLUE, [210, 75, 150, 125], pi, 3*pi/2, 5)
    pygame.draw.arc(screen, RED,  [210, 75, 150, 125], 3*pi/2, 2*pi, 5)
    
    # Draw a circle
    pygame.draw.circle(screen, BLUE, [60, 250], 40)
    
    # Go ahead and update the screen with what we've drawn.
    # This MUST happen after all the other drawing commands.
    pygame.display.flip()
    return True
 
if __name__=='__main__':
    import pygame
    from math import pi
    import time

    # Initialize the game engine
    pygame.init()

    # Define the colors we will use in RGB format
    BLACK = (  0,   0,   0)
    WHITE = (255, 255, 255)
    BLUE =  (  0,   0, 255)
    GREEN = (  0, 255,   0)
    RED =   (255,   0,   0)

    # Set the height and width of the screen
    WIDTH = 640
    HEIGHT = 480
    screen = pygame.display.set_mode([WIDTH, HEIGHT])

    pygame.display.set_caption("Example code for the draw module")

    # Loop until the user clicks the close button.
    clock = pygame.time.Clock()
    t, t_last = 0.0, 0.0
    time.clock()

    # main cycle
    while update(t-t_last):
        t_last = t
        t = time.clock()

    pygame.quit()
 