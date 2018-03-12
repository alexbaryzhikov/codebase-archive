import numpy as np

BOARD_SIDE = 7
BOARD_SIZE = 49
INDEX_BOARD = np.arange(BOARD_SIZE)

def get_neighbors():
    """ Get neighbors for each position """
    # Create framed board
    fb_side = BOARD_SIDE + 2
    framed_board = np.full(fb_side ** 2, -1)
    framed_board.reshape(fb_side, fb_side)[1:-1, 1:-1] = INDEX_BOARD.reshape(BOARD_SIDE, BOARD_SIDE)
    # Get neighbors
    neighbors = {}
    for i in np.arange(fb_side ** 2).reshape(fb_side,fb_side)[1:-1, 1:-1].ravel():
        ni = [i - fb_side - 1, i - fb_side, i - fb_side + 1, i - 1, i + 1, i + fb_side - 1, i + fb_side,
              i + fb_side + 1]
        neighbors[framed_board[i]] = list(filter(lambda x: x != -1, framed_board[ni]))
    return neighbors

NEIGHBORS = get_neighbors()

for i in INDEX_BOARD:
    print('{} - {}'.format(i, NEIGHBORS[i]))
