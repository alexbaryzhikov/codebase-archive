import numpy as np

N = 7
BOARD_SIZE = N * N
INDEX_BOARD = np.arange(BOARD_SIZE)


def get_segments(i):
    row = [[i + k + l for k in range(4)] for l in range(-3, 1)]
    column = [[i + N * (k + l) for k in range(4)] for l in range(-3, 1)]
    diagonal1 = [[i + (N + 1) * (k + l) for k in range(4)] for l in range(-3, 1)]
    diagonal2 = [[i + (N - 1) * (k + l) for k in range(4)] for l in range(-3, 1)]
    return row + column + diagonal1 + diagonal2


def is_valid(segment):
    # Out of bounds
    for i in segment:
        if i < 0 or i >= BOARD_SIZE:
            return False
    # Tearing
    for i in range(3):
        if abs(segment[i] % N - segment[i+1] % N) > 1:
            return False
    return True


def get_win_segments():
    """ Get all possible win segments for each square of the board """
    win_segments = {}
    for square in INDEX_BOARD:
        win_segments[square] = list(filter(is_valid, get_segments(square)))
    return win_segments


for seg in get_win_segments()[47]:
    print(seg)
