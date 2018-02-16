import numpy as np

BOARD_SIZE = 5

def get_winners():
    """ Get all win combinations of the board """
    index_board = np.array([i for i in range(BOARD_SIZE ** 2)])
    rows = get_rows(index_board)
    winners = []
    for row in rows:
        winners.extend(row_winners(row))
    return winners

def row_winners(row):
    """ All win combinations of the row """
    return [row[i:i+4] for i in range(len(row)-3)]

def get_rows(board):
    """ All rows, columns and diagonals of the board """
    board = board.reshape(BOARD_SIZE, BOARD_SIZE)
    board_flip = np.fliplr(board)
    rows = [board[0]]
    cols = [board[:,0]]
    diags1 = [board.diagonal(0)]
    diags2 = [board_flip.diagonal(0)]
    for i in range(1, BOARD_SIZE):
        rows.append(board[i])
        cols.append(board[:,i])
        if BOARD_SIZE - i >= 4:
            diags1.append(board.diagonal(i))
            diags1.append(board.diagonal(-i))
            diags2.append(board_flip.diagonal(i))
            diags2.append(board_flip.diagonal(-i))
    res = []
    res.extend(rows)
    res.extend(cols)
    res.extend(diags1)
    res.extend(diags2)
    return res

for w in get_winners():
    print(w)