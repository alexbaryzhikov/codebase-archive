# -*- coding: utf-8 -*-

def MarkCoverage( queens ):
    '''
    queens: queens positions as a tuple
    
    returns: board as a list, controlled squares marked with 1
             returns [ 0 ] in case of invalid queen position
    '''
    # check if any of the queens is on the black diagonal
    blackDiagonal = ( 7, 14, 21, 28, 35, 42, 49, 56 )    
    if set( queens ) & set( blackDiagonal ): return [ 0 ]
    
    # mark squares controlled by queens
    board = [ 0 for i in range( 64 ) ]
    for i in blackDiagonal: board[ i ] = 1
    for q in queens:
        row, col = q // 8, q % 8
        offsetNE = row + col    # NE-diagonal offset
        offsetNW = col - row    # NW-diagonal offset

        if offsetNE < 7:                            # upper half
            numRowElems =       7 - row
            numColElems =       7 - col
            numNEDiagElems =    1 + offsetNE
            firstRowElem =      8 * row
            firstColElem =      col
            firstNEDiagElem =   offsetNE
        else:                                       # lower half
            numRowElems =       row
            numColElems =       col
            numNEDiagElems =    15 - offsetNE
            firstRowElem =      7 * row + 8
            firstColElem =      64 - 7 * col
            firstNEDiagElem =   8 * offsetNE - 49

        for i in range( numRowElems ):              # mark row
            board[ firstRowElem + i ] = 1
        for i in range( numColElems ):              # mark column
            board[ firstColElem + 8 * i ] = 1
        for i in range( numNEDiagElems ):           # mark NE-diagonal
            board[ firstNEDiagElem + 7 * i ] = 1
        
        # setup and mark NW-diagonal
        numNWDiagElems = 8 - abs( offsetNW )
        if offsetNW > 0:                            # right offset
            firstNWDiagElem = offsetNW 
        else:                                       # left offset
            firstNWDiagElem = 8 * ( - offsetNW )
        if offsetNE % 2:                            # black squares have boundary
            numNWDiagElems //= 2                    # cut off half of the elements
            if offsetNE > 7:                        # if lower half
                firstNWDiagElem += 9 * ( numNWDiagElems + 1 )   # then offset first element

        for i in range( numNWDiagElems ):           # mark NW-diagonal
            board[ firstNWDiagElem + 9 * i ] = 1
    
    return board

def PrintBoard( board ):
    '''
    board: board as a list
    
    return: None, prints board layout
    '''    
    for i in range( 0, 57, 8 ):
        for j in range( 8 ):
            if board[ i + j ]: print( '+', end = ' ' )
            else: print( '-', end = ' ' )
        print()
        
def TestQueens( n ):
    '''
    n: number of queens
    
    returns: queens positions as a tuple, if queens control all of the board
    '''
    import itertools
    board = ( i for i in range( 64 ) )
    for queens in itertools.combinations( board, n ):
        if 0 in MarkCoverage( queens ):
            continue
        print( queens )
            
