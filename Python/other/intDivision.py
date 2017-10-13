# -*- coding: utf-8 -*-

def intdiv( N, D ):
    '''
    N       divident as int
    D       divisor as int
    
    outputs: quotient and remainder
    '''
    
    def strbin( n ):
        '''
        n       number as int
        
        returns: n as binary string w/o prefix '0b'
        '''

        return str( n ) if n <= 1 else strbin( n >> 1 ) + str( n & 1 )

    Q, R = 0, 0
    N_ = strbin( N )
    
    for i in range( len( N_ ) ):
        R <<= 1        
        R += int( N_[ i ] )
        if R >= D:
            R -= D
            Q += 1 << len( N_) - 1 - i

    return Q, R
