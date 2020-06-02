class logictic_sequence(object):
    """Generate logistic sequence of form x' = rx(1-x)."""

    def __init__(self, r, x_init):
        self.r = r
        self.x = x_init
    
    def __iter__(self):
        return self

    def __next__(self):
        return self.next()

    def next(self):
        result = self.x
        self.x = self.r * self.x * (1 - self.x)
        return result
    
    def take(self, n):
        result = []
        for _ in range(n):
            result.append(self.next())
        return result

if __name__ == '__main__':
    print(logictic_sequence(2.7, 0.1).take(20))
