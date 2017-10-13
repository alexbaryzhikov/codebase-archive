# import inspect, re

# def varname(p):
#   for line in inspect.getframeinfo(inspect.currentframe().f_back)[3]:
#     m = re.search(r'\bvarname\s*\(\s*([A-Za-z_][A-Za-z0-9_]*)\s*\)', line)
#     if m:
#       return m.group(1)

def namestr(obj, namespace):
    return [name for name in namespace if namespace[name] is obj]

class A:
    def __init__(self):
        self.foo = 0

if __name__ == '__main__':
  spam = 42
  print(namestr(spam, globals())[0])
  bar = A()
  print(namestr(bar.foo, bar.__dict__)[0])
