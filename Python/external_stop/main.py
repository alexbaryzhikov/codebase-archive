import sys
import time
from importlib import reload

import config

# To stop the loop change CONTINUE to False in config.py and save it
print('Looping', end='')
sys.stdout.flush()
while config.CONTINUE:
    time.sleep(1)
    print('.', end='')
    sys.stdout.flush()
    reload(config)
print()
