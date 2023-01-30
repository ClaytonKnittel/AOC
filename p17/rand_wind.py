import random
import sys

if len(sys.argv) != 2:
  print(f'usage: {sys.argv[0]} <n>')
  exit(-1)

n = int(sys.argv[1])

winds = ''.join((random.choice(('<', '>')) for _ in range(n)))
print(winds)
