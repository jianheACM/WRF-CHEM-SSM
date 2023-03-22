
import numpy as np

with open('./test.txt','r') as f:
    lines = f.readlines()[0]


print(len(lines.split(',')))
