from pylab import *
import pandas as pd

import sys
import json

lines = open(sys.argv[1]).readlines()
data_list = map(json.loads, lines)
data = []

for t,l in enumerate(data_list):
    sexual = filter(lambda x: x['sexual'], l)
    asexual = filter(lambda x: not x['sexual'], l)
    mean_sex = sum(x['fitness'] for x in sexual)/(len(sexual) or 1)
    mean_asex = sum(x['fitness'] for x in asexual)/(len(asexual) or 1)
    data.append((len(sexual), mean_sex, len(asexual), mean_asex))

df = pd.DataFrame(
    data, columns=['nsexual', 'sexfitness', 'nasexual', 'asexfitness'])

df.plot()

show()
