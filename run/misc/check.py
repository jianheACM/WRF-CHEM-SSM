
import numpy  as np
import pandas as pd
import matplotlib.pyplot as plt

import netCDF4 as ncdf


# READ DATA:
# ==================================================================
#ds1 = ncdf.Dataset('wrfinput_d01',mode='r+')
#ds1 = ncdf.Dataset('TEST.nc',mode='r+')
ds1 = ncdf.Dataset('wrfinput_d01',mode='r')

#xxx = ds1.createDimension("xxx", 144)

#xxx = ds1.dimensions['Time'].name


#print(ds1.variables)
#print(ds1.dimensions)

print(ds1['Times'][:])

#AAA = ds1.createVariable('AAB', np.float32, ('Time','bdy_width','bottom_top','south_north'))

#AAA = ds1.createVariable('AAA', np.float64, ('xxx'))

# SPECIES NAMES FOR SOA:
# ==================================================================
# GET NAMES:
'''
NAMEs = []

# PREFIXes:
PREFIX = ['ARO1_CN3_g']
PREFIX = ['num_a08']

for pp in PREFIX:
    for i in list(ds1.variables.keys()):
        if pp in i:
            NAMEs.append(i)

print(NAMEs)
'''
