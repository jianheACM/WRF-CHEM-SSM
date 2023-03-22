
import numpy  as np
import pandas as pd
import matplotlib.pyplot as plt

import netCDF4 as ncdf


# READ DATA:
# ==================================================================
ds1 = ncdf.Dataset('wrfout_d01_2014-03-10_00:00:00',mode='r')

# SPECIES NAMES FOR SOA:
# ==================================================================
# GET NAMES:
NAMEs = []

# PREFIXes:
PREFIX = ['TERP_CN3_a']

for pp in PREFIX:
    for i in list(ds1.variables.keys()):
        if pp in i:
            NAMEs.append(i)

print(NAMEs)
