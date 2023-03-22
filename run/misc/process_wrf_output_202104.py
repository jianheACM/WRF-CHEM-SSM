# KRB file for processing TOMAS data
# Last Updated Aug 2020

from pylab import *
import numpy as np
import netCDF4
from netCDF4 import Dataset
import os
import glob

#----------------------------------------------------------------
# Define relevant inputs for postprocessing
#----------------------------------------------------------------

newfile = 'aero_processed_all.nc'

files = [f for f in sorted(glob.glob("wrfout_d01_2014-03-13*"))]
#met_files = [f for f in sorted(glob.glob("GEOSChem.StateMet.*"))]
#----------------------------------------------------------------
# Calculate surface PM2.5 using bulk tracers
#----------------------------------------------------------------
print('reading in data')

aero_vars = ['ant1_c', 'ant2_c', 'ant3_c', 'ant4_c', 'ant2_o','ant3_o', 'biog1_c', 'biog2_c',\
             'biog3_c', 'biog4_c', 'biog1_o', 'biog2_o', 'biog3_o', 'biog4_o', 'pcg1_b_c', 'pcg1_f_c',\
             'opcg2_f_o', 'opcg3_f_o', 'opcg4_f_o', 'opcg5_f_o', 'opcg6_f_o', 'opcg1_b_o', 'opcg2_b_o', \
             'opcg3_b_o', 'opcg4_b_o', 'opcg5_b_o', 'opcg6_b_o', 'opcg7_b_o', 'iepox', 'iepoxos', 'tetrol',\
             'cldiepoxos', 'cldtetrol', 'gly', 'oc']

# Make netCDF file
nc_w_fid = netCDF4.Dataset(newfile, 'w', clobber=True,  format='NETCDF4')
nc_w_fid.description = 'Aerosol mass concentration data'
nc_w_fid.history = 'Created ' + datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S")

# define file dimensions
nc_w_fid.createDimension('time', None) # unlimited dimension
nc_w_fid.createDimension('lat', 99)
nc_w_fid.createDimension('lon', 149)

# create identity variables
lat_w = nc_w_fid.createVariable('lat', np.float32, ('lat',))
lon_w = nc_w_fid.createVariable('lon', np.float32, ('lon',))

nc_fid = netCDF4.Dataset(files[0],'r')

lat = nc_fid.variables['XLAT'][0]
lon = nc_fid.variables['XLONG'][0]

lon_w[:] = lon[0,:]
lat_w[:] = lat[:,0]

for var in aero_vars:
    #print(var)
    bin_1 = nc_fid.variables[var + '_a01'][16:21,7]
    bin_2 = nc_fid.variables[var + '_a02'][16:21,7]
    bin_3 = nc_fid.variables[var + '_a03'][16:21,7]
    bin_4 = nc_fid.variables[var + '_a04'][16:21,7]

    bin_cw1 = nc_fid.variables[var + '_cw01'][16:21,7]
    bin_cw2 = nc_fid.variables[var + '_cw02'][16:21,7]
    bin_cw3 = nc_fid.variables[var + '_cw03'][16:21,7]
    bin_cw4 = nc_fid.variables[var + '_cw04'][16:21,7]

    # add across bins
    data = np.mean(bin_1 + bin_2 + bin_3 + bin_4 + bin_cw1 + bin_cw2 + bin_cw3 + bin_cw4, 0)
    data_w = nc_w_fid.createVariable(var, np.float32, ('lat', 'lon',))
    data_w[:,:] = data
    #print('min: ', np.min(data))
    print(np.max(data))
                                    

nc_fid.close()
nc_w_fid.close()

