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

newfile = 'met_processed_all.nc'

files = [f for f in sorted(glob.glob("wrfout_d01_2014-03-1[3-7]*"))]
#met_files = [f for f in sorted(glob.glob("GEOSChem.StateMet.*"))]
#----------------------------------------------------------------
# Calculate surface PM2.5 using bulk tracers
#----------------------------------------------------------------
print('reading in data')

met_vars = ['T2', 'Q2', 'U10', 'V10', 'PBLH', 'LH', 'SWDOWN']

# Make netCDF file
nc_w_fid = netCDF4.Dataset(newfile, 'w', clobber=True,  format='NETCDF4')
nc_w_fid.description = 'Met data'
nc_w_fid.history = 'Created ' + datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S")

# define file dimensions
nc_w_fid.createDimension('time', None) # unlimited dimension
#nc_w_fid.createDimension('subgrid_stag', 10)
nc_w_fid.createDimension('lat', 99)
nc_w_fid.createDimension('lon', 149)

# create identity variables
lat_w = nc_w_fid.createVariable('lat', np.float32, ('lat',))
lon_w = nc_w_fid.createVariable('lon', np.float32, ('lon',))
#subgrid_stag_w = nc_w_fid.createVariable('subgrid_stag', np.float32, ('subgrid_stag',))

nc_fid = netCDF4.Dataset(files[0],'r')

lat = nc_fid.variables['XLAT'][0]
lon = nc_fid.variables['XLONG'][0]

lon_w[:] = lon[0,:]
lat_w[:] = lat[:,0]

for var in met_vars:
    print(var)
    data_all = []
    if (var == 'T_REF2M') or (var == 'Q_REF2M'):
        for f in files:
            print(f)
            nc_fid = netCDF4.Dataset(f,'r')
            data = nc_fid.variables[var][:]
            for i in range(len(data)):
                data_all.append(data[i])
        data_w = nc_w_fid.createVariable(var, np.float32, ('time', 'subgrid_stag', 'lat', 'lon',))
        data_w[:,:,:, :] = data_all
    else:
        for f in files:
            nc_fid = netCDF4.Dataset(f,'r')
            data = nc_fid.variables[var][:]
            for i in range(len(data)):
                data_all.append(data[i])
        data_w = nc_w_fid.createVariable(var, np.float32, ('time', 'lat', 'lon',))
        data_w[:,:,:] = data_all
        
nc_fid.close()
nc_w_fid.close()

