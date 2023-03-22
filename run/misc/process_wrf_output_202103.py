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

newfile = 'processed_output.nc'

files = [f for f in sorted(glob.glob("wrfout_d01_2014-03-13*"))]
#met_files = [f for f in sorted(glob.glob("GEOSChem.StateMet.*"))]
#----------------------------------------------------------------
# Calculate surface PM2.5 using bulk tracers
#----------------------------------------------------------------
print('reading in data')

oh_ppmv = []
o3_ppmv = []
no_ppmv = []
no2_ppmv = []
nox_ppmv = []
#sesq_ppmv = []
#isop_ppmv = []
#isop_iepox_ppmv = []
#anthro_soa_ppmv = []
#mono_ppmv = []


ttime = []
llat = []
llev = []
llon = []
#pm_dry_conc = []
for i in range(0,len(files)):

    fname = files[i]
    print(fname)
    nc_fid = netCDF4.Dataset(fname,'r')
    
    # save aerosol variables
    oh = nc_fid.variables['ho'][:] # ppmv
    o3 = nc_fid.variables['o3'][:] # ppmv

    no = nc_fid.variables['no'][:] # ppmv
    no2 = nc_fid.variables['no2'][:] # ppmv
    nox = no + no2

    #sesq = nc_fid.variables['ant1_c'][:] + nc_fid.variables['ant2_c'][:] + \
    #       nc_fid.variables['ant3_c'][:] + nc_fid.variables['ant4_c'][:]

    #isop = nc_fid.variables['biog1_c'][:] + nc_fid.variables['biog2_c'][:] + \
    #       nc_fid.variables['biog3_c'][:] + nc_fid.variables['biog4_c'][:]
    
    #mono = nc_fid.variables['biog1_o'][:] + nc_fid.variables['biog2_o'][:] + \
    #       nc_fid.variables['biog3_o'][:] + nc_fid.variables['biog4_o'][:]

    #isop_iepox = nc_fid.variables['iepox'][:] + nc_fid.variables['tetrol'][:] + \
    #             nc_fid.variables['cldtetrol'][:]

    # isoprene = nc_fid.variables['isoprene'][:]
    #sesq = nc_fid.variables['sesq'][:]
    #terp = nc_fid.variables['terp'][:]

    lat = nc_fid.variables['XLAT'][:]
    lon = nc_fid.variables['XLONG'][:]
    lev = (nc_fid.variables['PH'][:]+nc_fid.variables['PHB'][:]) / 9.81 #m
    time = nc_fid.variables['XTIME'][:]

    nc_fid.close()
    

    for i in range(len(time)):
        print(i)
        oh_ppmv.append(oh[i])
        o3_ppmv.append(o3[i])
        no_ppmv.append(no[i])
        no2_ppmv.append(no2[i])
        nox_ppmv.append(nox[i])
        #sesq_ppmv.append(sesq[i])
        #isop_ppmv.append(isop[i])
        #mono_ppmv.append(mono[i])
        #isop_iepox_ppmv.append(isop_iepox[i])
        #anthro_soa_ppmv.append(anthro_soa[i])
        ttime.append(time[i])
        llat.append(lat[i])
        llon.append(lon[i])
        llev.append(lev[i])

#----------------------------------------------------------------
# Save data to files
#----------------------------------------------------------------

# Make netCDF file
nc_w_fid = netCDF4.Dataset(newfile, 'w', clobber=True,  format='NETCDF4')
nc_w_fid.description = 'Aerosol mass concentration data'
nc_w_fid.history = 'Created ' + datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S")

# define file dimensions
nc_w_fid.createDimension('time', None) # unlimited dimension
nc_w_fid.createDimension('lat', 99)
nc_w_fid.createDimension('levc', 45)
nc_w_fid.createDimension('leve', 46)
nc_w_fid.createDimension('lon', 149)

# create identity variables
time_w = nc_w_fid.createVariable('time', np.float32, ('time',))
lat_w = nc_w_fid.createVariable('lat', np.float32, ('lat',))
lon_w = nc_w_fid.createVariable('lon', np.float32, ('lon',))

lon_w[:] = np.array(llon)[0,0,:]
lat_w[:] = np.array(llat)[0,:,0]
time_w[:] = ttime

#[pm[0], bc[0], oc[0], dst[0], so4[0], nit[0], nh4[0], nacl[0]]
o3_w = nc_w_fid.createVariable('o3_ppmv', np.float32, ('time','levc', 'lat', 'lon',))
oh_w = nc_w_fid.createVariable('oh_ppmv', np.float32, ('time','levc', 'lat', 'lon',))
no_w = nc_w_fid.createVariable('no_ppmv', np.float32, ('time','levc', 'lat', 'lon',))
no2_w = nc_w_fid.createVariable('no2_ppmv', np.float32, ('time','levc', 'lat', 'lon',))
nox_w = nc_w_fid.createVariable('nox_ppmv', np.float32, ('time','levc', 'lat', 'lon',))
#pm_w = nc_w_fid.createVariable('pm_conc', np.float32, ('time','levc', 'lat', 'lon',))
#biog_soa_w = nc_w_fid.createVariable('biog_soa_conc', np.float32, ('time','levc', 'lat', 'lon',))
llev_w = nc_w_fid.createVariable('llev', np.float32, ('time','leve', 'lat', 'lon',))
llat_w = nc_w_fid.createVariable('llat', np.float32, ('time', 'lat', 'lon',))
llon_w = nc_w_fid.createVariable('llon', np.float32, ('time','lat', 'lon',))

llev_w[:,:,:,:] = llev
llat_w[:,:,:] = llat
llon_w[:,:,:] = llon

o3_w[:,:,:,:] = np.array(o3_ppmv) 
oh_w[:,:,:,:] = np.array(oh_ppmv)
no_w[:,:,:,:] = np.array(no_ppmv)
no2_w[:,:,:,:] = np.array(no2_ppmv)
nox_w[:,:,:,:] = np.array(nox_ppmv)
#biog_soa_w[:,:,:,:] = np.array(biog_soa_mconc)
#pm_w[:,:,:,:] = np.array(pm_conc)

print('o3')
print(np.mean(np.array(o3_ppmv)))

print('oh')
print(np.mean(np.array(oh_ppmv)))

print('nox')
print(np.mean(np.array(nox_ppmv)))

print('bsoa')
#print(np.mean(np.array(biog_soa_mconc)))

nc_w_fid.close()

