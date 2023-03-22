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

files = [f for f in sorted(glob.glob("wrfout_d01_2014-03-11_12*"))]
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
biog_conc = []
hoa_conc = []
soa_conc = []
bboa_conc = []
bbsoa_conc = []
hsoa_conc = []
arosoa_conc = []
totoa_conc = []
pm_conc = []

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
    oh = nc_fid.variables['ho'][4:9] # ppmv
    o3 = nc_fid.variables['o3'][4:9] # ppmv

    no = nc_fid.variables['no'][4:9] # ppmv
    no2 = nc_fid.variables['no2'][4:9] # ppmv
    nox = no + no2

    pm = nc_fid.variables['PM2_5_DRY'][4:9]

    biog = nc_fid.variables['BIOG_A01'][4:9] + nc_fid.variables['BIOG_A02'][4:9] + \
           nc_fid.variables['BIOG_A03'][4:9] + nc_fid.variables['BIOG_A04'][4:9]#ug m-3
    
    hoa = nc_fid.variables['HOA_A01'][4:9] + nc_fid.variables['HOA_A02'][4:9] + \
          nc_fid.variables['HOA_A03'][4:9] + nc_fid.variables['HOA_A04'][4:9]
    
    soa = nc_fid.variables['SOA_A01'][4:9] + nc_fid.variables['SOA_A02'][4:9] + \
          nc_fid.variables['SOA_A03'][4:9] + nc_fid.variables['SOA_A04'][4:9]

    bboa = nc_fid.variables['BBOA_A01'][4:9] + nc_fid.variables['BBOA_A02'][4:9] + \
           nc_fid.variables['BBOA_A03'][4:9] + nc_fid.variables['BBOA_A04'][4:9]

    bbsoa = nc_fid.variables['BBSOA_A01'][4:9] + nc_fid.variables['BBSOA_A02'][4:9] + \
           nc_fid.variables['BBSOA_A03'][4:9] + nc_fid.variables['BBSOA_A04'][4:9]
    
    hsoa = nc_fid.variables['HSOA_A01'][4:9] + nc_fid.variables['HSOA_A02'][4:9] + \
           nc_fid.variables['HSOA_A03'][4:9] + nc_fid.variables['HSOA_A04'][4:9]

    arosoa = nc_fid.variables['AROSOA_A01'][4:9] + nc_fid.variables['AROSOA_A02'][4:9] + \
             nc_fid.variables['AROSOA_A03'][4:9] + nc_fid.variables['AROSOA_A04'][4:9]

    totoa = nc_fid.variables['TOTOA_A01'][4:9] + nc_fid.variables['TOTOA_A02'][4:9] + \
            nc_fid.variables['TOTOA_A03'][4:9] + nc_fid.variables['TOTOA_A04'][4:9]

    lat = nc_fid.variables['XLAT'][4:9]
    lon = nc_fid.variables['XLONG'][4:9]
    lev = (nc_fid.variables['PH'][4:9]+nc_fid.variables['PHB'][4:9]) / 9.81 #m
    time = nc_fid.variables['XTIME'][4:9]

    nc_fid.close()
    

    for i in range(len(time)):
        print(i)
        oh_ppmv.append(oh[i])
        o3_ppmv.append(o3[i])
        no_ppmv.append(no[i])
        no2_ppmv.append(no2[i])
        nox_ppmv.append(nox[i])
        biog_conc.append(biog[i])
        hoa_conc.append(hoa[i])
        soa_conc.append(soa[i])
        bboa_conc.append(bboa[i])
        bbsoa_conc.append(bbsoa[i])
        hsoa_conc.append(hsoa[i])
        arosoa_conc.append(arosoa[i])
        totoa_conc.append(totoa[i])
        pm_conc.append(pm[i])
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
pm_w = nc_w_fid.createVariable('pm_conc', np.float32, ('time','levc', 'lat', 'lon',))
llev_w = nc_w_fid.createVariable('llev', np.float32, ('time','leve', 'lat', 'lon',))
llat_w = nc_w_fid.createVariable('llat', np.float32, ('time', 'lat', 'lon',))
llon_w = nc_w_fid.createVariable('llon', np.float32, ('time','lat', 'lon',))

biog_w = nc_w_fid.createVariable('biog_conc', np.float32, ('time','levc', 'lat', 'lon',))
hoa_w = nc_w_fid.createVariable('hoa_conc', np.float32, ('time','levc', 'lat', 'lon',))
soa_w = nc_w_fid.createVariable('soa_conc', np.float32, ('time','levc', 'lat', 'lon',))
bboa_w = nc_w_fid.createVariable('bboa_conc', np.float32, ('time','levc', 'lat', 'lon',))
bbsoa_w = nc_w_fid.createVariable('bbsoa_conc', np.float32, ('time','levc', 'lat', 'lon',))
hsoa_w = nc_w_fid.createVariable('hsoa_conc', np.float32, ('time','levc', 'lat', 'lon',))
arosoa_w = nc_w_fid.createVariable('arosoa_conc', np.float32, ('time','levc', 'lat', 'lon',))
totoa_w = nc_w_fid.createVariable('totoa_conc', np.float32, ('time','levc', 'lat', 'lon',))

llev_w[:,:,:,:] = llev
llat_w[:,:,:] = llat
llon_w[:,:,:] = llon

o3_w[:,:,:,:] = np.array(o3_ppmv) 
oh_w[:,:,:,:] = np.array(oh_ppmv)
no_w[:,:,:,:] = np.array(no_ppmv)
no2_w[:,:,:,:] = np.array(no2_ppmv)
nox_w[:,:,:,:] = np.array(nox_ppmv)
pm_w[:,:,:,:] = np.array(pm_conc)

biog_w[:,:,:,:] = np.array(biog_conc)
hoa_w[:,:,:,:] = np.array(hoa_conc)
soa_w[:,:,:,:] = np.array(soa_conc)
bboa_w[:,:,:,:] = np.array(bboa_conc)
bbsoa_w[:,:,:,:] = np.array(bbsoa_conc)
hsoa_w[:,:,:,:] = np.array(hsoa_conc)
arosoa_w[:,:,:,:] = np.array(arosoa_conc)
totoa_w[:,:,:,:] = np.array(totoa_conc)

print('o3')
print(np.mean(np.array(o3_ppmv)))

print('oh')
print(np.mean(np.array(oh_ppmv)))

print('nox')
print(np.mean(np.array(nox_ppmv)))

print('bsoa')
print(np.mean(np.array(biog_conc)))

nc_w_fid.close()

