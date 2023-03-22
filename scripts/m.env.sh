#!/bin/bash

module load libraries/flex-2.6.4
module load use.own

export DIR=/home/yicongh/privatemodules
# export NETCDF=/home/yicongh/privatemodules/netcdf/

export NETCDF=/home/yicongh/port/netcdf/

export CC=/usr/local/oneapi-2021.1.1/compiler/2021.1.1/linux/bin/intel64/icc
export CXX=/usr/local/oneapi-2021.1.1/compiler/2021.1.1/linux/bin/intel64/icpc
export FC=/usr/local/oneapi-2021.1.1/compiler/2021.1.1/linux/bin/intel64/ifort
#export PATH=$DIR/netcdf/bin:$PATH

export PATH=/home/yicongh/port/netcdf/include:$PATH
export PATH=/home/yicongh/port/netcdf/bin:$PATH
export PATH=$DIR/mpich/bin:$PATH

export WRF_EM_CORE=1
export WRFIO_NCD_LARGE_FILE_SUPPORT=1
export NETCDF_classic=1

# for wrf-chem
export WRF_CHEM=1
export WRF_KPP=1
export FLEX_LIB_DIR=/usr/local/flex-2.6.4/lib/
export J="-j 2"

ulimit -s unlimited              # stacksize

export OMP_STACKSIZE=128m

#For WPS
export JASPERLIB=$DIR/grib2/lib
export JASPERINC=$DIR/grib2/include
