#!/bin/csh

##SBATCH --time 12:00:00
#SBATCH --partition i01203share
#SBATCH --partition tshp384

##SBATCH --nodes 1
##SBATCH --ntasks-per-node 2

#SBATCH --nodes 8
#SBATCH --ntasks-per-node 28


##SBATCH --tasks=224
#SBATCH --job-name batch
#SBATCH --error %j.err
#SBATCH --output %j.out
#SBATCH --exclusive

limit stacksize unlimited

module load mathlib/netcdf/4.7.1/impi_pnetcdf

#mpirun -np 2 ./real.exe
#./mozbc < cambc_8bin_d01.inp > log.mozbc.d01

mpirun -np 224 ./wrf.exe
