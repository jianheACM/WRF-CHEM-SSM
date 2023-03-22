# ==========================================================================
#           THIS IS THE MAKEFILE FOR THE SIMPLESOM-MOSAIC MODULE
# ==========================================================================

# OBJECT FILES:
OBJs = main.ss_mosaic.o param.ss_mosaic.o sub.step_soacond.o \
       sub.map_inout.o sub.step_olig.o sub.step_hetchem.o sub.step_gaschem.o

# DEFAULT TARGET:
# ==========================================================================
default: mssg $(OBJs)
	@printf "\n\e[1;33m%s\e[0m\n" archiving...
	@printf "%-20s %-20s %-20s %-20s\n" $(OBJs)
	@printf "\n"

	@cd ./objs/; ar -ru ../../../main/libwrflib.a $(OBJs)
	@cp ./objs/*.mod ../../chem/

mssg :
	@printf "\n\e[1;33m%s\e[0m\n\n" "in $(shell pwd) :"

# RECIPE PATTERN:
# ==========================================================================
%.o : %.f90
	@printf "\e[1;33m%s\e[0m %s\n" making... "$@"
	@ifort -c -I ../../frame/ -I ./objs/ -o ./objs/$@ -module ./objs/ $<

# DEPENDENCIES:
# ==========================================================================
main.ss_mosaic.o: param.ss_mosaic.o sub.map_inout.o sub.step_soacond.o \
	          sub.step_olig.o sub.step_hetchem.o sub.step_gaschem.o

param.ss_mosaic.o:

sub.step_soacond.o: param.ss_mosaic.o

sub.step_olig.o: param.ss_mosaic.o

sub.step_hetchem.o: param.ss_mosaic.o

sub.step_gaschem.o: param.ss_mosaic.o

sub.map_inout.o: param.ss_mosaic.o

# UTILITIES:
# ==========================================================================
clean:
	@rm ./objs/*
