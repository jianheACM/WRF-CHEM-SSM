# =====================================================================
#                 THIS IS THE MAKEFILE FOR REGISTRY.EXE
# =====================================================================

.SUFFIXES: .c .o

# COMPILER AND FLAGS:
# =====================================================================
COMP = cc

ffCPP   = -DNMM_CORE=0
ffDEBUG = -g
ffLD    = 

# TARGET OBJECTS:
# =====================================================================
OBJs = registry.o my_strtok.o reg_parse.o data.o type.o misc.o \
       gen_defs.o gen_allocs.o gen_mod_state_descr.o gen_scalar_indices.o \
       gen_args.o gen_config.o sym.o symtab_gen.o gen_irr_diag.o \
       gen_model_data_ord.o gen_interp.o gen_comms.o gen_scalar_derefs.o \
       set_dim_strs.o gen_wrf_io.o gen_streams.o

# MAKE DEFAULT ROUTINES:
# =====================================================================
default	: sub1
	@make -s --no-print-directory target

# ROUTINE:
# =====================================================================
sub1 :
	@printf "\n\e[1;33m%s\e[0m\n" "COMPILING in $(shell pwd) :"
	@printf "\e[1;33m=%.0s\e[0m" {1..80}
	@printf "\n\n"

	@cp -u ../source/* .

# RECIPE FOR TARGET:
# =====================================================================
target : $(OBJs)
	@printf "\n\e[1;33m%s\e[0m %s\n" "linking... to form" "registry.exe"

	@$(COMP) -o ../registry.exe $(ffDEBUG) $(OBJs)

# RECIPE FOR OBJECTS:
# =====================================================================
# GENERAL:
.c.o :
	@printf "\e[1;33m%s\e[0m %s\n" making... "$@"

	@$(COMP) $(ffCPP) -c $(ffDEBUG) -o $@ $<

# INDIVIDUAL:
gen_comms.c : gen_comms.stub
	@/bin/cp gen_comms.stub gen_comms.c

# UTILITIES:
# =====================================================================
clean:
	@/bin/rm -f *.o *.c *.h gen_comms.stub gen_comms_warning

# DEPENDENCIES:
# =====================================================================
data.o: registry.h protos.h data.h
gen_allocs.o: protos.h registry.h data.h
gen_args.o: protos.h registry.h data.h
gen_scalar_derefs.o: protos.h registry.h data.h
gen_config.o: protos.h registry.h data.h
gen_defs.o: protos.h registry.h data.h
gen_mod_state_descr.o: protos.h registry.h data.h
gen_model_data_ord.o: protos.h registry.h data.h
gen_scalar_indices.o: protos.h registry.h data.h
gen_wrf_io.o: protos.h registry.h data.h
misc.o: protos.h registry.h data.h
my_strtok.o: registry.h protos.h data.h
reg_parse.o: registry.h protos.h data.h
registry.o: protos.h registry.h data.h
sym.o: sym.h
type.o: registry.h protos.h data.h
gen_interp.o: registry.h protos.h data.h
gen_streams.o: registry.h protos.h data.h
