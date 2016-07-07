#-----------------------------------------------------------------------------
# System Shells/Commands 
#-----------------------------------------------------------------------------
SHELL = /bin/sh
RM = rm -rf
CP = cp
#-----------------------------------------------------------------------------
# Compiler, options, libraries, includes
#-----------------------------------------------------------------------------
#... Linux PG F90 compiler
FC = gfortran 
#FC = pgf90 

# Compiler Flags 
DEBUG_FLAGS=-g
COMMON_FLAGS=$(DEBUG_FLAGS) 			# IBM
#COMMON_FLAGS=$(DEBUG_FLAGS) +O2 -DA2.0 	# HPUX
#COMMON_FLAGS=$(DEBUG_FLAGS)  -DA2.0 -g77libs	# Linux

# Common Flags 

FC_FLAGS=$(COMMON_FLAGS) -c 
FL_FLAGS=$(COMMON_FLAGS) -o

# Libraries
FLIBS = -L/Users/longmore/local/lib -lnetcdff

# Includes
FINCS = -I/Users/longmore/local/include

#-----------------------------------------------------------------------------
# Define dependecies 
#-----------------------------------------------------------------------------

LIB = libfnail.a

#-------------
# Make program
#------------

#nail.o nail.mod: netcdfai.o netcdfai_meta.o netcdfai_var.o netcdfai_att.o netcdfai_common.o
#	ld -r $< -o $@ 

netcdfai.o netcdfai.mod: netcdfai.f90 netcdfai_meta.o netcdfai_var.o netcdfai_att.o netcdfai_common.o
	$(FC) $(FC_FLAGS) $< $(FLIBS) $(FINCS) 

#netcdfai.o : netcdfai.f90 netcdfai_meta.o netcdfai_var.o netcdfai_att.o netcdfai_common.o
#	$(FC) $(FC_FLAGS) $< $(FLIBS) $(FINCS) 

netcdfai_meta.o : netcdfai_meta.f90 netcdfai_var.o netcdfai_att.o netcdfai_common.o
	$(FC) $(FC_FLAGS) $< $(FLIBS) $(FINCS) 

netcdfai_var.o : netcdfai_var.f90 netcdfai_att.o netcdfai_common.o
	$(FC) $(FC_FLAGS) $< $(FLIBS) $(FINCS) 

netcdfai_att.o : netcdfai_att.f90 netcdfai_common.o
	$(FC) $(FC_FLAGS) $< $(FLIBS) $(FINCS) 

netcdfai_common.o : netcdfai_common.f90
	$(FC) $(FC_FLAGS) $< $(FLIBS) $(FINCS)

# Clean up
clean:
	$(RM) *.o *.mod $(EXE) 

#--------
# Copy exe 
#--------
#
# all executables (defalt)
#
install: $(EXE)
	   $(CP) $(EXE) $(LOC)
