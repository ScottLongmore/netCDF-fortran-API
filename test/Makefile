#==============================================================================
#
# Makefile for:  The CIRA JHT project to estimate 
# 		 tropical cyclone intensity and wind structure using data from 
# 		 the Advanced Microwave Sounding Unit on the GOES satellite
#		 platform.
#
# This version is for the IBM
#
# Notes: make  - compilies the two executables (pickavn.exe and oparet.exe)
#   	 make clean  - removes the object files and executables 
#   	 make pickavn.exe  - compilies the executable pickavn.exe
#   	 make oparet.exe   - compilies the executable oparet.exe
#   	 make install	- places the *.exe files in the ../bin directory
#
# Executables:  pickavn.exe 
#		oparet.exe
#
# Written by J. Knaff and M. DeMaria, May 2003
# Modified by: A. Krautkramer 5/19/03
#		Added comments and standardized makefile
# Modified by: ARK for IBM 2/10/2005
#
#==============================================================================

#-----------------------------------------------------------------------------
#                          -- Define default macros --
#-----------------------------------------------------------------------------

#--------------------
# Define default shell
#--------------------

SHELL = /bin/sh

RM	= rm -rf

CP	= cp

LOC	= ../../exec

#-----------------------------------------------------------------------------
#                       -- Define file lists --
#-----------------------------------------------------------------------------

#---------
# Libraries
#---------

FLIBS	= -L/Users/longmore/local/lib -lnetcdff

#--------
# Includes
#--------

FINCS = -I/Users/longmore/local/include

#------------
# Source/Object/Executable files
#------------

#PSRC	= 
#POBS	= $(PSRC:.f90=.o)

NCSRC	= netcdfai_common.f90 netcdfai_att.f90 netcdfai_var.f90 netcdfai_meta.f90 netcdfai.f90 
NCOBS	= $(NCSRC:.f90=.o)

TSRC    = fruit_util.f90 fruit.f90 \
          netcdfai_test_data.f90 netcdfai_test.f90 \
          netcdfai_test_driv.f90
TOBS    = $(TSRC:.f90=.o)

AOBS    = $(NCOBS) $(TOBS) 

EXE	= netcdfai_test_driv

#-----------------------------------------------------------------------------
#                          -- Define dependecies --
#-----------------------------------------------------------------------------

#------------------
# Compiler
#------------------

#... HP F90 compiler
#FC	= f95	

#... Linux PG F90 compiler
FC	= gfortran 

#... IBM AIX F90 compiler
#FC      = xlf_r
#FC	= ncepxlf

#-----------------------------
# Compiler Flags 
#-----------------------------

DEBUG_FLAGS=-g
#COMMON_FLAGS=$(DEBUG_FLAGS) +O2 -DA2.0 	# HPUX
#COMMON_FLAGS=$(DEBUG_FLAGS)  -DA2.0 -g77libs	# Linux
COMMON_FLAGS=$(DEBUG_FLAGS) 			# IBM

#------------ 
# Common Flags 
#------------

FC_FLAGS=$(COMMON_FLAGS) -c 
FL_FLAGS=$(COMMON_FLAGS) -o

#-------------
# Make program
#------------

# Master Test
netcdfai_test_driv: $(AOBS)
	$(FC) $(FL_FLAGS) $@ $(AOBS) $(FLIBS) $(FINCS)
%.o:%.f90
	$(FC) $(DEBUF_FLAGS) $(FC_FLAGS) $< $(FLIBS) $(FINCS)

#--------
# Clean up
#--------
	
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
