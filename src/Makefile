program = flare

#===============================================================================
# Object Files
#===============================================================================

objects =               \
flare.o                 \
geometry.o              \
coefficients.o          \
material_data.o         \
nuclear_data.o          \
nuclear_data_NOBP.o     \
nuclear_data_IFBA.o     \
nuclear_data_WABA.o     \
nuclear_data_GAD.o      \
state.o                 \
solver.o                \
utilities.o

#===============================================================================
# User Options
#===============================================================================

COMPILER = gnu
DEBUG    = no
OPTIMIZE = yes
PROFILE  = no
PYTHON   = yes
PAPI     = no
OPENMP   = no

#===============================================================================
# GNU Fortran compiler options
#===============================================================================
F90 = gfortran
ifeq ($(COMPILER),gnu)

  F90 = gfortran
  F90FLAGS = -cpp -fbacktrace -ffree-line-length-none -fPIC 
  LDFLAGS = -L/home/robertsj/opt/papi/lib/
  LIB = 
 
  # Debugging
  ifeq ($(DEBUG),yes)
    F90FLAGS += -g -Wall -pedantic -std=f2008 -fbounds-check \
                -ffpe-trap=invalid,zero,overflow,underflow
    LDFLAGS  += -g 
  endif

  # Profiling
  ifeq ($(PROFILE),yes)
    F90FLAGS += -pg
    LDFLAGS  += -pg
  endif

  # Optimization
  ifeq ($(OPTIMIZE),yes)
    F90FLAGS += -O3
  endif
  
  # PAPI 
  ifeq ($(PAPI),yes)
    F90FLAGS += -I$(PAPI_DIR)/include -DPAPI
    LDFLAGS  += -L$(PAPI_DIR)/lib/
    LIB      += -lpapi
  endif
  
  # OPENMP 
  ifeq ($(PAPI),yes)
    F90FLAGS += -fopenmp  -DOPENMP
  endif 
  
endif

#===============================================================================
# Intel Fortran compiler options
#===============================================================================
ifeq ($(COMPILER),ifort)

  F90 = ifort
  F90FLAGS := 
  LDFLAGS = 
  LIB =

  # Debugging
  ifeq ($(DEBUG),yes)
    F90FLAGS += -g -Wall -pedantic -std=f2008 -fbounds-check \
                -ffpe-trap=invalid,zero,overflow,underflow
    LDFLAGS  += -g 
  endif

  # Profiling
  ifeq ($(PROFILE),yes)
    F90FLAGS += -pg
    LDFLAGS  += -pg
  endif

  # Optimization
  ifeq ($(OPTIMIZE),yes)
    F90FLAGS += -fast
  endif
  
endif

#===============================================================================
# Targets
#===============================================================================

$(program): $(objects)
	$(F90) $(objects) -o $@ $(LDFLAGS) $(LIB) $(INCLUDE)


clean:
	@rm -f *.o *.mod $(program)
neat:
	@rm -f *.o *.mod

pyflare:
	f2py2.7 --opt="-O3" --f90flags="-ffree-line-length-none" -c -m pyflare2 \
	     nuclear_data_NOBP.f90 nuclear_data_IFBA.f90 nuclear_data_WABA.f90 \
	     nuclear_data_GAD.f90 nuclear_data.f90 material_data.f90 \
	     geometry.f90 state.f90 utilities.f90 coefficients.f90 solver.f90

#===============================================================================
# Rules
#===============================================================================

.SUFFIXES: .F90 .o
.PHONY: clean neat

%.o: %.f90
	$(F90) $(F90FLAGS) $(INCLUDE) -c $<

#===============================================================================
# Dependencies
#===============================================================================

coefficients.o: material_data.o
coefficients.o: geometry.o

flare.o: material_data.o
flare.o: geometry.o
flare.o: coefficients.o
flare.o: state.o
flare.o: solver.o
flare.o: utilities.o

solver.o: coefficients.o
solver.o: geometry.o
solver.o: state.o
solver.o: material_data.o

state.o: geometry.o

material_data.o: nuclear_data.o

nuclear_data.o: nuclear_data_NOBP.o
nuclear_data.o: nuclear_data_IFBA.o
nuclear_data.o: nuclear_data_WABA.o
nuclear_data.o: nuclear_data_GAD.o

utilities.o:  geometry.o

