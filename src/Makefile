program = flare

#===============================================================================
# Object Files
#===============================================================================

objects =               \
flare.o                 \
geometry.o              \
coefficients.o          \
group_data.o            \
state.o                 \
solver.o       

#===============================================================================
# User Options
#===============================================================================

COMPILER = ifort
DEBUG    = no
OPTIMIZE = yes
PROFILE  = no
PYTHON   = yes

#===============================================================================
# GNU Fortran compiler options
#===============================================================================
F90 = gfortran
ifeq ($(COMPILER),gnu)
  F90 = gfortran
  F90FLAGS := -fbacktrace
  LDFLAGS = 
  LIB = 
  #INCLUDE = -I ./rng/

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
endif

ifeq ($(COMPILER),ifort)
  F90 = ifort
  F90FLAGS := 
  LDFLAGS = 
  LIB =
  #INCLUDE = -I ./rng/

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
	f2py --opt="-fast" -c -m pyflare group_data.f90 geometry.f90 state.f90 coefficients.f90 solver.f90

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

coefficients.o: group_data.o
coefficients.o: geometry.o

flare.o: group_data.o
flare.o: geometry.o
flare.o: coefficients.o
flare.o: state.o
flare.o: solver.o

solver.o: coefficients.o
solver.o: geometry.o
solver.o: state.o

state.o: geometry.o


