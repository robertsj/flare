# biblis benchmark in python

from pyflare import *
import numpy as np
import time

# setting reflector to zeros; keeping the indexing.
d1 = np.array([1.4360000,1.4366000,0.0,1.4389000,1.4381000,1.4385000,1.4389000,1.4393000])
d2 = np.array([0.3635000,0.3636000,0.0,0.3638000,0.3665000,0.3665000,0.3679000,0.3680000])
r1 = np.array([0.0272582,0.0272995,0.0,0.0274640,0.0272930,0.0273240,0.0272900,0.0273210])
a2 = np.array([0.0750580,0.0784360,0.0,0.0914080,0.0848280,0.0873140,0.0880240,0.0905100])
f1 = np.array([0.0058708,0.0061908,0.0,0.0074527,0.0061908,0.0064285,0.0061908,0.0064285])
f2 = np.array([0.0960670,0.1035800,0.0,0.1323600,0.1035800,0.1091100,0.1035800,0.1091100])
s12= np.array([0.0177540,0.0176210,0.0,0.0171010,0.0172900,0.0171920,0.0171250,0.0170270]) 

# geometry
geometry.stencil = np.array([[1,8,2,6,1,7,1,4,0],
                            [0,1,8,2,8,1,1,4,0],
                            [0,8,1,8,2,7,1,4,0],
                            [0,2,8,2,8,1,8,4,0],
                            [0,8,2,8,2,5,4,0,0],
                            [0,1,7,1,5,4,4,0,0],
                            [0,1,1,8,4,4,0,0,0],
                            [0,4,4,4,0,0,0,0,0],
                            [0,0,0,0,0,0,0,0,0]], dtype='i')

geometry.number_bundles = 49
geometry.stencil_dimension = 9
geometry.initialize_geometry()
geometry.delta = 21.0
geometry.build_geometry()

# Set the group data.
group_data.d1  = np.zeros(geometry.number_bundles)
group_data.d2  = np.zeros(geometry.number_bundles)
group_data.r1  = np.zeros(geometry.number_bundles)
group_data.a2  = np.zeros(geometry.number_bundles)
group_data.f1  = np.zeros(geometry.number_bundles)
group_data.f2  = np.zeros(geometry.number_bundles)
group_data.s12 = np.zeros(geometry.number_bundles)
for i in range(0, geometry.number_bundles) :
  group_data.d1[i]  = d1[geometry.pattern[i]-1]
  group_data.d2[i]  = d2[geometry.pattern[i]-1]
  group_data.r1[i]  = r1[geometry.pattern[i]-1]
  group_data.a2[i]  = a2[geometry.pattern[i]-1]
  group_data.f1[i]  = f1[geometry.pattern[i]-1]
  group_data.f2[i]  = f2[geometry.pattern[i]-1]
  group_data.s12[i] = s12[geometry.pattern[i]-1]


# Initialize
coefficients.initialize_coefficients()
state.initialize_state()
solver.initialize_solver()

number = 10
start = time.time()
for i in range(0, number) :
  solver.solve()
elapsed = (time.time() - start)
print elapsed/number, "seconds per run"



