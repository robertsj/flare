from flare import *
import numpy as np
import time

# fresh, once, and twice burned; biblis reflector
d1  = np.array([1.4493e+00, 1.4479e+00, 1.4494e+00, 1.3200e+00])
d2  = np.array([3.8070e-01, 3.7080e-01, 3.6760e-01, 2.7720e-01])
r1  = np.array([2.5000e-02, 2.5800e-02, 2.6200e-02, 0.0257622 ])
a2  = np.array([1.0420e-01, 1.2000e-01, 1.1910e-01, 7.1596e-02])
f1  = np.array([7.9000e-03, 6.9000e-03, 6.0000e-03, 0.0000e+00])
f2  = np.array([1.6920e-01, 1.7450e-01, 1.6250e-01, 0.0000e+00])
s12 = np.array([1.5100e-02, 1.4800e-02, 1.4700e-02, 2.3106e-02])


# geometry
stencil = np.array( [[1, 1, 1, 0], \
                     [0, 1, 1, 0],
                     [0, 1, 1, 0],
                     [0, 0, 0, 0]], dtype='i')
pattern = np.array([ 1, 1, 1,  \
                        1, 1,  \
                        1, 1   ], dtype='i')
geometry.initialize_geometry(7, 4)
geometry.set_stencil(stencil)
geometry.delta = 21.0
geometry.build_geometry()
# can set the pattern after the fact
geometry.set_pattern(pattern)
n = np.size(pattern)

# Set the group data.
group_data.d1  = np.zeros(n)
group_data.d2  = np.zeros(n)
group_data.r1  = np.zeros(n)
group_data.a2  = np.zeros(n)
group_data.f1  = np.zeros(n)
group_data.f2  = np.zeros(n)
group_data.s12 = np.zeros(n)
for i in range(0, n) :
  group_data.d1[i]  = d1[pattern[i]-1]
  group_data.d2[i]  = d2[pattern[i]-1]
  group_data.r1[i]  = r1[pattern[i]-1]
  group_data.a2[i]  = a2[pattern[i]-1]
  group_data.f1[i]  = f1[pattern[i]-1]
  group_data.f2[i]  = f2[pattern[i]-1]
  group_data.s12[i] = s12[pattern[i]-1]

# Build coefficients
coefficients.initialize_coefficients()
#print coefficients.wpp

state.initialize_state()

# Initialize solver
solver.initialize_solver()

number = 100
start = time.time()
for i in range(0, number) :
  solver.solve()
elapsed = (time.time() - start)
print elapsed/number, "seconds per run"

#for i in range(0, 8) :
#  print "%13f %13f %13f" % (J_out[i], J_out[8+i], J_out[16+i]) 





