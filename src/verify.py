from poropy.nucleardata import *

from pyflare2 import *

import numpy as np

import matplotlib.pyplot as plt

b = np.linspace(0.0, 20.0, 100)

kinf = 0.0*b
kinf2 = 0.0*b
m =0.*b
m2 = 0.*b
f=0*b
f2=0*b

waba=0
gad=0
ifba=0
bp=3
if bp == 1 : ifba =1
if bp == 2 : waba =1
if bp == 3 : gad=1

for i in range(len(b)) :
  kinf[i] = nuclear_data.set_flare_data(b[i], 4.1, bp)[0]
  kinf2[i] = get_2g_parms(b[i], 4.1, ifba, waba, gad).K_INF_XE
  m[i] =  nuclear_data.set_flare_data(b[i], 4.1, bp)[1]
  m2[i] = get_2g_parms(b[i], 4.1, ifba, waba, gad).M2_XE
  f[i] = nuclear_data.set_two_group_data(b[i], 4.1, bp)[3]
  f2[i] = get_2g_parms(b[i], 4.1, ifba, waba, gad).ABS2
#print m, m2

plt.semilogy(b, np.abs(kinf-kinf2)/kinf, 'k', b, np.abs(m-m2)/m, 'r', b, np.abs(f-f2)/f, 'g')
plt.grid(True)
plt.show()
