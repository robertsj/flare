# This file shows how users can create a database file from nodal data.
import numpy as np
from scipy.spatial import Delaunay
from scipy.interpolate import griddata
import matplotlib.pyplot as plt

# Sample grid points (B HT_F HT_C HBC IT_F IT_C IBC)
X = np.loadtxt('sample_points.txt')

#X = X + np.random.rand(50,7) * 0.0001

# Sample data (king, M2, nubar, kappa)
Y = np.loadtxt('sample_data.txt')

T = Delaunay(X)
tri = T.vertices

out = "Sample Database for 4 w/o fuel, np BP\n"
out += str(len(X)) + "\n"    # number of points
out += str(len(tri)) + "\n"  # number of simplices
out += "1 \n"                # number of assemblies

# Print out points
for p in X :
    out += len(p) * "%6.1f " % tuple(p) + "\n"

# Print out simplices
for s in tri :
    out += len(s) * "%4i " % tuple(s) + "\n"

# Print out data
for d in Y :
    out += "%8.4f %8.4f %6.4f %10.3e " % tuple(d) + "\n"
 


f = open('sample_database.txt', 'w')
f.write(out)
f.close()

YY = Y.transpose()[0]
XX = [[ 0.0 , 900.0 , 580.0 , 900.0,  900.0,  580.0,  1000.]]


print T.find_simplex(XX)

#print griddata(X, YY, [[0.0, 900.0 , 580.0 , 900.0, 900.,  580.0,  899.9]])

# n = 100
# BU = np.linspace(200, 1500, n)
# Z = 0.0*BU
# for i in range(0, n) :
#   Z[i] = griddata(X, YY, [[0.0, 900.0 , 580.0 , 900.0, 900.,  580.0,   BU[i]]])
# 
# print Z
# plt.plot(BU, Z, '-o')
# plt.show()