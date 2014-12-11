# This file shows how users can create a database file from nodal data.
import numpy as np
from scipy.spatial import Delaunay
from scipy.interpolate import griddata
import matplotlib.pyplot as plt
import cPickle as pickle

# Sample grid points (B HT_F HT_C HBC IT_F IT_C IBC)
X = np.loadtxt('sample_points.txt')

# X = X + np.random.rand(len(X),len(X[0])) * 1e-8

# Sample data (king, M2, nubar, kappa)
Y = np.loadtxt('sample_data.txt')

if 1 == 0 :
    
    T = Delaunay(X, qhull_options="QJ Qbb Qc Qz Qx")
    pickle.dump(T, open('T.p', 'wb'))

else : 
    T = pickle.load(open('T.p', 'rb'))

tri = T.vertices

out = "Sample Database for 4 w/o fuel, np BP\n"
out += str(len(X)) + "\n"    # number of points
out += str(len(tri)) + "\n"  # number of simplices
out += "1 \n"                # number of assemblies

# Print out points
out_p = ""
for p in X :
    out_p += len(p) * "%6.1f " % tuple(p) + "\n"

# Print out simplices
out_s = ""
for s in tri :
    out_s += len(s) * "%4i " % tuple(s+1) + "\n"

# Print out data
out_d = ""
for d in Y :
    out_d += "%8.4f %8.4f %6.4f %10.3e " % tuple(d) + "\n"

out += out_p + out_s + out_d

f = open('sample_database.txt', 'w')
f.write(out)
f.close()

f = open('r.txt', 'w')
f.write(out_p)
f.close()
f = open('tri.txt', 'w')
f.write(out_s)
f.close()
f = open('f.txt', 'w')
f.write(out_d)
f.close()

YY = Y.transpose()[0]
XX = [[0.0, 900.0 , 580.0 , 900.0, 900.,  580.0,  899.],
                       [0.0, 900.0 , 580.0 , 900.0, 900.,  580.0,  900.],
                       [0.0, 900.0 , 580.0 , 900.0, 900.,  580.0,  901.]]


for i in range(0, 3) :
    s = T.find_simplex(XX[i])
    print s 
    for S in tri[s] :
        print X[S]
 

# print griddata(X, YY, [[0.0, 900.0 , 580.0 , 900.0, 900.,  580.0,  899.],
#                        [0.0, 900.0 , 580.0 , 900.0, 900.,  580.0,  900.],
#                        [0.0, 900.0 , 580.0 , 900.0, 900.,  580.0,  901.]])

# n = 100
# BU = np.linspace(200, 1500, n)
# Z = 0.0*BU
# for i in range(0, n) :
#   Z[i] = griddata(X, YY, [[0.0, 900.0 , 580.0 , 900.0, 900.,  580.0,   BU[i]]])
# 
# print Z
# plt.plot(BU, Z, '-o')
# plt.show()