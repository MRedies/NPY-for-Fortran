import numpy as np


a_soll =  np.zeros((100,230), dtype=np.complex128)
for ind in range(a_soll.shape[0]):
    for jnd in range(a_soll.shape[1]):
        i =  ind + 1
        j =  jnd +  1
        a_soll[ind,jnd] =  (0.7 * j - 0.34 *  i )+  1j*( - 0.5*j + i*i*0.001)

b_soll =  np.zeros(12, dtype=np.complex128)
for ind in range(b_soll.shape[0]):
    i =  ind +  1
    b_soll[ind] = - 0.34 *  i +  1j*( i*i*0.001)

a =  np.load("mtx.npy")
b =  np.load("vec.npy")

print(np.max(np.abs(a-a_soll)))
print(np.max(np.abs(b-b_soll)))
print(type(a[0,0]))
print(type(b[0]))

