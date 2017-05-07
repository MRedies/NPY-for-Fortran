import numpy as np


a_soll =  np.zeros((1000,20), dtype=np.complex64)
for ind in range(a_soll.shape[0]):
    for jnd in range(a_soll.shape[1]):
        i =  ind + 1
        j =  jnd +  1
        a_soll[ind,jnd] = - i * 0.3 + 1j*(  j*j + 0.4)

b_soll =  np.zeros(1200, dtype=np.complex64)
for ind in range(b_soll.shape[0]):
    i =  ind +  1
    b_soll[ind] = - i * 0.3 + 1j*( i + 0.4)


a =  np.load("mtx.npy")
b =  np.load("vec.npy")

print("A: ")
print(np.max(np.abs(a - a_soll)/a_soll  ))
print("B: ")
print(np.max(np.abs(b -   b_soll) / b_soll ))
