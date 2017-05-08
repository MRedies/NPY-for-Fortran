# NPY for Fortran
This Fortran module allows to save numerical Fortran arrays in Numpy's .npy or .npz format. Currently supported are:
```fortran
1. integer(1), integer(2), integer(4), integer(8)
2. real(4), real(8)
3. complex(4), complex(8)
```
### *.npy files
Saving an array into a .npy-file is simply done by calling:
```fortran
call save_npy("filename.npy", array)
```


### *.npz files
In order to save .npz-files the commandline too "zip" has to be installed. By calling 
```fortran
call add_npz("example.npz", "temperature", data_array)
```
one creates an .npz-file containing data_array, with the name "temperature". If example.npz already exists the field "temperature" is added to it. If the field temperature already exsits in example.npz it will be overwritten.


Reading .npy and .npz files isn't currently supported. (Maybe someone can give me ideas on dynamic typing in Fortran...)
