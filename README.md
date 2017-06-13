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
In order to save .npz-files the commandline tool 'zip' has to be installed. By calling 
```fortran
call add_npz("example.npz", "temperature", data_array)
```
one creates an .npz-file containing data_array, with the name "temperature". If example.npz already exists the field "temperature" is added to it. If the field temperature already exsits in example.npz it will be overwritten.


Reading .npy and .npz files isn't currently supported. (Maybe someone can give me ideas on dynamic typing in Fortran...)

### Compiling using ifort

The code uses the somewhat out-dated 
```fortran
succ = system(...)
```
command for which ifort needs the the IFPORT library:

```fortran
#ifdef INTEL_COMPILER_USED
    USE IFPORT
#endif
```

Therefore, Intel users need to add the flag:
```
-DINTEL_COMPILER_USED
```

### Compiling using gfortran
Since the function 'system' is not standard Fortran one cannot use flags such as
```
-std=f2008
```
but instead can (not must) use the equivalent
```
-std=gnu
```
### Control Flags

In the source file the parameter

```fortran
    character(len=*), parameter         :: zip_flag  = "-q0"  
```

can be used to control the compression. "-q0" tells the zip tool to have no output and no compression. The parameter 

```fortran
    character(len=*), parameter         :: endianess = 'LITTLE_ENDIAN'
```

sets the [endianess](https://en.wikipedia.org/wiki/Endianness) of the output. Most personal computers use little endian.



### Endianess

Using the flag

```fortran
    logical, parameter                  :: use_big_endian = .False.
```

the output [endianess](https://en.wikipedia.org/wiki/Least_significant_bit) can be set. This works independent of the compiler. Little endian (.False.) is a reasonable default for most personal computers.