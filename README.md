### I don't use Fortran anymore, so I don't actively maintain this repo. If someone wants to take on the job please reach out.

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
### Pull requests are welcome!
I see many of you have created forks. Let's not scatter this library over 500 projects, let's work on one repo together.
