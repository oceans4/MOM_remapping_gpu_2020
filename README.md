MOM remapping kernel test
=========================

A small sample code for testing MOM_remapping, the subroutines devoted to
remapping fields onto the output vertical levels.

Testing to port to GPUs

Build instructions:
```
$ autoreconf -if
$ mkdir -p build
$ cd build
$ FC=(...) FCFLAGS=(...) ../configure
$ make
```

Tests:
```
./test_units
./test_remap_4lvl
./test_remap_70lvl
./test_remap_70lvl 200
./test_remap_70lvl 30 4
```

Arguments for `./test_remap_70lvl TW HL` are tile width (tiles are square) and halo. Defaults are TW=300 and HL=4.


NOAA Hackathon Notes
=====================

1. From  the login node, start in interactive session on the compute node:

srun --ntasks=5 --nodes=1 --cpus-per-task=2 --partition=batch --time=06:00:00 --gres=gpu:1 --pty /bin/bash

2. Load compiler

module load compilers/nvhpc-20.9-mpi

2. Build and run in your interactive session

FC=pgf90 FCFLAGS="-acc -Minfo=accel -ta=tesla:cc70,managed,deepcopy" ../configure
make