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
```
srun --ntasks=5 --nodes=1 --cpus-per-task=2 --partition=batch --time=06:00:00 --gres=gpu:1 --pty /bin/bash
```

2. Load compiler
```
module load compilers/nvhpc-20.9-mpi
```

3. Build and run in your interactive session

CPU only
```
FC=pgf90 FCFLAGS="-r8 -O4" ../configure
make
```

GPU-enabled
```
FC=pgf90 FCFLAGS="-acc -r8 -O4 -Minfo=accel -ta=tesla:cc70,managed,deepcopy" LDFLAGS="-acc" ../configure
make
PGI_ACC_TIME=1 nvprof ./test_remap_70lvl
```


Some base line timings
======================

CPU
```
$ $ ./cpu-test_remap_70lvl
 Tile width =          300  halo =            4
 h0 chksum =    219290934
 u0 chksum =    219309289
 h1 chksum =    110132732
 u1 chksum =    109444831
PCM time taken:    0.458 secs
 u1 chksum =    109438112
PLM time taken:    0.579 secs
 u1 chksum =    109429446
PPM_H4 time taken:    0.685 secs
 u1 chksum =    109421885
PPM_IH4 time taken:    0.766 secs
 u1 chksum =    109430597
PQM_IH4IH3 time taken:    1.082 secs
 u1 chksum =    109424799
PQM_IH6IH5 time taken:    3.636 secs
```

GPU
```
./test_remap_70lvl
 h0 chksum =    219290934
 u0 chksum =    219309289
 h1 chksum =    110132732
 u1 chksum =    109446802
PCM time taken:    0.654 secs
 u1 chksum =    109471917
PLM time taken:    0.587 secs
 u1 chksum =    109249420
PPM_H4 time taken:    0.569 secs
 u1 chksum =    109260131
PPM_IH4 time taken:    0.827 secs
 u1 chksum =    109380914
PQM_IH4IH3 time taken:    1.258 secs
 u1 chksum =    109367724
PQM_IH6IH5 time taken:    1.558 secs
```
