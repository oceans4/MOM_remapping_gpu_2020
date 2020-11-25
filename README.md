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
