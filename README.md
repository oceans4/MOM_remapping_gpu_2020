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
