AC_INIT([MOM_remapping],[0.0])

AC_CONFIG_SRCDIR([src/MOM_remapping.F90])
AC_CONFIG_MACRO_DIR([m4])

AC_LANG([Fortran])
AC_FC_SRCEXT([f90])

# Force 8-byte reals
AX_FC_REAL8
AS_IF(
  [test "$enable_real8" != no],
  [FCFLAGS="$FCFLAGS $REAL8_FCFLAGS"])

AC_CONFIG_FILES([Makefile])
AC_OUTPUT
