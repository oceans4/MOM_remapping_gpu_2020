!> Handy functions for manipulating strings
module MOM_string_functions

! This file is part of MOM6. See LICENSE.md for the license.

use iso_fortran_env, only : stdout=>output_unit, stderr=>error_unit

implicit none ; private

public uppercase

contains

!> Return a string in which all uppercase letters have been replaced by
!! their lowercase counterparts.
function uppercase(input_string)
  character(len=*),     intent(in) :: input_string !< The string to modify
  character(len=len(input_string)) :: uppercase !< The modified output string
!   This function returns a string in which all lowercase letters have been
! replaced by their uppercase counterparts.  It is loosely based on the
! uppercase function in mpp_util.F90.
  integer, parameter :: co=iachar('A')-iachar('a') ! case offset
  integer :: k

  uppercase = input_string
  do k=1, len_trim(input_string)
    if (uppercase(k:k) >= 'a' .and. uppercase(k:k) <= 'z') &
        uppercase(k:k) = achar(ichar(uppercase(k:k))+co)
  enddo
end function uppercase

end module MOM_string_functions
