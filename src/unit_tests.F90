program unit_tests
  use MOM_remapping, only : remapping_unit_tests

  if ( remapping_unit_tests(.true.) ) stop 'Error detected!'

end program
