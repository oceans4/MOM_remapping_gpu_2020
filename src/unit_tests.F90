program unit_tests
  use MOM_remapping, only : remapping_unit_tests

  logical :: state

  state = remapping_unit_tests(.true.)
  !if ( remapping_unit_tests(.true.) ) stop 'Error detected!'

end program
