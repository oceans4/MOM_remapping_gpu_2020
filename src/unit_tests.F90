program unit_tests
  use MOM_remapping, only : remapping_unit_tests

  logical :: state

  state = remapping_unit_tests(.true.)

end program
