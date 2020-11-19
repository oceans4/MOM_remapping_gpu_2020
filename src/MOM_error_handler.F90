module MOM_error_handler

public MOM_error
character(len=5),public  :: FATAL = "ERROR"
contains

subroutine MOM_error(s1,s2)
  character(len=*), intent(in) :: s1,s2
  print*, trim(s1),trim(s2)
end subroutine MOM_error
end module MOM_error_handler
