program test_remap_70lvl

  use MOM_remapping, only : remapping_CS
  use MOM_remapping, only : initialize_remapping
  use MOM_remapping, only : remapping_core_h
  use MOM_remapping, only : remapping_set_param

  integer, parameter :: imax=100000 ! Number of columns
  integer, parameter :: n0 = 70 ! Number of layers in source grid
  integer, parameter :: n1 = 35 ! Number of layers in target grid
  real :: h0(imax,n0) ! Source grid 
  real :: u0(imax,n0) ! Source data
  real :: h1(imax,n1) ! Target grid
  real :: u1(imax,n1) ! Target data
  type(remapping_CS) :: CS ! Remapping control structure
  integer :: i, k ! Indices
  integer, allocatable :: seed(:) ! For getting the same numbers
  integer :: nseed ! size of seed
  real :: rn0(n0), rn1(n1) ! Random number vectors
  real :: h0sum, h1sum ! Total thicknesses of column

  ! Create some test data
  call random_seed(size=nseed)
  allocate( seed(nseed) )
  seed(:) = 0
  do i = 1, imax
    seed(1) = i
    call random_seed(put=seed)
    h0sum = 0.
    do k = 1, n0
      call random_number(rn0)
      u0(i,k) = rn0(k)
      call random_number(rn0)
      h0(i,k) = rn0(k)
      h0sum = h0sum + h0(i,k)
    enddo
    h1sum = 0.
    do k = 1, n1
      call random_number(rn1)
      h1(i,k) = rn1(k)
      h1sum = h1sum + h1(i,k)
    enddo
    ! Adjust target grid to be same total thickness as source grid
    do k = 1, n1
      h1(i,k) = h1(i,k) * ( h0sum / h1sum )
    enddo
  enddo

  ! Remap using PCM
  call initialize_remapping(CS, 'PCM', answers_2018=.false.)
  call cpu_time(cptim1)
  do i=1,imax
    call remapping_core_h( CS, n0, h0(i,:), u0(i,:), n1, h1(i,:), u1(i,:), h_neglect=1.e-30, h_neglect_edge=1.e-30)
  enddo
  call cpu_time(cptim2)
  print '(''PCM time taken: '',f8.3,'' secs'')', (cptim2 - cptim1)

  ! Remap using PLM
  call remapping_set_param(CS, remapping_scheme='PLM')
  call cpu_time(cptim1)
  do i=1,imax
    call remapping_core_h( CS, n0, h0(i,:), u0(i,:), n1, h1(i,:), u1(i,:), h_neglect=1.e-30, h_neglect_edge=1.e-30)
  enddo
  call cpu_time(cptim2)
  print '(''PLM time taken: '',f8.3,'' secs'')', (cptim2 - cptim1)

  ! Remap using PPM with explicit edge values
  call remapping_set_param(CS, remapping_scheme='PPM_H4')
  call cpu_time(cptim1)
  do i=1,imax
    call remapping_core_h( CS, n0, h0(i,:), u0(i,:), n1, h1(i,:), u1(i,:), h_neglect=1.e-30, h_neglect_edge=1.e-30)
  enddo
  call cpu_time(cptim2)
  print '(''PPM_H4 time taken: '',f8.3,'' secs'')', (cptim2 - cptim1)

  ! Remap using PPM with implicit edge values
  call remapping_set_param(CS, remapping_scheme='PPM_IH4')
  call cpu_time(cptim1)
  do i=1,imax
    call remapping_core_h( CS, n0, h0(i,:), u0(i,:), n1, h1(i,:), u1(i,:), h_neglect=1.e-30, h_neglect_edge=1.e-30)
  enddo
  call cpu_time(cptim2)
  print '(''PPM_IH4 time taken: '',f8.3,'' secs'')', (cptim2 - cptim1)

  ! Remap using PQM with IH4-IH3
  call remapping_set_param(CS, remapping_scheme='PQM_IH4IH3')
  call cpu_time(cptim1)
  do i=1,imax
    call remapping_core_h( CS, n0, h0(i,:), u0(i,:), n1, h1(i,:), u1(i,:), h_neglect=1.e-30, h_neglect_edge=1.e-30)
  enddo
  call cpu_time(cptim2)
  print '(''PQM_IH4IH3 time taken: '',f8.3,'' secs'')', (cptim2 - cptim1)

  ! Remap using PQM with IH6-IH5
  call remapping_set_param(CS, remapping_scheme='PQM_IH6IH5')
  call cpu_time(cptim1)
  do i=1,imax
    call remapping_core_h( CS, n0, h0(i,:), u0(i,:), n1, h1(i,:), u1(i,:), h_neglect=1.e-30, h_neglect_edge=1.e-30)
  enddo
  call cpu_time(cptim2)
  print '(''PQM_IH6IH5 time taken: '',f8.3,'' secs'')', (cptim2 - cptim1)

end program
