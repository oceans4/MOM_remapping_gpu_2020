program test_remap_70lvl

  use MOM_remapping, only : remapping_CS
  use MOM_remapping, only : initialize_remapping
  use MOM_remapping, only : remapping_core_h
  use MOM_remapping, only : remapping_set_param

  integer :: twdth=300 ! Tile width (excludes halo but assumes square arrays)
  integer :: halo=4 ! A halo of unused parts of the array
  integer, parameter :: n0 = 70 ! Number of layers in source grid
  integer, parameter :: n1 = 35 ! Number of layers in target grid
  real, allocatable :: h0(:,:,:) ! Source grid
  real, allocatable :: u0(:,:,:) ! Source data
  real, allocatable :: h1(:,:,:) ! Target grid
  real, allocatable :: u1(:,:,:) ! Target data
  type(remapping_CS) :: CS ! Remapping control structure
  integer :: i, k ! Indices
  integer, allocatable :: seed(:) ! For getting the same numbers
  integer :: nseed ! size of seed
  real :: rn0(n0), rn1(n1) ! Random number vectors
  real :: h0sum, h1sum ! Total thicknesses of column
  character(len=32) :: arg ! Command line argument

  ! Read tile width and halo from command line arguments (fall back to defaults above)
  select case ( command_argument_count() )
    case (0)
    case (1)
      call get_command_argument(1, arg)
      read(arg(1:32),*) twdth
    case (2)
      call get_command_argument(1, arg)
      read(arg(1:32),*) twdth
      call get_command_argument(2, arg)
      read(arg(1:32),*) halo
    case default
      stop 'Too many command line arguments'
  end select

  print *,'Tile width =',twdth,' halo =',halo
  allocate( h0(1-halo:twdth+halo,1-halo:twdth+halo,n0) )
  allocate( u0(1-halo:twdth+halo,1-halo:twdth+halo,n0) )
  allocate( h1(1-halo:twdth+halo,1-halo:twdth+halo,n1) )
  allocate( u1(1-halo:twdth+halo,1-halo:twdth+halo,n1) )

  ! Create some test data
  call random_seed(size=nseed)
  allocate( seed(nseed) )
  seed(:) = 0
  do j = 1, twdth
    do i = 1, twdth
      seed(1) = i
      call random_seed(put=seed)
      h0sum = 0.
      do k = 1, n0
        call random_number(rn0)
        u0(i,j,k) = rn0(k)
        call random_number(rn0)
        h0(i,j,k) = rn0(k)
        h0sum = h0sum + h0(i,j,k)
      enddo
      h1sum = 0.
      do k = 1, n1
        call random_number(rn1)
        h1(i,j,k) = rn1(k)
        h1sum = h1sum + h1(i,j,k)
      enddo
      ! Adjust target grid to be same total thickness as source grid
      do k = 1, n1
        h1(i,j,k) = h1(i,j,k) * ( h0sum / h1sum )
      enddo
    enddo
  enddo

  ! Remap using PCM
  call initialize_remapping(CS, 'PCM', answers_2018=.false., &
                            boundary_extrapolation=.false., &
                            check_reconstruction=.true., check_remapping=.true.)
  call cpu_time(cptim1)
  do j = 1, twdth
    do i = 1, twdth
      call remapping_core_h(CS, n0, h0(i,j,:), u0(i,j,:), n1, h1(i,j,:), u1(i,j,:), &
                            h_neglect=1.e-30, h_neglect_edge=1.e-30)
    enddo
  enddo
  call cpu_time(cptim2)
  print '(''PCM time taken: '',f8.3,'' secs'')', (cptim2 - cptim1)

  ! Remap using PLM
  call remapping_set_param(CS, remapping_scheme='PLM')
  call cpu_time(cptim1)
  do j = 1, twdth
    do i = 1, twdth
      call remapping_core_h(CS, n0, h0(i,j,:), u0(i,j,:), n1, h1(i,j,:), u1(i,j,:), &
                            h_neglect=1.e-30, h_neglect_edge=1.e-30)
    enddo
  enddo
  call cpu_time(cptim2)
  print '(''PLM time taken: '',f8.3,'' secs'')', (cptim2 - cptim1)

  ! Remap using PPM with explicit edge values
  call remapping_set_param(CS, remapping_scheme='PPM_H4')
  call cpu_time(cptim1)
  do j = 1, twdth
    do i = 1, twdth
      call remapping_core_h(CS, n0, h0(i,j,:), u0(i,j,:), n1, h1(i,j,:), u1(i,j,:), &
                            h_neglect=1.e-30, h_neglect_edge=1.e-30)
    enddo
  enddo
  call cpu_time(cptim2)
  print '(''PPM_H4 time taken: '',f8.3,'' secs'')', (cptim2 - cptim1)

  ! Remap using PPM with implicit edge values
  call remapping_set_param(CS, remapping_scheme='PPM_IH4')
  call cpu_time(cptim1)
  do j = 1, twdth
    do i = 1, twdth
      call remapping_core_h(CS, n0, h0(i,j,:), u0(i,j,:), n1, h1(i,j,:), u1(i,j,:), &
                            h_neglect=1.e-30, h_neglect_edge=1.e-30)
    enddo
  enddo
  call cpu_time(cptim2)
  print '(''PPM_IH4 time taken: '',f8.3,'' secs'')', (cptim2 - cptim1)

  ! Remap using PQM with IH4-IH3
  call remapping_set_param(CS, remapping_scheme='PQM_IH4IH3')
  call cpu_time(cptim1)
  do j = 1, twdth
    do i = 1, twdth
      call remapping_core_h(CS, n0, h0(i,j,:), u0(i,j,:), n1, h1(i,j,:), u1(i,j,:), &
                            h_neglect=1.e-30, h_neglect_edge=1.e-30)
    enddo
  enddo
  call cpu_time(cptim2)
  print '(''PQM_IH4IH3 time taken: '',f8.3,'' secs'')', (cptim2 - cptim1)

  ! Remap using PQM with IH6-IH5
  call remapping_set_param(CS, remapping_scheme='PQM_IH6IH5')
  call cpu_time(cptim1)
  do j = 1, twdth
    do i = 1, twdth
      call remapping_core_h(CS, n0, h0(i,j,:), u0(i,j,:), n1, h1(i,j,:), u1(i,j,:), &
                            h_neglect=1.e-30, h_neglect_edge=1.e-30)
    enddo
  enddo
  call cpu_time(cptim2)
  print '(''PQM_IH6IH5 time taken: '',f8.3,'' secs'')', (cptim2 - cptim1)

end program
