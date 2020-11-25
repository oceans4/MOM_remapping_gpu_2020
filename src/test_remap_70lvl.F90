program test_remap_70lvl

  use MOM_remapping, only : remapping_CS
  use MOM_remapping, only : initialize_remapping
  use MOM_remapping, only : remapping_core_h
  use MOM_remapping, only : remapping_set_param
  use MersenneTwister, only : randomNumberSequence
  use MersenneTwister, only : new_randomNumberSequence
  use MersenneTwister, only : getRandomReal

  implicit none

  integer :: twdth=300 ! Tile width (excludes halo but assumes square arrays)
  integer :: halo=4 ! A halo of unused parts of the array
  integer, parameter :: n0 = 70 ! Number of layers in source grid
  integer, parameter :: n1 = 35 ! Number of layers in target grid
  real, allocatable :: h0(:,:,:) ! Source grid
  real, allocatable :: u0(:,:,:) ! Source data
  real, allocatable :: h1(:,:,:) ! Target grid
  real, allocatable :: u1(:,:,:) ! Target data
  type(remapping_CS) :: CS ! Remapping control structure
  integer :: i, j, k ! Indices
  type(randomNumberSequence) :: twister ! Mersenne Twister
  real :: rn0(n0), rn1(n1) ! Random number vectors
  real :: h0sum, h1sum ! Total thicknesses of column
  character(len=32) :: arg ! Command line argument
  real :: cptime ! CPU time

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
  twister = new_RandomNumberSequence(seed=10)
  do j = 1, twdth
    do i = 1, twdth
      h0sum = 0.
      do k = 1, n0
        u0(i,j,k) = getRandomReal(twister)
        h0(i,j,k) = getRandomReal(twister)
        h0sum = h0sum + h0(i,j,k)
      enddo
      h1sum = 0.
      do k = 1, n1
        h1(i,j,k) = getRandomReal(twister)
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
                            boundary_extrapolation=.false. )
  call do_remap(twdth, halo, n0, n1, CS, h0, u0, h1, u1, cptime)
  print '(''PCM time taken: '',f8.3,'' secs'')', cptime

  ! Remap using PLM
  call remapping_set_param(CS, remapping_scheme='PLM')
  call do_remap(twdth, halo, n0, n1, CS, h0, u0, h1, u1, cptime)
  print '(''PLM time taken: '',f8.3,'' secs'')', cptime

  ! Remap using PPM with explicit edge values
  call remapping_set_param(CS, remapping_scheme='PPM_H4')
  call do_remap(twdth, halo, n0, n1, CS, h0, u0, h1, u1, cptime)
  print '(''PPM_H4 time taken: '',f8.3,'' secs'')', cptime

  ! Remap using PPM with implicit edge values
  call remapping_set_param(CS, remapping_scheme='PPM_IH4')
  call do_remap(twdth, halo, n0, n1, CS, h0, u0, h1, u1, cptime)
  print '(''PPM_IH4 time taken: '',f8.3,'' secs'')', cptime

  ! Remap using PQM with IH4-IH3
  call remapping_set_param(CS, remapping_scheme='PQM_IH4IH3')
  call do_remap(twdth, halo, n0, n1, CS, h0, u0, h1, u1, cptime)
  print '(''PQM_IH4IH3 time taken: '',f8.3,'' secs'')', cptime

  ! Remap using PQM with IH6-IH5
  call remapping_set_param(CS, remapping_scheme='PQM_IH6IH5')
  call do_remap(twdth, halo, n0, n1, CS, h0, u0, h1, u1, cptime)
  print '(''PQM_IH6IH5 time taken: '',f8.3,'' secs'')', cptime

  contains

  ! Remaps u0(h0) to h1 for each column in the computational domain.
  ! Returns u1(h1) and cputime.
  subroutine do_remap(twdth, halo, n0, n1, CS, h0, u0, h1, u1, cputime)
    integer, intent(in) :: twdth !< Width of square computational domain
    integer, intent(in) :: halo  !< Size of unused halo
    integer, intent(in) :: n0  !< Number of levels in source grid
    integer, intent(in) :: n1  !< Number of levels in target grid
    type(remapping_CS), intent(inout) :: CS ! Remapping control structure
    real, intent(in) :: h0(1-halo:twdth+halo,1-halo:twdth+halo,n0) !< Source grid
    real, intent(in) :: u0(1-halo:twdth+halo,1-halo:twdth+halo,n0) !< Source data
    real, intent(in) :: h1(1-halo:twdth+halo,1-halo:twdth+halo,n1) !< Target grid
    real, intent(inout) :: u1(1-halo:twdth+halo,1-halo:twdth+halo,n1) !< Target data
    real, intent(out) :: cputime !< CPU time used
    ! Local variables
    integer :: i,j
    real :: cptim1, cptim2

    ! Production version does not use "checks"
    call remapping_set_param(CS, check_reconstruction=.false., check_remapping=.false.)
    call cpu_time(cptim1)
    do j = 1, twdth
      do i = 1, twdth
        call remapping_core_h(CS, n0, h0(i,j,:), u0(i,j,:), n1, h1(i,j,:), u1(i,j,:), &
                              h_neglect=1.e-30, h_neglect_edge=1.e-30)
      enddo
    enddo
    call cpu_time(cptim2)
    cputime = cptim2 - cptim1

    ! Redo with checks turned on
    call remapping_set_param(CS, check_reconstruction=.true., check_remapping=.true.)
    do j = 1, twdth
      do i = 1, twdth
        call remapping_core_h(CS, n0, h0(i,j,:), u0(i,j,:), n1, h1(i,j,:), u1(i,j,:), &
                              h_neglect=1.e-30, h_neglect_edge=1.e-30)
      enddo
    enddo

  end subroutine do_remap

end program
