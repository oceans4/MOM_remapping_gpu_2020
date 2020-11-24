program test_remap_4lvl
  use MOM_remapping, only : remapping_CS
  use MOM_remapping, only : initialize_remapping
  use MOM_remapping, only : dzFromH1H2
  use MOM_remapping, only : remapping_core_w
  use MOM_remapping, only : dumpgrid

  logical :: verbose=.false. !< If true, write results to stdout
  ! Local variables
  integer, parameter :: imax=10000000
  integer, parameter :: n0 = 4, n1 = 3
  real, allocatable :: h0(:,:), x0(:,:), u0(:,:)
  real, allocatable :: h1(:,:), x1(:,:), u1(:,:), dx1(:,:)
  type(remapping_CS) :: CS !< Remapping control structure
  logical :: answers_2018 !  If true use older, less acccurate expressions.
  integer :: i,k
  real :: err, h_neglect, h_neglect_edge
  logical :: thisTest, v
  real :: a,H

  allocate(h0(imax,n0), x0(imax,n0+1), u0(imax,n0))
  allocate(h1(imax,n1), x1(imax,n1+1), u1(imax,n1), dx1(imax,n1+1))

  a=9.
  H=3.
  !Initialize arrays with test data
  do i=1,imax
     do k=1,n0
        u0(i,k)=a-(k-1)*2*a/(n0-1)
        h0(i,k)=H/n0
     enddo
     do k=1,n1
        h1(i,k)=H/n1
     enddo
  enddo

  v = verbose
  answers_2018 = .false. ! .true.
  h_neglect = hNeglect_dflt
  h_neglect_edge = hNeglect_dflt ; if (answers_2018) h_neglect_edge = 1.0e-10
  write(*,*) '==== MOM_remapping: remapping_unit_tests_gpu ================='
  thisTest = .false.
  call cpu_time(cptim1)
  call initialize_remapping(CS, 'PPM_H4', answers_2018=answers_2018)
  do i=1,imax
    if (verbose) write(*,*) 'h0 (test data)'
    if (verbose) call dumpGrid(n0,h0(i,:),x0(i,:),u0(i,:))
    call dzFromH1H2( n0, h0(i,:), n1, h1(i,:), dx1(i,:) )
    call remapping_core_w( CS, n0, h0(i,:), u0(i,:), n1, dx1(i,:), u1(i,:), h_neglect, h_neglect_edge)
    do k=1,n1
       err=u1(i,k)-8.*(0.5*real(1+n1)-real(k))
       if (abs(err)>real(n1-1)*epsilon(err)) thisTest = .true.
    enddo
    if (verbose) write(*,*) 'h1 (by projection)'
    if (verbose) call dumpGrid(n1,h1(i,:),x1(i,:),u1(i,:))
  enddo
  call cpu_time(cptim2)
  print '(''time taken '',f8.3)', (cptim2 - cptim1)
  if (thisTest) write(*,*) 'remapping_unit_tests_gpu: Failed remapping_core_w() with error: ',err
end program
