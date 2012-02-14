!==============================================================================!
! PROGRAM: flare
!
!> @author Jeremy Roberts
!>
!> @brief A simple 2-d nodal code
!>
!> Most of the equations used here follow 
!>    Gupta, N. K., "Nodal Methods for Three-Dimensional Simulators",
!>        Progress in Nuclear Energy, 7, pp 127-149 (1981)
!==============================================================================!
program flare

  use group_data
  use geometry
  use coefficients
  use state
  use solver

  implicit none

  ! temporary variables for reading in
  character(80)  :: inputfile
  integer :: io, uinp = 5, i, j

  namelist /specs/ delta, number_materials, number_bundles, stencil_dimension
  namelist /model/ mixing_factor, alpha1, alpha2

  print *, "=========================="
  print *, "= a FLARE implementation ="
  print *, "=== for 2-d neutronics ==="
  print *, "=========================="
  !============================================================================!
  ! INPUT
  !============================================================================!

  if ( COMMAND_ARGUMENT_COUNT() .lt. 1 ) then
    stop "*** ERROR: user input file not specified ***"
  else
    call get_command_argument(1, inputfile);
    open (unit = uinp, file = inputfile, action = "read", &
          status = "old", position = "rewind", iostat = io)
    if (io > 0) stop "*** ERROR: user input file not found ***"
  end if

  ! Read namelists
  read (uinp, nml=specs)
  read (uinp, nml=model)

  ! Initialize geometry and read in stencil
  call initialize_geometry()
  read (uinp,'(a)') ! read the comment line
  do i = 1, stencil_dimension
    read (uinp, *) stencil(i, :)
  end do

  ! Read in cross section data
  call allocate_group_data(number_materials)
  read (uinp,'(a)') ! read the comment line
  read (uinp, *) D1
  read (uinp,'(a)') ! read the comment line 
  read (uinp, *) D2
  read (uinp,'(a)') ! read the comment line
  read (uinp, *) R1
  read (uinp,'(a)') ! read the comment line
  read (uinp, *) A2
  read (uinp,'(a)') ! read the comment line
  read (uinp, *) F1
  read (uinp,'(a)') ! read the comment line
  read (uinp, *) F2
  read (uinp,'(a)') ! read the comment line
  read (uinp, *) S12

  !============================================================================!
  ! SETUP
  !============================================================================!

  call initialize_state()
  call build_geometry()
  call initialize_coefficients()

  !============================================================================!
  ! SOLVE
  !============================================================================!

  call solve()

  !============================================================================!
  ! POST PROCESS, etc.
  !============================================================================!

  call print_state()
  call print_peaking()

end program flare
