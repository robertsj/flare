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

  use material_data
  use geometry
  use coefficients
  use state
  use solver
  !use burnup, only: power, number_steps, burnup_steps, burn

  implicit none

  ! temporary variables for reading in
  character(80)  :: inputfile
  integer :: io, uinp = 5, i, j

  namelist /material_options/ number_materials, material_source
  namelist /geometry_options/ number_assemblies, stencil_dimension,  delta
  namelist /solver_options/   verbose, max_inners, max_outers, &
                              ktol, stol, number_burnup_steps, reactor_power
  namelist /model_options/    mixing_factor, alpha1, alpha2

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
  read (uinp, nml=material_options)
  read (uinp, nml=geometry_options)
  read (uinp, nml=solver_options)
  read (uinp, nml=model_options)

  ! Initialize geometry and read in stencil
  call initialize_geometry()
  read (uinp,'(a)') ! read the comment line
  do i = 1, stencil_dimension
    read (uinp, *) stencil(i, :)
  end do

  ! Read in cross section data.  If a cycle is to be run, then the number
  ! of materials specified must be at least as great as the number of
  ! assemblies.  This allows a database of different materials (i.e.,
  ! potential assemblies) to be
  ! defined of which only a subset is used in a given pattern.  If, instead,
  ! assemblies could be assigned identical materials, then there is no
  ! good way to redefine those materials after the burnup of each assembly
  ! diverges in a cycle.
  call allocate_material_data(number_materials)

  if (material_source == MODEL_MATERIAL) then
    ! read parameters to compute data from the built-in fits
    do i = 1, number_materials
      read (uinp,'(a)')
      read (uinp, *) B(i), E(i), BP(i)
      !print *, "id=",i, "burnup=", B(i), "enrichment=", E(i), "bp=", BP(i)
    end do
  else
    ! otherwise, read two-group data directly
    read (uinp,'(a)') ! read the comment line
    read (uinp, *) D1
    read (uinp,'(a)') ! read the comment line
    read (uinp, *) D2
    read (uinp,'(a)') ! read the comment line
    read (uinp, *) A1
    read (uinp,'(a)') ! read the comment line
    read (uinp, *) A2
    read (uinp,'(a)') ! read the comment line
    read (uinp, *) F1
    read (uinp,'(a)') ! read the comment line
    read (uinp, *) F2
    read (uinp,'(a)') ! read the comment line
    read (uinp, *) S12
  end if
  call compute_flare_parameters()

  if (number_burnup_steps > 0) then
    allocate(burnup_steps(number_burnup_steps))
    read (uinp,'(a)') ! read the comment line
    read (uinp, *) burnup_steps
  end if

  !============================================================================!
  ! SETUP
  !============================================================================!

  call initialize_state()
  call build_geometry()
  call initialize_coefficients()

  !============================================================================!
  ! SOLVE
  !============================================================================!

  do i = 1, 1
    call burn()
  end do

  !============================================================================!
  ! POST PROCESS, etc.
  !============================================================================!

  call print_state()
  call print_peaking()

end program flare
