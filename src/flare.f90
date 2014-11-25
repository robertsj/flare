!==============================================================================!
! PROGRAM: flare
!
!> @author Jeremy Roberts (jaroberts@ksu.edu)
!> @brief A simple 2-d nodal code
!==============================================================================!
program flare

  use material_data
  use geometry
  use coefficients
  use state
  use solver
  use utilities

  implicit none

  ! temporary variables for reading in
  character(80)  :: inputfile
  integer :: io, uinp = 5, i

  namelist /material_options/ number_materials, material_source, &
                              database_name
  namelist /reactor_options/  reactor_power, assembly_mass, &
                              number_assemblies, stencil_dimension, &
                              assembly_width
  namelist /solver_options/   run_mode, & 
                              max_burnup_steps, &
                              axial_buckling, &
                              boron_worth, &
                              mixing_factor, alpha1, alpha2, &
                              k_tol, s_tol, b_tol, t_tol, &
                              max_inner_iters, &
                              max_outer_iters, &
                              max_boron_iters, &
                              verbose

  !============================================================================!
  ! INPUT
  !============================================================================!

  if (command_argument_count() < 1) then
    stop "*** ERROR: user input file not specified ***"
  else
    call get_command_argument(1, inputfile);
    open (unit = uinp, file = inputfile, action = "read", &
          status = "old", position = "rewind", iostat = io)
    if (io > 0) stop "*** ERROR: user input file not found ***"
  end if

  ! Read namelists
  read (uinp, nml=material_options)
  read (uinp, nml=reactor_options)
  read (uinp, nml=solver_options)

  if (.true.) then
    print *, ""
    print *, "=========================="
    print *, "= a FLARE implementation ="
    print *, "=== for 2-d neutronics ==="
    print *, "=========================="
    print *, ""
  end if

  ! Initialize geometry and read in stencil
  call initialize_geometry()
  
  read (uinp,'(a)') ! read the comment line
  do i = 1, stencil_dimension
    read (uinp, *) stencil(i, :)
  end do

  ! Initialize material data and read in parameters
  call initialize_material_data(number_materials)

  select case (material_source)
  
    case (MATERIAL_SOURCE_INPUT_FLARE)
    
      print *, " MATERIAL SOURCE: Reading FLARE parameters."
      do i = 1, number_materials
        read (uinp,'(a)')
        read (uinp, *) KINF(i), M2(i) 
      end do
      
    case (MATERIAL_SOURCE_INPUT_2G)
    
      print *, " MATERIAL SOURCE: Reading two-group parameters."
      do i = 1, number_materials
        read (uinp,'(a)')
        read (uinp, *) D1(i), D2(i), A1(i), A2(i), F1(i), F2(i), S12(i)
      end do
      
      call compute_flare_parameters()
    
    case (MATERIAL_SOURCE_BUILT_IN)
    
      print *, " MATERIAL SOURCE: Using built-in data model."

      do i = 1, number_materials
        read (uinp,'(a)')
        read (uinp, *) B(i), E(i), BP(i)
      end do
      
    case (MATERIAL_SOURCE_DATABASE)
    
      print *, " MATERIAL SOURCE: Using database in ", trim(database_name)

      stop "FATAL ERROR: Not yet implemented."

      do i = 1, number_materials
        read (uinp,'(a)')
        read (uinp, *) B(i), HT_F(i), HT_C(i), HBC(i)
      end do
      
    case default
    
      stop "FATAL ERROR: Invalid material source."
      
  end select

  ! Initialize solver
  call initialize_solver()
  
  if (max_burnup_steps > 0) then
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

  call solve()

  !============================================================================!
  ! POST PROCESS, etc.
  !============================================================================!

  call print_state()
  call print_map(assembly_peaking, "ASSEMBLY_PEAKING", GEOMETRY_INDEXED)
  
  if (allocated(B)) then
    call print_map(B, "ASSEMBLY_BURNUP", MATERIAL_INDEXED)
  end if
  if (allocated(E)) then
    call print_map(KINF, "ASSEMBLY_ENRICHMENT", MATERIAL_INDEXED)
  end if

  call finalize_geometry()

end program flare
