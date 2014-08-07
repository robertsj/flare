!==============================================================================!
! MODULE: state
!
!> @author Jeremy Roberts
!>
!> @brief State vectors (i.e. the solution right now)
!==============================================================================!
module state

  use geometry, only: number_assemblies, stencil_dimension, stencil

  implicit none

  !> Eigenvalue
  double precision :: keff

  !> Assembly power peaking factors
  double precision, allocatable :: assembly_peaking(:)

  !> Assembly moderator temperature
  double precision, allocatable :: assembly_temperature(:)

  !> Assembly fuel temperature 
  double precision, allocatable :: fuel_temperature(:)


  !> Power peaking map
  double precision, allocatable :: peaking_map(:, :)

!> Power  map
double precision, allocatable :: power_map(:, :)

!> Temperature  map
double precision, allocatable :: temperature_map(:, :)

 

  !> Maximum assembly power peaking factor
  double precision :: mappf

  !> Burnup at which the max peaking was computed
  double precision :: mappf_bu

  !> Cycle length (i.e., burnup at keff = 1.0)
  double precision :: cycle_length

contains

  !=============================================================================
  !> @brief Initialize the unknowns.
  !=============================================================================
  subroutine initialize_state()

    if (allocated(assembly_peaking)) call deallocate_state()
    
    allocate(assembly_peaking(number_assemblies),                &
             peaking_map(stencil_dimension, stencil_dimension))

    if (allocated(assembly_temperature)) call deallocate_state()
 
   allocate(assembly_temperature(number_assemblies),                &
            power_map(stencil_dimension, stencil_dimension))

    if (allocated(fuel_temperature)) call deallocate_state()

    allocate(fuel_temperature(number_assemblies),                &
            temperature_map(stencil_dimension, stencil_dimension))


    keff = 1.0
    mappf = 0.0
    mappf_bu = 0.0
    cycle_length = 0.0

  end subroutine

  !=============================================================================
  !> @brief Print values.
  !=============================================================================
  subroutine print_state()
    print *, "------------------------------ "
    print '(a, f10.6)', "            keff = ", keff
    print '(a, f10.6)', " maximum peaking = ", mappf
    print '(a, f10.6)', "    cycle length = ", cycle_length
    print *, "------------------------------ "
  end subroutine print_state

  !=============================================================================
  !> @brief Return keff
  !=============================================================================
  double precision function get_keff()
    get_keff = keff
  end function get_keff

  !=============================================================================
  !> @brief Return maximum power peaking factor
  !=============================================================================
  double precision function get_mppf()
    get_mppf = mappf
  end function get_mppf

  !=============================================================================
  !> @brief Deallocate state
  !=============================================================================
  subroutine deallocate_state()
    deallocate(assembly_peaking, peaking_map)
    deallocate(assembly_temperature, power_map)
    deallocate(fuel_temperature, temperature_map)
  end subroutine

end module state
