!==============================================================================!
! MODULE: state
!
!> @author Jeremy Roberts
!>
!> @brief State vectors (i.e., the solution right now)
!==============================================================================!
module state

  use geometry, only: number_assemblies, stencil_dimension, stencil

  implicit none

  !> Multipplication factor
  real(8) :: keff

  !> Assembly power peaking factors
  real(8), allocatable :: assembly_peaking(:)

  !> Power peaking map
  real(8), allocatable :: peaking_map(:, :)
  
  !> (Instantaneous) fuel temperature (K)
  real(8), allocatable :: T_F(:)
  
  !> (Instantaneous) coolant temperature (K)
  real(8), allocatable :: T_C(:)

  !> (Instantaneous) boron concentration (ppm)
  real(8) :: BC 

  !> Maximum assembly power peaking factor
  real(8) :: mappf

  !> Burnup at which the max peaking was computed
  real(8) :: mappf_bu

  !> Cycle length (i.e., burnup at keff = 1.0)
  real(8) :: cycle_length
  
contains

  !=============================================================================
  !> @brief Initialize the unknowns.
  !=============================================================================
  subroutine initialize_state()

    if (allocated(assembly_peaking)) call deallocate_state()
    
    allocate(assembly_peaking(number_assemblies),                &
             peaking_map(stencil_dimension, stencil_dimension))
    keff = 1.0
    mappf = 0.0
    mappf_bu = 0.0
    cycle_length = 0.0

    ! Set the temperatures and boron concentration to the baseline values
    ! used in the model.
    T_F = 900.0
    T_C = 580.0
    BC  = 900.0

  end subroutine

  !=============================================================================
  !> @brief Print values.
  !=============================================================================
  subroutine print_state()
    print *, ""
    print *, "------------------------------ "
    print '(a, f10.6)', "            keff = ", keff
    print '(a, f10.6)', " maximum peaking = ", mappf
    print '(a, f10.6)', "    cycle length = ", cycle_length
    print *, "------------------------------ "
    print *, ""
  end subroutine print_state

  !=============================================================================
  !> @brief Return keff
  !=============================================================================
  real(8) function get_keff()
    get_keff = keff
  end function get_keff

  !=============================================================================
  !> @brief Return maximum power peaking factor
  !=============================================================================
  real(8) function get_mppf()
    get_mppf = mappf
  end function get_mppf

  !=============================================================================
  !> @brief Deallocate state
  !=============================================================================
  subroutine deallocate_state()
    deallocate(assembly_peaking, peaking_map)
  end subroutine

end module state
