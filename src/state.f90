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

  !> Fission density
  double precision, allocatable :: fission_density(:)

  !> Assembly power peaking factors
  double precision, allocatable :: assembly_peaking(:)

  !> Power peaking map
  double precision, allocatable :: peaking_map(:, :)

  !> Maximum assembly power peaking factor
  double precision :: max_assembly_peaking

  !> Burnup at which the max peaking was computed
  double precision :: burnup_at_max_assembly_peaking

  !> Cycle length (i.e., burnup at keff = 1.0)
  double precision :: cycle_length

contains

  !=============================================================================
  !> @brief Initialize the unknowns.
  !=============================================================================
  subroutine initialize_state()

    if (allocated(fission_density)) call deallocate_state()
    
    allocate(fission_density(number_assemblies),                 &
             assembly_peaking(number_assemblies),                &
             peaking_map(stencil_dimension, stencil_dimension))
    keff = 1.0
    max_assembly_peaking = 0.0
    burnup_at_max_assembly_peaking = 0.0

  end subroutine

  !=============================================================================
  !> @brief Print values.
  !=============================================================================
  subroutine print_state()
    print *, "------------------------------ "
    print '(a, f10.6)', "            keff = ", keff
    print '(a, f10.6)', " maximum peaking = ", max_assembly_peaking
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
    get_mppf = max_assembly_peaking
  end function get_mppf


  !=============================================================================
  !> @brief Print assembly power peaking factor map.
  !=============================================================================
  subroutine print_peaking()
    integer :: i, j, k
    call make_peaking_map()
    print *, "------------------------------------------------------------------------------------------"
    print *, "peaking map = "
    do i = 1, stencil_dimension
      print '(9f10.6)', peaking_map(i, :)
    end do
    print *, "-------------------------------------------------------------------------------------------"
  end subroutine print_peaking

  !=============================================================================
  !> @brief Make assembly power peaking factor map.
  !=============================================================================
  subroutine make_peaking_map()
    integer :: i, j, k
    peaking_map = 0.0
    do i = 1, stencil_dimension
      do j = 1, stencil_dimension
        if (stencil(i, j) .gt. 0) then
          peaking_map(i, j) = assembly_peaking(stencil(i, j))
        end if
      end do
    end do
  end subroutine make_peaking_map

  !=============================================================================
  !> @brief Deallocate state
  !=============================================================================
  subroutine deallocate_state()
    deallocate(fission_density, assembly_peaking, peaking_map)
  end subroutine

end module state
