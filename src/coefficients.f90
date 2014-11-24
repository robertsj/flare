!==============================================================================!
! MODULE: coefficients
!
!> @author Jeremy Roberts
!> @brief Coefficients for the neutron balance equation.
!==============================================================================!
module coefficients

  use material_data
  use geometry, only: pattern, assembly_width, number_assemblies, &
                      number_neighbors

  implicit none

  !> Coefficients
  real(8), allocatable :: wpp(:), & ! Self collision probability
                          wqp(:), & ! Neigbor collision probability
                          wleak(:)  ! 

  !> Model parameters (with defaults)
  real(8) :: mixing_factor=0.88, alpha1=0.3, alpha2=0.6

contains

  !============================================================================
  !> @brief Initialize coefficients.
  !============================================================================
  subroutine initialize_coefficients()
    if (allocated(wleak)) deallocate(wleak)
    if (allocated(wpp)) deallocate(wpp)
    if (allocated(wqp)) deallocate(wqp)
    allocate(wleak(number_assemblies), wpp(number_assemblies), &
             wqp(number_assemblies))
  end subroutine initialize_coefficients

  !============================================================================
  !> @brief Set model parameters.
  !============================================================================
  subroutine set_model(g, a1, a2)
    real(8), intent(in) :: g, a1, a2
    mixing_factor = g
    alpha1 = a1
    alpha2 = a2
  end subroutine set_model

  !============================================================================
  !> @brief Compute coefficients.
  !>
  !> Note, elements are ordered left to right, top to bottom in normal bottom
  !> right quarter core.
  !============================================================================
  subroutine build_coefficients()
    integer :: i, id
    real(8) :: g1, g2, aI, aII ! flare model parameters
    real(8) :: w

    ! Now, compute the coefficients, wpp and wqp.
    g1 = 0.5 * (1.0 - mixing_factor) / assembly_width
    g2 = mixing_factor / assembly_width**2
    aI  = 1.0 - alpha1
    aII = 1.0 - alpha2

    do i = 1, number_assemblies
      ! Bundle id
      id = pattern(i)
      ! Compute a single probability
      w = g1 * sqrt(M2(id)) + g2 * M2(id)
      ! Subtract leakage to actual neigbors
      wpp(i) = 1.0 - number_neighbors(i) * w
      ! Build coefficients
      if (number_neighbors(i) .eq. 3) then
        ! account for one external surface
        wleak(i) = w * aI
        wpp(i)   = wpp(i) - wleak(i)
      elseif (number_neighbors(i) .eq. 2) then
        ! account for two external surfaces
        wleak(i) = 2.0 * w * aII
        wpp(i)   = wpp(i) - wleak(i)
      elseif (number_neighbors(i) .eq. 4) then
        ! I'm internal and there is no leakage
        wleak(i)  = 0.0
      else
        stop 'number of neighbors is off!!'
      end if
      ! My leakage to another
      wqp(i) = w
    end do

  end subroutine build_coefficients

end module coefficients
