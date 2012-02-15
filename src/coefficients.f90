!==============================================================================!
! MODULE: coefficients
!
!> @author Jeremy Roberts
!>
!> @brief Coefficients for the neutron balance equation.
!==============================================================================!
module coefficients

  use group_data, only: D1, D2, R1, A2, F1, F2, S12
  use geometry, only: pattern, delta, number_bundles, number_neighbors

  implicit none

  !> Coefficients
  double precision, allocatable :: wpp(:), wqp(:), wleak(:), kinf(:)

  !> Model parameters (with defaults)
  double precision :: mixing_factor=0.88, alpha1=0.3, alpha2=0.6

contains

  !> @name Python interface
  !> @{


  !> @}

  !============================================================================
  !> @brief Initialize coefficients.
  !============================================================================
  subroutine initialize_coefficients()
    allocate( wleak(number_bundles), wpp(number_bundles), &
              wqp(number_bundles), kinf(number_bundles) )
  end subroutine initialize_coefficients

  !============================================================================
  !> @brief Set model parameters.
  !============================================================================
  subroutine set_model(g, a1, a2)
    double precision, intent(in) :: g, a1, a2
    mixing_factor = g
    alpha1 = a1
    alpha2 = a2
  end subroutine set_model


  !============================================================================
  !> @brief Compute coefficients.
  !>
  !> Note, elements are ordered left to right, top to bottom in normal bottom
  !> right quarter core.
  !>
  !============================================================================
  subroutine build_coefficients()
    integer :: i, j, k, n, id
    double precision :: g, aI, aII ! flare model parameters
    double precision :: M, w

    ! Now, compute the coefficients, wpp and wqp.  
    g   = mixing_factor
    aI  = alpha1
    aII = alpha2

!    print *, "      g = ", mixing_factor
!    print *, " alpha1 = ", alpha1
!    print *, " alpha2 = ", alpha2

    do i = 1, number_bundles
      ! Bundle id
      id = pattern(i)
      ! Compute the migration area
      M = sqrt(D1(id)/R1(id) + D2(id)/A2(id))
      ! Compute k-infinity
      kinf(i) = (F1(id) + F2(id) * S12(id)/A2(id)) / R1(id)
      ! Compute a single probability
      w = (1 - g) * 0.5 * M / delta + g * M**2 / delta**2
      ! Subtract leakage to actual neigbors
      wpp(i) = 1.0 - number_neighbors(i) * w 
      ! Build coefficients
      if (number_neighbors(i) .eq. 3) then 
        ! account for one external surface
        wpp(i)   = wpp(i) - w * (1.0 - aI)
        wleak(i) = w * (1.0 - aI)
      elseif (number_neighbors(i) .eq. 2) then
        ! account for two external surfaces
        wpp(i)   = wpp(i) - 2.0 * w * (1.0 - aII)
        wleak(i) = 2.0 * w * (1.0 -aII)
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

