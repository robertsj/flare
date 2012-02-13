!==============================================================================!
! MODULE: solve
!
!> @author Jeremy Roberts
!>
!> @brief Solver
!>
!==============================================================================!
module solver

  use coefficients
  use geometry, only: pattern, number_bundles, number_neighbors, neighbors
  use state

  implicit none

contains

  !============================================================================
  !> @brief Initialize.
  !============================================================================
  subroutine initialize_solver()
    ! nothing for now
  end subroutine initialize_solver

  !============================================================================
  !> @brief Solve the problem.
  !============================================================================
  subroutine solve()
    ! local
    integer :: i, j, p, q, qq
    double precision :: k,                   & ! temporary current keff
                        k_o,                 & ! temporary past keff
                        s(number_bundles),   & ! temporary current density
                        s_o(number_bundles), & ! temporary past density
                        k_num,               & ! numerator in keff expression
                        k_den,               & ! denomenator in keff expression
                        serr,                & ! density residual
                        kerr,                & ! keff residual
                        mean_s                 !

    ! Initialize the fission source and normalize
    s = 1.0 / sqrt(dble(number_bundles))

    ! Guess k = 1
    k = 1.0
    
    ! Update coefficients
    call build_coefficients()
        
    ! Outer iteration.
    OUTER: do j = 1, 200
                
      ! Jacobi inner iteration (could do more than one)
      s_o = s
      do p = 1, number_bundles
        s(p) = wpp(p) * s_o(p)
        do q = 1, 4
          if (neighbors(p, q) .gt. 0) then
            qq   = neighbors(p, q)
            s(p) = s(p) + wqp(qq)*s_o(qq)*kinf(qq)/k
          end if
        end do
        s(p) = s(p) * kinf(p) / k
      end do      
      ! Update k
      k_o = k
      k_num = 0.0
      k_den = 0.0
      do i = 1, number_bundles
        k_num = k_num + s(i)*wleak(i)
        k_den = k_den + s(i)/kinf(i)
      end do
      k = (sum(s) - k_num) / k_den
      s = s / norm(s)

      ! Update errors.  Check only density, as k always converges faster.
      kerr = abs(k - k_o)
      serr = norm(s - s_o) 
      if  (serr < 1e-4) then
        exit
      end if

    end do OUTER

    ! Post process.  Store values and print out.
    keff = k
    mean_s = (0.25*s(1) + sum(s(2:number_bundles))) / (0.25 + dble(number_bundles-1))
    assembly_peaking = s / mean_s
    max_assembly_peaking = maxval(assembly_peaking)

    print *, "------------------------------"
    print '(a, i10)',   " iterations = ", j
    print '(a, f10.6)', "       keff = ", k
    print '(a, f10.6)', " keff resid = ", kerr
    print '(a, f10.6)', "   fd resid = ", serr
    print *, "------------------------------"

  end subroutine solve

  double precision function norm(v)
    double precision, intent(in) :: v(:)
    integer :: i
    norm = 0.0
    do i = 1, size(v)
      norm = norm + v(i)*v(i)
    end do
    norm = sqrt(norm)
  end function norm

end module solver

