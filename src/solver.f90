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

  integer :: verbose = 0

contains

  !============================================================================
  !> @brief Initialize.
  !============================================================================
  subroutine initialize_solver()
    ! nothing for now
    ! make sure to deallocate before allocating
  end subroutine initialize_solver

  !============================================================================
  !> @brief Solve the problem.
  !>
  !> The solution approach is pretty simple, using a series of Jacobi inner
  !> iterations followed by outer k-updates.  The bounds were found to yield
  !> convergence in about 70 iterations for a "typical" problem.   (Having 
  !> some inners helps avoid "false" convergence, too)
  !============================================================================
  subroutine solve()
    ! local
    integer :: i, j, p, q, qq
    double precision :: k,                   & ! temporary current keff
                        k_o,                 & ! temporary past keff
                        s(number_bundles),   & ! temporary current density
                        s_o(number_bundles), & ! temporary past density (inners)
                        s_oo(number_bundles),& ! temporary past density (outers)
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
    OUTER: do j = 1, 100
      s_oo = s       
         
      ! Jacobi inner iteration
      INNER: do i = 1, 10
        s_o = s
        do p = 1, number_bundles
          s(p) = wpp(p) * s_o(p)
          do q = 1, 4
            if (neighbors(p, q) .gt. 0) then
              qq   = neighbors(p, q)
              s(p) = s(p) + wqp(qq)*s_o(qq)
            end if
          end do
          s(p) = s(p) * kinf(p) / k
        end do      
      end do INNER
      s = s / norm(s)

      ! Update k
      k_o = k
      k_num = 0.0
      k_den = 0.0
      do i = 1, number_bundles
        k_num = k_num + s(i)*wleak(i)
        k_den = k_den + s(i)/kinf(i)
      end do
      k = (sum(s) - k_num) / k_den
      ! Update errors.  Check only density, as k always converges faster.
      kerr = abs(k - k_o)
      serr = norm(s - s_oo) 
      if  (kerr < 0.0001 .and. serr < 0.001) then
        exit
      end if

    end do OUTER

    ! Post process.  Store values and print out.
    keff = k
    mean_s = (0.25*s(1) + sum(s(2:number_bundles))) / (0.25 + dble(number_bundles-1))
    assembly_peaking = s / mean_s
    max_assembly_peaking = maxval(assembly_peaking)

    if (verbose .eq. 1) then
      print *, "------------------------------"
      print '(a, i10)',   " iterations = ", j
      print '(a, f10.6)', "       keff = ", k
      print '(a, f10.6)', " keff resid = ", kerr
      print '(a, f10.6)', "   fd resid = ", serr
      print *, "------------------------------"
    end if
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

