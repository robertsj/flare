!==============================================================================!
! MODULE: solve
!
!> @author Jeremy Roberts
!> @brief Solver
!==============================================================================!
module solver

  use coefficients
  use geometry, only: pattern, number_assemblies, number_neighbors, neighbors
  use state
  use material_data, only: compute_flare_parameters

  implicit none

  !> Maximum number of inners
  integer :: max_inners = 10
  !> Maximum number of outers
  integer :: max_outers = 100
  !> Eigenvalue tolerance
  double precision :: ktol = 0.0001_8
  !> Fission source tolerance
  double precision :: stol = 0.001_8
  !> Flag to print solver diagnostics
  integer :: verbose = 0

  !> Reactor power (thermal) in GW
  double precision :: reactor_power = 0.0_8
  !> Average assembly power
  double precision :: average_assembly_power
  !> Number of burnup steps
  integer :: number_burnup_steps = 0
  !> Burnup steps (full power days)
  double precision, allocatable, dimension(:) :: burnup_steps

  !> Assembly heavy metal mass (MTU), a single value applied to all assemblies
  !> taken from the Westinghouse PWR book for a 4-loop plant with "OFA" fuel
  double precision :: assembly_mass = 0.423_8

contains

  !============================================================================!
  !> @brief Initialize.
  !============================================================================!
  subroutine initialize_solver()
    ! nothing for now
    ! make sure to deallocate before allocating
  end subroutine initialize_solver

  !============================================================================!
  !> @brief Solve the eigenvalue problem for a single configuration.
  !>
  !> The solution approach is pretty simple, using a series of Jacobi inner
  !> iterations followed by outer k-updates.  The bounds were found to yield
  !> convergence in about 70 iterations for a "typical" problem.   (Having 
  !> some inners helps avoid "false" convergence, too)
  !============================================================================!
  subroutine solve()
    implicit none
    ! local
    integer :: i, j, p, q, qq
    double precision :: k,                   & ! temporary current keff
                        k_o,                 & ! temporary past keff
                        s(number_assemblies),   & ! fission density
                        s_o(number_assemblies), & ! temporay density (inners)
                        s_oo(number_assemblies),& ! temporary density (outers)
                        k_num,               & ! numerator in keff expression
                        k_den,               & ! denomenator in keff expression
                        serr,                & ! density residual
                        kerr,                & ! keff residual
                        mean_s                 !

    ! Initialize the fission source and normalize
    s = 1.0 / sqrt(dble(number_assemblies))

    ! Guess k = 1
    k = 1.0
    
    ! Update coefficients
    call build_coefficients()

    ! Outer iteration
    OUTER: do j = 1, max_outers
      s_oo = s       
         
      ! Inner iteration
      INNER: do i = 1, max_inners
        s_o = s
        do p = 1, number_assemblies
          s(p) = wpp(p) * s_o(p)
          do q = 1, 4
            if (neighbors(p, q) .gt. 0) then
              qq   = neighbors(p, q)
              s(p) = s(p) + wqp(qq)*s_o(qq)
            end if
          end do
          s(p) = s(p) * KINF(pattern(p)) / k
        end do      
      end do INNER
      s = s / norm(s)

      ! Update k
      k_o = k
      k_num = 0.0
      k_den = 0.0
      do i = 1, number_assemblies
        k_num = k_num + s(i)*wleak(i)
        k_den = k_den + s(i)/kinf(pattern(i))
      end do
      k = (sum(s) - k_num) / k_den

      ! Update errors.  Check only density, as k always converges faster.
      kerr = abs(k - k_o)
      serr = norm(s - s_oo) 
      if  (kerr < ktol .and. serr < stol) then
        exit
      end if

    end do OUTER

    ! Post process.  Store values and print out.
    keff = k
    mean_s = (0.25*s(1) + sum(s(2:number_assemblies))) / &
             (0.25 + dble(number_assemblies-1))
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

  !============================================================================!
  !> @brief Compute the L2 norm of an array of values
  !============================================================================!
  subroutine burn()

    implicit none

    integer :: i
    double precision :: cycle_max_assembly_peaking = 0.0_8
    double precision :: burnup = 0.0_8, burnup_step = 0.0_8
    double precision :: full_power_days = 0.0_8

    if (number_burnup_steps == 0) then
      call solve()
      return
    end if

    ! the central assembly is only a quarter assembly, so divide quarter
    ! of the power over N-1 + 0.25 assemblies.
    average_assembly_power = 0.25_8 * reactor_power / &
                             (dble(number_assemblies)-0.75)

    ! DEBUG checks
    if (number_assemblies > number_materials) then
      stop "The number of materials must be equal to or greater &
            than the number of assemblies for cycle depletions."
    end if
    do i = 1, number_materials
      if (count(pattern == i) > 1) then
        stop "Non-unique material assignment for a burnup problem &
              is not allowed."
      end if
    end do

    do i = 1, number_burnup_steps

      burnup_step = burnup_steps(i)

      ! Compute the initial power distribution
      call solve()

      print '(a, i3, 6f10.4)', " +++ ", i-1, burnup, full_power_days, &
            cycle_max_assembly_peaking, max_assembly_peaking, keff

      ! Update the assembly burnups
      call update_assembly_burnup(burnup_step)

      ! Update the material data
      call compute_flare_parameters()

      ! Compute new core burnup (GWd) and full power days
      burnup = burnup + &
               burnup_step*reactor_power/(number_assemblies*assembly_mass)
      full_power_days = full_power_days + burnup_step

      ! Record the new maximum peaking for the cycle and the burnup it occurs
      if (max_assembly_peaking > cycle_max_assembly_peaking) then
        cycle_max_assembly_peaking = max_assembly_peaking
        burnup_at_max_assembly_peaking = burnup
      end if

      !print '(a, i3, 5f9.3)', " +++ ", i, burnup, full_power_days, cycle_max_assembly_peaking, max_assembly_peaking, keff

    end do

    ! Solve for the conditions at the end-of-cycle
    call solve()
    print '(a, i3, 6f10.4)', " +++ ", i-1, burnup, full_power_days, &
            cycle_max_assembly_peaking, max_assembly_peaking, keff

  end subroutine burn

  subroutine update_assembly_burnup(step)
    double precision, intent(in) :: step
    integer :: i
    do i = 1, number_assemblies
      B(pattern(i)) = B(pattern(i)) + &
                      assembly_peaking(i) * average_assembly_power * step
    end do
  end subroutine update_assembly_burnup

  !============================================================================!
  !> @brief Compute the L2 norm of an array of values
  !============================================================================!
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

