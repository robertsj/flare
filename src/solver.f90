!==============================================================================!
! MODULE: solve
!
!> @author Jeremy Roberts
!> @brief  Provides an implementation of the FLARE model with burnup
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
  !> Assembly HM mass (MTU), from WH PWR book for 4-loop plant with "OFA" fuel
  double precision :: assembly_mass = 0.483 !0.423_8
  !> Burnup option (0 = user steps, 1 = automated cycle length calculation)
  integer :: burnup_option = 0
  !> Number of burnup steps
  integer :: number_burnup_steps = 0
  !> Burnup steps (full power days)
  double precision, allocatable, dimension(:) :: burnup_steps

  double precision, private :: average_assembly_power
  double precision, private :: power_per_mass
  double precision, private :: mappf_cycle

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

        !$OMP DO
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
        !$OMP END DO

      end do INNER
      s = s / norm(s)

      ! Update k
      k_o = k
      k_num = 0.0
      k_den = 0.0
      do i = 1, number_assemblies
        k_num = k_num + s(i)*wleak(i)
        k_den = k_den + s(i)/KINF(pattern(i))
      end do
      k = (sum(s) - k_num) / k_den

      ! Update errors and check for convergence
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
    mappf = maxval(assembly_peaking)

    if (verbose .ge. 2) then
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
    double precision :: burnup_step, fpd, burnup, burnup1, keff1
    fpd = 0.0
    burnup = 0.0
    burnup1 = 0.0
    keff1 = 0.0

    !$OMP PARALLEL

    call solve()

    ! just get the power distribution and escape if not burning
    if (number_burnup_steps == 0) then
      return
    end if

    ! the central assembly is only a quarter assembly, so divide quarter
    ! of the power over N-1 + 0.25 assemblies.
    average_assembly_power = 0.25_8 * reactor_power / &
                             (dble(number_assemblies)-0.75)

    ! total core power divided by the total fuel mass
    power_per_mass = reactor_power / &
                     (1.0 + 4.0 * (number_assemblies-1)) / assembly_mass

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

    call print_burnup_header()
    call print_burnup(0, burnup, fpd, mappf, mappf, keff, cycle_length)

    BURNITS: do i = 1, number_burnup_steps

      ! select the burnup step
      burnup_step = burnup_steps(i)
      if (burnup_option == 1 .and. i > 2) then
        burnup_step = critical_step(keff, keff1, burnup, burnup1)
        ! estimated cycle length
        cycle_length = burnup_step * power_per_mass + burnup
        if (burnup_step > burnup_steps(i) .or. burnup_step == 0.0) then
          burnup_step = burnup_steps(i)
        end if
      end if

      call update_assembly_burnup(burnup_step)
      call compute_flare_parameters()

      ! record old values and update new ones
      burnup1 = burnup
      keff1   = keff
      burnup  = burnup + burnup_step * power_per_mass
      fpd     = fpd + burnup_step
      call solve()

      if (mappf > mappf_cycle) then
        mappf_cycle = mappf
        mappf_bu    = burnup
      end if

      call print_burnup(i, burnup, fpd, mappf_cycle, mappf, keff, cycle_length)

      if (burnup_option == 1 .and. &
         (keff <= 1.0 .or. abs(burnup-burnup1) <= ktol)) exit

    end do BURNITS

    !$OMP END PARALLEL

    if (burnup_option == 1) then
      ! compute final cl estimate based on latest info
      cycle_length = burnup + &
                     critical_step(keff,keff1,burnup,burnup1)*power_per_mass
    else
      cycle_length = burnup
    end if



  end subroutine burn

  !============================================================================!
  !> @brief Update the burnup for each assembly
  !============================================================================!
  subroutine update_assembly_burnup(step)
    double precision, intent(in) :: step
    integer :: i
    do i = 1, number_assemblies
      B(pattern(i)) = B(pattern(i)) + &
                      assembly_peaking(i) * average_assembly_power * step
    end do
  end subroutine update_assembly_burnup

  !============================================================================!
  !> @brief Estimate step to reach critical based on linear reactivity
  !============================================================================!
  double precision function critical_step(k0, k1, b0, b1)
    double precision, intent(in) :: k0, k1, b0, b1
    double precision :: p0, p1
    p0 = (k0-1.0)/k0
    p1 = (k1-1.0)/k1
    critical_step = (-(1.*(p1*b0-1.*b1*p0))/(p0-1.*p1)-b0) / power_per_mass
    if (critical_step < 0.0) critical_step = 0.0
  end function critical_step

  !============================================================================!
  subroutine print_burnup_header()
    if (verbose > 0) then

      if (burnup_option == 0) then
        print *, " *** BURNUP CALCULATION WITH USER-SPECIFED BURNUP STEPS ***"
      elseif (burnup_option == 1) then
        print *, " *** BURNUP CALCULATION FOR DETERMINATION OF CYCLE LENGTH ***"
      end if
      print *, ""
      print *, &
        "   -------------------------------------------------------------------"
      print *, &
        "    STEP   BURNUP     FPD     MAX APPF  MAX APPF    KEFF   APPX.CYC.L "
      print *, &
        "          [GWd/MTU]  [day]     [cycle]   [step]            [GWd/MTU] "
      print *, &
        "   -------------------------------------------------------------------"
    end if
  end subroutine print_burnup_header

  !============================================================================!
  subroutine print_burnup(i, bu, fpd, mappf_c, mppff_s, k, cl)
     integer, intent(in) :: i
     double precision, intent(in) :: bu, fpd, mappf_c, mppff_s, k, cl
     if (verbose > 0) then
       print '(a, i3, 7f10.4)', "     ", i, bu, fpd, mappf_c, mppff_s, k, cl
       if (i == number_burnup_steps) then
         print *, "   -----------------------------------&
           &--------------------------------"
       end if
     end if
  end subroutine print_burnup

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

