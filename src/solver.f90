!==============================================================================!
! MODULE: solver
!
!> @author Jeremy Roberts
!> @brief  Provides an implementation of the FLARE model 
!>
!> Several solvers modes are implemented, including a standard eigenvalue 
!> iteration (with or without feedback), critical boron search, and depletion
!==============================================================================!
module solver

  use coefficients
  use geometry, only: pattern, number_assemblies, number_neighbors, neighbors
  use state
  use material_data

  implicit none
  
  !> Solver mode
  integer :: run_mode = 2
  !> Number of burnup steps
  integer :: max_burnup_steps = 0
  !> Axial leakage
  real(8) :: axial_buckling = 0.0001_8
  !> Differential boron worth (pcm per ppm)
  real(8) :: boron_worth = -10_8
  !> Eigenvalue tolerance (absolute, relative error in pcm)
  real(8) :: k_tol = 1.0_8
  !> Fission source tolerance (on maximum, absolute, relative error)
  real(8) :: s_tol = 0.001_8
  !> Temperature tolerance (on maximum, absolute, relative error)
  real(8) :: t_tol = 0.1_8
  !> Boron tolerance (absolute error in ppm)
  real(8) :: b_tol = 2.0_8
  !> Maximum number of inners
  integer :: max_inner_iters = 10
  !> Maximum number of outers
  integer :: max_outer_iters = 100
  !> Maximum number of boron search iterations
  integer :: max_boron_iters = 10

  !> Flag to print solver diagnostics
  integer :: verbose = 0


  !> Reactor power (thermal) in GW
  real(8) :: reactor_power = 0.0_8
  !> Assembly HM mass (MTU), from WH PWR book for 4-loop plant with "OFA" fuel
  real(8) :: assembly_mass = .423_8

  logical :: do_burnup = .false.
  logical :: do_feedback = .false.
  logical :: do_boron = .false.

  !> Burnup steps (full power days)
  real(8), allocatable, dimension(:) :: burnup_steps

  real(8), private :: average_assembly_power
  real(8), private :: power_per_mass
  real(8), private :: mappf_cycle
  
  logical :: depletion, boron, feedback 

contains

  !============================================================================!
  !> @brief Initialize.
  !============================================================================!
  subroutine initialize_solver()
    allocate (burnup_steps(max_burnup_steps))
  end subroutine initialize_solver
  
  !============================================================================!
  !> @brief Finalize.
  !============================================================================!
  subroutine finalize_solver()
    if (allocated(burnup_steps)) then
      deallocate(burnup_steps)
    end if
  end subroutine finalize_solver

  !============================================================================!
  !> @brief Solve the problem
  !============================================================================!
  subroutine solve()

    do_burnup = modulo(run_mode, 2) == 1
    do_feedback = modulo(run_mode/2, 2) == 1
    do_boron = modulo(run_mode/4, 2) == 1

    if (verbose > 0) then
      print *, " CYCLE DEPLETION: ", do_burnup
      print *, "THERMAL FEEDBACK: ", do_feedback
      print *, "CRICTICAL BURNUP: ", do_boron
    end if

    if (do_burnup) then
      call burn()
    else
      call balance()
    end if

  end subroutine solve


  !============================================================================!
  !> @brief Solve the k-eigenvalue problem for a single configuration.
  !>
  !> The solution approach is pretty simple, using a series of Jacobi inner
  !> iterations followed by outer k-updates.  The bounds were found to yield
  !> convergence in about 70 iterations for a "typical" problem.   (Having 
  !> some inners helps avoid "false" convergence, too)
  !============================================================================!
  subroutine balance()

    integer :: i, j, p, q, qq
    real(8) ::          k,                       & ! temporary current keff
                        k_o,                     & ! temporary past keff
                        s(number_assemblies),    & ! fission density
                        s_o(number_assemblies),  & ! temporay density (inners)
                        s_oo(number_assemblies), & ! temporary density (outers)
                        k_num,                   & ! numerator in keff
                        k_den,                   & ! denomenator in keff
                        serr,                    & ! density residual
                        kerr,                    & ! keff residual
                        mean_s                     !
    real(8) :: kinf_leak(number_assemblies) ! kinf adjusted for axial leakage

    ! Initialize the fission source and normalize
    s = 1.0 / sqrt(dble(number_assemblies))

    ! Guess k = 1
    k = 1.0
    
    ! Update nodal parameters
    call compute_flare_parameters()
    
    ! Update coefficients
    call build_coefficients()

    ! Compute leakage-corrected infinite multiplication factor
    do p = 1, number_assemblies
      kinf_leak(p) = KINF(pattern(p)) / (1.0 + M2(pattern(p))*axial_buckling)
    end do

    ! Outer iteration
    OUTER: do j = 1, max_outer_iters
      s_oo = s       
         
      ! Inner iteration
      INNER: do i = 1, max_inner_iters
        s_o = s
        do p = 1, number_assemblies
          s(p) = wpp(p) * s_o(p)
          do q = 1, 4
            if (neighbors(p, q) .gt. 0) then
              qq   = neighbors(p, q)
              s(p) = s(p) + wqp(qq)*s_o(qq)
            end if
          end do
          s(p) = s(p) * kinf_leak(p) / k
        end do
      end do INNER
      s = s / norm(s)

      ! Update k
      k_o = k
      k_num = 0.0
      k_den = 0.0
      do p = 1, number_assemblies
        k_num = k_num + s(p)*wleak(p)
        k_den = k_den + s(p)/kinf_leak(p)
      end do
      k = (sum(s) - k_num) / k_den

      ! Update temperatures
      if (do_feedback) then
        ! call thermal_feedback()
      end if

      ! Update boron concentration
      if (do_boron) then
        ! call critical_boron_search(1.0)
      end if

      ! Update errors and check for convergence
      kerr = abs(k - k_o)
      serr = norm(s - s_oo) 
      if  (kerr < k_tol .and. serr < s_tol) then
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
  end subroutine balance
  
  !============================================================================!
  !> @brief Determine the critical boron concentration.
  !============================================================================!
  subroutine critical_boron_search(target_keff)
    real(8), intent(in) :: target_keff ! Target multiplication factor
    real(8) :: BC0, keff0
    integer :: i
    
    BC0 = BC
    BC  = BC0 + (target_keff-keff)/boron_worth
    
    do i = 1, max_boron_iters
    
      if (verbose .ge. 2) then
        print *, "BC ITER ", i, " BC = ", BC
      end if
    
      if (abs(BC - BC0) < b_tol) then
        exit
      end if
      
      ! Update the eigenvalue
      keff0 = keff
      call balance()
            
      ! Update the estimated differential boron worth.  
      boron_worth = (keff-keff0)/(BC-BC0)*1.0e5
      
      ! Update the critical boron concentration
      BC0 = BC
      BC = BC0 + 1.0e5*(target_keff-keff)/boron_worth
      
    end do
    
  end subroutine critical_boron_search

  !============================================================================!
  !> @brief Perform a depletion sequence
  !============================================================================!
  subroutine burn()

    integer :: i
    real(8) :: burnup_step, & !
               fpd,         & !
               burnup,      & !
               burnup1,     & !
               keff1
    fpd = 0.0
    burnup = 0.0
    burnup1 = 0.0
    keff1 = 0.0

    call balance()
    mappf_cycle = mappf
    
    if (max_burnup_steps == 0) then
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
            & than the number of assemblies for cycle depletions."
    end if
    do i = 1, number_materials
      if (count(pattern == i) > 1) then
        stop "Non-unique material assignment for a burnup problem &
              & is not allowed."
      end if
    end do

    call print_burnup_header()
    call print_burnup(0, burnup, fpd, mappf, mappf, keff, cycle_length)

    BURNITS: do i = 1, max_burnup_steps

      ! select the burnup step
      burnup_step = burnup_steps(i)
      if (i > 2) then
        burnup_step = critical_step(keff, keff1, burnup, burnup1)
        ! estimated cycle length
        cycle_length = burnup_step * power_per_mass + burnup
        if (burnup_step > burnup_steps(i) .or. burnup_step == 0.0) then
          burnup_step = burnup_steps(i)
        end if
      end if

      call update_assembly_burnup(burnup_step)

      ! record old values and update new ones
      burnup1 = burnup
      keff1   = keff
      burnup  = burnup + burnup_step * power_per_mass
      fpd     = fpd + burnup_step
      call balance()
      if (mappf > mappf_cycle) then
        mappf_cycle = mappf
        mappf_bu    = burnup
      end if

      call print_burnup(i, burnup, fpd, mappf_cycle, mappf, keff, cycle_length)

      if (keff <= 1.0 .or. abs(burnup-burnup1) <= k_tol) exit

    end do BURNITS

    cycle_length = burnup + &
                   critical_step(keff,keff1,burnup,burnup1)*power_per_mass

  end subroutine burn

  !============================================================================!
  !> @brief Update the burnup for each assembly
  !============================================================================!
  subroutine update_assembly_burnup(step)
    real(8), intent(in) :: step
    integer :: i
    do i = 1, number_assemblies
      B(pattern(i)) = B(pattern(i)) + &
                      assembly_peaking(i) * average_assembly_power * step
    end do
  end subroutine update_assembly_burnup

  !============================================================================!
  !> @brief Estimate step to reach critical based on linear reactivity
  !============================================================================!
  real(8) function critical_step(k0, k1, b0, b1)
    real(8), intent(in) :: k0, k1, b0, b1
    real(8) :: p0, p1
    p0 = (k0-1.0)/k0
    p1 = (k1-1.0)/k1
    critical_step = (-(1.*(p1*b0-1.*b1*p0))/(p0-1.*p1)-b0) / power_per_mass
    if (critical_step < 0.0) critical_step = 0.0
  end function critical_step

  !============================================================================!
  subroutine print_burnup_header()
    if (verbose > 0) then
      !if (burnup_option == 0) then
      !  print *, " *** BURNUP CALCULATION WITH USER-SPECIFED BURNUP STEPS ***"
      !elseif (burnup_option == 1) then
      !  print *, " *** BURNUP CALCULATION FOR DETERMINATION OF CYCLE LENGTH ***"
      !end if
      !print *, " *** CYCLE LENGTH CALCULATION***"
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
     real(8), intent(in) :: bu, fpd, mappf_c, mppff_s, k, cl
     if (verbose > 0) then
       print '(a, i3, 7f10.4)', "     ", i, bu, fpd, mappf_c, mppff_s, k, cl
       if (i == max_burnup_steps) then
         print *, "   -----------------------------------&
           &--------------------------------"
       end if
     end if
  end subroutine print_burnup

  !============================================================================!
  !> @brief Compute the L2 norm of an array of values
  !============================================================================!
  real(8) function norm(v)
    real(8), intent(in) :: v(:)
    integer :: i
    norm = 0.0
    do i = 1, size(v)
      norm = norm + v(i)*v(i)
    end do
    norm = sqrt(norm)
  end function norm

end module solver
