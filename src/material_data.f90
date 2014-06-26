!==============================================================================!
! MODULE: material_data
!
!> @author Jeremy Roberts
!> @brief  Two-group and related material data for each possible assembly
!==============================================================================!
module material_data

  use nuclear_data, only: NOBP, IFBA, WABA, GAD, &
                          set_flare_data, set_two_group_data

  !> Number of materials
  integer :: number_materials

  !> @name Material source options
  !> @{
  !> Use the data model based on fits to CASMO output
  integer, parameter :: MODEL_MATERIAL = 1
  !> Use input data from the user
  integer, parameter :: USER_MATERIAL = 2
  !> Use tabulated data from a database (not implemented)
  integer, parameter :: DB_MATERIAL = 3
  !> Material source option (default is the internal model)
  integer :: material_source = MODEL_MATERIAL
  !> @}

  !> @name Two-group cross-section data
  !> @{
  !> Group 1 diffusion coefficient
  double precision, allocatable, dimension(:) :: D1
  !> Group 2 diffusion coefficient
  double precision, allocatable, dimension(:) :: D2
  !> Group 1 absorption cross section
  double precision, allocatable, dimension(:) :: A1
  !> Group 2 absorption cross section
  double precision, allocatable, dimension(:) :: A2
  !> Group 1 nu times fission cross section
  double precision, allocatable, dimension(:) :: F1
  !> Group 2 nu times fission cross section
  double precision, allocatable, dimension(:) :: F2
  !> Group 1 to 2 scattering cross section
  double precision, allocatable, dimension(:) :: S12
  !> Average number of fission neutrons
  double precision, allocatable, dimension(:) :: NU
  !> @}

  !> @name Data needed for FLARE model
  !> @{
  !> Infinite multiplication factor
  double precision, allocatable, dimension(:) :: KINF
  !> Migration area
  double precision, allocatable, dimension(:) :: M2
  !> @}

  !> Energy release per fission (J)
  double precision, allocatable, dimension(:) :: KAPPA

  !> @name Data required for the internal model (for each unique assembly)
  !> @{
  !> Burnup (GWd/MTU), which can *change* during cycle depletions
  double precision, allocatable, dimension(:) :: B
  !> Enrichment (initial weight-percent)
  double precision, allocatable, dimension(:) :: E
  !> Burnable poison type (none, ifba, waba, or gad)
  integer, allocatable, dimension(:) :: BP
  !> @}

contains

  !=============================================================================
  !> @brief Allocate the group constants.
  !=============================================================================
  subroutine allocate_material_data(n)
    integer, intent(in) :: n
    allocate(D1(n), D2(n), A1(n), A2(n), F1(n), F2(n), &
             S12(n), NU(n), KINF(n), M2(n), KAPPA(n))
    if (material_source == MODEL_MATERIAL) then
      allocate(B(n), E(n), BP(n))
    end if
  end subroutine allocate_material_data

  !=============================================================================
  !> @brief Deallocate the group constants.
  !=============================================================================
  subroutine deallocate_material_data()
    if (allocated(D1)) then
      deallocate(D1, D2, A1, A2, F1, F2, S12, NU, KINF, M2, KAPPA)
    end if
    if (allocated(B)) then
      deallocate(B, E, BP)
    end if
  end subroutine deallocate_material_data

  !=============================================================================
  !> @brief Compute FLARE constants
  !=============================================================================
  subroutine compute_flare_parameters()
    integer :: i
    if (material_source == MODEL_MATERIAL) then
      do i = 1, number_materials
        call set_flare_data(B(i), E(i), BP(i), KINF(i), M2(i), KAPPA(i))
      end do
      KINF = KINF * 0.99_8
    else
      do i = 1, number_materials
        KINF(i) = (F1(i) + F2(i) * S12(i)/A2(i)) / (A1(i)+S12(i))
        M2(i) = D1(i)/(A1(i)+S12(i)) + D2(i)/A2(i)
      end do
      KAPPA = 3.22e-11_8 ! roughly 200 MeV/fission
    end if
  end subroutine compute_flare_parameters

  !=============================================================================
  !> @brief Compute two-group constants from model (useful for testing)
  !=============================================================================
  subroutine compute_two_group_data()
    integer :: i
    if (material_source == MODEL_MATERIAL) then
      do i = 1, number_materials
        call set_two_group_data(B(i), E(i), BP(i), D1(i), D2(i), A1(i), A2(i), &
                                F1(i), F2(i), S12(i), NU(i))
      end do
    else
      stop "Internal data model not used, so cross sections not computed!"
    end if
  end subroutine compute_two_group_data

end module material_data
