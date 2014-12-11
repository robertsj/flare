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
  
  !> Material data source option
  integer :: material_source = 2
  
  !> @name Material source options
  !> @{
  !> User inputs FLARE parameters
  integer, parameter :: MATERIAL_SOURCE_INPUT_FLARE = 0
  !> User inputs two-group parameters
  integer, parameter :: MATERIAL_SOURCE_INPUT_2G = 1
  !> Built-in model 1 (the original)
  integer, parameter :: MATERIAL_SOURCE_BUILT_IN_1 = 2
  !> Built-in model 2 (limited selection but with state variables)
  integer, parameter :: MATERIAL_SOURCE_BUILT_IN_2 = 3
  !> Data read from database for interpolation
  integer, parameter :: MATERIAL_SOURCE_DATABASE = 4
  !> @}

  !> @name Two-group cross-section data
  !> @{
  !> Group 1 diffusion coefficient
  real(8), allocatable, dimension(:) :: D1
  !> Group 2 diffusion coefficient
  real(8), allocatable, dimension(:) :: D2
  !> Group 1 absorption cross section
  real(8), allocatable, dimension(:) :: A1
  !> Group 2 absorption cross section
  real(8), allocatable, dimension(:) :: A2
  !> Group 1 nu times fission cross section
  real(8), allocatable, dimension(:) :: F1
  !> Group 2 nu times fission cross section
  real(8), allocatable, dimension(:) :: F2
  !> Group 1 to 2 scattering cross section
  real(8), allocatable, dimension(:) :: S12
  !> Average number of fission neutrons
  real(8), allocatable, dimension(:) :: NU
  !> @}

  !> @name Data needed for FLARE model
  !> @{
  !> Infinite multiplication factor
  real(8), allocatable, dimension(:) :: KINF
  !> Migration area
  real(8), allocatable, dimension(:) :: M2
  !> @}

  !> Energy release per fission (J)
  real(8), allocatable, dimension(:) :: KAPPA

  !> @name Data required for internal model 1
  !> @{
  !> Burnup (GWd/MTU), which can *change* during cycle depletions
  real(8), allocatable, dimension(:) :: B
  !> Enrichment (initial weight-percent)
  real(8), allocatable, dimension(:) :: E
  !> Burnable poison type (none, ifba, waba, or gad)
  integer, allocatable, dimension(:) :: BP
  !> @}
  
  !> @name Data required for internal model 2
  !> @{
  !> Historical fuel temperature (K)
  real(8), allocatable, dimension(:) :: HT_F
  !> Historical coolant temperature (K)
  real(8), allocatable, dimension(:) :: HT_C
  !> Historical boron concentration (ppm)
  integer, allocatable, dimension(:) :: HBC
  !> @}
  
  !> Database file name
  character(80) :: database_name

  ! Note that instantaneous variables are stored in the state vector
  ! because those values are not material properties

contains

  !=============================================================================
  !> @brief Allocate the group constants.
  !=============================================================================
  subroutine initialize_material_data(n)
    integer, intent(in) :: n
    allocate(D1(n), D2(n), A1(n), A2(n), F1(n), F2(n), &
             S12(n), NU(n), KINF(n), M2(n), KAPPA(n))
    if (material_source == MATERIAL_SOURCE_BUILT_IN_1) then
      allocate(B(n), E(n), BP(n))
    else if (material_source == MATERIAL_SOURCE_BUILT_IN_2) then
      allocate(B(n), BP(n), HT_F(n), HT_C(n), HBC(n))
    end if
  end subroutine initialize_material_data

  !=============================================================================
  !> @brief Deallocate the group constants.
  !=============================================================================
  subroutine finalize_material_data()
    if (allocated(D1)) then
      deallocate(D1, D2, A1, A2, F1, F2, S12, NU, KINF, M2, KAPPA)
    end if
    if (allocated(B)) then
      deallocate(B)
    end if
    if (allocated(BP)) then
      deallocate(BP)
    end if
    if (allocated(HT_F)) then
      deallocate(HT_F, HT_C, HBC)
    end if
  end subroutine finalize_material_data

  !=============================================================================
  !> @brief Compute FLARE constants
  !=============================================================================
  subroutine compute_flare_parameters()
    integer :: i
    
    select case(material_source)
    
      case (MATERIAL_SOURCE_INPUT_FLARE)
      
        ! Already set---nothing to do.
      
      case (MATERIAL_SOURCE_INPUT_2G)
      
        do i = 1, number_materials
          KINF(i) = (F1(i) + F2(i) * S12(i)/A2(i)) / (A1(i)+S12(i))
          M2(i) = D1(i)/(A1(i)+S12(i)) + D2(i)/A2(i)
        end do
      
      case (MATERIAL_SOURCE_BUILT_IN_1)
      
        do i = 1, number_materials
          call set_flare_data_1(B(i), E(i), BP(i), KINF(i), M2(i), KAPPA(i))
        end do
      
      case (MATERIAL_SOURCE_BUILT_IN_2)

        do i = 1, number_materials
          call set_flare_data_2(B(i), BP(i), HT_F(i), HT_C(i), HBC(i),         &
                                KINF(i), M2(i), KAPPA(i))
        end do

      case (MATERIAL_SOURCE_DATABASE)
      
        stop "NOT IMPLEMENTED"
      
    end select
    
  end subroutine compute_flare_parameters

  !=============================================================================
  !> @brief Compute two-group constants from model (useful for testing)
  !=============================================================================
  subroutine compute_two_group_data()
    integer :: i
    if (material_source == MATERIAL_SOURCE_BUILT_IN_1) then
      do i = 1, number_materials
        call set_two_group_data(B(i), E(i), BP(i), D1(i), D2(i), A1(i), A2(i), &
                                F1(i), F2(i), S12(i), NU(i))
      end do
    else
      stop "Internal data model 1 not used, so cross sections not computed!"
    end if
  end subroutine compute_two_group_data

end module material_data
