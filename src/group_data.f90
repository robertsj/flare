!==============================================================================!
! MODULE: group_data
!
!> @author Jeremy Roberts
!>
!> @brief Response function and related data
!==============================================================================!
module group_data

  !> Number of materials (nominally, the number of bundles)
  integer :: number_materials
  !> Group 1 diffusion coefficient
  double precision, allocatable, dimension(:) :: D1
  !> Group 2 diffusion coefficient
  double precision, allocatable, dimension(:) :: D2
  !> Group 1 diffusion coefficient
  double precision, allocatable, dimension(:) :: A1
  !> Group 2 diffusion coefficient
  double precision, allocatable, dimension(:) :: A2
  !> Group 1 diffusion coefficient
  double precision, allocatable, dimension(:) :: F1
  !> Group 2 diffusion coefficient
  double precision, allocatable, dimension(:) :: F2
  !> Group 1 to 2 scattering cross section
  double precision, allocatable, dimension(:) :: S12
  !> Group 1 removal cross section
  double precision, allocatable, dimension(:) :: R1
  !> Group 1 energy release per fission
  double precision, allocatable, dimension(:) :: K1
  !> Group 2 energy release per fission
  double precision, allocatable, dimension(:) :: K2

contains

  !=============================================================================
  !> @brief Allocate the group constants.
  !=============================================================================
  subroutine allocate_group_data(n)
    integer, intent(in) :: n

    allocate(D1(n), D2(n), A1(n), A2(n), F1(n), F2(n), &
             S12(n), R1(n), K1(n), K2(n))
   ! D1 = 1.0
  end subroutine allocate_group_data

  !=============================================================================
  !> @brief Deallocate the group constants.
  !=============================================================================
  subroutine deallocate_group_data()

    deallocate(D1, D2, A1, A2, F1, F2, S12, R1, K1, K2)

  end subroutine deallocate_group_data

end module group_data
