!==============================================================================!
! MODULE: utilities
!
!> @author Jeremy Roberts
!> @brief Routines for printing output, etc.
!==============================================================================!

module utilities

  use geometry, only: number_assemblies, stencil_dimension, stencil, &
                      number_per_row, pattern

  implicit none

  !============================================================================!
  !> @brief Make a printable, 2-D map of assembly values
  !============================================================================!
  interface print_map
    module procedure print_map_d, print_map_i
  end interface print_map

  !> An array indexed by geometry has values matched to assembly location
  logical, parameter :: GEOMETRY_INDEXED=.false.
  !> A material-indexed array's values go with a material, not a location
  logical, parameter :: MATERIAL_INDEXED=.true.

contains

  !============================================================================!
  subroutine print_map_d(f, s, flag)
    double precision, dimension(:), intent(in) :: f
    character(len = *), intent(in) :: s
    logical, intent(in) :: flag
    ! local
    double precision, allocatable, dimension(:, :) :: map
    integer :: i, j, k
    
    allocate(map(stencil_dimension, stencil_dimension))
    map = 0.0
    
    do i = 1, stencil_dimension
      do j = 1, stencil_dimension
        if (stencil(i, j) .gt. 0) then
          if (flag) then
            map(i, j) = f(pattern(stencil(i, j)))
          else
            map(i, j) = f(stencil(i, j))
          end if
        end if
      end do
    end do

    k = 0
    print *, ""
    print *, "*** ", s, " *** "
    print *, ""
    do i = 1, stencil_dimension
      if (i > 1) k = 1
      do j = 1, number_per_row(i) + k
        if ((i == 1) .or. (j > 1 .and. i > 1)) then
          write(*, ' (f10.4) ', advance='no') map(i, j)
        else
          write(*, ' (a10) ', advance='no') "            "
        end if
      end do
      print *, ""
    end do
    print *, ""
    
  end subroutine print_map_d

  !============================================================================!
  subroutine print_map_i(f, s, flag)
    integer, dimension(:), intent(in) :: f
    character(len = *), intent(in) :: s
    logical, intent(in) :: flag
    ! local
    integer, allocatable, dimension(:, :) :: map
    integer :: i, j, k

    allocate(map(stencil_dimension, stencil_dimension))
    map = 0

    do i = 1, stencil_dimension
      do j = 1, stencil_dimension
        if (stencil(i, j) .gt. 0) then
          if (flag) then
            map(i, j) = f(pattern(stencil(i, j)))
          else
            map(i, j) = f(stencil(i, j))
          end if
        end if
      end do
    end do

    k = 0
    print *, ""
    print *, "*** ", s, " *** "
    print *, ""
    do i = 1, stencil_dimension
      if (i > 1) k = 1
      do j = 1, number_per_row(i) + k
        if ((i == 1) .or. (j > 1 .and. i > 1)) then
          write(*, ' (i6) ', advance='no') map(i, j)
        else
          write(*, ' (a6) ', advance='no') "            "
        end if
      end do
      print *, ""
    end do
    print *, ""

  end subroutine print_map_i

end module utilities
