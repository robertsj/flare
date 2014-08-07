!==============================================================================!
! MODULE: geometry
!
!> @author Jeremy Roberts
!>
!> @brief Geometry
!>
!> The problem geometry is defined in terms of a "stencil" that specifies
!> where fuel exists, where reflector exists, and where void exists.  Only
!> quarter cores can be modeled.  An example small model is 
!> @verbatim
!>       [  2,  1,  1,  1,  1,  0 , 
!>         -1,  1,  1,  1,  1,  0 ,
!>         -1,  1,  1,  1,  1,  0 ,
!>         -1,  1,  1,  1,  0,  0 ,
!>         -1,  1,  1,  0,  0,  0 , 
!>          0,  0,  0,  0,  0,  0 ]
!> @endverbatim
!> where a nonzero represents fuel (different values denoting possible 
!> regions), zero representing reflector, and -1 indicating rotational
!> symmetry.
!>
!> From this stencil, the connectivity operator M is constructed.  This 
!> is applied to outgoing boundary partial currents to reroute them as
!> incident currents in adjacent cells.  To save on computation, albedos
!> are incorporated directly into M at reflector interfaces.
!> 
!> This module contains some self-building routines along with several
!> subroutines that comprise a Python interface.
!>
!==============================================================================!
module geometry

  implicit none

  !> Stencil
  integer, allocatable :: stencil(:, :)
  !> Loading pattern
  integer, allocatable :: pattern(:)
  !> Neighbor list
  integer, allocatable :: neighbors(:, :)
  !> Number of neighbors for each bundle
  integer, allocatable :: number_neighbors(:)
  !> Number of assemblies for each row
  integer, allocatable :: number_per_row(:)
  !> Number of assemblies
  integer :: number_assemblies
  !> Stencil dimension
  integer :: stencil_dimension
  !> Size of assemblies (cm)
  double precision :: delta

contains

  !============================================================================!
  !> @brief Initialize geometry.
  !>
  !> This *must* be called before build_geometry.
  !============================================================================!
  subroutine initialize_geometry()
    if (.not. allocated(stencil)) then
      allocate(stencil(stencil_dimension, stencil_dimension))
    end if
    if (.not. allocated(pattern)) allocate(pattern(number_assemblies))
  end subroutine initialize_geometry


  !============================================================================!
  !> @brief Deallocate geometry.
  !>
  !> This must be called before loading a new core configuration.
  !============================================================================!
  subroutine deallocate_geometry()
    if (allocated(stencil)) deallocate(stencil)
    if (allocated(pattern)) deallocate(pattern)
    if (allocated(neighbors)) deallocate(neighbors)
    if (allocated(number_neighbors)) deallocate(number_neighbors)
    if (allocated(number_per_row)) deallocate(number_per_row)
  end subroutine deallocate_geometry

  !============================================================================!
  !> @brief Build the neighbor list and related items.
  !>
  !> The stencil becomes the cardinal index map.  Neighbors are defined by
  !> their cardinal location.  That is, a node i has up to four neighbors, 
  !> each of which has an index j.  This index can be used to index into
  !> pattern, the value of which defines the material index.
  !>
  !============================================================================!
  subroutine build_geometry()
    integer :: i, j, k, n
    integer :: number_per_col(stencil_dimension) ! number bundles per row

    allocate(number_per_row(stencil_dimension))

    ! go through the stencil once and record dimensions
    n = size(stencil, 1)
    number_per_row = 0
    number_per_col = 0
    k = 0
    do i = 1, n
      do j = 1, n
        if (stencil(i, j) .gt. 0) then
          k = k + 1
          ! The pattern is the initial map given on stencil.  The client
          ! can reset with set_pattern.
          pattern(k) = stencil(i, j)
          ! Stencil will now hold cardinal location index.
          stencil(i, j)  = k 
          ! Rotational symmetries
          number_per_row(i) = number_per_row(i) + 1
          number_per_col(j) = number_per_col(j) + 1
        else if (stencil(i, j) .eq. 0) then
          continue
        else if (stencil(i, j) .eq. -1) then
          stencil(i, j) = 0
        end if
      end do
    end do

    ! Subtract the first element, since we handle it explicitly below.
    number_per_row(1) = number_per_row(1) - 1

    ! My neighbors.
    allocate(neighbors(number_assemblies, 4))
    neighbors = -1

    ! Central node is always adjacent to node 2 all four times.
    neighbors(1, :) = 2

    ! do other nodes
    k = 1
    do i = 1, n
      do j = 1, n
        ! skip the first element, elements defined by symmetry, and reflector
        if (((i .eq. 1) .and. (j .eq. 1)) .or. stencil(i, j) .le. 0) then
          ! nothing
        else
          k = k + 1
          ! top
          if (j .lt. number_per_row(i) + 1) then
            neighbors(k, 2) = stencil(i, j+1)
          else
            neighbors(k, 2) = 0             ! reflector
          end if
          ! bottom
          if (j .gt. 2) then
            neighbors(k, 1) = stencil(i, j-1)
          else
            neighbors(k, 1) = stencil(1, i) ! rotational
          end if
          ! right
          if (i .lt. number_per_col(j) + 1) then
            neighbors(k, 3) = stencil(i+1, j)
          else
            neighbors(k, 3) = 0             ! reflector
          end if
          ! left
          if (i .gt. 1) then
            neighbors(k, 4) = stencil(i-1, j)
          else
            neighbors(k, 4) = stencil(j, 2) ! rotational
          end if
        end if
      end do
    end do

    allocate(number_neighbors(number_assemblies))
    do i = 1, number_assemblies
      k = 0
      do j = 1, 4
        if (neighbors(i, j) .gt. 0) k = k + 1
      end do
      number_neighbors(i) = k
    end do

    ! Add back to the first element for use in printing
    number_per_row(1) = number_per_row(1) + 1

  end subroutine build_geometry

  !> @name Python Interface
  !> @{

  !============================================================================!
  !> @brief Set the stencil.
  !>
  !> Note, the second argument is IMPLICIT when called from Python.  That is
  !> the client can simply execute 
  !> @code
  !>   geometry.set_stencil(s)
  !> @endcode
  !> @param   s     A 2-D integer array specifying fuel and reflector regions.
  !> @param   n     Length of one dimension of the stencil array (IMPLICIT)
  !============================================================================!
  subroutine set_stencil(s, n)
    integer, intent(in) :: n, s(n, n)
    if (.not. allocated(stencil)) allocate(stencil(n, n))
    stencil(:, :) = s(:, :)
  end subroutine set_stencil

  !============================================================================!
  !> @brief Print the pattern using default array formatting.
  !============================================================================!
  subroutine print_stencil()
    print *, stencil
  end subroutine print_stencil

  !============================================================================!
  !> @brief Set the pattern.
  !>
  !> Note, the second argument is IMPLICIT when called from Python.  That is
  !> the client can simply execute 
  !> @code
  !>   geometry.set_pattern(p)
  !> @endcode
  !> @param   p     An integer array specifying assembly locations.
  !> @param   n     Length of the pattern array (IMPLICIT)
  !============================================================================!
  subroutine set_pattern(p, n)
    integer, intent(in) :: n, p(n)
    if (.not. allocated(pattern)) allocate(pattern(n))
    pattern(:) = p(:)
  end subroutine set_pattern

  !============================================================================!
  !> @brief Print the pattern using default array formatting.
  !============================================================================!
  subroutine print_pattern()
    print *, pattern
  end subroutine print_pattern

  !> @}

end module geometry
