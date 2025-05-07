module quadtree_class
   use iso_fortran_env, only: error_unit
   use precision, only: WP
   implicit none
   private

   integer, parameter, public :: NUM_SUBNODES = 4

   public :: QuadtreeNode, construct_node, destruct_node

   type :: QuadtreeNode
      ! Common node state
      real(WP) :: x_min, y_min, dx, dy
      logical :: is_leaf

      ! Data in case of a leaf node
      integer :: size
      integer :: num_points_per_leaf
      integer, dimension(:), allocatable :: idxs

      ! Data in case of non-leaf node
      type(QuadtreeNode), dimension(:), allocatable :: nodes

   contains

      procedure, public :: add_point
      procedure, public :: print => print_node
      procedure, public :: contains => node_contains
      procedure, public :: find => node_find
      procedure, public :: circle_intersects => circle_intersects_node

      procedure, private :: split => split_node
      procedure, private :: get_subnode_idx => get_subnode_idx

   end type QuadtreeNode

contains

   ! -----------------------------------------------------------------------------------------------
   subroutine construct_node(self, x_min, y_min, dx, dy, num_points_per_leaf)
      type(QuadtreeNode), intent(out) :: self
      real(WP), intent(in) :: x_min, y_min, dx, dy
      integer, intent(in) :: num_points_per_leaf

      self%x_min = x_min
      self%y_min = y_min
      self%dx    = dx
      self%dy    = dy

      if (num_points_per_leaf.le.0) then
         write (error_unit, "('num_points_per_leaf must be greater than 0 but is ',I5)") num_points_per_leaf
         error stop
      end if
      self%num_points_per_leaf = num_points_per_leaf

      self%is_leaf = .true.
      self%size    = 0
      allocate(self%idxs(self%num_points_per_leaf))
   end subroutine construct_node

   ! -----------------------------------------------------------------------------------------------
   recursive subroutine destruct_node(this)
      type(QuadtreeNode) :: this
      integer :: i

      if (this%is_leaf) then
         deallocate(this%idxs)
      else
         do i=1,NUM_SUBNODES
            call destruct_node(this%nodes(i))
         end do
         deallocate(this%nodes)
      end if
   end subroutine destruct_node

   ! -----------------------------------------------------------------------------------------------
   recursive subroutine add_point(this, idx, xs, ys)
      class(QuadtreeNode), intent(inout) :: this
      integer, intent(in) :: idx
      real(WP), dimension(:), intent(in) :: xs, ys
      integer :: subnode_idx
      
      if (.not.this%contains(xs(idx), ys(idx))) then
         write (error_unit, "('Error: Point (',ES14.6,', ',ES14.6,') is not in node.')") xs(idx), ys(idx )
         error stop
      end if

      if (this%is_leaf) then

         if (this%size.lt.this%num_points_per_leaf) then
            this%size = this%size + 1
            this%idxs(this%size) = idx
            return
         else 
            call this%split(xs, ys)
            call this%add_point(idx, xs, ys)
            return
         end if

      else

         subnode_idx = this%get_subnode_idx(xs(idx), ys(idx))
         call this%nodes(subnode_idx)%add_point(idx, xs, ys)

      end if
   end subroutine add_point

   ! -----------------------------------------------------------------------------------------------
   subroutine split_node(this, xs, ys)
      class(QuadtreeNode), intent(inout) :: this
      real(WP), dimension(:), intent(in) :: xs, ys
      
      integer :: i
      real(WP) :: half_dx, half_dy
      real(WP) :: x_split, y_split


      half_dx = 0.5_WP * this%dx
      half_dy = 0.5_WP * this%dy
      x_split = this%x_min + half_dx
      y_split = this%y_min + half_dy

      allocate(this%nodes(NUM_SUBNODES))
      
      ! Bottom-left
      call construct_node(this%nodes(1), this%x_min, this%y_min, half_dx, half_dy, this%num_points_per_leaf)
      ! Bottom-right
      call construct_node(this%nodes(2), x_split,    this%y_min, half_dx, half_dy, this%num_points_per_leaf)
      ! Top-left
      call construct_node(this%nodes(3), this%x_min, y_split,    half_dx, half_dy, this%num_points_per_leaf)
      ! Top-left
      call construct_node(this%nodes(4), x_split,    y_split,    half_dx, half_dy, this%num_points_per_leaf)

      ! Clean up the contained indices
      this%is_leaf = .false.
      do i=1,this%size
         call this%add_point(this%idxs(i), xs, ys)
      end do
      this%size = 0
      deallocate(this%idxs)
   end subroutine split_node

   ! -----------------------------------------------------------------------------------------------
   pure function node_contains(this, x, y) result(res)
      class(QuadtreeNode), intent(in) :: this
      real(WP), intent(in) :: x, y
      logical :: res
      
      res = x.ge.this%x_min           .and. &
          & x.le.this%x_min + this%dx .and. &
          & y.ge.this%y_min           .and. &
          & y.le.this%y_min + this%dy
   end function node_contains

   ! -----------------------------------------------------------------------------------------------
   pure function get_subnode_idx(this, x, y) result(idx)
      class(QuadtreeNode), intent(in) :: this
      real(WP), intent(in) :: x, y
      real(WP) :: x_split, y_split
      integer :: idx

      x_split = this%x_min + 0.5_WP * this%dx
      y_split = this%y_min + 0.5_WP * this%dy
      idx = merge(2, 0, y.gt.y_split) + merge(1, 0, x.gt.x_split) + 1
   end function get_subnode_idx

   ! -----------------------------------------------------------------------------------------------
   recursive subroutine node_find(this, xc, yc, radius, xs, ys, num_found, found)
      class(QuadtreeNode), intent(in) :: this
      real(WP), intent(in) :: xc, yc, radius          ! Circle in which the elements are found
      real(WP), dimension(:), intent(in) :: xs, ys
      integer, intent(inout) :: num_found
      integer, dimension(:), intent(inout) :: found
      integer :: i, idx

      
      if (this%is_leaf) then 
         do i=1,this%size
            idx = this%idxs(i)
            if (((xs(idx) - xc)**2 + (ys(idx) - yc)**2) .le. radius**2) then
               call append_to_array(num_found, found, idx)
            end if
         end do
      else 
         do i=1,NUM_SUBNODES
            if (this%nodes(i)%circle_intersects(xc, yc, radius)) then
               call this%nodes(i)%find(xc, yc, radius, xs, ys, num_found, found)
            end if
         end do
      end if
   end subroutine node_find

   ! -----------------------------------------------------------------------------------------------
   subroutine append_to_array(count, array, element)
      integer, intent(inout) :: count
      integer, dimension(:), intent(inout) :: array
      integer, intent(in) :: element

      if (count.ge.size(array)) then
         write (error_unit, "('ERROR: Cannot append into array with size ',I5,', are at max capacity')") size(array)
         error stop
      end if

      count = count + 1
      array(count) = element
   end subroutine append_to_array

   ! -----------------------------------------------------------------------------------------------
   pure function circle_intersects_node(this, xc, yc, radius) result(intersects)
      class(QuadtreeNode), intent(in) :: this
      real(WP), intent(in) :: xc, yc, radius
      logical :: intersects

      real(WP) :: tmp_x, tmp_y

      if (xc.lt.this%x_min) then 
         tmp_x = this%x_min
      else if (xc.gt.this%x_min + this%dx) then
         tmp_x = this%x_min + this%dx
      else
         tmp_x = xc
      end if

      if (yc.lt.this%y_min) then 
         tmp_y = this%y_min
      else if (yc.gt.this%y_min + this%dy) then
         tmp_y = this%y_min + this%dy
      else
         tmp_y = yc
      end if

      tmp_x = tmp_x - xc
      tmp_y = tmp_y - yc

      intersects = (tmp_x**2 + tmp_y**2 ).le.radius**2;
   end function circle_intersects_node

   ! -----------------------------------------------------------------------------------------------
   subroutine print_node(this, recursive)
      class(QuadtreeNode), intent(in) :: this
      logical, intent(in), optional :: recursive

      if (present(recursive)) then
         call print_node_impl(this, recursive, 1)
      else
         call print_node_impl(this, .false., 1)
      end if
   end subroutine print_node

   recursive subroutine print_node_impl(this, recursive, indent)
      class(QuadtreeNode), intent(in) :: this
      logical, intent(in) :: recursive
      integer, intent(in) :: indent
      character(128) :: fmt_str
      integer :: i

      ! TODO: Require an indent of at least 1, otherwise the formatting breaks
      if (recursive) then
         write (fmt_str, "('(',I3,'X,',A,')')") indent, "'------------------------'"; print fmt_str
      end if
      write (fmt_str, "('(',I3,'X,',A,')')") indent, "'x_min   = ',ES14.6"; print fmt_str, this%x_min
      write (fmt_str, "('(',I3,'X,',A,')')") indent, "'y_min   = ',ES14.6"; print fmt_str, this%y_min
      write (fmt_str, "('(',I3,'X,',A,')')") indent, "'dx      = ',ES14.6"; print fmt_str, this%dx
      write (fmt_str, "('(',I3,'X,',A,')')") indent, "'dy      = ',ES14.6"; print fmt_str, this%dy
      write (fmt_str, "('(',I3,'X,',A,')')") indent, "'is_leaf = ',L1";     print fmt_str, this%is_leaf

      if (this%is_leaf) then
         write (fmt_str, "('(',I3,'X,',A,')')") indent, "'size    = ',I5,' ('I5')'"
         print fmt_str, this%size, this%num_points_per_leaf
         write (fmt_str, "('(',I3,'X,',A,')')") indent, "'idxs    = ',*(I5,', ')"
         print fmt_str, this%idxs(:this%size)
      else if (recursive) then
         do i=1,NUM_SUBNODES
            call print_node_impl(this%nodes(i), recursive, indent+3)
         end do
      end if
   end subroutine print_node_impl

end module quadtree_class
