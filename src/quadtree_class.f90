module quadtree_class
   use iso_fortran_env, only: error_unit
   use precision, only: WP
   implicit none
   private

   integer, parameter, public :: NUM_SUBNODES = 4

   public :: QuadtreeNode

   type :: QuadtreeNode
      real(WP) :: x_min, y_min, dx, dy
      integer :: num_points_per_leaf

      logical :: is_leaf
      integer :: size
      integer, dimension(:), allocatable :: idxs

      type(QuadtreeNode), dimension(:), allocatable :: nodes

   contains

      procedure, public :: add_point
      procedure, public :: print => print_node
      procedure, public :: contains => node_contains

      procedure, private :: split => split_node
      procedure, private :: get_subnode_idx => get_subnode_idx

   end type QuadtreeNode

   interface QuadtreeNode
      module procedure construct_leaf
   end interface QuadtreeNode

contains

   ! -----------------------------------------------------------------------------------------------
   function construct_leaf(x_min, y_min, dx, dy, num_points_per_leaf) result(self)
      type(QuadtreeNode) :: self
      real(WP), intent(in) :: x_min, y_min, dx, dy
      integer, intent(in) :: num_points_per_leaf

      self%x_min = x_min
      self%y_min = y_min
      self%dx = dx
      self%dy = dy

      if (num_points_per_leaf.le.0) then
         write (error_unit, "('num_points_per_leaf must be greater than 0 but is ',I5)") num_points_per_leaf
         error stop
      end if
      self%num_points_per_leaf = num_points_per_leaf

      self%is_leaf = .true.
      self%size = 0
      allocate(self%idxs(NUM_POINTS_PER_LEAF))
   end function construct_leaf

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
      this%nodes(1) = QuadtreeNode(this%x_min, this%y_min, half_dx, half_dy, this%num_points_per_leaf)
      ! Bottom-right
      this%nodes(2) = QuadtreeNode(x_split,    this%y_min, half_dx, half_dy, this%num_points_per_leaf)
      ! Top-left
      this%nodes(3) = QuadtreeNode(this%x_min, y_split,    half_dx, half_dy, this%num_points_per_leaf)
      ! Top-left
      this%nodes(4) = QuadtreeNode(x_split,    y_split,    half_dx, half_dy, this%num_points_per_leaf)

      ! Clean up the contained indices
      this%is_leaf = .false.
      do i=1,this%size
         call this%add_point(this%idxs(i), xs, ys)
      end do
      this%size = 0
      deallocate(this%idxs)
   end subroutine split_node

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

      if (recursive) then
         write (fmt_str, "('(',I3,'X,',A,')')") indent, "'------------------------'"; print fmt_str
      end if
      write (fmt_str, "('(',I3,'X,',A,')')") indent, "'x_min   = ',ES14.6"; print fmt_str, this%x_min
      write (fmt_str, "('(',I3,'X,',A,')')") indent, "'y_min   = ',ES14.6"; print fmt_str, this%y_min
      write (fmt_str, "('(',I3,'X,',A,')')") indent, "'dx      = ',ES14.6"; print fmt_str, this%dx
      write (fmt_str, "('(',I3,'X,',A,')')") indent, "'dy      = ',ES14.6"; print fmt_str, this%dy
      write (fmt_str, "('(',I3,'X,',A,')')") indent, "'is_leaf = ',L1";     print fmt_str, this%is_leaf

      if (this%is_leaf) then
         write (fmt_str, "('(',I3,'X,',A,')')") indent, "'size    = ',I5,' ('I5')'"; print fmt_str, this%size, this%num_points_per_leaf
         write (fmt_str, "('(',I3,'X,',A,')')") indent, "'idxs    = ',*(I5,', ')"
         print fmt_str, this%idxs(:this%size)
      else if (recursive) then
         do i=1,NUM_SUBNODES
            call print_node_impl(this%nodes(i), recursive, indent+3)
         end do
      end if
   end subroutine print_node_impl

   pure function node_contains(this, x, y) result(res)
      class(QuadtreeNode), intent(in) :: this
      real(WP), intent(in) :: x, y
      logical :: res
      
      res = x.ge.this%x_min           .and. &
          & x.le.this%x_min + this%dx .and. &
          & y.ge.this%y_min           .and. &
          & y.le.this%y_min + this%dy
   end function node_contains

   pure function get_subnode_idx(this, x, y) result(idx)
      class(QuadtreeNode), intent(in) :: this
      real(WP), intent(in) :: x, y
      real(WP) :: x_split, y_split
      integer :: idx

      x_split = this%x_min + 0.5_WP * this%dx
      y_split = this%y_min + 0.5_WP * this%dy
      idx = merge(2, 0, y.gt.y_split) + merge(1, 0, x.gt.x_split) + 1
   end function get_subnode_idx

end module quadtree_class
