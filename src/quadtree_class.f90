module quadtree_class
   use iso_fortran_env, only: error_unit
   use precision, only: WP
   implicit none
   private

   integer, parameter, public :: NUM_SUBNODES = 4
   integer, parameter, public :: NUM_POINTS_PER_LEAF = 5

   public :: QuadtreeNode

   type :: QuadtreeNode
      real(WP) :: x_min, y_min, dx, dy

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
      ! module procedure construct_mid
   end interface QuadtreeNode

contains

   ! -----------------------------------------------------------------------------------------------
   function construct_leaf(x_min, y_min, dx, dy) result(self)
      type(QuadtreeNode) :: self
      real(WP), intent(in) :: x_min, y_min, dx, dy

      self%x_min = x_min
      self%y_min = y_min
      self%dx = dx
      self%dy = dy

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

         if (this%size.lt.NUM_POINTS_PER_LEAF) then
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
      this%nodes(1) = QuadtreeNode(this%x_min, this%y_min, half_dx, half_dy)
      ! Bottom-right
      this%nodes(2) = QuadtreeNode(x_split,    this%y_min, half_dx, half_dy)
      ! Top-left
      this%nodes(3) = QuadtreeNode(this%x_min, y_split,    half_dx, half_dy)
      ! Top-left
      this%nodes(4) = QuadtreeNode(x_split,    y_split,    half_dx, half_dy)

      ! Clean up the contained indices
      this%is_leaf = .false.
      do i=1,this%size
         call this%add_point(this%idxs(i), xs, ys)
      end do
      this%size = 0
      deallocate(this%idxs)
   end subroutine split_node

   subroutine print_node(this)
      class(QuadtreeNode), intent(in) :: this

      print "('x_min   = ',ES14.6)", this%x_min
      print "('y_min   = ',ES14.6)", this%y_min
      print "('dx      = ',ES14.6)", this%dx
      print "('dy      = ',ES14.6)", this%dy
      print "('is_leaf = ',L1)", this%is_leaf

      if (this%is_leaf) then
         print "('idxs    = ',*(I5,', '))", this%idxs(:this%size)
      end if
   end subroutine print_node

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
