program main
   use quadtree_class, only: QuadtreeNode
   use precision,      only: WP
   implicit none

   real(WP), dimension(:), allocatable :: xs, ys
   integer, dimension(:, :), allocatable :: qtree_data
   type(QuadtreeNode) :: root
   integer :: i, j, N, idx

   N = 7
   allocate(xs(N**2))
   allocate(ys(N**2))
   allocate(qtree_data(N**2, 2))

   root = QuadtreeNode(x_min=0.0_WP, y_min=0.0_WP, dx=real(N, WP), dy=real(N, WP), num_points_per_leaf=100)

   do i=1,N
      do j=1,N
         idx = (i-1) * N + (j-1) + 1
         xs(idx) = real(i, WP)
         ys(idx) = real(j, WP)
         qtree_data(idx, 1) = i
         qtree_data(idx, 2) = j
      end do
   end do

   do i=1,N**2
      call root%add_point(i, xs, ys)
   end do

   call root%print(recursive=.true.)

end program main
