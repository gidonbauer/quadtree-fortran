program main
   use quadtree_class, only: QuadtreeNode, construct_node, destruct_node
   use precision,      only: WP
   implicit none

   real(WP), dimension(:), allocatable :: xs, ys
   integer, dimension(:, :), allocatable :: qtree_data
   type(QuadtreeNode) :: root
   integer :: i, j, N, idx
   real(WP) :: xc, yc, radius

   integer, dimension(:), allocatable :: found
   integer :: num_found, capacity_found

   N = 70
   allocate(xs(N**2))
   allocate(ys(N**2))
   allocate(qtree_data(N**2, 2))

   call construct_node(root, 0.0_WP, 0.0_WP, real(N, WP), real(N, WP), 10)

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

   capacity_found = 16
   num_found = 0
   allocate(found(capacity_found))
   xc = 3.5_WP; yc = 3.5_WP; radius = 1.0_WP
   call root%find(xc, yc, radius, xs, ys, num_found, found)

   print *
   print "('Found in circle (x=',F3.1,', y=',F3.1,', r=',F3.1,'):')", xc, yc, radius
   do i=1,num_found
      print "(I3,' => (',F3.1,', ',F3.1,')')", found(i), xs(found(i)), ys(found(i))
   end do

   call destruct_node(root)
   deallocate(xs, ys, qtree_data, found)     ! Do this to make valgrind happy, I believe this is not necessary

end program main
