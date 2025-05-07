program main
   use quadtree_class, only: QuadtreeNode, construct_node, destruct_node
   use precision,      only: WP
   implicit none

   real(WP), dimension(6) :: xs, ys
   type(QuadtreeNode) :: root

   !     y
   ! 1.0 +--------+--------+
   !     | 2      |      4 |
   !     |   5    |        |
   !     |   [3]  |  [4]   |
   !     |        |        |
   ! 0.5 +--------+--------+
   !     |        |        |
   !     |   [1]  |  [2]   |
   !     |   6    |        |
   !     | 1      |      3 |
   ! 0.0 +--------+--------+x
   !    0.0      0.5      1.0


   xs = [0.1_WP, 0.1_WP, 0.9_WP, 0.9_WP, 0.2_WP, 0.2_WP]
   ys = [0.1_WP, 0.9_WP, 0.1_WP, 0.9_WP, 0.8_WP, 0.2_WP]
   call construct_node(root, 0.0_WP, 0.0_WP, 1.0_WP, 1.0_WP, 4)

   call root%add_point(1, xs, ys)
   call root%add_point(2, xs, ys)
   call root%add_point(3, xs, ys)
   call root%add_point(4, xs, ys)
   call root%add_point(5, xs, ys)
   call root%add_point(6, xs, ys)

   print "('Example points:')"
   print "('    y                   ')"
   print "('1.0 +--------+--------+ ')"
   print "('    | 2      |      4 | ')"
   print "('    |   5    |        | ')"
   print "('    |   [3]  |  [4]   | ')"
   print "('    |        |        | ')"
   print "('0.5 +--------+--------+ ')"
   print "('    |        |        | ')"
   print "('    |   [1]  |  [2]   | ')"
   print "('    |   6    |        | ')"
   print "('    | 1      |      3 | ')"
   print "('0.0 +--------+--------+x')"
   print "('   0.0      0.5      1.0')"
   print *
   print "('Added point 1-6; [*] is sub-node index')"
   print *


   print "('Quadtree:')"
   call root%print(recursive=.true.)

   call destruct_node(root)
end program main
