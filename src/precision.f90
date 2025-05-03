module precision
   use iso_fortran_env, only: int32, int64, real32, real64, real128
   implicit none

   private
   integer, public, parameter :: I4 = int32
   integer, public, parameter :: I8 = int64  
   integer, public, parameter :: F4 = real32
   integer, public, parameter :: F8 = real64
   integer, public, parameter :: F16 = real128
   integer, public, parameter :: WP = F8
end module precision
