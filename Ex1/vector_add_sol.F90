#ifdef USE_GPU
#define MY_ROUTINE(x)  x##_gpu
#else
#define MY_ROUTINE(x)  x##_cpu
#endif

subroutine MY_ROUTINE(vector_add)(a,b,N)
#ifndef USE_GPU
  real:: a(:)
#else
  use cudafor
  real, DEVICE :: a(:)
#endif

  real:: b
  integer:: i,N

  print *,"N=",N
!$cuf kernel do <<<*,*>>>
   do i=1,N
     a(i)=a(i)+b
   end do
  
end subroutine 


