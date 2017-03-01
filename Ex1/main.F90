program example_vector_add
use cpu_gpu_interface
#ifdef USE_GPU
use cudafor
real, allocatable, device:: a_d(:)
#endif
integer:: i, N
real, allocatable:: a(:)
#
N=32
allocate(a(N))

a=1.
#ifdef USE_GPU
allocate(a_d, source=a)
#endif

call vector_add(a_d,10.,N)

#ifdef USE_GPU
a=a_d
#endif

if(any(a /= 11.) )  then
  print *,"Vector add failed"
else
  print *,"Vector add passed"
end if 
#ifdef USE_GPU
deallocate(a_d)
#endif
deallocate(a)

end program example_vector_add
