MODULE cpu_gpu_interface
!=----------------------------------------------------------------------=!
  use cudafor
  IMPLICIT NONE
  INTERFACE vector_add
     SUBROUTINE vector_add_cpu( a, b, N )
        real:: a(:)
        real:: b
        integer:: N
     END SUBROUTINE vector_add_cpu
     SUBROUTINE vector_add_gpu( a, b, N )
        real,  DEVICE  :: a(:)
        real:: b
        integer:: N
     END SUBROUTINE vector_add_gpu
  END INTERFACE
END MODULE
