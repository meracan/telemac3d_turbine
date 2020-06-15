PROGRAM MyProgram
  USE ModuleSort
  
  IMPLICIT NONE

  REAL, dimension(10)   :: a = [1.,1.,5.,3.,-1.,11.,10.,31.,-5.,1.1]

  REAL, dimension(8,3)   :: b = TRANSPOSE(reshape(&
    [&
    1.,0.,1.,&
    1.,0.,3.,&
    1.,0.,2.,&
    1.,0.,0.,&
    0.,0.,0.,&
    2.,0.,0.,&
    6.,0.,0.,&
    3.,0.,0.&
    ], (/3,8/)))
  
  REAL*8, dimension(5,2) :: c=reshape([9.5000000000000000,9.5000761524208475,&
  10.000000000000000,10.499923847581165,10.499923847581165,0.18515624999999999,&
  -0.55524667299999997,-0.55546874999999996,-0.55524667299999997,0.18493417300000001],(/5,2/))
  
  ! CALL quicksort(a)
  ! print *,a
  CALL quicksort(c)
  
  ! print *,TRANSPOSE(b)
  print *,c
  
  
END PROGRAM