PROGRAM MyProgram
  USE ModuleQHull,ONLY: QHull,BBOX
  USE FILE_CSV, ONLY:Write_CSV_2D
  IMPLICIT NONE
  
  REAL*8,dimension(:,:),ALLOCATABLE :: b
  
  REAL*8, dimension(10,3)   :: a = TRANSPOSE(reshape(&
    [&
    -1.,-1.,0.,&
    -1.,1.,0.,&
    1.,1.,0.,&
    1.,-1.,0.0,&
    -0.9,0.,0.,&
    0.9,0.,0.,&
    0.,0.9,0.,&
    0.,-0.9,0.,&
    0.,0.,0.,&
    0.5,0.5,0.&
    ], (/3,10/)))

  
  b=QHull(a(:,1:2))
  CALL Write_CSV_2D("output/qhull.csv",b)
  
  ! print *,TRANSPOSE(b)
  ! print *,"bbox"
  ! print *,BBOX(a(:,1:2))
  
END PROGRAM