PROGRAM MyProgram
  USE ModuleMat4
  USE CheckModule, ONLY:Check
  
  IMPLICIT NONE
  TYPE(Mat4) :: a
  REAL, dimension(2,3)   :: p = reshape([1.,0.,0.,0.,0.,0.], (/2,3/))
  
  ! a=[1.0,1.0]
  ! b= a + 1.0
  CALL a%reset()
  CALL a%translate([10.,0.,0.])
  CALL a%transform(p)
  IF(Check(p,reshape([11.,10.,0.,0.,0.,0.], (/2,3/)))) print *,"Equal!"
  
  
  CALL a%reset()
  CALL a%translate([10.,0.,0.])
  CALL a%transform(p)
  IF(Check(p,reshape([21.,20.,0.,0.,0.,0.], (/2,3/)))) print *,"Equal!"
  
  
  
END PROGRAM