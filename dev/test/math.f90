PROGRAM MyProgram
  USE CheckModule, ONLY:Check
  USE MathModule, ONLY:PI,PI2,degrees,radians,unique
  IMPLICIT NONE
  REAL*8, dimension(3,2)   :: p = reshape([0.,1.,1.,1.,1.,1.], (/3,2/))

  IF(.NOT. Check(radians(180.D0),PI)) print *,"Not equal - Test1"
  IF(.NOT. Check(radians(360.D0),PI2)) print *,"Not equal - Test2"
  IF(.NOT. Check(degrees(PI),180.D0)) print *,"Not equal - Test3"
  IF(.NOT. Check(degrees(PI2),360.D0)) print *,"Not equal - Test4"
  
  
  print *,unique([2.0D0,2.0D0,1.0D0,2.0D0])
  print *,unique(p)
  
  ! IF(.NOT. Check(unique([2.0D0,2.0D0,1.0D0,2.0D0]),360.D0)) print *,"Not equal - Test4"
  
END PROGRAM