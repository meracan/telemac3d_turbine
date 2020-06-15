PROGRAM MyProgram
  ! USE GEOMETRY, ONLY:Sphere,Cylinder,Box,Geo
  
  USE GEOMETRY, ONLY:Geo,radians
  USE FILE_CSV
  IMPLICIT NONE
  
  TYPE(Geo) :: c1,b1
  
  

  CALL c1%Cylinder_int(2,18,2)
  CALL c1%Translate([0.,0.,20.])
  CALL c1%Scale(2.)
  CALL c1%Rotatez(radians(45.))
  
  CALL b1%Box_int(2,2,2)
  
  

  
  CALL Write_CSV_2D('test.box.csv',b1%x)
  CALL Write_CSV_2D('test.cylinder.csv',c1%x)

  
END PROGRAM