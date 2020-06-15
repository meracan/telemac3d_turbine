PROGRAM MyProgram
  USE CheckModule, ONLY:Check
  USE FILE_CSV, ONLY:Write_CSV_2D
  
  USE MathModule, ONLY:PI,PI2,degrees,radians
  ! USE TurbineModule, ONLY:initialize
  USE TurbinesModule, ONLY:readTEC,readCD,TEC,NTEC
  IMPLICIT NONE
  
  INTEGER :: F1ID,F2ID,stat
  INTEGER,POINTER :: p
  integer, target :: t1 
  
  F1ID=1
  F2ID=2
  p=>t1
  p=1
  open(F1ID, file = 'input/turbine1.txt', status = 'old')
  open(F2ID, file = 'input/newEPTM.csv', status = 'old') 
  
  CALL readTEC(F1ID,stat)
  IF(stat>0) STOP("ERROR in readTEC")
  
  CALL readCD(F2ID,stat)
  IF(stat>0) STOP("ERROR in readCD")
  print *,NTEC
  
  CALL Write_CSV_2D("output/xy.csv",TEC(1)%xy)
  CALL Write_CSV_2D("output/yz.csv",TEC(1)%yz)
  CALL Write_CSV_2D("output/xz.csv",TEC(1)%xz)
  
  
  close(F1ID)
  close(F2ID)
  
  
END PROGRAM