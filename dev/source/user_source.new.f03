!                    *****************
                     SUBROUTINE USER_SOURCE &
!                    *****************
!
    (S0U,S0V,S0W,S1U,S1V,S1W, &
     UN3,VN3,WSN3,WN3, &
     VOLU,VOLUN,T3,NPOIN3,NTRAC,LT,AT,DT,PRIVE,NONHYD, &
     NPOIN2,NSCE,ISCE,KSCE,QSCE,USCE,VSCE,MAXSCE)
!
!***********************************************************************
! TELEMAC3D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    PREPARES SOURCE TERMS FOR DIFFUSION OF TRACERS.
!
!history  CDG/SOGREAH
!+        **/06/2001
!+
!+   TRACER SOURCES
!
!history  J-M HERVOUET (LNHE)
!+        29/08/2008
!+        V5P6
!+
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        13/07/2010
!+        V6P0
!+   Translation of French comments within the FORTRAN sources into
!+   English comments
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        21/08/2010
!+        V6P0
!+   Creation of DOXYGEN tags for automated documentation and
!+   cross-referencing of the FORTRAN sources
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AT             |-->| TIME
!| DT             |-->| TIME STEP
!| ISCE           |-->| NODE ADRESSES IN 2D MESH FOR SOURCES
!| KSCE           |<->| NUMBER OF PLANE FOR SOURCES
!| LT             |-->| ITERATION NUMBER
!| MAXSCE         |-->| MAXIMUM NUMBER OF SOURCES
!| NONHYD         |-->| LOGICAL FOR NON-HYDROSTATIC OPTION
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D
!| NPOIN3         |-->| NUMBER OF POINTS IN THE MESH
!| NSCE           |-->| NUMBER OF GIVEN POINTS FOR SOURCES
!| NTRAC          |-->| NUMBER OF TRACERS
!| PRIVE          |-->| BLOCK OF ARRAYS FOR USER
!| QSCE           |-->| WATER DISCHARGE OF SOURCES
!| S0U            |<->| EXPLICIT SOURCE TERMS ON VELOCITIES U
!| S0V            |<->| EXPLICIT SOURCE TERMS ON VELOCITIES V
!| S0W            |<->| EXPLICIT SOURCE TERMS ON VELOCITIES W
!| S1U            |<->| IMPLICIT SOURCE TERMS ON VELOCITIES U
!| S1V            |<->| IMPLICIT SOURCE TERMS ON VELOCITIES V
!| S1W            |<->| IMPLICIT SOURCE TERMS ON VELOCITIES W
!| T3             |<->| WORK ARRAY: NOT USED
!| UN3            |-->| COMPONENTS OF VELOCITY AT PREVIOUS TIME STEP
!| USCE           |-->| VELOCITY FOR SOURCE
!| VN3            |-->| COMPONENTS OF VELOCITY AT PREVIOUS TIME STEP
!| VOLU           |-->| VOLUME AROUND POINTS AT TIME N+1
!| VOLUN          |-->| VOLUME AROUND POINTS AT TIME N
!| VSCE           |-->| VELOCITY FOR SOURCE
!| WN3            |-->| COMPONENTS OF VELOCITY AT PREVIOUS TIME STEP
!| WSN3           |-->| SIGMA-TRANSFORMED VERTICAL VELOCITY COMPONENT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
  USE BIEF
  USE DECLARATIONS_SPECIAL
  USE DECLARATIONS_TELEMAC3D, ONLY :MESH2D,MESH3D,T3D_FILES,T3DFO1,T3DFO2,NPLAN,PRIVE1
  USE TurbinesModule, ONLY:readTEC,readCD,TEC,NTEC,Turbine
  USE FILE_CSV, ONLY:Write_CSV_2D
  IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
  INTEGER, INTENT(IN)           :: NPOIN3, NTRAC, LT, MAXSCE
!
  TYPE(BIEF_OBJ), INTENT(IN)    :: UN3, VN3, WSN3, WN3
  TYPE(BIEF_OBJ), INTENT(INOUT) :: S0U, S0V, S1U, S1V, S0W, S1W
  TYPE(BIEF_OBJ), INTENT(INOUT) :: T3
  TYPE(BIEF_OBJ), INTENT(IN)    :: VOLU, VOLUN,PRIVE
!
  DOUBLE PRECISION, INTENT(IN)  :: AT,DT
  LOGICAL, INTENT(IN)           :: NONHYD
!
  INTEGER, INTENT(IN)           :: NPOIN2
  INTEGER, INTENT(IN)           :: NSCE
  INTEGER, INTENT(IN)           :: ISCE(NSCE)
  INTEGER, INTENT(IN)           :: KSCE(NSCE)
  DOUBLE PRECISION, INTENT(IN)  :: QSCE(NSCE)
  DOUBLE PRECISION, INTENT(IN)  :: USCE(NSCE)
  DOUBLE PRECISION, INTENT(IN)  :: VSCE(NSCE)
!
!-----------------------------------------------------------------------
!     ALLOCATE THE EXTRA VARIABLES FOR TEC
!-----------------------------------------------------------------------
  INTEGER :: FO1,FO2,stat
  INTEGER :: I,J,I3,ITEC,NNODE,INODE
  
  
  REAL*8  :: X,Y,Z
  INTEGER,ALLOCATABLE :: meshIds(:)
  TYPE(Turbine) ::temptec
!
!-----------------------------------------------------------------------      
!     INITIALIZATION TURBINES AT FIRST ITERATION
!-----------------------------------------------------------------------     
  IF(LT.EQ.1) THEN
    IF(T3D_FILES(T3DFO1)%NAME(1:1).EQ.' ') THEN
     WRITE(LU,*) "SOURCE: FORMATTED DATA FILE 1 IS REQUIRED"
     CALL PLANTE(1)
     STOP
    ENDIF
    IF(T3D_FILES(T3DFO2)%NAME(1:1).EQ.' ') THEN
     WRITE(LU,*) "SOURCE: FORMATTED DATA FILE 2 IS REQUIRED"
     CALL PLANTE(1)
     STOP
    ENDIF
    
    FO1=T3D_FILES(T3DFO1)%LU
    FO2=T3D_FILES(T3DFO2)%LU
    
    CALL readTEC(FO1,stat)
    IF(stat>0) THEN
      WRITE(LU,*) "ERROR in readTEC"
      CALL PLANTE(1)
      STOP
    ENDIF
  
    CALL readCD(FO2,stat)
    IF(stat>0)THEN
      WRITE(LU,*) "ERROR in readCD"
      CALL PLANTE(1)
      STOP
    ENDIF
    
    ! Save XY nodes
    DO ITEC=1,NTEC
      temptec=TEC(ITEC)
      INODE=0
      DO I=1,MESH2D%NPOIN
        IF(INPOLY(MESH2D%X%R(I), MESH2D%Y%R(I), temptec%xy(:,1), temptec%xy(:,2), size(temptec%xy,1))) THEN
          INODE = INODE+1
        ENDIF
      ENDDO
      
      ALLOCATE(meshIds(INODE))
      
      CALL Write_CSV_2D("xy.csv",TEC(1)%xy)
      CALL Write_CSV_2D("yz.csv",TEC(1)%yz)
      CALL Write_CSV_2D("xz.csv",TEC(1)%xz)
      
      
      INODE=0
      DO I=1,MESH2D%NPOIN
        IF(INPOLY(MESH2D%X%R(I), MESH2D%Y%R(I), temptec%xy(:,1), temptec%xy(:,2), size(temptec%xy,1))) THEN
          INODE = INODE+1
          meshIds(INODE)=I
        ENDIF
      ENDDO
      CALL TEC(ITEC)%saveMeshIds(meshIds)
      
      DEALLOCATE(meshIds)
      
    ENDDO ! ITEC=1,NTEC
    
  ENDIF ! LT.EQ.1
  
!-----------------------------------------------------------------------      
!     INITIALIZATION OF SOURCE TERMS
!-----------------------------------------------------------------------       
  S1U%TYPR='Q'
  S1V%TYPR='Q'

  IF(NONHYD) THEN
    S0W%TYPR='0'
    S1W%TYPR='Q'
  ENDIF 
  
  DO I=1,NPOIN3
     S1U%R(I)=0.D0
     S1V%R(I)=0.D0
     IF(NONHYD) THEN ! IF NOT HYDROSTATIC CASE
      S1W%R(I)=0.D0
     ENDIF
     PRIVE1%R(I) = 0.D0
  ENDDO ! END OF DO I=1,NPOIN3   

!-----------------------------------------------------------------------      
!     SET SOURCE TERMS
!-----------------------------------------------------------------------     
  DO ITEC=1,NTEC
    temptec=TEC(ITEC)
    
    ALLOCATE (meshIds(size(temptec%meshIds)))
    meshIds=0
    
    ! Find all nodes vertically
    DO INODE=1,size(temptec%meshIds)
      I = temptec%meshIds(INODE)
      
      DO J=1,NPLAN
        I3=I+(J-1)*NPOIN2
        X=MESH3D%X%R(I3)
        Y=MESH3D%Y%R(I3)
        Z=MESH3D%Z%R(I3)
        INODE3=0
        IF(INPOLY(Y,Z, temptec%yz(:,1), temptec%yz(:,2), size(temptec%yz,1)).AND.&
          INPOLY(X,Z, temptec%xz(:,1), temptec%xz(:,2), size(temptec%xz,1))) THEN
          INODE3=INODE3+1
          meshIds(INODE3)=I3
        ENDIF
      ENDDO ! J=1,NPLAN
    ENDDO ! INODE
    meshIds=PACK(meshIds,meshIds /=0)
    ! Associate turbine nodes to mesh nodes
    DO INODE=1,size(temptec%xyz)
      
    END DO ! INODE
    
    DEALLOCATE(meshIds)
    
  ENDDO ! ITEC=1,NTEC
  
  
  
  ! PRINT *,"MESH---->",MESH2D%NPOIN

  
END SUBROUTINE
      