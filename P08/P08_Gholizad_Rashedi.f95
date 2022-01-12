! Programmed By Amir Gholizad and Kosar Rashedi
! --------------------------------------------------------------
! *NOTICE: WE CHANGED THE PHI(X,Y) POTENTIAL NOTATION TO U(X,Y)*
!---------------------------------------------------------------
PROGRAM FIND_POTENTIAL
IMPLICIT NONE
INTEGER :: N,I,J
REAL, DIMENSION(100,100) :: U=0.0
!------------------------
OPEN(111,FILE="U(X,Y).TXT")
!------------------------
! AFTER QUANTIZING THE LATTICE WE GET THE MASTER EQUATION AS: U(I,J) = ( U(I+1,J) + U(I-1,J) + U(I,J+1) + U(I,J-1) )/4
DO J=1,100
  U(1,J)=25.0
  U(100,J)=25.0
END DO
!-------------------------------------------------------------
DO N=1,1000
  DO I=2,99
    DO J=2,99
      U(I,J)=( U(I+1,J) + U(I-1,J) + U(I,J+1) + U(I,J-1) )/4.0
    END DO
  END DO
END DO
!-------------------------------------------------------------
DO I=1,100
  WRITE(111,'(2000F25.20)') (U(I,J),J=1,100)
END DO
!------------------------
CLOSE(111)
!------------------------
END PROGRAM FIND_POTENTIAL
!------------------------
