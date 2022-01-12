! Programmed By Amir Gholizad and Kosar Rashedi

PROGRAM ODE_SOLVER
IMPLICIT NONE
DOUBLE PRECISION :: T,V,P   ! P AND V REPRESENTS THE POSITION AND VELOCITY OF OSCILLATOR
DOUBLE PRECISION :: A,B,H   !   MEANING THAT OUR MASTER EQUATIONS ARE: dP/dT = V   ,   dV/dT = 1 + (GAMMA * V) - (K/M)*P - exp(-T)
INTEGER :: N
!------------------------
OPEN(111,FILE="RK4_RESULTS.csv")
OPEN(222,FILE="T3_RESULTS.csv")
!------------------------
! T:[0,50] ====> A = 0.0 ; B = 50
! H = 0.5 ====> N=(B-A)/H = 100, THEN :
A = 0.0
B = 50.0
H = 0.5
N = (B-A)/H
T = 0.0 ! INITIAL TIME
V = -0.5 ! VELOCITY AT (T=0)
P = -2.0/3.0 ! POSITION AT (T=0)
!------------------------
CALL RK4(T,P,V,H,N)
!------------------------
T = 0.0 ! TIME AT (T=0) --> (NEED TO RESET THE INITIAL VALUES)
V = -0.5 ! VELOCITY AT (T=0)
P = -2.0/3.0 ! POSITION AT (T=0)
!------------------------
CALL T3(T,P,V,H,N)
!------------------------
CLOSE(111)
CLOSE(222)
!------------------------
END PROGRAM ODE_SOLVER
!------------------------


SUBROUTINE RK4(T,P,V,H,N)
IMPLICIT NONE
DOUBLE PRECISION :: FUN1,FUN2,T,V,P,H
DOUBLE PRECISION :: K1, K2, K3, K4
DOUBLE PRECISION :: L1, L2, L3, L4
DOUBLE PRECISION :: W1, W2, W3
INTEGER :: N,I
! PREPARE AND WRITE THE RK4_RESULTS.TXT FILE
WRITE(111,*) "T,P,V"
WRITE(111,*) T,",",P,",",V
T = T + H
!------------------------
! CALCULATE P AND V USING RK4 ALGORITHM
DO I=1,N
K1 = H*FUN1(T,P,V)
L1 = H*FUN2(T,P,V)
!-------
W1 = T + H/2.0 ; W2 = P + K1/2.0 ; W3 = V + L1/2 .0
K2 = H*FUN1( W1, W2, W3)
L2 = H*FUN2( W1, W2, W3)
!-------
W1 = T + H/2.0 ; W2 = P + K2/2.0 ; W3 = V + L2/2 .0
K3 = H*FUN1( W1, W2, W3)
L3 = H*FUN2( W1, W2, W3)
!-------
W1 = T + H ; W2 = P + K3 ; W3 = V + L3
K4 = H*FUN1( W1, W2, W3)
L4 = H*FUN2( W1, W2, W3)
!-------
V = V + (1.0/6.0)*( L1+2* L2+2* L3+ L4)
P = P + (1.0/6.0)*( K1+2* K2+2* K3+ K4)
!-------
WRITE(111,*) T,",",P,",",V
!------
T = T + H
END DO
END SUBROUTINE
!------------------------

SUBROUTINE T3(T,P,V,H,S)
IMPLICIT NONE
DOUBLE PRECISION :: FUN1,FUN2,FUN3,FUN4,T,V,P,H
INTEGER :: R,S
! PREPARE AND WRITE THE RK4_RESULTS.TXT FILE
WRITE(222,*) "T,P,V"
WRITE(222,*) T,",",P,",",V
T = T + H
!------------------------
! CALCULATE P AND V USING T3 ALGORITHM
DO R=1,S
V = V + H*FUN2(T,P,V) + ((H**2)/2.0)*FUN3(T,P,V) + ((H**3)/6.0)*FUN4(T,P,V)
P = P + H*FUN1(T,P,V) + ((H**2)/2.0)*FUN2(T,P,V) + ((H**3)/6.0)*FUN3(T,P,V)
!-------
WRITE(222,*) T,",",P,",",V
!------
T = T + H
END DO
STOP
END SUBROUTINE
!------------------------


FUNCTION FUN1(T,P,V)
IMPLICIT NONE
DOUBLE PRECISION :: T,P,V,FUN1
FUN1 = V + 0.0*(T+P)
END FUNCTION
!------------------------


FUNCTION FUN2(T,P,V)
IMPLICIT NONE
DOUBLE PRECISION :: T,P,V,GAMMA=8.0,K=2.0,M=1.0,FUN2
FUN2 = 1 + (GAMMA * V) - (K/M)*P - exp(-T)
END FUNCTION
!------------------------


FUNCTION FUN3(T,P,V)
IMPLICIT NONE
DOUBLE PRECISION :: T,P,V,GAMMA=8.0,K=2.0,M=1.0,FUN3
FUN3 = (GAMMA * (1 + (GAMMA * V) - (K/M)*P - exp(-T))) - (K/M)*V + exp(-T)
END FUNCTION
!------------------------


FUNCTION FUN4(T,P,V)
IMPLICIT NONE
DOUBLE PRECISION :: T,P,V,GAMMA=8.0,K=2.0,M=1.0,FUN4
FUN4 = (GAMMA * ((GAMMA * (1 + (GAMMA * V) - (K/M)*P - exp(-T))) - (K/M)*V + exp(-T))) - (K/M)*(1 + (GAMMA * V) - (K/M)*P - exp(-T)) - exp(-T)
END FUNCTION