C  -------------------------------------------------------------------
C
C	Copyright (1990-1995) by Alexander Khibnik, Yuri Kuznetsov, and 
C	Eugene Nikolaev.
C
C The Locbif computation code in DsTool is distributed in the hope that it  
C will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty  
C of FITNESS FOR A PARTICULAR PURPOSE.  The software is provided as is without 
C any obligation on the part of Locbif authors, Cornell faculty, 
C Cornell staff or Cornell students to assist in its use, correction, 
C modification or enhancement. 
C
C  -------------------------------------------------------------------

      SUBROUTINE DETHPF(NK,X,DRG,IER)
C
C   DETIFI COMPUTE BIFURCATION FUNCTION FOR NEUTRAL EIGENVALUES CASE
C                      IN ITERATED MAPS     
C     (THE FUNCTION IS ADAPTATION OF DETERMINANT OF HURWITZ MATRIX)
C        24.04.1990
C        31.08.1990
C        16.11.92
C
C   FORMAL PARAMETERS:
C    NK     - DIMENSION OF THE CONTINUATION PROBLEM
C    X      - VECTOR OF THE STATE VARIABLES (LENGTH NK)
C    DRG    - COMPUTED VALUE OF DETERMINANT
C    IER    - ERROR CONDITION
C               0 - NORMAL VALUE
C               
C   VARIABLES IN COMMON BLOCKS:
C    NF     - PHASE DIMENSION OF THE PROBLEM
C    DHF    - STEPSIZE TO COMPUTE JACOBIAN NUMERICALLY
C
C   WORKING SPACE:
C    D      - REAL ARRAY OF THE LENGTH NF*NF CONTAINING JACOBIAN
C    C      - REAL ARRAY OF THE LENGTH NF CONTAINING COEFFICIENTS OF 
C               THE CHARACTERISTIC POLINOMIAL
C    CA     - REAL ARRAY OF THE LENGTH NF+1
C    CB     - REAL ARRAY OF THE LENGTH NF+1
C
      IMPLICIT REAL*8(A-H,O-Z)
      IMPLICIT INTEGER*2(I-N)
      include 'linfpbf.dim'
      parameter (NFMAX1=NFMAX+1)
      DIMENSION X(NK)
      DIMENSION C(NFMAX),CA(NFMAX1),CB(NFMAX1)
      COMMON/LINBF/NF,NAP,ILN,NF2,ITP
      COMMON/LINBF1/DHF
      COMMON/LINBF2/D(NFMAX2)
      EXTERNAL FUNM
      IER=0
C
C   CASE: PHASE DIMENSION NF = 1
C
      IF (NF.GT.1) GO TO 10
        IER=1
        DRG=1.
        RETURN
C
C   CASE: PHASE DIMENSION NF > 1
C
 10   CALL DER1BF(NK,DHF,X,D,FUNM,IER)
        IF (IER.NE.0) RETURN
      CALL MODJAC(NK,X,D,IER)
        IF (IER.NE.0) RETURN
C
      IF (NF.EQ.2) GO TO 70
      IF (NF.EQ.3) GO TO 60
C
C   CASE: PHASE DIMENSION NF > 3
C
C    COMPUTE COEFFICIENTS OF THE CHARACTERISTIC POLINOMIAL
C
      CALL KOEEV1(D,NF,NF,C,IER)
      IF (IER.EQ.0) GO TO 20
        RETURN
C
 20   NFP1=NF+1
      CA(1)=1.D0
      DO 22 I=2,NFP1
        CA(I)=C(I-1)
 22   CONTINUE
      DO 25 K=1,NFP1
        L=NF+2-K
        CB(L)=0.D0
        DO 23 I=1,L
          CB(L)=CB(L)+CA(I)
 23   CONTINUE
      IF (L.EQ.1) GO TO 25
      LM1=L-1
      DO 24 I=1,LM1
        CA(I)=((L-I)*CA(I)-I*CA(I+1))/K
 24   CONTINUE
 25   CONTINUE
      M=NF-1
      K=0
      DO 50 I=1,M
        DO 40 J=1,M
          L=2*I-J
          K=K+1
          IF (L.GT.NF.OR.L.LT.1) GO TO 30
            D(K)=CB(L+1)
            GO TO 40
 30       D(K)=0.D0
          IF (L.EQ.0) D(K)=CB(1)
 40     CONTINUE
 50   CONTINUE
      CALL DETBF(D,M,DRG)
      RETURN
C
C   CASE: PHASE DIMENSION NF = 3
C
 60   R1=D(1)*D(5)*D(9)
      R2=D(4)*D(8)*D(3)
      R3=D(2)*D(6)*D(7)
      R4=D(7)*D(5)*D(3)
      R5=D(8)*D(6)*D(1)
      R6=D(4)*D(2)*D(9)
      DET=R1+R2+R3-R4-R5-R6
      R1=D(5)*D(9)-D(6)*D(8)
      R2=D(1)*D(5)-D(2)*D(4)
      R3=D(1)*D(9)-D(3)*D(7)
      DRG=DET**2-(D(1)+D(5)+D(9))*DET+(R1+R2+R3-1.D0)
      RETURN
C
C   CASE: PHASE DIMENSION NF = 2
C
 70   DRG=D(1)*D(4)-D(2)*D(3)-1.D0
      RETURN
      END
 
