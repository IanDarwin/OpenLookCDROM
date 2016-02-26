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

      SUBROUTINE DETRG(NK,X,DRG,IER)
C
C   DETRG COMPUTE BIFURCATION FUNCTION FOR NEUTRAL EIGENVALUES CASE
C     (THE FUNCTION IS ADAPTATION OF DETERMINANT OF HURWITZ MATRIX)
C        24.04.1990
C        17.11.92
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
C
      IMPLICIT REAL*8(A-H,O-Z)
      IMPLICIT INTEGER*2(I-N)
      include 'epbf.dim'
      parameter (NDS=(NFMAX-2)*(NFMAX-2))
      DIMENSION X(NK)
      DIMENSION C(NFMAX)
c
      DIMENSION DS(NDS)
c
      COMMON/LINBF/NF,NAP,ILN,NF2,ITP
      COMMON/LINBF1/DHF
      COMMON/LINBF2/D(NFMAX2)
      EXTERNAL FUNS
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
 10   CALL DER1BF(NK,DHF,X,D,FUNS,IER)
        IF (IER.NE.0) RETURN
      IF (NF.EQ.2) GO TO 70
      IF (NF.EQ.3) GO TO 60
C
C   CASE: PHASE DIMENSION NF > 3
C
C    COMPUTE COEFFICIENTS OF THE CHARACTERISTIC POLINOMIAL
C
c          PRINT 901,(D(I),I=1,36)
c 901      FORMAT (/2X,'D=',/(2X,6G13.6))      
      CALL KOEEV1(D,NF,NF,C,IER)
c          PRINT 902,(C(I),I=1,6)
c 902      FORMAT (/2X,'C=',/(2X,6G13.6))      
      IF (IER.EQ.0) GO TO 20
        RETURN
C
 20   M=NF-1
      K=0
      DO 50 I=1,M
        DO 40 J=1,M
          L=2*I-J
          K=K+1
          IF (L.GT.NF.OR.L.LT.1) GO TO 30
            D(K)=C(L)
            GO TO 40
 30       D(K)=0.D0
          IF (L.EQ.0) D(K)=1.D0
 40     CONTINUE
 50   CONTINUE
c
      MS=M-2
      L=M
      K=0
      DO 55 I=1,MS
	 L=L+2
	 DO 54 J=1,MS
            L=L+1
            K=K+1
	    DS(K)=D(L)
 54      CONTINUE
 55   CONTINUE
c
      CALL DETBF(D,M,DRG)
c
c---      CALL DETBF(DS,MS,DRGDS)
c
c---      DRG=DRG/(1.D-10 + DABS(DRGDS))
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
      DYAK=R1+R2+R3-R4-R5-R6
      R1=D(5)*D(9)-D(6)*D(8)
      R2=D(1)*D(5)-D(2)*D(4)
      R3=D(1)*D(9)-D(3)*D(7)
      DRG=(D(1)+D(5)+D(9))*(R1+R2+R3)-DYAK
c
c      SC=3.D0/(1.D0+DABS(D(1)+D(5)+D(9)))
      STR=D(1)*D(1)+D(5)*D(5)+D(9)*D(9)
      STR=DSQRT(STR)
      SC=3.D0/(1.D-2+STR)
      SC3=SC*SC*SC
      DRG=SC3*DRG

c
      RETURN
C
C   CASE: PHASE DIMENSION NF = 2
C
 70   DRG=D(1)+D(4)
      RETURN
      END
