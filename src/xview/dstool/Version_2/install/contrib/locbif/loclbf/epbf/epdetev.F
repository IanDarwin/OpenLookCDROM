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

      SUBROUTINE epDETEV(NK,X,DEV,IER)
C
C   DETEV COMPUTE BIFURCATION FUNCTION FOR MULTIPLE EIGENVALUES CASE
C        24.04.1990
C        15.11.92
C        10.07.94
C
C   FORMAL PARAMETERS:
C    NK     - DIMENSION OF THE CONTINUATION PROBLEM
C    X      - VECTOR OF THE STATE VARIABLES (LENGTH NK)
C    DEV    - COMPUTED VALUE OF BIFURCATION FUNCTION
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
C    A      - REAL ARRAY OF THE LENGTH NF+1 
C    B      - REAL ARRAY OF THE LENGTH NF 
C    U      - REAL ARRAY OF THE LENGTH (2*NF-1)*(2*NF-1) 
C
      IMPLICIT REAL*8(A-H,O-Z)
      IMPLICIT INTEGER*2(I-N)
      include 'epbf.dim'
      parameter (NFMAX1=NFMAX+1, NU=(2*NFMAX-1)**2 )
      DIMENSION X(NK)
      DIMENSION A(NFMAX1),B(NFMAX),C(NFMAX)
      COMMON/LINBF/NF,NAP,ILN,NF2,ITP
      COMMON/LINBF1/DHF
      COMMON/LINBF2/D(NFMAX2)
      COMMON/WORK5/ U(NU)	
      EXTERNAL FUNS
      IER=0
C
      IF (NF.GT.1) GO TO 10
C
C   PROVIDE NONZERO ERROR CODE FOR THE CASE NF=1
C
        IER=1
        DRG=1.
        RETURN
C
C   CALCULATE JACOBY MATRIX
C
 10   CALL DER1BF(NK,DHF,X,D,FUNS,IER)
        IF (IER.NE.0) RETURN
      IF (NF.EQ.2) GO TO 50
C
C   CASE: PHASE DIMENSION NF > 2
C
C    COMPUTE COEFFICIENTS OF THE CHARACTERISTIC POLINOMIAL
C
      CALL KOEEV1(D,NF,NF,C,IER)
        IF (IER.EQ.0) GO TO 20
        RETURN
C
 20   M=NF-1
      A(1)=1.
      DO 30 I=1,NF
 30   A(I+1)=C(I)
      DO 40 I=1,NF
 40   B(I)=(NF-I+1)*A(I)
C
C   COMPUTE REZULTANT (IN LOGARITHMIC SCALE)
C
      CALL RZLTNT(NF,M,A,B,U,DEV)
      RETURN
C
C   CASE: PHASE DIMENSION NF > 2
C
 50   DEV=(D(1)+D(4))**2-4.*(D(1)*D(4)-D(2)*D(3))
      RETURN
      END
 
