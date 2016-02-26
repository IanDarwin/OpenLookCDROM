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

      SUBROUTINE DETFLP(NK,X,DJAC,IER)
C
C   DETP1 COMPUTE DETERMINANT OF THE JACOBY MATRIX FOR THE CASE -1
C        24.04.1990
C        31.08.1990
C        16.11.92
C
C   FORMAL PARAMETERS:
C    NK     - DIMENSION OF THE CONTINUATION PROBLEM
C    X      - VECTOR OF THE STATE VARIABLES (LENGTH NK)
C    DYAC   - COMPUTED VALUE OF DETERMINANT
C    IER    - ERROR CONDITION
C               0 - NORMAL VALUE
C               
C   VARIABLES IN COMMON BLOCKS:
C    NF     - PHASE DIMENSION OF THE PROBLEM
C    DHF    - STEPSIZE TO COMPUTE JACOBIAN NUMERICALLY
C
C   WORKING SPACE:
C    D      - REAL ARRAY OF THE LENGTH NF*NF CONTAINING JACOBIAN
C
      IMPLICIT REAL*8(A-H,O-Z)
      IMPLICIT INTEGER*2(I-N)
      include 'linfpbf.dim'
      DIMENSION X(NK)
      COMMON/LINBF/NF,NAP,ILN,NF2,ITP
      COMMON/LINBF1/DHF
      COMMON/LINBF2/D(NFMAX2)
      EXTERNAL FUNM
      IER=0
      CALL DER1BF(NK,DHF,X,D,FUNM,IER)
      IF (IER.NE.0) RETURN
      CALL MODJAC(NK,X,D,IER)
      IF (IER.NE.0) RETURN
C
      NF2=NF*NF
      NFP1=NF+1
      DO 15 I=1,NF2,NFP1
        D(I)=D(I)+1.D0
 15   CONTINUE 
      GO TO (20,30,40), NF
      GO TO 50
C
C   CASE: PHASE DIMENSION NF = 1
C
 20   DJAC=D(1)
      RETURN
C
C   CASE: PHASE DIMENSION NF = 2
C
 30   DJAC=D(1)*D(4)-D(2)*D(3)
      RETURN
C
C   CASE: PHASE DIMENSION NF = 3
C
 40   R1=D(1)*D(5)*D(9)
      R2=D(4)*D(8)*D(3)
      R3=D(2)*D(6)*D(7)
      R4=D(7)*D(5)*D(3)
      R5=D(8)*D(6)*D(1)
      R6=D(4)*D(2)*D(9)
      DJAC=R1+R2+R3-R4-R5-R6
      RETURN
C
C   CASE: PHASE DIMENSION NF > 3
C
 50   CALL DETBF(D,NF,DJAC)
      RETURN
      END
 
