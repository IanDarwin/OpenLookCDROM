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

      SUBROUTINE DETBF(A,N,D)
C
C   COMPUTE DETERMINANT BY GAUSSIAN METHOD  WITH ELIMINATION
C     OVER ALL MATRIX. THE DETERMINANT IS LOGARITHMICALLY 
C     SCALED TO AVOID OVERFLOWS AND ABNORMALLY BIG VALUES.
C     LET DET IS A DETERMINANT, THEN RESULTING VALUE D EQUALS
C       D={ DET, IF ABS(DET)<=1,                    
C         { SIGN(DET)*(1+LOG(ABS(DET))), OTHERWISE.
C
C            VERSION  24.04.1990
C                     17.11.92
C
C   THIS IS ADAPTATION OF SUBROUTINE DMINV FROM SSP LIBRARY
C
C   FORMAL PARAMETERS:
C     A    - N*N MATRIX
C     N    - THE ORDER OF MATRIX A
C     D    - OUTPUT VALUE OF SCALED DETERMINANT
C
      IMPLICIT REAL*8(A-H,O-Z)
      IMPLICIT INTEGER*2(I-N)
      DIMENSION A(1)
      R=0.D0
      D=1.D0
      DS=1.D0
      NK=-N
      DO 80 K=1,N
      NK=NK+N
      L=K
      M=K
      KK=NK+K
      BIGA=A(KK)
      DO 20 J=K,N
      IZ=N*(J-1)
      DO 20 I=K,N
      IJ=IZ+I
   10 IF(DABS(BIGA)-DABS(A(IJ)))15,20,20
   15 BIGA=A(IJ)
      L=I
      M=J
   20 CONTINUE
      J=L
      IF(J-K)35,35,25
   25 KI=K-N
      DO 30 I=1,N
      KI=KI+N
      HOLD=-A(KI)
      JI=KI-K+J
      A(KI)=A(JI)
   30 A(JI)=HOLD
   35 I=M
      IF(I-K)45,45,38
   38 JP=N*(I-1)
      DO 40 J=1,N
      JK=NK+J
      JI=JP+J
      HOLD=-A(JK)
      A(JK)=A(JI)
   40 A(JI)=HOLD
   45 IF(BIGA)48,46,48
   46 D=0.D0
      RETURN
   48 DO 55 I=1,N
      IF(I-K)50,55,50
   50 IK=NK+I
      A(IK)=A(IK)/(-BIGA)
   55 CONTINUE
      DO 65 I=1,N
      IK=NK+I
      HOLD=A(IK)
      IJ=I-N
      DO 65 J=1,N
      IJ=IJ+N
      IF(I-K)60,65,60
   60 IF(J-K)62,65,62
   62 KJ=IJ-I+K
      A(IJ)=HOLD*A(KJ)+A(IJ)
   65 CONTINUE
      KJ=K-N
      DO 75 J=1,N
      KJ=KJ+N
      IF(J-K)70,75,70
   70 A(KJ)=A(KJ)/BIGA
   75 CONTINUE
      IF (DABS(D).GE.1.D10) THEN
	 D=1.D10
	 RETURN
      ENDIF
       D=D*DABS(BIGA)
c      R=R+DLOG(DABS(BIGA))
      IF (BIGA.LT.0) DS=-DS
   80 A(KK)=1.D0/BIGA
c      IF (R.LE.0.) D=DEXP(R)
c      IF (R.GT.0.) D=R+1.D0
c      WRITE (*,*) '  D=',D
      D=D*DS
  150 RETURN
      END
