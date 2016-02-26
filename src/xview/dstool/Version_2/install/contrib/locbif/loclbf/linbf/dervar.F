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

      SUBROUTINE DERVAR(N,ND,DH0,NDH,X,DER,DERH,DERIV,FUN,IER)
C
C   CHOOSING THE STEP SIZES FOR NUMERICAL DERIVATIVES
C           21.02.90
C           14.11.92
C
      IMPLICIT REAL*8(A-H,O-Z)
      IMPLICIT INTEGER*2(I-N)
      include 'linbf.dim'
      DIMENSION DER(ND),DERH(ND),X(N)
      DIMENSION DER1(NFMAX2),DER2(NFMAX2),IHND(NFMAX2)
      EXTERNAL DERIV,FUN
      DATA TEN/10./, E1/1.01/,E2/1.D-8/
      IER=0 
C
C   COMPUTING DERIVATIVES WITH STEP DH0
C
      DH=DH0
      CALL DERIV(N,ND,DH,X,DER,FUN,IER)
        IF (IER.NE.0) RETURN
      DO 10 I=1,ND
        IHND(I)=0
        DERH(I)=DH0
  10  CONTINUE
      IF ( NDH.LE.0) GO TO 100
C
C   PREPARATION FOR MAIN SEARCHING PROCEDURE
C
      DH10=DH*TEN
      CALL DERIV(N,ND,DH10,X,DER1,FUN,IER)
        IF (IER.NE.0) RETURN
C
C   SEARCHING OPTIMAL STEP SIZE AND COMPUTING DERIVATIVES
C
      KD=0
      DO 40 J=1,NDH
        DH=DH/TEN
        CALL DERIV(N,ND,DH,X,DER2,FUN,IER)
        IF (IER.NE.0) RETURN
        DO 30 I=1,ND
          IF (IHND(I).GT.0) GO TO 30
            R1=DABS(DER(I)-DER1(I))
            R2=DABS(DER(I)-DER2(I))
            R =DABS(DER(I))
            IF (E1*R2.LT.R1.AND.R2.GT.E2*R) GO TO 20
              IHND(I)=J
              KD=KD+1
              IF (KD.EQ.ND) GO TO 100
            GO TO 30
  20          IHND(I)=-J
              DER1(I)=DER(I)
              DER(I)=DER2(I)
              DERH(I)=DH
  30    CONTINUE
  40  CONTINUE
  100 CONTINUE
      RETURN
      END
