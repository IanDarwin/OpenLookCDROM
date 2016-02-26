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

      SUBROUTINE epDER2Q(N,ND,DH,X,DER,FUNS,IER)
C
C   COMPUTING THE SECOND DERIVATIVE OF FUNCTION (V2,F(X+P*V1)) OF P
C         20.05.1990
C         15.11.92
C         16.08.94
C
      IMPLICIT REAL*8(A-H,O-Z)
      IMPLICIT INTEGER*2(I-N)
      include 'epbf.dim'
      DIMENSION X(N),DER(ND)
      DIMENSION XP(NFMAX),XM(NFMAX),F(NFMAX),FP(NFMAX),FM(NFMAX)
      COMMON/QUAVEC/VEC1(NFMAX),VEC2(NFMAX),VEC3(NFMAX)
      COMMON /IERFS/ IERFS,IERDUM
      CALL FUNS(X,F)
        IER=IERFS
        IF (IER.NE.0) RETURN
      DO 10 I=1,N
        XP(I)=X(I)+DH*VEC1(I)
        XM(I)=X(I)-DH*VEC1(I)
 10   CONTINUE
      CALL FUNS(XP,FP)
        IER=IERFS
        IF (IER.NE.0) RETURN
      CALL FUNS(XM,FM)
        IER=IERFS
        IF (IER.NE.0) RETURN
      R=0.D0
      DO 20 I=1,N
        R=R+VEC2(I)*(FP(I)+FM(I)-2.D0*F(I))
 20   CONTINUE
      DER(1)=R/DH**2
      RETURN
      END
