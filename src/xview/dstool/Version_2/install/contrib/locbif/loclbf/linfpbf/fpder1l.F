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

      SUBROUTINE fpDER1L(N,ND,DH,X,DER,FUNM,IER)
C
C   CALCULATION OF JACOBIAN MATRIX DFI/DXJ  (I,J=1,N) FOR MAP
C          31.08.90
C          28.09.90
C          16.11.92
C          10.07.94
C
      IMPLICIT REAL*8(A-H,O-Z)
      IMPLICIT INTEGER*2(I-N)
      include 'linfpbf.dim'
      DIMENSION DER(N,N),X(N)
      DIMENSION FP(NFMAX),FM(NFMAX),F(NFMAX)
      COMMON /IERFS/ IERFS,IERDUM
      COMMON /NJAC/ NJAC,NJACN,NJACB
      EXTERNAL FUNM
      DATA EPS0/1.D-10/
      NJACN=NJACN+1
C      DH2=DH+DH
C
      CALL FUNM(X,F)
      IER=IERFS
      IF (IER.NE.0) RETURN
C
      DO 20 J=1,N
        DHX=DH*(1.D0+DABS(X(J)))
        R=X(J)
        X(J)=R+DHX
        CALL FUNM(X,FP)
        X(J)=R
        IER=IERFS
        IF (IER.NE.0) RETURN
C        X(J)=R-DH
C        CALL FUNM(X,FM)
C        X(J)=R
C        IER=IERFS
C        IF (IER.NE.0) RETURN
          DO 10 I=1,N
C            DER(I,J)=(FP(I)-FM(I))/DH2
            DER(I,J)=(FP(I)-F(I))/DHX
            IF (DABS(DER(I,J)).LE.EPS0) DER(I,J)=0.
  10      CONTINUE
  20  CONTINUE
      RETURN
      END
