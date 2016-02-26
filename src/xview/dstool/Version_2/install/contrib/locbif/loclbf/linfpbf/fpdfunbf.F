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

      SUBROUTINE fpDFUNBF(X,DFUN,NDIMM,NX,IER,ISTOP)
C         ITERATED MAPS
C           31.08.1990
C           16.11.92
C           10.07.94
C
      IMPLICIT REAL*8(A-H,O-Z)
      IMPLICIT INTEGER*2(I-N)
      include 'linfpbf.dim'
      parameter (NDIM1=NDIM-1)
      DIMENSION X(1),DFUN(NDIMM,NX)
      DIMENSION F(NDIM1),FP(NDIM1)
      EXTERNAL FUNM
      COMMON/LINBF/NF,NAP,ILN,NF2,ITP
      COMMON/LINBF1/DHF
      COMMON/LINBF2/D(NFMAX2)
      COMMON/LINBFO/INDFUN
      COMMON/NJAC/NJAC,NJACN,NJACB
      COMMON/ADD1B/DX,HVMN,HVMX
      DATA EPS0/1.D-10/
      IER=0
      ISTOP=0
      NJACB=NJACB+1
      CALL DER1BF(NX,DHF,X,D,FUNM,IER)
      IF (IER.LT.0) ISTOP=IER
      IF (IER.NE.0) RETURN
      K=0
      DO 20 I=1,NF
        DO 10 J=1,NF
          K=K+1
          DFUN(J,I)=D(K)
 10     CONTINUE
        DFUN(I,I)=DFUN(I,I)-1.D0
 20   CONTINUE
      DH=DABS(DX)
      CALL FUNBF(X,F,IER,ISTOP)
      IF (IER.NE.0.OR.ISTOP.NE.0) RETURN
      NXM1=NX-1
      NFP1=NF+1
      DO 40 I=NFP1,NX
        R=X(I)
        DHX=DH*(1.D0+DABS(R))
        X(I) =R+DHX
        CALL FUNBF(X,FP,IER,ISTOP)
        X(I)=R
        IF (IER.NE.0.OR.ISTOP.NE.0) RETURN
        DO 30 J=1,NXM1
          DFUN(J,I)=(FP(J)-F(J))/DHX
          IF (DABS(DFUN(J,I)).LT.EPS0) DFUN(J,I)=0.D0
 30     CONTINUE
 40   CONTINUE
C      PRINT 101,(X(I),I=1,NX)
C 101  FORMAT(1X,'X=',3G13.5)
C      PRINT 102,((DFUN(I,J),J=1,NX),I=1,NF)
C 102  FORMAT(1X,'D=',3G13.5)
      IF (NX.EQ.NF+1) RETURN
      DO 60 I=1,NF
        R=X(I)
        DHX=DH*(1.+DABS(R))
        X(I) =R+DHX
        INDFUN=1
        CALL FUNBF(X,FP,IER,ISTOP)
        INDFUN=0
        X(I)=R
        IF (IER.NE.0.OR.ISTOP.NE.0) RETURN
        DO 50 J=NFP1,NXM1
          DFUN(J,I)=(FP(J)-F(J))/DHX
          IF (DABS(DFUN(J,I)).LT.EPS0) DFUN(J,I)=0.D0
 50     CONTINUE
 60   CONTINUE
      RETURN
      END
 
