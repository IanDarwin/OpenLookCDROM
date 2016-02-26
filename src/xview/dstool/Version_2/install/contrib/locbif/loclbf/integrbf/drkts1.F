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

c      SUBROUTINE DRKTS1(NDIM,X,XEND,Y,HI,HMIN,HMAX,EPS,
      SUBROUTINE DRKTS1(NDIM,XT,XEND,Y,HI,HMIN,HMAX,EPS,
     *   FCT,OUTP,IEND,DERY,AUX)
C      SUBROUTINE DRKTS1(PRMT,Y,DERY,NDIM,IEND,FCT,OUTP, AUX)
C
      IMPLICIT REAL*8(A-H,O-Z)
	IMPLICIT INTEGER*2 (I-N)
      DIMENSION Y(1),DERY(1),AUX(8,1),A(4),B(4),C(4)
C      COMMON/CDRKAS/HMIN,HMAX,HI,TK,NH
      COMMON/STAT/NFCN,NSTEP,NACCPT,NREJCT,NDEC,NSOL
      COMMON /IERFS/ IERFS,IERDUM
      COMMON /iabort/iabort
      IERFS=0
      iabort=0
      ISTOP=0
      IEND=0
      NFCN=0
      DO I=1,NDIM
        AUX(8,I)=.066666666666666667*DERY(I)
      ENDDO
      IF ((XEND-X)*HI.EQ.0.D0) GO TO 37
      IF (HMIN.LT.0.D0.OR.HI.LT.HMIN.OR.HI.GT.HMAX) GO TO 37
C
      IF (XEND.GT.X) THEN 
         H=HI
         SH=1.D0
      ELSE
         H=-HI
         SH=-1.D0
      ENDIF   
C
      NH=-1
      TK=X
      CALL FCT(NDIM,X,Y,DERY)
        IF (ierfs.ne.0)	GOTO 36
      NFCN=NFCN+1
      A(1)=.5D0
      A(2)=.29289321881345248D0
      A(3)=1.7071067811865475D0
      A(4)=.16666666666666667D0
      B(1)=2.D0
      B(2)=1.D0
      B(3)=1.D0
      B(4)=2.D0
      C(1)=.5D0
      C(2)=.29289321881345248D0
      C(3)=1.7071067811865475D0
      C(4)=.5D0
      DO I=1,NDIM
        AUX(1,I)=Y(I)
        AUX(2,I)=DERY(I)
        AUX(3,I)=0.D0
        AUX(6,I)=0.D0
      ENDDO
      IREC=0
      H=H+H
      IHLF=-1
      ISTEP=0
      IENDD=0
      ICODE=0
C
      X=XT
C
C
 4    IF ((XEND-X-1.1D0*H)*SH.GT.0.D0) GO TO 7
      H=XEND-X
      IENDD=1
 7    TK=X
      NH=NH+1
      WRITE (6,*) '  x=',X,'  h=',H,'  nh=',NH,'  XT=',XT
      CALL OUTP(NDIM,X,Y,H,ICODE,ISTOP)
      ICODE=1
      IF (ISTOP.NE.0) GO TO 40
 8    ITEST=0
 9    ISTEP=ISTEP+1
      J=1
C
 10   AJ=A(J)
      BJ=B(J)
      CJ=C(J)
      DO I=1,NDIM
        R1=H*DERY(I)
        R2=AJ*(R1-BJ*AUX(6,I))
        Y(I)=Y(I)+R2
        R2=R2+R2+R2
        AUX(6,I)=AUX(6,I)+R2-CJ*R1
      ENDDO
      IF(J-4)12,15,15
   12 J=J+1
      IF(J-3)13,14,13
   13 X=X+.5D0*H
   14 CALL FCT(NDIM,X,Y,DERY)
        IF (ierfs.ne.0)	GOTO 36
      NFCN=NFCN+1
      GOTO 10
C
 15   IF(ITEST.GT.0) GO TO 20
      DO I=1,NDIM
        AUX(4,I)=Y(I)
      ENDDO
      ITEST=1
      ISTEP=ISTEP+ISTEP-2
 18   IHLF=IHLF+1
      X=X-H
      H=.5D0*H
      IF (IENDD.EQ.0) HI=DABS(H)
      DO I=1,NDIM
        Y(I)=AUX(1,I)
        DERY(I)=AUX(2,I)
        AUX(6,I)=AUX(3,I)
      ENDDO
      GOTO 9
C
 20   IMOD=ISTEP/2
      IF(ISTEP-IMOD-IMOD.EQ.0) GO TO 23
      CALL FCT(NDIM,X,Y,DERY)
        IF (ierfs.ne.0)	GOTO 36
      NFCN=NFCN+1
      DO I=1,NDIM
        AUX(5,I)=Y(I)
        AUX(7,I)=DERY(I)
      ENDDO
      GOTO 9
C
 23   DELT=0.D0
      DO I=1,NDIM
        DELT=DELT+AUX(8,I)*DABS(AUX(4,I)-Y(I))
      ENDDO
      IF (DELT.LE.EPS) GO TO 28
      IF (H*SH.LT.2.D0*HMIN) GO TO 36
      DO I=1,NDIM
        AUX(4,I)=AUX(5,I)
      ENDDO
      ISTEP=ISTEP+ISTEP-4
      X=X-H
      IENDD=0
      GOTO 18
C
 28   CALL FCT(NDIM,X,Y,DERY)
        IF (ierfs.ne.0)	GOTO 36
      NFCN=NFCN+1
      DO I=1,NDIM
        AUX(1,I)=Y(I)
        AUX(2,I)=DERY(I)
        AUX(3,I)=AUX(6,I)
        Y(I)=AUX(5,I)
        DERY(I)=AUX(7,I)
      ENDDO
      TK=X-H
      NH=NH+1
      WRITE (6,*) '  x-h=',X-H,'  nh=',NH,'  x=',X,'  h=',H,'  XT=',XT
c      CALL OUTP(NDIM,X-H,Y,H,ICODE,ISTOP)
      XMH=X-H
      CALL OUTP(NDIM,XMH,Y,H,ICODE,ISTOP)
      WRITE (6,*) '  x-h=',X-H,'  nh=',NH,'  x=',X,'  h=',H,'  XT=',XT
      IF (ISTOP.NE.0) GO TO 40
      DO I=1,NDIM
        Y(I)=AUX(1,I)
        DERY(I)=AUX(2,I)
      ENDDO
      IREC=IHLF
      IF (IENDD.GT.0) THEN
        ICODE=2
        CALL OUTP(NDIM,X,Y,H,ICODE,ISTOP)
        IF (ISTOP.NE.0) GO TO 40
        IEND=1
        RETURN
      ENDIF
      IHLF=IHLF-1
      ISTEP=ISTEP/2
      H=H+H
      WRITE (6,*) '  x-h=',X-H,'  nh=',NH,'  x=',X,'  h=',H
      IF (H*SH.GT.HMAX) GO TO 4
      IMOD=ISTEP/2
      IF(ISTEP-IMOD-IMOD.NE.0) GO TO 4
      IF(DELT-.02D0*EPS.GT.0.D0) GO TO 4
      IHLF=IHLF-1
      ISTEP=ISTEP/2
      H=H+H
      GOTO 4
C
 36   IEND=-3
      IF (ierfs.LT.0 .OR. iabort.NE.0) GO TO 40
      return
 37   IEND=-1
      return
 40   IEND=-5
      RETURN
      END
