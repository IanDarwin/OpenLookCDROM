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

      SUBROUTINE epFUNBF(X,F,IER,ISTOP)
C
C   COMPUTING DETERMINING FUNCTIONS FOR LOCAL BIFURCATION CURVES
C
C          18.05.1990
C          08.08.1991
C          15.11.92
C          06.09.93
C          10.07.94
C
      IMPLICIT REAL*8(A-H,O-Z)
      IMPLICIT INTEGER*2(I-N)
      include 'epbf.dim'
      DIMENSION X(1),F(1)
      COMMON/PPAR/P(NPARMX)
      COMMON/IPPAR/IP(NPARMX)
      COMMON/LINBF/NF,NAP,ILN,NF2,ITP
      COMMON/LINBFE/RR(NFMAX),RI(NFMAX),IPR
      COMMON/LINBFO/INDFUN
      COMMON /IERFS/ IERFS, IERDUM
      COMMON /FUNVAL/ FVAL(NDIM)
      COMMON /IHOMP/  IHOMP
      IER=0
      ISTOP=0
      NK=NF+NAP
      IPR=1
      DO 1  I=1,NAP
        K=IP(I)
        P(K)=X(NF+I)
 1    CONTINUE
      IF (INDFUN.NE.0) GO TO 3
      CALL FUNS(X,F)
        IER=IERFS
        IF (IERFS.NE.0) GO TO 1000
 3    GO TO (1000, 5,100,200),NAP
 5    GO TO (10,20,30,40),ILN
C
C   Fold curve
 10   CALL DETJAC(NK,X,DJAC,IER)
      F(NF+1)=DJAC
      GO TO 1000
C
C   Hopf curve
 20   CALL DETRG(NK,X,DRG,IER)
      F(NF+1)=DRG
      GO TO 1000
C
C   L1=0 curve
 30   CALL VLP(NK,X,VL,IER)
      F(NF+1)=VL
      IF (IER.NE.0) GO TO 1000
#if defined(hib)
        PRINT 901,IER
        PRINT 903,X
#endif
        CALL EIGLBF(NK,X,IER1)
        GO TO 1000
C
C   Double eigenvalue curve
 40   CALL DETEV(NK,X,DEV,IER)
      F(NF+1)=DEV
      GO TO 1000
C
 100  ILN1=ILN-10
      GO TO (110,110,130,140,150,160,170),ILN1
C
C   Double zero curve and Fold+Hopf curve  (ILN=11,12)
 110  CALL DETJAC(NK,X,DJAC,IER)
        IF (IER.NE.0) GOTO 1000
      F(NF+1)=DJAC
      CALL DETRG(NK,X,DRG,IER)
      F(NF+2)=DRG
      GO TO 1000
C
C   Double Hopf curve (ILN=13)
 130  RETURN
C
C   Cusp curve  (ILN=14)
 140  CALL DETJAC(NK,X,DJAC,IER)
        IF (IER.NE.0) GOTO 1000
      F(NF+1)=DJAC
      CALL QUADR(NK,X,QDR,IER)
      F(NF+2)=QDR
      IF (IER.EQ.0) GO TO 1000
#if defined(hib)
        PRINT 902,IER
        PRINT 903,X
#endif
        CALL EIGLBF(NK,X,IER1)
        GO TO 1000
C
C   Degenerate Hopf curve (ILN=15)
 150  CALL DETRG(NK,X,DRG,IER)
        IF (IER.NE.0) GOTO 1000
      F(NF+1)=DRG
      CALL VLP(NK,X,VL,IER)
      F(NF+2)=VL
      IF (IER.EQ.0) GO TO 1000
#if defined(hib)
        PRINT 901,IER
        PRINT 903,X
#endif
        CALL EIGLBF(NK,X,IER)
        GO TO 1000
C
C   Fold+NT curve  (ILN=16)
 160  CALL DETJAC(NK,X,DJAC,IER)
        IF (IER.NE.0) GOTO 1000
      F(NF+1)=DJAC
      CALL DETKRV(NK,X,DKRV,IER)
      F(NF+2)=DKRV
      GO TO 1000
C
C   Hopf+NT curve  (ILN=17)
 170  CALL DETRG(NK,X,DRG,IER)
        IF (IER.NE.0) GOTO 1000
      F(NF+1)=DRG
      CALL DETNTV(NK,X,DNTV,IER)
      F(NF+2)=DNTV
      GO TO 1000
C
 200  ILN1=ILN-20
      GO TO (210,210,230,230),ILN
C
C   Degenerate BT and GG curves (ILN=21,22)
 210  CALL DETJAC(NK,X,DJAC,IER)
        IF (IER.NE.0) GOTO 1000
      F(NF+1)=DJAC
      CALL DETRG(NK,X,DRG,IER)
        IF (IER.NE.0) GOTO 1000
      F(NF+2)=DRG
      CALL QUADR(NK,X,QDR,IER)
      F(NF+3)=QDR
      IF (IER.EQ.0) GO TO 1000
#if defined(hib)
        PRINT 902,IER
        PRINT 903,X
#endif
        CALL EIGLBF(NK,X,IER1)
        GO TO 1000
C
C   BT+NT and GG+NT curves (ILN=23,24)
 230  CALL DETJAC(NK,X,DJAC,IER)
        IF (IER.NE.0) GOTO 1000
      F(NF+1)=DJAC
      CALL DETRG(NK,X,DRG,IER)
        IF (IER.NE.0) GOTO 1000
      F(NF+2)=DRG
      CALL DETVPR(NK,X,DVPR,IER)
      F(NF+3)=DVPR
C
 1000 IF(IERFS.LT.0) ISTOP=IERFS
      IF (IHOMP.NE.0) THEN
         DO 1100 I=1,NK-1
            F(I)=P(IHOMP)*F(I)+(1.D0-P(IHOMP))*(F(I)-FVAL(I))
 1100    CONTINUE
      ENDIF
      RETURN
C
#if defined(hib)
 901  FORMAT(/1X,'DFUN: Computing Lyapunov's quantity failed, IER=',I3)
 902  FORMAT(/1X,'DFUN: Computing coefficient ',
                        'at a quadratic term failed, IER=',I3)
 903  FORMAT( 5X,'X =',8G14.6 /(8X,8G14.6))
#endif
      END
 
