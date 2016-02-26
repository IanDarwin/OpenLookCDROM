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

      SUBROUTINE fpFUNBF(X,F,IER,ISTOP)
C
C   COMPUTING DETERMINING FUNCTIONS FOR LOCAL BIFURCATION CURVES.
C      FIXED POINTS OF ITERATED MAPS.       
C
C          18.05.1990
C          02.09.1990
C          08.08.1991
C          16.11.92
C          06.09.93
C          10.07.94
C
      IMPLICIT REAL*8(A-H,O-Z)
      IMPLICIT INTEGER*2(I-N)
      include 'fpbf.dim'
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
      CALL FUNM(X,F)
      DO 2 I=1,NF
        F(I)=F(I)-X(I)
 2    CONTINUE
        IER=IERFS
        IF (IERFS.NE.0) GO TO 1000
 3    GO TO (1000, 5,100,200),NAP
 5    GO TO (10,20,30,40),ILN
C
C   FOLD CURVE
 10   CALL DETFLD(NK,X,DJAC,IER)
      F(NF+1)=DJAC
      GO TO 1000
C
C   HOPF CURVE
 20   CALL DETHPF(NK,X,DRG,IER)
      F(NF+1)=DRG
      GO TO 1000
C
C   FLIP CURVE
 30   CALL DETFLP(NK,X,DJAC,IER)
      F(NF+1)=DJAC
      GO TO 1000
C
C   DOUBLE EIGENVALUE CURVE
 40   CALL DETEV(NK,X,DEV,IER)
      F(NF+1)=DEV
      GO TO 1000
C
 100  ILN1=ILN-10
      GO TO (110,120,110,120,150,160,170,180,190),ILN1
C
C   FOLD + HOPF  (ILN=11,13)
 110  CALL DETFLD(NK,X,DJAC,IER)
        IF (IER.NE.0) GOTO 1000
      F(NF+1)=DJAC
      CALL DETHPF(NK,X,DRG,IER)
      F(NF+2)=DRG
      GO TO 1000
C
C   FLIP + HOPF  (ILN=12,14)
 120  CALL DETFLP(NK,X,DJAC,IER)
        IF (IER.NE.0) GOTO 1000
      F(NF+1)=DJAC
      CALL DETHPF(NK,X,DRG,IER)
      F(NF+2)=DRG
      GO TO 1000
C
C   FOLD + FLIP  (ILN=15)
 150  CALL DETFLD(NK,X,DJAC,IER)
        IF (IER.NE.0) GOTO 1000
      F(NF+1)=DJAC
      CALL DETFLP(NK,X,DJAC,IER)
      F(NF+2)=DJAC
      GO TO 1000
C
C   CUSP CURVE (ILN=16)
 160  CALL DETFLD(NK,X,DJAC,IER)
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
C   FOLD + NONTRANSVERSALITY  (ILN=17)
 170  CALL DETFLD(NK,X,DJAC,IER)
        IF (IER.NE.0) GOTO 1000
      F(NF+1)=DJAC
      CALL TRFLD(NK,X,DTR,IER)
      F(NF+2)=DTR
      GO TO 1000
C
C   HOPF + NONTRANSVERSALITY  (ILN=18)
 180  CALL DETHPF(NK,X,DRG,IER)
        IF (IER.NE.0) GOTO 1000
      F(NF+1)=DRG
      CALL TRHPF(NK,X,DTR,IER)
      F(NF+2)=DTR
      GO TO 1000
C
C   FLIP + NONTRANSVERSALITY  (ILN=19)
 190  CALL DETFLP(NK,X,DJAC,IER)
        IF (IER.NE.0) GOTO 1000
      F(NF+1)=DJAC
      CALL TRFLP(NK,X,DTR,IER)
      F(NF+2)=DTR
      GO TO 1000
C
 200  ILN1=ILN-20
      GO TO 1000
C      GO TO (210,210,230,230),ILN
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
 902  FORMAT(/1X,'DFUN: Computing coefficient ',
                        'at a quadratic term failed, IER=',I3)
 903  FORMAT( 5X,'X =',8G14.6 /(8X,8G14.6))
#endif
      END
 
