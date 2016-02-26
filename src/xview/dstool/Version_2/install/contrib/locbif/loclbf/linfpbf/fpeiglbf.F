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

      SUBROUTINE fpEIGLBF(NK,X,IER)
C
C   COMPUTING AND PRINTING EIGENVALUES
C           ITERATED MAPS         
C     31.08.1990
C     16.11.92
C     10.07.94
C
C         IPR=0  - COMPUTING,
C         IPR=1  - COMPUTING AND PRINTING,
C         IPR=2  - PRINTING.
C
      IMPLICIT REAL*8(A-H,O-Z)
      IMPLICIT INTEGER*2(I-N)
      include 'linfpbf.dim'
      DIMENSION X(NK)
      DIMENSION SCALE(NFMAX),INT(NFMAX)
      COMMON/LINBF/NF,NAP,ILN,NF2,ITP
      COMMON/LINBF1/DHF
      COMMON/LINBF2/D(NFMAX2)
      COMMON/LINBFE/RR(NFMAX),RI(NFMAX),IPR
      COMMON/LINBEE/RMOD(NFMAX),ARG(NFMAX)
      EXTERNAL FUNM
C
      IF (IPR.EQ.2) GO TO 40
C
      PI=4.*DATAN(1.D0)
      PID2=PI/2.D0
C
C   By~iClEHiE MATPicy liHEAPizAcii
C
      CALL DER1BF(NK,DHF,X,D,FUNM,IER)
      IF (IER.NE.0) GO TO 60
C
C   By~iClEHiE CObCTBEHHyX zHA~EHij
C
      CALL DBALAN(NF,NF,D,LOW,IGH,SCALE)
      CALL DELMHE(NF,NF,LOW,IGH,D,INT)
      CALL DHQR(NF,NF,LOW,IGH,D,RR,RI,IER)
      IF (IER.NE.0) GO TO 50
C
C   ORDERING EIGENVALUES BY MODULUS
C
      DO 15 I=1,NF
C        RMOD(I)=DSQRT(RR(I)*RR(I)+RI(I)*RI(I))
        RMOD(I)=DSQRT(RR(I)**2+RI(I)**2)
c	PRINT 801,I,RR(I),RI(I),WRK
c        WRK=RR(I)**2+RI(I)**2
c	PRINT 801,I,RR(I),RI(I),WRK
c801    FORMAT('  EIGLBF: I,RR,RI,WRK= ',I2,3G15.6)
c        RMOD(I)=DSQRT(WRK)
c	PRINT 802,RMOD(I)
c802    FORMAT('  EIGLBF: RMOD=',G15.6)
        IF (DABS(RR(I)/RMOD(I)).GT.0.5D0)
     1    ARG(I)=DATAN(DABS(RI(I)/RR(I)))
c	PRINT 803,ARG(I)
c803    FORMAT('  EIGLBF: case 1  ARG=',G15.6)
        IF (DABS(RR(I)/RMOD(I)).LE.0.5D0)
     1    ARG(I)=PID2-DATAN(DABS(RR(I)/RI(I)))
c	PRINT 804,ARG(I)
c804    FORMAT('  EIGLBF: case 2  ARG=',G15.6)
        IF (RR(I).LT.0.D0) ARG(I)=PI-ARG(I)
        IF (RI(I).LT.0.D0) ARG(I)=-ARG(I)
        ARG(I)=ARG(I)/PI*180.D0
 15   CONTINUE
      NF1=NF-1
      DO 30 J=1,NF1
        RMDMIN=RMOD(J)
        JMIN=J
        DO 20 I=J,NF1
          IF (RMOD(I+1).GE.RMDMIN) GO TO 20
            RMDMIN=RMOD(I+1)
            JMIN=I+1
 20     CONTINUE
        IF (JMIN.EQ.J) GO TO 30
          R1=RR(J)
          R2=RI(J)
          R3=RMOD(J)
          R4=ARG(J)
          RR(J)=RR(JMIN)
          RI(J)=RI(JMIN)
          RMOD(J)=RMOD(JMIN)
          ARG(J)=ARG(JMIN)
          RR(JMIN)=R1
          RI(JMIN)=R2
          RMOD(JMIN)=R3
          ARG(JMIN)=R4
 30   CONTINUE
C
C   PRINTING EIGENVALUES
C
      IF (IPR.EQ.0) RETURN
 40   CONTINUE
#if defined(hib)
        PRINT 901
C       PRINT 902
        PRINT 903,(I,RR(I),RI(I),RMOD(I),ARG(I),I=1,NF)
#endif
      RETURN
C
C   ObPAbOTKA B Clu~AE HEBy~iClEHiq CObCTBEHHyX zHA~EHij
C
 50   CONTINUE
#if defined(hib)
      IF (IPR.NE.0) PRINT 904,IER
#endif
      RETURN
 60   CONTINUE
#if defined(hib)
      IF (IPR.NE.0) PRINT 905,IER
#endif
      RETURN
#if defined(hib)
 901  FORMAT( 10X,'MULTIPLIERS ( RE,IM,MOD,ARG ) :')
 902  FORMAT( 5X,2(6X,'RE',11X,'IM',9X))
 903  FORMAT( (5X,2(I2,')',1X,G11.4,1X,G11.4,1X,G11.4,1X,G11.4,3X)))
 904  FORMAT( 5X,'MULTIPLIERS NOT COMPUTED IER=',I3)
 905  FORMAT( 5X,'MULTIPLIERS NOT COMPUTED ',
     A       /5X,'(çÖ ÇõóàëãÖçÄ åÄíêàñÄ ãàçÖÄêàáÄñàà  IER=',I3,')')
#endif
      END
