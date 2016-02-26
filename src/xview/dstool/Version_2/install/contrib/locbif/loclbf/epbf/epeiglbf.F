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

      SUBROUTINE epEIGLBF(NK,X,IER)
C
C   Compute and output eigenvalues
C      07.02.1983
C      15.11.92
C      10.07.94 
C
C         IPR=0  - TOlxKO By~iClEHiE,
C         IPR=1  - By~iClEHiE i pE~ATx,
C         IPR=2  - TOlxKO pE~ATx.
C
      IMPLICIT REAL*8(A-H,O-Z)
      IMPLICIT INTEGER*2(I-N)
      include 'epbf.dim'
      DIMENSION X(NK)
      DIMENSION SCALE(NFMAX),INT(NFMAX)
      COMMON/LINBF/NF,NAP,ILN,NF2,ITP
      COMMON/LINBF1/DHF
      COMMON/LINBF2/D(NFMAX2)
      COMMON/LINBFE/RR(NFMAX),RI(NFMAX),IPR
      EXTERNAL FUNS
C
      IF (IPR.EQ.2) GO TO 40
C
C
C   By~iClEHiE MATPicy liHEAPizAcii
C
      CALL DER1BF(NK,DHF,X,D,FUNS,IER)
      IF (IER.NE.0) GO TO 60
C
C   By~iClEHiE CObCTBEHHyX zHA~EHij
C
      CALL DBALAN(NF,NF,D,LOW,IGH,SCALE)
      CALL DELMHE(NF,NF,LOW,IGH,D,INT)
      CALL DHQR(NF,NF,LOW,IGH,D,RR,RI,IER)
      IF (IER.NE.0) GO TO 50
C
C   upOPqdO~iBAHiE CObCTBEHHyX zHA~EHij pO BOzPACTAHi`
C      dEjCTBiTElxHOj ~ACTi
C
      NF1=NF-1
      DO 30 J=1,NF1
        RRMIN=RR(J)
        JMIN=J
        DO 20 I=J,NF1
          IF (RR(I+1).GE.RRMIN) GO TO 20
            RRMIN=RR(I+1)
            JMIN=I+1
 20     CONTINUE
        IF (JMIN.EQ.J) GO TO 30
          R1=RR(J)
          R2=RI(J)
          RR(J)=RRMIN
          RI(J)=RI(JMIN)
          RR(JMIN)=R1
          RI(JMIN)=R2
 30   CONTINUE
C
C   pE~ATx CObCTBEHHyX zHA~EHij
C
      IF (IPR.EQ.0) RETURN
 40   CONTINUE
#if defined(hib)
        PRINT 901
C       PRINT 902
        PRINT 903,(I,RR(I),RI(I),I=1,NF)
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
 901  FORMAT( 10X,'ëéÅëíÇÖççõÖ áçÄóÖçàü ( RE,IM ) :')
 902  FORMAT( 5X,4(6X,'RE',11X,'IM',9X))
 903  FORMAT( (5X,4(I2,')',1X,G11.4,1X,G11.4,3X)))
 904  FORMAT( 5X,'ëéÅëíÇÖççõÖ áçÄóÖçàü çÖ çÄâÑÖçõ IER=',I3)
 905  FORMAT( 5X,'ëéÅëíÇÖççõÖ áçÄóÖçàü çÖ çÄâÑÖçõ ',
     A       /5X,'(çÖ ÇõóàëãÖçÄ åÄíêàñÄ ãàçÖÄêàáÄñàà  IER=',I3,')')
#endif
      END
