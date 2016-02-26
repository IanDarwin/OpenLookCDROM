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

      SUBROUTINE fpZEROBF(NK,X,IFZERO,G,JG)
C
C   Processing zeroes of bifurcation functions
C        ITERATED MAPS
C      20.05.1990
C      02.09.1990
C      16.11.92
C      10.07.94
C
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER*2(I-N)
      include 'linfpbf.dim'
      DIMENSION X(1)
      COMMON/LINBF/NF,NAP,ILN,NF2,ITP
C----
      COMMON /LINBFQ/ IPT(NPTYP), INDER, IVAL(NVALPT),ISTP
      COMMON /LINBFR/ VALPT(NVALPT)
      IPT(2)=3
      IPT(3)=1
      IPT(4)=1
      INDER =JG
C----
#if defined(hib)
      PRINT 921
#endif
      IF (ILN.EQ.0.AND.IFZERO.EQ.1)
     1   IPT(3)=1
      IF (ILN.EQ.0.AND.IFZERO.EQ.2)
     1   IPT(3)=2
      IF (ILN.EQ.0.AND.IFZERO.EQ.3)
     1   IPT(3)=3
      IF (ILN.EQ.0.AND.IFZERO.EQ.4)
     1   IPT(3)=4
      IF (ILN.EQ.1.AND.IFZERO.EQ.2)
     1   IPT(3)=1
      IF (ILN.EQ.1.AND.IFZERO.EQ.3)
     1   IPT(3)=2
      IF (ILN.EQ.1.AND.IFZERO.EQ.5)
     1   IPT(3)=3
      IF (ILN.EQ.1.AND.IFZERO.EQ.4)
     1   IPT(3)=4
      IF (ILN.EQ.2.AND.IFZERO.EQ.1)
     1   IPT(3)=1
      IF (ILN.EQ.2.AND.IFZERO.EQ.3)
     1   IPT(3)=2
      IF (ILN.EQ.2.AND.IFZERO.EQ.4)
     1   IPT(3)=3
      IF (ILN.EQ.3.AND.IFZERO.EQ.1)
     1   IPT(3)=1
      IF (ILN.EQ.3.AND.IFZERO.EQ.2)
     1   IPT(3)=2
      IF (ILN.EQ.3.AND.IFZERO.EQ.4)
     1   IPT(3)=3
      IF (ILN.EQ.4.AND.IFZERO.EQ.1)
     1   IPT(3)=1
      IF (ILN.EQ.4.AND.IFZERO.EQ.2)
     1   IPT(3)=2
      IF (ILN.EQ.4.AND.IFZERO.EQ.3)
     1   IPT(3)=3
      IF (ILN.EQ.11.AND.IFZERO.EQ.5)
     1   IPT(3)=1
      IF (ILN.EQ.11.AND.IFZERO.EQ.3)
     1   IPT(3)=2
      IF (ILN.EQ.12.AND.IFZERO.EQ.1)
     1   IPT(3)=1
      IF (ILN.EQ.13.AND.IFZERO.EQ.3)
     1   IPT(3)=1
      IF (ILN.EQ.13.AND.IFZERO.EQ.5)
     1   IPT(3)=2
      IF (ILN.EQ.14.AND.IFZERO.EQ.1)
     1   IPT(3)=1
      IF (ILN.EQ.15.AND.IFZERO.EQ.2)
     1   IPT(3)=1
      IF (ILN.EQ.15.AND.IFZERO.EQ.5)
     1   IPT(3)=2
      IF (ILN.EQ.16.AND.IFZERO.EQ.2)
     1   IPT(3)=1
      IF (ILN.EQ.16.AND.IFZERO.EQ.3)
     1   IPT(3)=2
      IF (ILN.EQ.17.AND.IFZERO.EQ.2)
     1   IPT(3)=1
      IF (ILN.EQ.17.AND.IFZERO.EQ.3)
     1   IPT(3)=2
      IF (ILN.EQ.17.AND.IFZERO.EQ.5)
     1   IPT(3)=3
      IF (ILN.EQ.18.AND.IFZERO.EQ.1)
     1   IPT(3)=1
      IF (ILN.EQ.18.AND.IFZERO.EQ.3)
     1   IPT(3)=2
      IF (ILN.EQ.19.AND.IFZERO.EQ.1)
     1   IPT(3)=1
      IF (ILN.EQ.19.AND.IFZERO.EQ.2)
     1   IPT(3)=2
C
      IF (ILN.EQ.1.OR.ILN.EQ.11.OR.ILN.EQ.13.OR.ILN.EQ.15.OR.
     1    ILN.EQ.17.OR.IFZERO.EQ.1) GO TO 5 
      GO TO 700
 5    CONTINUE
C    By~iClEHiE KO|fficiEHTA pPi KBAdPATi~HOM ~lEHE
      CALL QUADR(NK,X,QDR,IERQDR)
      VALPT(1)=QDR
      IVAL (1)=IERQDR
C
      GO TO 700
C
C---ObPAbOTKA TO~Ki KPATHOCTi HA liHii gPubyX CTAciOHAPOB
C
 10   CONTINUE
#if defined(hib)
      PRINT 901,G
      IF (JG .NE.0) PRINT 950
#endif
C    By~iClEHiE KO|fficiEHTA pPi KBAdPATi~HOM ~lEHE
      CALL QUADR(NK,X,QDR,IERQDR)
C----
      IPT(3)=1
      VALPT(1)=QDR
      IVAL (1)=IERQDR
C----
      IF (IERQDR.NE.0) GO TO 510
#if defined(hib)
        PRINT 911,QDR
#endif
        GO TO 520
 510  CONTINUE
#if defined(hib)
      PRINT 912
#endif
 520  GO TO 700
C
C---ObPAbOTKA TO~Ki HEjTPAlxHOCTi HA liHii gPubyX CTAciOHAPOB
C
 20   CONTINUE
#if defined(hib)
c      PRINT 902,G
c      IF (JG .NE.0) PRINT 950
#endif
C    By~iClEHiE lqpuHOBCKOj BEli~iHy
C      CALL VLP(NK,X,VL,IERVL)
C----
c      IPT(3)=2
c      VALPT(1)=VL
c      IVAL (1)=IERVL
c                      IPT(4)= 1
c      IF (IERVL.NE.0) IPT(4)= 2
c      IF (IERVL.EQ.3) IPT(4)=-1
C
C	      IPT(4)=1,2 - Hopf case, IPT(4)=-1 - saddle case 
C----
c      IF (IERVL.NE.0) GO TO 530
#if defined(hib)
c        PRINT 913,VL
#endif
c        GO TO 540
 530  CONTINUE
#if defined(hib)
      PRINT 914
#endif
 540  GO TO 700
C
C---ObPAbOTKA TO~Ki KPATHyX CObCTBEHHyX zHA~EHij HA liHii gPubyX
C         CTAciOHAPOB
 30   CONTINUE
#if defined(hib)
      PRINT 903,G
      IF (JG .NE.0) PRINT 950
#endif
C----
      IPT(3)=3
C----
      GO TO 700
C
C---ObPAbOTKA TO~Ki    "KPATHOCTx" + "HEjTPAlxHOCTx"
C
 40   CONTINUE
#if defined(hib)
      IF (ILN.EQ.1.OR.ILN.EQ.16) PRINT 902,G
      IF (ILN.EQ.2.OR.ILN.EQ.17) PRINT 901,G
      IF (JG .NE.0) PRINT 950
#endif
C    By~iClEHiE KO|fficiEHTA pPi KBAdPATi~HOM ~lEHE
      CALL QUADR(NK,X,QDR,IERQDR)
C----
      IPT(3)=1
      VALPT(1)=QDR
      IVAL (1)=IERQDR
C----
      IF (IERQDR.NE.0) GO TO 550
#if defined(hib)
        PRINT 911,QDR
#endif
        GO TO 560
 550  CONTINUE
#if defined(hib)
      PRINT 912
#endif
 560  GO TO 700
C
C---ObPAbOTKA TO~Ki TPEXKPATHOCTi HA liHii KPATHyX CTAciOHAPOB
C
 50   CONTINUE
#if defined(hib)
      PRINT 904,G
      IF (JG  .NE.0) PRINT 903
#endif
C----
      IPT(3)=2
C----
      GO TO 700
C
C---ObPAbOTKA TO~Ki "KPATHyE CObCTBEHHyE zHA~EHiq" +  "KPATHOCTx"
C
 60   CONTINUE
#if defined(hib)
      IF (IFZERO.EQ.1) PRINT 901,G
      IF (IFZERO.EQ.4) PRINT 903,G
      IF (JG  .NE.0) PRINT 903
#endif
C   By~iClEHiE KO|fficiEHTA pPi KBAdPATi~HOM ~lEHE
      CALL QUADR(NK,X,QDR,IERQDR)
C----
      IF (ILN.EQ.1) IPT(3)=3
      IF (ILN.EQ.4) IPT(3)=1
      VALPT(1)=QDR
      IVAL (1)=IERQDR
C----
      IF (IERQDR.NE.0) GO TO 570
#if defined(hib)
        PRINT 911,QDR
#endif
        GO TO 580
 570  CONTINUE
#if defined(hib)
      PRINT 912
#endif
 580  GO TO 700
C
C---ObPAbOTKA TO~Ki "HEjTPAlxHOCTx" + "L1=0"
C
 70   CONTINUE
#if defined(hib)
      IF (IFZERO.EQ.2) PRINT 902,G
      IF (IFZERO.EQ.5) PRINT 905,G
      IF (JG .NE.0) PRINT 950
#endif
C----
      IF (ILN.EQ.2) IPT(3)=2
      IF (ILN.EQ.3) IPT(3)=1
C----
      GO TO 700
C
C---ObPAbOTKA TO~Ki "KPATHyE CObCTBEHHyE zHA~EHiq" + "HEjTPAlxHOCTx"
C
 80   CONTINUE
#if defined(hib)
      IF (IFZERO.EQ.2) PRINT 902,G
      IF (IFZERO.EQ.3) PRINT 903,G
      IF (JG .NE.0) PRINT 950
#endif
C----
      IF (ILN.EQ.2) IPT(3)=3
      IF (ILN.EQ.4) IPT(3)=2
C----
      GO TO 700
C
C---ObPAbOTKA TO~Ki "TPEXKPATHOCTx" + "HEjTPAlxHOCTx"
C
 90   CONTINUE
#if defined(hib)
      IF (IFZERO.EQ.2) PRINT 902,G
      IF (IFZERO.EQ.4) PRINT 904,G
      IF (JG .NE.0) PRINT 950
#endif
C----
      IPT(3)=1
C----
      GO TO 700
C
 700  CALL PRNTBF(NK,X)
      IF (ILN.LT.0) GO TO 750
      CALL EIGLBF(NK,X,IER)
 750  CONTINUE
#if defined(hib)
      PRINT 921
#endif
      RETURN
#if defined(hib)
 901  FORMAT(/3X,'ÇõèéãçÖçé ìëãéÇàÖ äêÄíçéëíà  (G(1)=',G11.4,')')
 902  FORMAT(/3X,'ÇõèéãçÖçé ìëãéÇàÖ çÖâíêÄãúçéëíà  (G(2)=',G11.4,')')
 903  FORMAT(/3X,'äêÄíçõÖ ëéÅëíÇÖççõÖ áçÄóÖçàü  (G(3)=',G11.4,')')
 904  FORMAT(/3X,'ÇõèéãçÖçé ìëãéÇàÖ íêÖïäêÄíçéëíà   (G(4)=',G11.4,')')
 905  FORMAT(/3X,'èÖêÇÄü ãüèìçéÇëäÄü ÇÖãàóàçÄ êÄÇçÄ çìãû  ',
     A  '(G(5)=',G11.4,')')
 950  FORMAT(/ 5X,'(íêÖÅìÖåÄü íéóçéëíú çÖ ÑéëíàÉçìíÄ)'/)
 911  FORMAT(/4X,'äéùîîàñàÖçí èêà äÇÄÑêÄíàóçéå óãÖçÖ Ç ìêÄÇçÖçàà ',
     A  'êÄáÇÖíÇãÖçàü =',G13.6)
 912  FORMAT(/4X,'äéùîîàñàÖçí èêà äÇÄÑêÄíàóçéå óãÖçÖ Ç ìêÄÇçÖçàà ',
     A  'êÄáÇÖíÇãÖçàü çÖ ÇõóàëãÖç')
 913  FORMAT(/4X,'çÖâíêÄãúçéëíú îéäìëçéÉé íàèÄ - ',
     A  'èÖêÇÄü ãüèìçéÇëäÄü ÇÖãàóàçÄ =',G13.6)
 914  FORMAT(/4X,'çÖâíêÄãúçéëíú ëÖÑãéÇéÉé íàèÄ')
 921  FORMAT(/1X,120('-')/)
 931  FORMAT(/5X,'P(',I2,')=',G13.6,2X,'X =',6G14.6 /(29X,6G14.6))
#endif
      END
 
