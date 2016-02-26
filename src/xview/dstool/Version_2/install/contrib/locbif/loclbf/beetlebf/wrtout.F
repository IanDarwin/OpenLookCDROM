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

      SUBROUTINE WRTOUT (IWRT,ISTEP,IPR,IT,IDIV,NCAN,NX,NDIMM,X,DFUN)
C     3.05.90ƒ.
C     13.11.92
C* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C*                                                                     *
C*    POƒPAMMA WRTOUT PE„HA‡HA—EHA „‹Ÿ E—ATˆ CTAH„APTHOƒO POTOKO‹A *
C*    PAOT› POƒPAMM›  BEETLE.                                        *
C*                                                                     *
C*     A P A M E T P ›:                                               *
C*    ===================                                              *
C*    IWRT  -Pˆ‡HAK E—ATˆ TPE“EMOƒO COO™EHˆŸ:                      *
C*                                                                     *
C*           1 -CTAH„APTHAŸ E—ATœ KOOP„ˆHAT O—EPE„HO‰ TO—Kˆ HA KPˆBO‰;*
C*              (E—ATœ HEKOTOP›X KOHTPO‹œH›X XAPAKTEPˆCTˆK KPˆBO‰).   *
C*                                                                     *
C*           2 -COO™EHˆE: HE CO˜‹ˆCœ HœžTOHOBCKˆE ˆTEPA–ˆˆ,           *
C*              ‡A„AHHAŸ TO—HOCTœ HE „OCTˆƒH“TA.                       *
C*                                                                     *
C*           3 -COO™EHˆE: HE CO˜‹ˆCœ HœžTOHOBCKˆE ˆTEPA–ˆˆ,           *
C*              B›PO†„EHA MATPˆ–A —ACTH›X POˆ‡BO„H›X                  *
C*              (HA E—ATœ B›„AETCŸ MATPˆ–A ŸKOˆ).                    *
C*                                                                     *
C*           4 -COO™EHˆE: HE CO˜‹ˆCœ HœžTOHOBCKˆE ˆTEPA–ˆˆ,           *
C*              HE B›—ˆC‹EH› ‡HA—EHˆŸ ”“HK–ˆ‰ B O—EPE„HO‰ TO—KE        *
C*              HœžTOHOBCKOƒO POƒHO‡A.                                *
C*                                                                     *
C*           5 -COO™EHˆE: HE CO˜‹ˆCœ HœžTOHOBCKˆE ˆTEPA–ˆˆ,           *
C*              HE B›—ˆC‹EHA MATPˆ–A —ACTH›X POˆ‡BO„H›X B             *
C*              O—EPE„HO‰ TO—KE HœžTOHOBCKOƒO POƒHO‡A.                *
C*                                                                     *
C*           6 -COO™EHˆE: HE HA‰„EH KACATE‹œH›‰ BEKTOP, B›PO†„EHA     *
C*              MATPˆ–A —ACTH›X POˆ‡BO„H›X (HA E—ATœ B›„AETCŸ        *
C*              MATPˆ–A —ACTH›X POˆ‡BO„H›X).                          *
C*                                                                     *
C*           7 -COO™EHˆE: HE HA‰„EH KACATE‹œH›‰ BEKTOP, HE B›—ˆC‹EH›  *
C*              ‡HA—EHˆŸ ”“HK–ˆ‰ B TO—KE HA KPˆBO‰.                    *
C*                                                                     *
C*           8 -COO™EHˆE: HE HA‰„EH KACATE‹œH›‰ BEKTOP, HE B›—ˆC‹EHA  *
C*              MATPˆ–A —ACTH›X POˆ‡BO„H›X B TO—KE HA KPˆBO‰.         *
C*                                                                     *
C*           9 -COO™EHˆE: “TO—HEHHAŸ TO—KA HE ‹ˆ‡KA K TO—KE-POƒHO‡“.*
C*                                                                     *
C*          10 -COO™EHˆE: POƒHO‡ O KACATE‹œHO‰ B›˜E‹ ‡A ƒPAHˆ–“.    *
C*                                                                     *
C*          11 -COO™EHˆE: POƒHO‡ O HœžTOH“ B›˜E‹ ‡A ƒPAHˆ–“.        *
C*                                                                     *
C*   (12,13,14)-COO™EHˆE: “TO—HŸETCŸ TO—KA ‡AM›KAHˆŸ.                 *
C*                                                                     *
C*          15 -COO™EHˆE: O‹œ‡OBATE‹EM PˆHŸTO PE˜EHˆE EPEC—ˆTATœ   *
C*                         “TO—HEHH“ž TO—K“ C O‹OBˆHH›M ˜AƒOM.        *
C*                                                                     *
C*    ISTEP -—ˆC‹O PO„E‹AHH›X ˜AƒOB B„O‹œ KPˆBO‰.                     *
C*    IPR   -Pˆ‡HAK E—ATˆ:                                           *
C*           (-2;-1)-B›„A—A POTOKO‹A OTC“CTB“ET;                      *
C*                0 -HA E—ATœ B›„AžTCŸ KOOP„ˆHAT› TO—EK HA KPˆBO‰;    *
C*                1 -HA E—ATœ B›„AžTCŸ KOOP„ˆHAT› TO—EK HA KPˆBO‰ ˆ   *
C*                   HEKOTOPAŸ KOHTPO‹œHAŸ ˆH”OPMA–ˆŸ;                 *
C*                2 -HA E—ATœ B›„AžTCŸ BCE Pˆ—ˆH› „E‹EHˆŸ ˜AƒA OO‹AM
C*                   ˆ HEKOTOPAŸ „OO‹HˆTE‹œHAŸ ˆH”OPMA–ˆŸ.            *
C*    IT    -—ˆC‹O HœžTOHOBCKˆX ˆTEPA–ˆ‰ PO„E‹AHH›X Pˆ “TO—HEHˆˆ     *
C*           TEK“™E‰ TO—Kˆ HA KPˆBO‰.                                  *
C*    IDIV  -—ˆC‹O „E‹EHˆ‰ ˜AƒA OO‹AM.                               *
C*    NCAN  -HOMEP KAHA‹A „‹Ÿ B›BO„A PE‡“‹œTATOB.                      *
C*    NX    -PA‡MEPHOCTœ POCTAHCTBA EPEMEHH›X.                       *
C*    NFUN  -PA‡MEPHOCTœ BEKTOP-”“HK–ˆˆ.                               *
C*    X     -KOOP„ˆHAT› TO—Kˆ (HA KPˆBO‰ ˆ‹ˆ HEKOTOPO‰ POME†“TO—HO‰). *
C*    DFUN  -MACCˆB —ACTH›X POˆ‡BO„H›X.                               *
C* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C*    Pˆ ˆ‡MEHEHˆˆ MAKCˆMA‹œHOƒO —ˆC‹A EPEMEHH›X- NDIM,              *
C*    HEOXO„ˆMO ˆ‡MEHˆTœ PA‡MEPHOCTœ MACCˆBA  VMOVE(NDIM)             *
C* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER*2(I-N)
      include 'beetlebf.dim'
      LOGICAL MAS1(8), MAS2(8)
      DIMENSION X(1), DFUN(NDIM,NX)
      COMMON /ALL1/  XGMN, XGMX
C
      COMMON /LEQV1/ DET1, COND1
      COMMON /CORR1/ DET,  COND, SGNDET
C
      COMMON /WRTO1/ NFIX1, NFIX
      COMMON /WRTO2/ DMOVE1, DMOVE
      COMMON /WRTO3/ COSI1, COSI2, COSI0,COSI3
      COMMON /WRTO4/ EPSC, EPSN
      COMMON /WRTO6/ ITC,IT2
C!!!  COMMON /WRTO7/ VMOVE(25)
      COMMON /WRTO7/ VMOVE(NDIM),VMOVN(NDIM)
C
      DATA MAS1 /' ',' ',' ',' ',' ',' ',' ',' '/,
     A     MAS2 /'+','+','+','+','+','+','+','+'/
C
#if defined(hib)
      IF (IPR .LE. -2) RETURN
          GOTO (100,110,120,130,140,150,160,170,180,190,
     A          200,210,220,230,240), IWRT
 100  CONTINUE
C
C.... IWRT = 1;
C     E—ATœ KOOP„ˆHAT O—EPE„HO‰ TO—Kˆ HA KPˆBO‰.
C
      IF (IPR .LT. 0) RETURN
          IF (IPR .LT. 2 .OR. IDIV .EQ. 0) GOTO 102
C
C.... E—ATœ —ˆC‹A „E‹EHˆ‰ ˆCXO„HOƒO ˜AƒA OO‹AM.
C
              WRITE (NCAN,4060) IDIV
              WRITE (NCAN,2010)
              WRITE (NCAN,2000)
 102      CONTINUE
          NT = ISTEP + 1
          WRITE (NCAN,1005) NT, (X(I),I=1,NX)
      IF (IPR .LT. 1) GOTO 11
C
C.... E—ATœ „OO‹HˆTE‹œHO‰ ˆH”OPMA–ˆˆ.
C
          WRITE (NCAN,1010)
          WRITE (NCAN,1020)
          IF (DABS(DMOVE1) .NE. DABS(DMOVE)) WRITE (NCAN,1030)  DMOVE1,
     A                                        (MAS2(I),I=1,8),  DMOVE
          IF (DABS(DMOVE1) .EQ. DABS(DMOVE)) WRITE (NCAN,1030)  DMOVE1,
     A                                        (MAS1(I),I=1,8),  DMOVE
          IF (NFIX1 .NE. NFIX)               WRITE (NCAN,1040)  NFIX1,
     A                                        (MAS2(I),I=1,8),  NFIX
          IF (NFIX1 .EQ. NFIX)               WRITE (NCAN,1040)  NFIX1,
     A                                        (MAS1(I),I=1,8),  NFIX
          WRITE (NCAN,1050) IT
          WRITE (NCAN,4090)
          WRITE (NCAN,1052) EPSN
          WRITE (NCAN,1053) EPSC
          WRITE (NCAN,4090)
                               WRITE (NCAN,5000) DET1
          IF (NFIX .NE. NFIX1) WRITE (NCAN,5020) DET
                               WRITE (NCAN,5010) COND1
          IF (NFIX .NE. NFIX1) WRITE (NCAN,5030) COND
          WRITE (NCAN,4090)
          IF (IPR .LT. 2) GOTO 10
              WRITE (NCAN,1060)
              WRITE (NCAN,1070) COSI1
              WRITE (NCAN,1080) COSI0
              WRITE (NCAN,1090) COSI2
              WRITE (NCAN,1095) (VMOVE(I),I=1,NX)
 10       CONTINUE
      WRITE (NCAN,2000)
 11   CONTINUE
      RETURN
 110  CONTINUE
C
C.... IWRT = 2;
C     COO™EHˆE: HE CO˜‹ˆCœ HœžTOHOBCKˆE ˆTEPA–ˆˆ
C                (TPE“EMAŸ TO—HOCTœ HE „OCTˆƒH“TA).
C
      IF (IPR .LT. 2)   RETURN
          WRITE (NCAN,2010)
          WRITE (NCAN,2020) ITC
          WRITE (NCAN,4090)
          WRITE (NCAN,1052) EPSN
          WRITE (NCAN,1053) EPSC
          WRITE (NCAN,4090)
          WRITE (NCAN,3080) NFIX
          WRITE (NCAN,5000) DET1
          WRITE (NCAN,5010) COND1
          WRITE (NCAN,4090)
          WRITE (NCAN,2030) DMOVE
          WRITE (NCAN,4090)
          WRITE (NCAN,5040)
          WRITE (NCAN,3010) (X(I),I=1,NX)
          RETURN
 120  CONTINUE
C
C.... IWRT = 3;
C     COO™EHˆE: HE CO˜‹ˆCœ HœžTOHOBCKˆE ˆTEPA–ˆˆ
C                (B›PO†„EHA MATPˆ–A —ACTH›X POˆ‡BO„H›X).
C
      IF (IPR .LT. 2)   RETURN
          IT = IT - 1
          WRITE (NCAN,2010)
          WRITE (NCAN,3070)
          WRITE (NCAN,3010) (X(I),I=1,NX)
          WRITE (NCAN,1050) IT
          WRITE (NCAN,4090)
          WRITE (NCAN,5000) DET1
          WRITE (NCAN,5010) COND1
          WRITE (NCAN,4090)
          WRITE (NCAN,2030) DMOVE
          WRITE (NCAN,3080) NFIX
          WRITE (NCAN,3089)
          DO 125 I=1,NX
             WRITE (NCAN,3090)  (DFUN(I,J), J=1,NX)
 125      CONTINUE
          RETURN
 130  CONTINUE
C
C.... IWRT = 4;
C     COO™EHˆE: HE CO˜‹ˆCœ HœžTOHOBCKˆE ˆTEPA–ˆˆ
C                (HE B›—ˆC‹EH› ‡HA—EHˆŸ ”“HK–ˆ‰ B O—EPE„HO‰
C                 TO—KE HœžTOHOBCKOƒO POƒHO‡A).
C
      IF (IPR .LT. 2)   RETURN
          IT = IT - 1
          WRITE (NCAN,2010)
          WRITE (NCAN,4000)
          WRITE (NCAN,3010) (X(I),I=1,NX)
          WRITE (NCAN,1050) IT
          WRITE (NCAN,2030) DMOVE
          RETURN
 140  CONTINUE
C
C.... IWRT = 5;
C     COO™EHˆE: HE CO˜‹ˆCœ HœžTOHOBCKˆE ˆTEPA–ˆˆ
C                (HE B›—ˆC‹EH› ‡HA—EHˆŸ —ACTH›X POˆ‡BO„H›X B O—EPE„HO‰
C                 TO—KE HœžTOHOBCKOƒO POƒHO‡A).
C
      IF (IPR .LT. 2)   RETURN
          IT = IT - 1
          WRITE (NCAN,2010)
          WRITE (NCAN,4020)
          WRITE (NCAN,3010) (X(I),I=1,NX)
          WRITE (NCAN,1050) IT
          WRITE (NCAN,2030) DMOVE
          RETURN
 150  CONTINUE
C
C.... IWRT = 6;
C     COO™EHˆE: HE HA‰„EH KACATE‹œH›‰ BEKTOP
C                (B›PO†„EHA MATPˆ–A —ACTH›X POˆ‡BO„H›X).
C
      IF (IPR .LT. 2)   RETURN
          WRITE (NCAN,2010)
          WRITE (NCAN,4030)
          WRITE (NCAN,3010) (X(I),I=1,NX)
          WRITE (NCAN,4090)
          WRITE (NCAN,5000) DET1
          WRITE (NCAN,5010) COND1
          WRITE (NCAN,4090)
          WRITE (NCAN,2030) DMOVE
          WRITE (NCAN,3080) NFIX
          WRITE (NCAN,3089)
          DO 155 I=1,NX
             WRITE (NCAN,3090)  (DFUN(I,J), J=1,NX)
 155      CONTINUE
          RETURN
 160  CONTINUE
C
C.... IWRT = 7;
C     COO™EHˆE: HE HA‰„EH KACATE‹œH›‰ BEKTOP
C                (HE B›—ˆC‹EH› ”“HK–ˆˆ B TO—KE HA KPˆBO‰).
C
      IF (IPR .LT. 2)   RETURN
          WRITE (NCAN,2010)
          WRITE (NCAN,4040)
          WRITE (NCAN,3010) (X(I),I=1,NX)
          WRITE (NCAN,2030) DMOVE
          RETURN
 170  CONTINUE
C
C.... IWRT = 8;
C     COO™EHˆE: HE HA‰„EH KACATE‹œH›‰ BEKTOP
C                (HE B›—ˆC‹EH› ‡HA—EHˆŸ —ACTH›X POˆ‡BO„H›X
C                 B TO—KE HA KPˆBO‰).
C
      IF (IPR .LT. 2)   RETURN
          WRITE (NCAN,2010)
          WRITE (NCAN,4050)
          WRITE (NCAN,3010) (X(I),I=1,NX)
          WRITE (NCAN,2030) DMOVE
          RETURN
 180  CONTINUE
C
C.... IWRT = 9;
C     COO™EHˆE:“TO—HEHHAŸ TO—KA HE ‹ˆ‡KA K TO—KE-POƒHO‡“
C               (˜Aƒ „E‹ˆTCŸ OO‹AM).
C
      IF (IPR .LT. 2) RETURN
          WRITE (NCAN,2010)
          WRITE (NCAN,2060)
          WRITE (NCAN,2070) COSI0
          WRITE (NCAN,2080) COSI1
          WRITE (NCAN,1050) IT
          WRITE (NCAN,2030) DMOVE
          WRITE (NCAN,3050)
          WRITE (NCAN,3010) (X(I),I=1,NX)
          RETURN
 190  CONTINUE
C
C.... IWRT = 10;
C     COO™EHˆE: POƒHO‡ O KACATE‹œHO‰ B›˜E‹ ‡A ƒPAHˆ–“.
C
      IF (IPR .LT. 2) RETURN
          WRITE (NCAN,2010)
          WRITE (NCAN,2090)
          WRITE (NCAN,3000)
          WRITE (NCAN,3010) (X(I),I=1,NX)
          WRITE (NCAN,2030) DMOVE1
          WRITE (NCAN,2040) DMOVE
          WRITE (NCAN,2010)
          WRITE (NCAN,2000)
          RETURN
 200  CONTINUE
C
C.... IWRT = 11;
C     COO™EHˆE: POƒHO‡ O HœžTOH“ B›˜E‹ ‡A ƒPAHˆ–“.
C
      IF (IPR .LT. 2) RETURN
          WRITE (NCAN,2010)
          WRITE (NCAN,3020)
          WRITE (NCAN,3021)
          WRITE (NCAN,3010) (X(I),I=1,NX)
          WRITE (NCAN,2010)
          WRITE (NCAN,2000)
          RETURN
 210  CONTINUE
C
C.... IWRT = 12
C     COO™EHˆE: “TO—HŸETCŸ TO—KA ‡AM›KAHˆŸ.
C
      IF (IPR .LT. 2) RETURN
          WRITE (NCAN,2010)
          WRITE (NCAN,3030)
          WRITE (NCAN,3040)
          WRITE (NCAN,3010) (X(I),I=1,NX)
          RETURN
C
C.... IWRT = 13
C
 220  CONTINUE
      IF (IPR .LT. 2) RETURN
          WRITE (NCAN,3050)
          WRITE (NCAN,3010) (X(I),I=1,NX)
          RETURN
C
C.... IWRT = 14
C
 230  CONTINUE
      IF (IPR .LT. 2) RETURN
          WRITE (NCAN,3060)
          WRITE (NCAN,3010) (X(I),I=1,NX)
          WRITE (NCAN,2010)
          WRITE (NCAN,2000)
          RETURN
 240  CONTINUE
C
C.... IWRT = 15;
C     COO™EHˆE:O‹œ‡OBATE‹EM PˆHŸTO PE˜EHˆE EPEC—ˆTATœ TO—K“
C               (˜Aƒ „E‹ˆTCŸ OO‹AM).
C
      IF (IPR .LT. 2) RETURN
          WRITE (NCAN,2010)
          WRITE (NCAN,5050)
          WRITE (NCAN,3050)
          WRITE (NCAN,3010) (X(I),I=1,NX)
#endif
          RETURN
C
C.... CˆCOK ”OPMATOB.
C
#if defined(hib)
 1005 FORMAT
     A('   NT=',I6,' X=',7(G14.5,1X)/10(15X,7(G14.5,1X)/))
C
 1010 FORMAT
     A(/40X,'‡HA—EHˆŸ KOHTPO‹œH›X APAMETPOB:')
C
 1020 FORMAT
     A(30X,'TEK“™ˆE ‡HA—EHˆŸ',15X,'POƒHO‡ˆP“EM›E ‡HA—EHˆŸ'/)
C
 1030 FORMAT
     A('   ˜Aƒ B„O‹œ KPˆBO‰:            ',G14.5,8X,8A1,6X,G14.5)
C
 1040 FORMAT
     A('   HOMEP ”ˆKCˆPOBAHHO‰ EPEMEHHO‰:  ',I6,12X,8A1,6X,I6)
C
 1050 FORMAT
     A('   PO„E‹AHO HœžTOHOBCKˆX ˆTEPA–ˆ‰: ',I6)
C
 1052 FORMAT
     A('   „OCTˆƒH“TAŸ TO—HOCTœ:            ',G15.6)
C
 1053 FORMAT
     A('   TPE“EMAŸ   TO—HOCTœ:            ',G15.6)
C
 1060 FORMAT
     A('   ‡HA—EHˆŸ KOCˆH“CA “ƒ‹A ME†„“ KACATE‹œHO‰ ˆ XOP„O‰:')
C
 1070 FORMAT
     A('   MˆHˆMA‹œHOE KOHTPO‹œHOE ‡HA—EHˆE :',G14.5)
C
 1080 FORMAT
     A('   TEK“™EE ‡HA—EHˆE KOCˆH“CA “ƒ‹A   :',G14.5)
C
 1090 FORMAT
     A('   MAKCˆMA‹œHOE KOHTPO‹œHOE ‡HA—EHˆE:',G14.5)
C
 1095 FORMAT
     A(/'   KOOP„ˆHAT› KACATE‹œHOƒO BEKTOPA:'/
     B 10(2X,7(G14.5,1X)/))
C
 2000 FORMAT
     A(1X,120('.'))
C
 2010 FORMAT
     A(1X,120('-'))
C
 2020 FORMAT
     A(/'   PO„E‹AHO MAKCˆMA‹œHOE —ˆC‹O HœžTOHOBCKˆX ˆTEPA–ˆ‰:',I6/
     B  '   ---------------------------------------------------')
C
 2030 FORMAT
     A('   BE‹ˆ—ˆHA PE„BAPˆTE‹œHOƒO ˜AƒA:    ',G14.5)
C
 2040 FORMAT
     A('   BE‹ˆ—ˆHA “TO—HEHHOƒO ˜AƒA :        ',G14.5)
C
 2050 FORMAT
     A('   —ˆC‹O „E‹EHˆ‰ PE„BAPˆTE‹œHOƒO ˜AƒA OO‹AM:',I6)
C
 2060 FORMAT
     A(/'   “TO—HEHHAŸ TO—KA HE ‹ˆ‡KA K TO—KE-POƒHO‡“.'/
     B  '   --------------------------------------------')
C
 2070 FORMAT
     A('   TEK“™EE     ‡HA—EHˆE KOCˆH“CA “ƒ‹A:',G14.5)
C
 2080 FORMAT
     A('   KOHTPO‹œHOE ‡HA—EHˆE KOCˆH“CA “ƒ‹A:',G14.5)
C
 2090 FORMAT
     A(/'    POƒHO‡ O KACATE‹œHO‰ B›˜E‹ ‡A ƒPAHˆ–“.'/
     B  '    ----------------------------------------')
C
 3000 FORMAT
     A('    KOOP„ˆHAT› POƒHO‡A ƒPAHˆ—HO‰ TO—Kˆ :')
C
 3010 FORMAT
     A(/10(2X,7(G14.5,1X)/))
C
 3020 FORMAT
     A(/'   POƒHO‡ O HœžTOH“ B›˜E‹ ‡A ƒPAHˆ–“.'/
     B  '   ------------------------------------')
C
 3021 FORMAT
     A ('   KOOP„ˆHAT› TO—Kˆ POƒHO‡A:')
C
 3030 FORMAT
     A(/'   “TO—HŸETCŸ TO—KA ‡AM›KAHˆŸ.'/
     B  '   ---------------------------')
C
 3040 FORMAT
     A(/'   KOOP„ˆHAT› POƒHO‡A TO—Kˆ ‡AM›KAHˆŸ:')
C
 3050 FORMAT
     A(/'   KOOP„ˆHAT› “TO—HEHHO‰ TO—Kˆ:        ')
C
 3060 FORMAT
     A(/'   KOOP„ˆHAT› EPBO‰ TO—Kˆ HA KPˆBO‰:  ')
C
 3070 FORMAT
     A(/'   HE CO˜‹ˆCœ HœžTOHOBCKˆE ˆTEPA–ˆˆ- B›PO†„EHA MATPˆ–A'
     B /'   —ACTH›X POˆ‡BO„H›X B TO—KE C KOOP„ˆHATAMˆ:')
C
 3080 FORMAT
     A('   HOMEP ”ˆKCˆPOBAHHO‰ EPEMEHHO‰: ',I6)
C
 3089 FORMAT
     A(/'   MATPˆ–A —ACTH›X POˆ‡BO„H›X  DF(I)/DX(J):')
C
 3090 FORMAT
     A (/10(2X,7(G14.5,1X)/))
C
 4000 FORMAT
     A(/'   HE CO˜‹ˆCœ HœžTOHOBCKˆE ˆTEPA–ˆˆ- HE B›—ˆC‹EH› ‡HA—EHˆŸ'
     B /'   ”“HK–ˆ‰ B TO—KE HœžTOHOBCKOƒO POƒHO‡A C KOOP„ˆHATAMˆ:')
C
 4020 FORMAT
     A(/'   HE CO˜‹ˆCœ HœžTOHOBCKˆE ˆTEPA–ˆˆ- HE B›—ˆC‹EH› ‡HA—EHˆŸ'
     B /'   —ACTH›X POˆ‡BO„H›X B TO—KE HœžTOHOBCKOƒO POƒHO‡A:   ')
 4030 FORMAT
     A(/'   HE HA‰„EH KACATE‹œH›‰ BEKTOP- MATPˆ–A —ACTH›X POˆ‡BO„H›X'
     B /'   B›PO†„EHA B TO—KE HA KPˆBO‰ C KOOP„ˆHATAMˆ:')
C
 4040 FORMAT
     A(/'   HE HA‰„EH KACATE‹œH›‰ BEKTOP- HE B›—ˆC‹EH› ”“HK–ˆˆ'
     B /'   B TO—KE HA KPˆBO‰ C KOOP„ˆHATAMˆ:')
C
 4050 FORMAT
     A(/'   HE B›—ˆC‹EH KACATE‹œH›‰ BEKTOP- HE B›—ˆC‹EH› ‡HA—EHˆŸ'
     B /'   —ACTH›X POˆ‡BO„H›X B TO—KE HA KPˆBO‰:')
C
 4060 FORMAT
     A(/   '   C“MMAPHOE —ˆC‹O „E‹EHˆ‰ ˜AƒA OO‹AM: ',I6/
     B     '   ------------------------------------  ')
C
 4090 FORMAT
     A(1X)
C
 5000 FORMAT
     A('   TEK“™EE ‡HA—EHˆE OPE„E‹ˆTE‹Ÿ:   ',G15.6)
C
 5010 FORMAT
     A('   O–EHKA O“C‹OB‹EHHOCTˆ:          ',G15.6)
C
 5020 FORMAT
     A('   (HOBOE ‡HA—EHˆE OPE„E‹ˆTE‹Ÿ:    ',G15.6,')')
C
 5030 FORMAT
     A('   (HOBAŸ O–EHKA O“C‹OB‹EHHOCTˆ:   ',G15.6,')')
C
 5040 FORMAT
     A('   KOOP„ˆHAT› OC‹E„HE‰ TO—Kˆ:      ')
C
 5050 FORMAT
     A(/'   O‹œ‡OBATE‹EM PˆHŸTO PE˜EHˆE EPEC—ˆTATœ TO—K“'
     B /'            (˜Aƒ „E‹ˆTCŸ OO‹AM)'
     C /'   -----------------------------------------------')
#endif
      END
