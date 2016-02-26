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

C
      SUBROUTINE WRTEND (IEND,IPR,ISTEP,NG,NFIX,NCAN,EPS,DMOVE)
C     3.05.90ƒ.
C* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C*                                                                     *
C*    POƒPAMMA  WRTEND PE„HA‡HA—EHA „‹Ÿ B›BO„A COO™EHˆ‰ O ‡ABEP˜EHˆˆ*
C*    PAOT› POƒPAMM›  BEETLE. B›BO„ H“†HOƒO COO™EHˆŸ OPE„E‹ŸET     *
C*    APAMETP  IEND (CM.TEKCT POƒPAMM›  WRTEND).                     *
C*    OCTA‹œH›E APAMETP› POƒPAMM› WRTEND PE„HA‡HA—EH› „‹Ÿ           *
C*    B›BO„A BMECTE C COOTBETCTB“ž™ˆM COO™EHˆEM (Tˆ APAMETP›        *
C*    OˆCAH› B POƒPAMME BEETLE).                                     *
C*                                                                     *
C* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER*2 (I-N)
      COMMON /WRTE1/ HMIN, HMAX
#if defined(hib)
      IF (IPR .LT. -1) RETURN
          IF (IEND .LT. 0) GOTO 300
C
C.... B›BO„  COO™EHˆ‰ O HOPMA‹œHOM ‡ABEP˜EHˆˆ PAOT› POƒPAMM› BEETLE.
C
          GOTO (100,110,120,130), IEND
 100      CONTINUE
C
C.... IEND=1:B„O‹œ KPˆBO‰ C„E‹AHO ‡A„AHHOE —ˆC‹O ˜AƒOB.
C
            WRITE (NCAN,1000) IEND, ISTEP
            WRITE (NCAN,1040) HMIN, HMAX
            RETURN
 110      CONTINUE
C
C.... IEND=2:B›„E‹EHHAŸ EPEMEHHAŸ „OCTˆƒ‹A CBOEƒO ƒPAHˆ—HOƒO ‡HA—EHˆŸ
C           (ƒPAHˆ—HAŸ TO—KA “TO—HEHA C ‡A„AHHO‰ TO—HOCTœž).
C
            WRITE (NCAN,1010) IEND, NG, ISTEP
            WRITE (NCAN,1040) HMIN, HMAX
            RETURN
 120      CONTINUE
C
C.... IEND=3:KPˆBAŸ ‡AMKH“‹ACœ C TO—HOCTœž : 5.D0*EPSL.
C
            EPSL  = 5.D0*EPS
            WRITE (NCAN,1020) IEND, EPSL, NFIX, ISTEP
            WRITE (NCAN,1040) HMIN, HMAX
            RETURN
 130      CONTINUE
C
C.... IEND=4:B›„E‹EHHAŸ EPEMEHHAŸ „OCTˆƒ‹A CBOEƒO ƒPAHˆ—HOƒO ‡HA—EHˆŸ.
C           (ƒPAHˆ—HAŸ TO—KA “TO—HEHA C TO—HOCTž „O BE‹ˆ—ˆH›
C            TEK“™EƒO ˜AƒA).
C
            WRITE (NCAN,1030) IEND, NG, DMOVE, ISTEP
            WRITE (NCAN,1040) HMIN, HMAX
            RETURN
C
 300  CONTINUE
C
C.... COO™EHˆŸ O PEP›BAHˆŸX PAOT› POƒPAMM› BEETLE.
C
      IENDD = -1*IEND
      GOTO (310,320,330,340,350), IENDD
 310  CONTINUE
C
C.... IEND=-1:Pˆ ‡A„AHˆˆ BXO„H›X APAMETPOB „O“™EHA O˜ˆKA.
C
        WRITE (NCAN,3000) IEND
        RETURN
 320  CONTINUE
C
C.... IEND=-2:EPBAŸ TO—KA HA KPˆBO‰ HE HA‰„EHA.
C
        WRITE (NCAN,3010) IEND
        RETURN
 330  CONTINUE
C
C.... IEND=-3:˜Aƒ „Bˆ†EHˆŸ O KPˆBO‰ CTA‹ MEHœ˜E MˆHˆMA‹œHOƒO.
C
        ISTEPR = ISTEP-1
        WRITE (NCAN,3020) IEND, NFIX, ISTEPR
        WRITE (NCAN,1040) HMIN, HMAX
        RETURN
 340  CONTINUE
C
C.... IEND=-4:HE HA‰„EHA KACATE‹œHAŸ B EPBO‰ TO—KE.
C
        WRITE (NCAN,3030) IEND
        RETURN
 350  CONTINUE
C
C     IEND=-5:PAOTA POƒPAMM› PEPBAHA O‹œ‡OBATE‹EM.
C
        WRITE (NCAN,3040) IEND, ISTEP
        WRITE (NCAN,1040) HMIN, HMAX
#endif
        RETURN
C
#if defined(hib)
 1000 FORMAT
     A(///1X,120('=')//
     B'    IEND=',I4,'  B„O‹œ KPˆBO‰ B O„H“ CTOPOH“ C„E‹AHO ',I6,
     C' ˜AƒOB.')
C
 1010 FORMAT
     A(///1X,120('=')//
     B'    IEND=',I4,'  B›„E‹EHHAŸ ',I4,' EPEMEHHAŸ „OCTˆƒ‹A CBOEƒO',
     C' ƒPAHˆ—HOƒO ‡HA—EHˆŸ'/
     D15X,'ƒPAHˆ—H›E ‡HA—EHˆŸ KOOP„ˆHAT “TO—HEH› C ‡A„AHHO‰ TO—HOCTœž'/
     E15X,'B„O‹œ KPˆBO‰ B O„H“ CTOPOH“ C„E‹AHO ',I6,' ˜AƒOB.')
C
 1020 FORMAT
     A(///120('=')//
     B'    IEND=',I4,'  KPˆBAŸ ‡AMKH“‹ACœ C TO—HOCTœž  5.D0*EPS:',G14.5/
     C15X,'HOMEP B›„E‹EHHO‰ EPEMEHHO‰  ',I4/
     D15X,'B„O‹œ KPˆBO‰ B O„H“ CTOPOH“ C„E‹AHO ',I6,' ˜AƒOB.')
C
 1030 FORMAT
     A(///120('=')//
     B'    IEND=',I4,'  B›„E‹EHHAŸ ',I4,' EPEMEHHAŸ „OCTˆƒ‹A CBOEƒO',
     C' ƒPAHˆ—HOƒO ‡HA—EHˆŸ'/
     D15X,'ƒPAHˆ—H›E ‡HA—EHˆŸ KOOP„ˆHAT “TO—HEH› C TO—HOCTœž „O',
     E' BE‹ˆ—ˆH› TEK“™EƒO ˜AƒA ',G14.5/
     F15X,'B„O‹œ KPˆBO‰ B O„H“ CTOPOH“ C„E‹AHO ',I6,' ˜AƒOB.')
C
 1040 FORMAT
     A(/15X,'‡HA—EHˆE MˆHˆMA‹œHOƒO  ˜AƒA B„O‹œ KPˆBO‰: ',G14.5
     B /15X,'‡HA—EHˆE MAKCˆMA‹œHOƒO ˜AƒA B„O‹œ KPˆBO‰: ',G14.5
     C ///120('.'))
C
 3000 FORMAT
     A(///120('*')//
     B'    IEND=',I4,'  Pˆ ‡A„AHˆˆ BXO„H›X APAMETPOB „O“™EHA O˜ˆKA.'
     C/1X,120('.'))
C
 3010 FORMAT
     A(///120('*')//
     B'    IEND=',I4,'  EPBAŸ TO—KA HA KPˆBO‰ HE HA‰„EHA.'/
     C1X,120('.'))
C
 3020 FORMAT
     A(///120('*')//
     B'    IEND=',I4,'  ˜Aƒ „Bˆ†EHˆŸ O KPˆBO‰ CTA‹ MEHœ˜E MˆHˆMA‹œHOƒO'
     C/15X,'HOMEP ”ˆKCˆPOBAHHO‰ EPEMEHHO‰ ',I4
     D/15X,'B„O‹œ KPˆBO‰ B O„H“ CTOPOH“ C„E‹AHO ',I6,' ˜AƒOB.')
C
 3030 FORMAT
     A(///1X,120('*')//
     B'    IEND=',I4,'  HE HA‰„EHA KACATE‹œHAŸ B EPBO‰ TO—KE.'/
     C1X,120('.'))
C
 3040 FORMAT
     A(///1X,120('*')//
     B '    IEND=',I4,'  PAOTA POƒPAMM› PEPBAHA O‹œ‡OBATE‹EM.'
     C/15X,'B„O‹œ KPˆBO‰ B O„H“ CTOPOH“ C„E‹AHO ',I6,' ˜AƒOB.')
#endif
      END
