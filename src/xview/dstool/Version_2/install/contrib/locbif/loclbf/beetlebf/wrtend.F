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
C     3.05.90�.
C* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C*                                                                     *
C*    �PO�PAMMA  WRTEND �PE�HA�HA�EHA ��� B�BO�A COO��EH�� O �ABEP�EH��*
C*    PA�OT� �PO�PAMM�  BEETLE. B�BO� H��HO�O COO��EH�� O�PE�E��ET     *
C*    �APAMETP  IEND (CM.TEKCT �PO�PAMM�  WRTEND).                     *
C*    OCTA��H�E �APAMETP� �PO�PAMM� WRTEND �PE�HA�HA�EH� ���           *
C*    B�BO�A BMECTE C COOTBETCTB����M COO��EH�EM (�T� �APAMETP�        *
C*    O��CAH� B �PO�PAMME BEETLE).                                     *
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
C.... B�BO�  COO��EH�� O HOPMA��HOM �ABEP�EH�� PA�OT� �PO�PAMM� BEETLE.
C
          GOTO (100,110,120,130), IEND
 100      CONTINUE
C
C.... IEND=1:B�O�� KP�BO� C�E�AHO �A�AHHOE ��C�O �A�OB.
C
            WRITE (NCAN,1000) IEND, ISTEP
            WRITE (NCAN,1040) HMIN, HMAX
            RETURN
 110      CONTINUE
C
C.... IEND=2:B��E�EHHA� �EPEMEHHA� �OCT���A CBOE�O �PAH��HO�O �HA�EH��
C           (�PAH��HA� TO�KA �TO�HEHA C �A�AHHO� TO�HOCT��).
C
            WRITE (NCAN,1010) IEND, NG, ISTEP
            WRITE (NCAN,1040) HMIN, HMAX
            RETURN
 120      CONTINUE
C
C.... IEND=3:KP�BA� �AMKH��AC� C TO�HOCT�� : 5.D0*EPSL.
C
            EPSL  = 5.D0*EPS
            WRITE (NCAN,1020) IEND, EPSL, NFIX, ISTEP
            WRITE (NCAN,1040) HMIN, HMAX
            RETURN
 130      CONTINUE
C
C.... IEND=4:B��E�EHHA� �EPEMEHHA� �OCT���A CBOE�O �PAH��HO�O �HA�EH��.
C           (�PAH��HA� TO�KA �TO�HEHA C TO�HOCT� �O BE����H�
C            TEK��E�O �A�A).
C
            WRITE (NCAN,1030) IEND, NG, DMOVE, ISTEP
            WRITE (NCAN,1040) HMIN, HMAX
            RETURN
C
 300  CONTINUE
C
C.... COO��EH�� O �PEP�BAH��X PA�OT� �PO�PAMM� BEETLE.
C
      IENDD = -1*IEND
      GOTO (310,320,330,340,350), IENDD
 310  CONTINUE
C
C.... IEND=-1:�P� �A�AH�� BXO�H�X �APAMETPOB �O���EHA O���KA.
C
        WRITE (NCAN,3000) IEND
        RETURN
 320  CONTINUE
C
C.... IEND=-2:�EPBA� TO�KA HA KP�BO� HE HA��EHA.
C
        WRITE (NCAN,3010) IEND
        RETURN
 330  CONTINUE
C
C.... IEND=-3:�A� �B��EH�� �O KP�BO� CTA� MEH��E M�H�MA��HO�O.
C
        ISTEPR = ISTEP-1
        WRITE (NCAN,3020) IEND, NFIX, ISTEPR
        WRITE (NCAN,1040) HMIN, HMAX
        RETURN
 340  CONTINUE
C
C.... IEND=-4:HE HA��EHA KACATE��HA� B �EPBO� TO�KE.
C
        WRITE (NCAN,3030) IEND
        RETURN
 350  CONTINUE
C
C     IEND=-5:PA�OTA �PO�PAMM� �PEPBAHA �O���OBATE�EM.
C
        WRITE (NCAN,3040) IEND, ISTEP
        WRITE (NCAN,1040) HMIN, HMAX
#endif
        RETURN
C
#if defined(hib)
 1000 FORMAT
     A(///1X,120('=')//
     B'    IEND=',I4,'  B�O�� KP�BO� B O�H� CTOPOH� C�E�AHO ',I6,
     C' �A�OB.')
C
 1010 FORMAT
     A(///1X,120('=')//
     B'    IEND=',I4,'  B��E�EHHA� ',I4,' �EPEMEHHA� �OCT���A CBOE�O',
     C' �PAH��HO�O �HA�EH��'/
     D15X,'�PAH��H�E �HA�EH�� KOOP��HAT �TO�HEH� C �A�AHHO� TO�HOCT��'/
     E15X,'B�O�� KP�BO� B O�H� CTOPOH� C�E�AHO ',I6,' �A�OB.')
C
 1020 FORMAT
     A(///120('=')//
     B'    IEND=',I4,'  KP�BA� �AMKH��AC� C TO�HOCT��  5.D0*EPS:',G14.5/
     C15X,'HOMEP B��E�EHHO� �EPEMEHHO�  ',I4/
     D15X,'B�O�� KP�BO� B O�H� CTOPOH� C�E�AHO ',I6,' �A�OB.')
C
 1030 FORMAT
     A(///120('=')//
     B'    IEND=',I4,'  B��E�EHHA� ',I4,' �EPEMEHHA� �OCT���A CBOE�O',
     C' �PAH��HO�O �HA�EH��'/
     D15X,'�PAH��H�E �HA�EH�� KOOP��HAT �TO�HEH� C TO�HOCT�� �O',
     E' BE����H� TEK��E�O �A�A ',G14.5/
     F15X,'B�O�� KP�BO� B O�H� CTOPOH� C�E�AHO ',I6,' �A�OB.')
C
 1040 FORMAT
     A(/15X,'�HA�EH�E M�H�MA��HO�O  �A�A B�O�� KP�BO�: ',G14.5
     B /15X,'�HA�EH�E MAKC�MA��HO�O �A�A B�O�� KP�BO�: ',G14.5
     C ///120('.'))
C
 3000 FORMAT
     A(///120('*')//
     B'    IEND=',I4,'  �P� �A�AH�� BXO�H�X �APAMETPOB �O���EHA O���KA.'
     C/1X,120('.'))
C
 3010 FORMAT
     A(///120('*')//
     B'    IEND=',I4,'  �EPBA� TO�KA HA KP�BO� HE HA��EHA.'/
     C1X,120('.'))
C
 3020 FORMAT
     A(///120('*')//
     B'    IEND=',I4,'  �A� �B��EH�� �O KP�BO� CTA� MEH��E M�H�MA��HO�O'
     C/15X,'HOMEP ��KC�POBAHHO� �EPEMEHHO� ',I4
     D/15X,'B�O�� KP�BO� B O�H� CTOPOH� C�E�AHO ',I6,' �A�OB.')
C
 3030 FORMAT
     A(///1X,120('*')//
     B'    IEND=',I4,'  HE HA��EHA KACATE��HA� B �EPBO� TO�KE.'/
     C1X,120('.'))
C
 3040 FORMAT
     A(///1X,120('*')//
     B '    IEND=',I4,'  PA�OTA �PO�PAMM� �PEPBAHA �O���OBATE�EM.'
     C/15X,'B�O�� KP�BO� B O�H� CTOPOH� C�E�AHO ',I6,' �A�OB.')
#endif
      END
