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

      SUBROUTINE WRTCRS (IPRTCL,IWRT,IPRCRS,ITER,ITNWTZ,ITEND,NCAN,NX,
     A                   NDIM,EPSCRS,EPSR,DETZ,CONDZ,XCRS,XABD,DFUN)
C     3.05.90É.
C* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C*                                                                     *
C*    èPOÉPAMMA WRTCRS èPEÑHAáHAóEHA Ñãü BõBOÑA èPOTOKOãA ìTOóHEHàü    *
C*    TOóKà CAMOèEPECEóEHàü KPàBOâ.                                    *
C*                                                                     *
C* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER*2(I-N)
      DIMENSION XCRS(1), XABD(1), DFUN(NDIM,NX), ITNWTZ(1)
C
#if defined(hib)
      IF (IPRCRS .LT. 0) RETURN
          WRITE (NCAN,2000)
          WRITE (NCAN,2010)
          WRITE (NCAN,2020)
          WRITE (NCAN,2010)
          WRITE (NCAN,2030)      EPSR
          IF (IPRTCL .NE. 1)     WRITE (NCAN,2040) EPSCRS
          IF (IPRTCL .EQ. 2)     WRITE (NCAN,2050)
          IF (IPRTCL .EQ. 3)     WRITE (NCAN,2060)
          IF (IPRCRS .LT. 1)     GOTO 200
C
C.... CTAHÑAPTHOE COOÅôEHàE: HE COòãàCú HúûTOHOBCKàE àTEPAñàà.
C
              IDIV = 1
              IPR  = 2
              IF (IPRTCL .EQ. 3) CALL WRTOUT(IWRT,ISTEP,IPR,ITEND,IDIV,
     A                                       NCAN,NX,NDIM,XABD,DFUN)
              IF (IPRTCL .GT. 1) WRITE (NCAN,3050)
              WRITE (NCAN,2010)
              WRITE (NCAN,2010)
              WRITE (NCAN,2080)  DETZ
              WRITE (NCAN,2090)  CONDZ
              WRITE (NCAN,2010)
              WRITE (NCAN,3000)  ITER
              WRITE (NCAN,3010)  (ITNWTZ(I), I=1,ITER)
 200      CONTINUE
          WRITE (NCAN,2010)
          WRITE (NCAN,3020)
          WRITE (NCAN,2010)
          WRITE (NCAN,3030)      (XCRS(I),I=1,NX)
          WRITE (NCAN,2010)
          WRITE (NCAN,2000)
          WRITE (NCAN,3040)
#endif
      RETURN
C
#if defined(hib)
 2000 FORMAT
     A(1X,120('-'))
C
 2010 FORMAT
     A(1X)
C
 2020 FORMAT
     A('   èPOâÑEHA TOóKA CAMOèEPCEóEHàü.'/
     B '   ------------------------------')
C
 2030 FORMAT
     A('   ÑOCTàÉHìTA TOóHOCTú:              ',G14.5)
C
 2040 FORMAT
     A('   áAÑAHHAü TOóHOCTú HE ÑOCTàÉHìTA:  ',G14.5)
C
 2050 FORMAT
     A('   èPOÑEãAHO MAKCàMAãúHOE óàCãO òAÉOB- 10.')
C
 2060 FORMAT
     A('   HE ìTOóHEHO OóEPEÑHOE èPàÅãàÜEHàE K   '
     B/'   TOóKE CAMOèEPECEóEHàü.')
C
 2080 FORMAT
     A('   áHAóEHàE OèPEÑEãàTEãü  (DET):     ',G14.6)
C
 2090 FORMAT
     A('   OñEHKA OÅìCãOBãEHHOCTà (COND):    ',G14.6)
C
 3000 FORMAT
     A('   èPOÑEãAHO òAÉOB ìTOóHEHàü:        ',I4)
C
 3010 FORMAT
     A('   èPOÑEãAHO HúûTOHOBCKàX àTEPAñàâ   '
     B/'   HA BCEX òAÉAX:                    ',2X,10(I3,1X))
C
 3020 FORMAT
     A('   KOOPÑàHATõ TOóKà HA KPàBOâ:       ')
C
 3030 FORMAT
     A(10(2X,7(G14.5,1X)/))
C
 3040 FORMAT
     A(1X,120('.'))
C
 3050 FORMAT
     A(1X,120('-'))
C
#endif
      END
