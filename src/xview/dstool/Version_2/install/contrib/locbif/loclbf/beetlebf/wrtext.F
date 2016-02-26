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

      SUBROUTINE WRTEXT (IPRTCL,IWRT,IPRE,ITER,ITNWT,NCAN,NX,NDIM,NEXT,
     A                   IEXT,EPSE,EPSR,XEXT,XABD,VMOVE,DFUN)
C     3.05.90É.
C* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C*                                                                     *
C*    èPOÉPAMMA  WRTEXT  èPEÑHAáHAóEHA Ñãü èEóATà èPOTOKOãA ìTOóHEHàü  *
C*    TOóEK HA KPàBOâ, B KOTOPõX áAÑAHHõE èEPEMEHHõE ÑOCTàÉAûT CBOàX   *
C*    ùKCTPEMAãúHõX áHAóEHàâ.                                          *
C*                                                                     *
C* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER*2(I-N)
      DIMENSION XEXT(1), XABD(1), VMOVE(1), DFUN(NDIM,NX)
C
#if defined(hib)
      IF (IPRE .LT. 0) RETURN
          WRITE (NCAN,2000)
          WRITE (NCAN,2010)
          WRITE (NCAN,2020)      NEXT
          IF (IPRTCL .EQ. 2)     WRITE (NCAN,2022)
          IF (IEXT .GT. 0.D0)    WRITE (NCAN,2030) NEXT, XEXT(NEXT)
          IF (IEXT .LT. 0.D0)    WRITE (NCAN,2035) NEXT, XEXT(NEXT)
          WRITE (NCAN,2010)
          WRITE (NCAN,2040)      EPSR
          IF (IPRTCL .EQ. 2)     WRITE (NCAN,2060) EPSE
          IF (IPRTCL .EQ. 3)     WRITE (NCAN,2080)
          IF (IPRE   .LT. 1)     GOTO 200
C
C.... CTAHÑPTHõE COOÅôEHàü: 1) HE COòãàCú HúûTOHOBCKàE àTEPAñàà;
C                           2) HE HAâÑEH KACATEãúHõâ BEKTOP;
C
              IDIV = 1
              IPR  = 2
              IF (IPRTCL .EQ. 3) CALL WRTOUT (IWRT,ISTEP,IPR,ITNWT,IDIV,
     A                                        NCAN,NX,NDIM,XABD,DFUN)
              IF (IPRTCL .EQ. 3) WRITE (NCAN,2070)
              WRITE (NCAN,2010)
              WRITE (NCAN,3000)  ITER
              WRITE (NCAN,2010)
              WRITE (NCAN,3010)
              WRITE (NCAN,2010)
              WRITE (NCAN,3020)  (VMOVE(I),I=1,NX)
 200  CONTINUE
              WRITE (NCAN,2010)
              WRITE (NCAN,3030)
              WRITE (NCAN,2010)
              WRITE (NCAN,3020)  (XEXT(I),I=1,NX)
              WRITE (NCAN,2010)
              WRITE (NCAN,2000)
              WRITE (NCAN,3040)
#endif
              RETURN
C
C.... îOPMATõ èPOTOKOãA.
C
#if defined(hib)
 2000 FORMAT
     A(1X,120('-'))
C
 2010 FORMAT
     A(1X)
C
 2020 FORMAT
     A('   èPOâÑEHO ùKCTPEMAãúHOE áHAóEHàE ',I5,'  -èEPEMEHHOâ.    '/
     B '   ---------------------------------------------------     ')
C
 2022 FORMAT
     A('   TPEÅìEMAü TOóHOCTú HE ÑOCTàÉHìTA.')
C
 2030 FORMAT
     A('   MAKCàMAãúHOE áHAóEHàE ',I4,' èEPEMEHHOâ:          ',G14.5)
C
 2035 FORMAT
     A('   MàHàMAãúHOE áHAóEHàE  ',I4,' èEPEMEHHOâ:          ',G14.5)
C
 2040 FORMAT
     A('   ÑOCTàÉHìTA TOóHOCTú:                              ',G14.5)
C
 2060 FORMAT
     A('   áAÑAHHAü   TOóHOCTú:                              ',G14.5)
C
 2070 FORMAT
     A(1X,120('-'))
C
 2080 FORMAT
     A('   HE HAâÑEHO OóEPEÑHOE èPàÅãàÜEHàE  K TOóKE ùKCTPEMìMA.')
C
 3000 FORMAT
     A('   èPOÑEãAHO òAÉOB ìTOóHEHàü:                        ',I6)
C
 3010 FORMAT
     A('   KOOPÑàHATõ KACATEãúHOÉO BEKTOPA:                  ')
C
 3020 FORMAT
     A(10(4X,7(G14.5,2X)/))
C
 3030 FORMAT
     A('   KOOPÑàHATõ TOóKà ùKCTPEMìMA:                      ')
C
 3040 FORMAT
     A(1X,120('.'))
C
#endif
      END
