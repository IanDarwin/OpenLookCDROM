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
      SUBROUTINE WRTDT (NX,NG,IADD,X0,HV0,EPS,FUN,IER,ISTOP)
C     3.05.90ƒ.
C     13.11.92
C     15.06.94
C* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C*                                                                     *
C*    POƒPAMMA  WRTDT PE„HA‡HA—EHA „‹Ÿ POBEPKˆ PABˆ‹œHOCTˆ         *
C*    ‡A„AHˆŸ ”OPMA‹œH›X APAMETPOB POƒPAMM›  BEETLE ˆ B›BO„A ˆX      *
C*    HA E—ATœ.                                                       *
C*                                                                     *
C*  Pˆ ˆ‡MEHEHˆˆ MAKCˆMA‹œHOƒO —ˆC‹A EPEMEHH›X HEOXO„ˆMO:           *
C*      1) ˆ‡MEHˆTœ PA‡MEPHOCTˆ MACCˆBOB:FX0(NXC-1) ˆ IVARS(NXC);      *
C*      2) ˆ‡MEHˆTœ ‡HA—EHˆE APAMETPA NXC;                            *
C*  Pˆ ˆ‡MEHEHˆˆ MAKCˆMA‹œHOƒO —ˆC‹A ˆCC‹E„“EM›X ”“HK–ˆ‰ HEOXO„ˆMO:  *
C*      1) ˆ‡MEHˆTœ PA‡MEPHOCTœ MACCˆBA IFUNS(NUFC);                   *
C*      2) ˆ‡MEHˆTœ ‡HA—EHˆE APAMETPA  NUFC;                          *
C* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER*2 (I-N)
      include 'beetlebf.dim'
      EXTERNAL FUN
      DIMENSION X0(1), FX0(NDIM1), IADD(1)
C
      COMMON /ADD1A/ IPR, NSTEP, NCAN
      COMMON /ADD1B/ DX, HVMN, HVMX
      COMMON /ADD2A/ ICRS, IPRCRS
      COMMON /ADD2B/ EPSCRS
      COMMON /ADD3A/ NUF, IPRZER, IFUNS(IFMAX)
      COMMON /ADD3B/ EPSZER
      COMMON /ADD4A/ NUE, IPREXT, IVARS(NDIM)
      COMMON /ADD4B/ EPSEXT
      COMMON /ADD5A/ PXGMN, PXGMX
      COMMON /ADD6A/  XGMN,  XGMX
      COMMON /ADD7A/ EPSNWT
      COMMON /ADD7B/ ITNWT
C
C.... HA‡HA—EHˆE ‡HA—EHˆ‰ APAMETPOB, ‡A„AHH›X O “MO‹—AHˆž.
C
      ISTOP= 0
      NXC  = NDIM
      NUFC = IFMAX
C.... 1) APAMETP›, OTHOCŸ™ˆECŸ K „Bˆ†EHˆž O KPˆBO‰:
C
      IPR   = 0
      NSTEP = 1000
      NCAN  = 6
      DX    = 1.D-07
      HVMN  = DABS(HV0)*1.D-02
      HVMX  = DABS(HV0)*1.D+02
C
C.... 2) APAMETP›, OTHOCŸ™ˆECŸ K “TO—HEHˆž TO—EK CAMOEPECE—EHˆŸ:
C
      ICRS   = 1
      IPRCRS = 0
      EPSCRS = 10.D0*EPS
C
C.... 3) APAMETP›, OTHOCŸ™ˆECŸ K “TO—HEHˆž H“‹E‰ ‡A„AHH›X ”“HK–ˆ‰:
C
      NUF    = 0
      IPRZER = 0
      EPSZER = 10.D0*EPS
      DO 220 I=1,NUFC
         IFUNS(I) = I
 220  CONTINUE
C
C.... 4) APAMETP›, OTHOCŸ™ˆECŸ K “TO—HEHˆž KCTPEM“MOB ‡A„AHH›X
C        KOOP„ˆHAT:
C
      NUE    = 0
      IPREXT = 0
      EPSEXT = 10.D0*EPS
      DO 260 I=1,NXC
         IVARS(I) = I
 260  CONTINUE
C
C.... 5) APAMETP›, OTHOCŸ™ˆECŸ K PO–E„“PE “TO—HEHˆŸ EPBO‰ TO—Kˆ
C        HA KPˆBO‰:
C
      PXGMN = X0(NG) 
      PXGMX = X0(NG) 
C
C.... 6). APAMETP›, OTHOCŸ™ˆECŸ K ‡A„AHˆž „O“CTˆMOƒO ˆHTEPBA‹A
C         ˆ‡MEHEHˆŸ „‹Ÿ B›„E‹EHHO‰ EPEMEHHO‰.
C
      XGMN = X0(NG) - 100.D0
      XGMX = X0(NG) + 100.D0
C
C.... 7). APAMETP›, OTHOCŸ™ˆECŸ K PO–E„“PE HœžTOHA:
C
      EPSNWT = EPS
      ITNWT  = 10
C
C.... KOPPEKTˆPOBKA APAMETPOB, ‡A„AHH›X O “MO‹—AHˆž, O‹œ‡OBATE‹EM.
C
      CALL BDATA
C
      IF (IADD(3) .EQ. 0) NUF = 0
      IF (IADD(4) .EQ. 0) NUE = 0
C
C.... E—ATœ ‡A„AHH›X APAMETPOB.
C
      IF (IPR .EQ. -2) RETURN
C
C..1. APAMETP›, OTHOCŸ™ˆECŸ K “TO—HEHˆž TO—EK HA KPˆBO‰.
C
      IER    =-1
#if defined(hib)
      WRITE (NCAN,1000)
      WRITE (NCAN,1001) NX
#endif
         IF (NX.LT.1 .OR. NX.GT.NXC)                     RETURN
#if defined(hib)
      WRITE (NCAN,1002) NG
#endif
         IF (NG.LT.1 .OR. NG.GT.NXC)                     RETURN
#if defined(hib)
      WRITE (NCAN,1004) (X0(I),I=1,NX)
#endif
      IERR = 0
      CALL FUN (X0,FX0,IERR,ISTOP)
      IF (ISTOP .NE. 0) RETURN
      IF (IERR  .EQ. 0) GOTO 500
#if defined(hib)
          WRITE (NCAN,3090)
#endif
          RETURN
 500  CONTINUE
      NFUN = NX - 1
#if defined(hib)
      WRITE (NCAN,1005) (FX0(I),I=1,NFUN)
      WRITE (NCAN,1007) HV0
#endif
         IF (DABS(HV0) .LE. 1.D-14)                      RETURN
#if defined(hib)
      WRITE (NCAN,1010) EPS
#endif
         IF (     EPS  .LE. 1.D-14)                      RETURN
      IF (IADD(1) .EQ. 0) GOTO 502
#if defined(hib)
          WRITE (NCAN,2000)
          WRITE (NCAN,2020)
          WRITE (NCAN,2000)
          WRITE (NCAN,1012) NSTEP
#endif
             IF (NSTEP .GE. 10000)                       RETURN
#if defined(hib)
          WRITE (NCAN,2030) NCAN
          WRITE (NCAN,1011) DX
#endif
             IF (DABS(DX) .LE. 1.D-14)                   RETURN
#if defined(hib)
          WRITE (NCAN,1008) HVMN
          WRITE (NCAN,1009) HVMX
#endif
          AHV0   = DABS(HV0)
             IF (.NOT.(HVMN.LE.AHV0 .AND. AHV0.LE.HVMX)) RETURN
 502  CONTINUE
C
C..2. APAMETP› OTHOCŸ™ˆECŸ K “TO—HEHˆž TO—EK CAMŽEPECE—EHˆŸ.
C
          IF (ICRS .EQ. 0) GOTO 510
#if defined(hib)
              WRITE (NCAN,2010)
              WRITE (NCAN,2080) EPSCRS
#endif
              IF (EPSCRS .LE. 1.D-14)                    RETURN
 510  CONTINUE
C
C..3. APAMETP›, OTHOCŸ™ˆECŸ K “TO—HEHˆž H“‹E‰ ”“HK–ˆ‰.
C
      IF (IADD(3) .EQ. 0) GOTO 520
#if defined(hib)
          WRITE (NCAN,2000)
          WRITE (NCAN,2010)
          WRITE (NCAN,2000)
          WRITE (NCAN,2040) NUF
#endif
             IF (NUF .GT. NUFC)                          RETURN
#if defined(hib)
          WRITE (NCAN,2090) EPSZER
#endif
             IF (EPSZER .LE. 1.D-14)                     RETURN
#if defined(hib)
          IF (NUF .NE. 0) WRITE (NCAN,2050) (IFUNS(I),I=1,NUF)
#endif
          DO 518 I=1,NUFC
            IF (IFUNS(I) .LE. 0) RETURN
 518      CONTINUE
 520  CONTINUE
C
C..4. APAMETP›, OTHOCŸ™ˆECŸ K “TO—HEHˆž KCTPEM“MOB.
C
      IF (IADD(4) .EQ. 0) GOTO 530
#if defined(hib)
          WRITE (NCAN,2000)
          WRITE (NCAN,2010)
          WRITE (NCAN,2000)
          WRITE (NCAN,2060) NUE
#endif
             IF (NUE .GT. NXC)                           RETURN
#if defined(hib)
          WRITE (NCAN,3000) EPSEXT
#endif
             IF (EPSEXT .LE. 1.D-14)                     RETURN
#if defined(hib)
          IF (NUE .NE. 0) WRITE (NCAN,2070) (IVARS(I),I=1,NUE)
#endif
          DO 528 I=1,NXC
             IF (IVARS(I) .LE. 0) RETURN
 528      CONTINUE
 530  CONTINUE
C
C..5. APAMETP›, OTHOCŸ™ˆECŸ K “TO—HEHˆž EPBO‰ TO—Kˆ HA KPˆBO‰.
C
      IF (IADD(5) .EQ. 0) GOTO 540
#if defined(hib)
          WRITE (NCAN,2000)
          WRITE (NCAN,2010)
          WRITE (NCAN,2000)
          WRITE (NCAN,3010) PXGMN, PXGMX
#endif
          IF (.NOT.(PXGMN.LE.X0(NG) .AND. X0(NG).LE.PXGMX)) RETURN
 540  CONTINUE
C
C..6. APAMETP›, OTHOCŸ™ˆECŸ K ‡A„AHˆž „O“CTˆMOƒO ˆHTEPBA‹A
C     ˆ‡MEHEHˆŸ „‹Ÿ B›„E‹EHHO‰ EPEMEHHO‰.
C
      IF (IADD(6) .EQ. 0) GOTO 550
#if defined(hib)
          WRITE (NCAN,2000)
          WRITE (NCAN,2010)
          WRITE (NCAN,2000)
          WRITE (NCAN,1006) XGMN, XGMX
#endif
          IF (.NOT.(XGMN.LE.X0(NG) .AND. X0(NG).LE.XGMX)) RETURN
 550  CONTINUE
C
C..7. APAMETP›, OTHOCŸ™ˆECŸ K PO–E„“PE HœžTOHA:
C
      IF (IADD(7) .EQ. 0) GOTO 560
#if defined(hib)
          WRITE (NCAN,2000)
          WRITE (NCAN,2010)
          WRITE (NCAN,2000)
          WRITE (NCAN,3020) EPSNWT, ITNWT
#endif
 560  CONTINUE
#if defined(hib)
      WRITE (NCAN,1014)
#endif
      IER    = 0
      RETURN
C
C.... ”OPMAT› COO™EHˆ‰.
#if defined(hib)
C
 1000 FORMAT
     A(////' ',119('=')//
     B '   ‡HA—EHˆŸ APAMETPOB POƒPAMM›  BEETLE :'/
     C '   -------------------------------------  ' )
C
 1001 FORMAT
     A('   —ˆC‹O EPEMEHH›X:                      ',I5/)
C
 1002 FORMAT
     A('   HOMEP B›„E‹EHHO‰ EPEMEHHO‰:           ',I5/)
C
 1004 FORMAT
     A('   KOOP„ˆHAT› HA—A‹œHOƒO Pˆ‹ˆ†EHˆŸ X0:  '/
     B10(2X,7(G14.5,1X)/))
C
 1005 FORMAT
     A(/'   ‡HA—EHˆŸ ”“HK–ˆ‰ B TO—KE X0:          '/
     B10(2X,7(G14.5,1X)/))
C
 3090 FORMAT
     A('   HE B›—ˆC‹EH› ‡HA—EHˆŸ ”“HK–ˆ‰ B TO—KE X0')
C
 1006 FORMAT
     A(/'   ˆHTEPBA‹ ˆ‡MEHEHˆŸ B›„E‹EHHO‰ EPEMEHHO‰:'/
     B  '   MˆHˆMA‹œHOE ‡HA—EHˆE:                 ',G14.5/
     C  '   MAKCˆMA‹œHOE ‡HA—EHˆE:                ',G14.5/)
C
 1007 FORMAT
     A('   HA—A‹œH›‰ ˜Aƒ „Bˆ†EHˆŸ O KPˆBO‰:      ',G14.5/)
C
 1008 FORMAT
     A('   MˆHˆMA‹œH›‰ ˜Aƒ „Bˆ†EHˆŸ O KPˆBO‰:    ',G14.5/)
C
 1009 FORMAT
     A('   MAKCˆMA‹œH›‰ ˜Aƒ „Bˆ†EHˆŸ O KPˆBO‰:   ',G14.5)
C
 1010 FORMAT
     A('   TO—HOCTœ OPE„E‹EHˆŸ TO—Kˆ HA KPˆBO‰:  ',G14.5)
C
 1011 FORMAT
     A('   ˜Aƒ —ˆC‹EHHOƒO „ˆ””EPEH–ˆPOBAHˆŸ:      ',G14.2/)
C
 1012 FORMAT
     A('   MAKCˆMA‹œHOE —ˆC‹O ˜AƒOB B„O‹œ KPˆBO‰: ',I5/)
C
 1014 FORMAT
     A(//1X,119('=')//)
C
 2000 FORMAT
     A(1X)
C
 2010 FORMAT
     A(1X,120('.'))
C
 2020 FORMAT
     A('   ‡HA—EHˆŸ APAMETPOB „OO‹HˆTE‹œH›X BO‡MO†HOCTE‰: '/
     B '   ------------------------------------------------ ')
C
 2030 FORMAT
     A('   HOMEP KAHA‹A „‹Ÿ B›BO„A PE‡“‹œTATOB:   ',I5/)
C
 2040 FORMAT
     A('   —ˆC‹O ˆCC‹E„“EM›X ”“HK–ˆ‰ HA KPˆBO‰:   ',I5/)
C
 2050 FORMAT
     A('   OPŸ„OK “TO—HEHˆŸ H“‹E‰:      '/2(30(1X,I3))/)
C
 2060 FORMAT
     A('   —ˆC‹O ˆCC‹E„“EM›X  KOOP„ˆHAT:          ',I5/)
C
 2070 FORMAT
     A('   OPŸ„OK “TO—HEHˆŸ KCTPEM“MOB:'/2(30(1X,I3))/)
C
 2080 FORMAT
     A('   TO—HOCTœ “TO—HEHˆŸ TO—EK CAMOEPECE—EHˆŸ:',G14.5)
C
 2090 FORMAT
     A('   TO—HOCTœ “TO—HEHˆŸ KOOP„ˆHAT H“‹E‰:    ',G14.5)
C
 3000 FORMAT
     A('   TO—HOCTœ “TO—HEHˆŸ KOOP„ˆHAT KCTPEM“MOB:',G14.5)
C
 3010 FORMAT
     A('   Pˆ “TO—HEHˆˆ EPBO‰ TO—Kˆ HA KPˆBO‰ - TECTˆP“ETCŸ'/
     B '   ˆHTEPBA‹:  ',G14.5,'< X0(NG)<',G14.5)
C
 3020 FORMAT
     A( '   MATPˆ–A —ACTH›X POˆ‡BO„H›X HE B›—ˆC‹ŸETCŸ, EC‹ˆ:'
     B /'   „OCTˆƒ“TA TO—HOCTœ :           ',G14.5
     C /'   PO„E‹AHO HœžTOHOBCKˆX ˆTEPA–ˆ‰:',I5)
#endif
      END
