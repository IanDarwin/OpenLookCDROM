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

      SUBROUTINE BEETLE (NX,NG,IADD,X0,HV0,EPS,FUN,DFDX,OUT,IEND)
C     3.05.90ƒ.
C     31.07.1991
C     19.11.92
C     09.06.94
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C *                                                                   *
C *   CˆCOK ”OPMA‹œH›X  A P A M E T P O B :                         *
C *                                                                   *
C *   NX    -PA‡MEPHOCTœ POCTPAHCTBA EPEMEHH›X.                     *
C *   NG    -HOMEP B›„E‹EHHO‰ EPEMEHHO‰.                             *
C *   IADD  -–E‹O—ˆC‹EHH›‰ MACCˆB „‹ˆH› 7, PE„HA‡HA—EHH›‰ „‹Ÿ        *
C *          ˆHˆ–ˆA‹ˆ‡A–ˆˆ APAMETPOB „OO‹HˆTE‹œH›X BO‡MO†HOCTE‰     *
C *          (OˆCAHˆE ˆCO‹œ‡OBAHˆŸ TOƒO APAMETPA CM. B OˆCAHˆˆ   *
C *           POƒPAMM›);                                             *
C *          Pˆ EPBOHA—A‹œHOM ‡HAKOMCTBE C POƒPAMMO‰ PEKOMEH„“EM   *
C *          ‡A„ATœ MACCˆB IADD(1)=IADD(2)= ...  =IADD(7)= 0;         *
C *   X0    -MACCˆB KOOP„ˆHAT HA—A‹œHOƒO Pˆ‹ˆ†EHˆŸ K EPBO‰ TO—KE   *
C *          HA KPˆBO‰;                                               *
C *   HV0   -HA—A‹œH›‰ ˜Aƒ „Bˆ†EHˆŸ O KPˆBO‰;                        *
C *          HV0 > 0 -„Bˆ†EHˆE HA—ˆHAETCŸ B HAPAB‹EHˆˆ BP‡PACTAHˆŸ   *
C *                   EPEMEHHO‰ X0(NG);                              *
C *          HV0 < 0 -„Bˆ†EHˆE HA—ˆHAETCŸ B HAPAB‹EHˆˆ “›BAHˆŸ      *
C *                   EPEMEHHO‰ X0(NG);                              *
C *   EPS   -TO—HOCTœ “TO—HEHˆŸ TO—EK HA KPˆBO‰ B METO„E HœžTOHA;     *
C *   FUN   -ˆMŸ O„POƒPAMM› B›—ˆC‹EHˆŸ PAB›X —ACTE‰;               *
C *   DFDX  -ˆMŸ O„POƒPAMM› B›—ˆC‹EHˆŸ MATPˆ–› —ACTH›X              *
C *          POˆ‡BO„H›X O ‡A„AHH›M ”OPM“‹AM;                        *
C *   OUT   -ˆMŸ O„POƒPAMM› „OO‹HˆTE‹œHO‰ OPAOTKˆ TO—EK HA       *
C *          KPˆBO‰;                                                  *
C *   IEND  -KO„ ‡ABEP˜EHˆŸ PAOT› POƒPAMM› BEETLE:                  *
C *          1-COBEP˜EHO ‡A„AHHOE KO‹ˆ—ECTBO ˜AƒOB B„O‹œ KPˆBO‰;      *
C *          2-B›„E‹EHHAŸ EPEMEHHAŸ „OCTˆƒ‹A CBOEƒO ƒPAHˆ—HOƒO       *
C *            ‡HA—EHˆŸ („OCTˆƒH“TA TO—HOCTœ EPS);                    *
C *          3-KPˆBAŸ ‡AMKH“‹ACœ C ‡A„AHHO‰ TO—HOCTœž;                *
C *          4-B›„E‹EHHAŸ EPEMEHHAŸ „OCTˆƒ‹A CBOEƒO ƒPAHˆ—HOƒO       *
C *            ‡HA—EHˆŸ (C TO—HOCTœž „O BE‹ˆ—ˆH› TEK“™EƒO ˜AƒA);      *
C *         -1-Pˆ ‡A„AHˆˆ BXO„H›X APAMETPOB „O“™EHA O˜ˆKA;        *
C *         -2-EPBAŸ TO—KA HA KPˆBO‰ HE HA‰„EHA;                     *
C *         -3-˜Aƒ „Bˆ†EHŸ O KPˆBO‰ CTA‹ MEHœ˜E MˆHˆMA‹œHOƒO;        *
C *         -4-HE HA‰„EHA KACATE‹œHAŸ B EPBO‰ TO—KE;                 *
C *         -5-C—ET PEPBAH O‹œ‡OBATE‹EM;                            *
C * . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . *
C *                                                                   *
C *   B›‡›BAEM›E O„POƒPAMM›:                                        *
C *    TANGNT, NEWTN, CORR, SERVCE, WRTEND, WRTOUT, POISK             *
C *                   CURNTP        WRTDT                             *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C *   Pˆ ˆ‡MEHEHˆˆ MAKCˆMA‹œHOƒO —ˆC‹A EPEMEHH›X  NDIM -HEOXO„ˆMO  *
C *   ˆ‡MEHˆTœ PA‡MEPHOCTˆ MACCˆBOB:                                  *
C *   XOLD(*) XNEW(*) VTNG0(*) F(*) DFUN(*,*) IVARS(*) VMOVE(*)       *
C *                                                                   *
C *   HEOXO„ˆMO ˆ‡MEHˆTœ TAK†E ‡HA—EHˆE APAMETPA NDIM.              *
C * . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . *
C *   Pˆ ˆ‡MEHEHˆˆ MAKCˆMA‹œHOƒO —ˆC‹A ˆCC‹E„“EM›X ”“HK–ˆ‰ HA KPˆBO‰ *
C *   HEOXO„ˆMO ˆ‡MEHˆTœ PA‡MEPHOCTˆ MACCˆBOB:                       *
C *   IFUNS(*) IERF(*) IFUNSA(*) ˆ ‡HA—EHˆE APAMETPA IFMAX.          *
C *   FUNS2(*)                                                        *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
      IMPLICIT REAL*8(A-H,O-Z)
      IMPLICIT INTEGER*2(I-N)
      include 'beetlebf.dim'
c      Parameter (NDIM=14, NDIM1=13, IFMAX=20, NPTYP=4)
      EXTERNAL FUN, DFDX, OUT
      DIMENSION X0(NX)
      DIMENSION XOLD(NDIM), XNEW(NDIM), IPRNT(NPTYP), IADD(NPTYP),
     A          VTNG0(NDIM), F(NDIM1)
C
      COMMON /ADD1A/ IPR, NSTEP, NCAN
      COMMON /ADD1B/ DX, HVMN, HVMX
      COMMON /ADD2A/ ICRS, IPRCRS
      COMMON /ADD2B/ EPSCRS
      COMMON /ADD3A/ NUF, IPRZER, IFUNS(IFMAX)
      COMMON /ADD3B/ EPSZER
      COMMON /ADD4A/  NUE, IPREXT, IVARS(NDIM)
      COMMON /ADD4B/ EPSEXT
      COMMON /ADD5A/ PXGMN, PXGMX
      COMMON /ADD6A/  XGMN,  XGMX
      COMMON /ADD7A/  EPSNWT
      COMMON /ADD7B/  ITNWT
C
      COMMON /ALL1/  XGMNC, XGMXC
      COMMON /ALL2/  IPOISK
      COMMON /ALL3/  IBND
      COMMON /ALL4/  DETSTP
C
      COMMON /DERN1/ DXCOM
C
      COMMON /WRTO1/ NFIX1, NFIX
      COMMON /WRTO2/ DMOVE1, DMOVE
      COMMON /WRTO3/ COSI1, COSI2, COSI0, COSI3
      COMMON /WRTO4/ EPSC, EPSN
      COMMON /WRTO6/ ITC,IT
      COMMON /WRTO7/ VMOVE(NDIM),VMOVN(NDIM)
C
      COMMON /WRTE1/ HMIN, HMAX
      COMMON /OUT1/  FUNS2(IFMAX)
      COMMON /OUT2/  INDZ, IERF(IFMAX)
      COMMON /ZERO2/ IFUNSA(IFMAX)
      COMMON /LEQV1/ DETL, CONDL
      COMMON /CORR1/  DET, COND, SGNDET
      COMMON /CHECKN/ EPSNAD,EPSNNF
C
      COMMON /WORKBE/ DFUN(NDIM,NDIM), ARR2(NDIM,NDIM)
C
      COMMON /MESSG/  MESSAG
C
C***B
C      Parameters for the adaptive step control.
C
      COMMON /CONTRH/ CDEMP,RMODE,ANGMAX,ANGMIN,ANGCRV
           CDEMP  = 0.1D0
           RMODE  = 1.D0
           ANGMAX = 16.D0
           ANGMIN =  7.D0
           ANGCRV = 10.D0
C***E
C
C.... Setting of the main parameters.
C
      DETSTP=1.0D-40
      ITC    = 5
      COSI1  = DCOS(3.14D0*ANGMAX/180.0D0)
      COSI2  = DCOS(3.14D0*ANGMIN/180.0D0)
      DENCRV = DSIN(2.D0*3.14D0*ANGCRV/180.0D0)
      VK     = 0.707107D0
      DET    = 1.D0
      DETL   = 1.D0
      COND   = 1.D0
      CONDL  = 1.D0
      SGNDET = 1.D0
C
      IPRR   = 0
C
C.... The initialization and printing of input parameters.
C
      ISTOP = 0
      IER   = 0
      CALL WRTDT (NX,NG,IADD,X0,HV0,EPS,FUN,IER,ISTOP)
      IF (ISTOP .NE. 0) GOTO 123
C
      XGMNC  = XGMN
      XGMXC  = XGMX
C
      IPRNT(1) = IPR
      IPRNT(2) = IPRCRS
      IPRNT(3) = IPRZER
      IPRNT(4) = IPREXT
C
      IPOISK   = IADD(5)
      IBND     = IADD(6)
C***B
      COSI1  = DCOS(3.14D0*ANGMAX/180.0D0)
      COSI2  = DCOS(3.14D0*ANGMIN/180.0D0)
      DENCRV = DSIN(2.D0*3.14D0*ANGCRV/180.0D0)
C***E
      IF (IER .EQ. 0) GOTO 102
C
C.... Failure in input parameters.
C
          IEND = -1
          CALL WRTEND (IEND,IPR,ISTEP,NG,NFIX,NCAN,EPS,DMOVE)
          RETURN
 102  CONTINUE
C
C.... The tuning of working parameters.
C
      DXCOM  = DX
      DMOVE  = HV0
      NFIX   = NG
      HCONTR = HVMX/2.D0
      EPSC   = EPS
      ITC1   = ITC - 1
      HMIN   = DABS (DMOVE)
      HMAX   = HMIN
      EPSL   = 5.D0*EPS
      SIDIV  = 0.D0
      SFACT  = 3.D0
      IF (NUF .EQ. 0) GOTO 106
          DO 105 I=1,IFMAX
             IFUNSA(I) = IABS (IFUNS(I))
               IERF(I) = 0
              FUNS2(I) =-666.D+33
 105      CONTINUE
 106  CONTINUE

C***B
#if defined(hib)
      IF (MESSAG.LE.-4) THEN
      PRINT 1001
 1001 FORMAT(/'  BEETLE started',/'    Initial data:')
      PRINT 1002,(X0(I),I=1,NX)
 1002 FORMAT('     X0=',
     *     /('        ',3G23.15)) 
      PRINT 1003,NG,EPS,HV0,HMAX,ANGCRV,ANGMIN,ANGMAX,COSI1,COSI2,
     *           DENCRV
 1003 FORMAT('     NG=',I2,'  EPS=',G11.4,'  HV0=',G11.4,'  HMAX=',
     *                                                          G11.4,  
     *      /'     ANGCRV=',G11.4,'  ANGMIN=',G11.4,'  ANGMAX=',G11.4,
     *      /'     COSI1 =',G11.4,'  COSI2 =',G11.4,'  DENCRV=',G11.4)
      ENDIF
#endif
C***E
C
C.... Computing the first point on a curve.
C
      IER = 0

C***B
#if defined(hib)
      IF (MESSAG.LE.-4) THEN
      PRINT 1011
 1011 FORMAT(/'  POISK starting...')
      ENDIF
#endif
C***E

      CALL POISK (NX,NFIX,NCAN,IPR,X0,EPS,FUN,DFDX,IER,ISTOP)

C***B
#if defined(hib)
      IF (MESSAG.LE.-4) THEN
      PRINT 1012,IER,ISTOP
 1012 FORMAT(/'  POISK completed with IER=',I3,'  ISTOP=',I3)
      PRINT 1013,(X0(I),I=1,NX)
 1013 FORMAT('     Xfinal (X0)=',
     *     /('         ',3G23.15)) 
      PRINT 1014,NFIX
 1014 FORMAT('     NFIX=',I2)
      ENDIF
#endif
C***E
      IF (ISTOP .NE. 0) GOTO 123
      IF (IER   .EQ. 0) GOTO 110
C
C.... Failure in the search of the first point.
C
          IEND = -2
          CALL WRTEND (IEND,IPR,ISTEP,NG,NFIX,NCAN,EPS,DMOVE)
          RETURN
 110  CONTINUE
      DO 120 I=1,NX
         XOLD(I) = X0(I)
 120  CONTINUE
C
C.... Output at the first point.
C
      ISTEP = 0
      IT    = 0
      IWRT  = 1
      IDIV  = 0
      IF (IPR .GE. 0)
     A    CALL WRTOUT (IWRT,ISTEP,IPRR,IT,IDIV,NCAN,NX,NDIM,X0,DFUN)
C
C.... Preliminary processing of the current point by the user.
C
      IER = 0
      CALL CURNTP (NX,X0,IER,ISTOP)
      IF (ISTOP .EQ. 0) GOTO 124
 123      IEND =-5
          CALL WRTEND (IEND,IPR,ISTEP,NG,NFIX,NCAN,EPS,DMOVE)
          RETURN
 124  CONTINUE
C
C.... Setting of the initial direction along the curve.
C
C***B
#if defined(hib)
      IF (MESSAG.LE.-4) THEN
      PRINT 1021
 1021 FORMAT(/'  TANGNT starting...')
      ENDIF
#endif
C***E

      CALL TANGNT (NX,NDIM,NFIX,X0,F,VMOVE,FUN,DFDX,DFUN,IER,ISTOP)

C***B
#if defined(hib)
      IF (MESSAG.LE.-4) THEN
      PRINT 1022,IER,ISTOP
 1022 FORMAT(/'  TANGNT completed with IER=',I3,'  ISTOP=',I3)
      PRINT 1023,(VMOVE(I),I=1,NX)
 1023 FORMAT('     Tangent (VMOVE)=',
     *     /('           ',3G23.15)) 
      ENDIF
#endif
C***E
      IF (ISTOP .NE. 0) GOTO 550
      IF (IER   .EQ. 0) GOTO 130
C
C.... No tangent vector at the first point found.
C
          IWRT  = 5 - IER
          CALL WRTOUT (IWRT,ISTEP,IPR,IT,IDIV,NCAN,NX,NDIM,X0,DFUN)
          IEND = -4
          CALL WRTEND (IEND,IPR,ISTEP,NG,NFIX,NCAN,EPS,DMOVE)
          RETURN
 130  CONTINUE
C
C.... The selection of the optimal fixed variable.
C
      NFIX1= NFIX
      IF (DABS(VMOVE(NFIX)) .GT. VK) GOTO 210
          NFIX = 1
          DO 190 I=1,NX
             IF (DABS(VMOVE(I)) .GT. DABS(VMOVE(NFIX))) NFIX = I
 190      CONTINUE
          IF (NFIX .EQ. NFIX1) GOTO 210
C
C.... Updating of the direction along the curve, computing 
C     the determinant (DET) and the condition number (COND)
C     for the matrix DFUN after choosing a new fixed variable.
C
              CALL CORR (NX,NDIM,NFIX1,NFIX,VMOVE,DMOVE,DFUN)
 210  CONTINUE
      DIMOVE = 1
      IF (VMOVE(NG) .LT. 0.D0) DIMOVE =-1
      DMOVE = HV0*DIMOVE
C
C.... Storing the tangent vector at the first point.
C
      DO 220 I=1,NX
         VTNG0(I) = VMOVE(I)
 220  CONTINUE
      DMOVE0 = DMOVE
      DMOVE1 = DMOVE
C***B
#if defined(hib)
      IF (MESSAG.LE.-4) THEN
      PRINT 1031
 1031 FORMAT(/'  CHOOSING fixed variable:')
      PRINT 1032,NFIX,DMOVE
 1032 FORMAT('     NFIX='I2,'  DMOVE=',G11.4)
      ENDIF
#endif
C***E
      CALL SERVCE (ISTEP,NX,NDIM,NG,NCAN,ICRS,NUF,IFUNS,NUE,IVARS,IPRNT,
     A             X0,EPS,FUN,DFDX,OUT,DFUN,ISTOP)
      IF (ISTOP .EQ. 0) GOTO 222
          IEND =-5
          CALL WRTEND (IEND,IPR,ISTEP,NG,NFIX,NCAN,EPS,DMOVE)
          RETURN
 222  CONTINUE
C
C.... Loop for computing NSTEP points in one direction along the curve.
C
      DO 470 ISTEP = 1,NSTEP
         IDIV  = 0
         ICORR = 1
C
C.... Tangent predictor.
C
 230     CONTINUE
         DO 240 I=1,NX
            XNEW(I) = XOLD(I) + DMOVE*VMOVE(I)/VMOVE(NFIX)
 240     CONTINUE
         IF (IBND .EQ. 0) GOTO 301
C
C.... Does the predicted point lie within the bounds?
C
            IBOUND = 0
         IF (XNEW(NG) .GT. XGMN .AND. XNEW(NG) .LT. XGMX) GOTO 300
            IBOUND = 1
C
C.... The predicted point put on a bound.
C
            XBOUND = XGMX
            IF (XNEW(NG) .LE.XGMN) XBOUND =  XGMN
C
C.... POƒHO‡ O KACATE‹œHO‰ „O EPECE—EHˆŸ C ƒˆEP‹OCKOCTœž
C     XNEW(NG) = XBOUND
C
            DMOVE1 = DMOVE
            DMOVE  = (XBOUND - XOLD(NG))/VMOVE(NG)
            DO 250 I=1,NX
               XNEW(I) =  XOLD(I) + DMOVE*VMOVE(I)
 250        CONTINUE
            IWRT = 10
            CALL WRTOUT (IWRT,ISTEP,IPR,IT,IDIV,NCAN,NX,NDIM,XNEW,DFUN)
C
C.... “TO—HEHˆE TO—Kˆ, ‹ˆ‡KO‰ K ƒPAHˆ–E.
C
               NFIXR = NG
               IT    = ITC
               IER   = 0
               CALL NEWTN (NX,NDIM,NG,NFIXR,NCAN,XNEW,EPS,EPSN,IT,IPR,
     A                     FUN,DFDX,DFUN,IER,ISTOP)
               IF (ISTOP .NE. 0) GOTO 550
               IF (IER   .EQ. 0) GOTO 310
                   IWRT  = 1 - IER
                   NFIXR = NFIX
                   NFIX  = NG
                   CALL WRTOUT (IWRT,ISTEP,IPR,IT,IDIV,NCAN,NX,NDIM,
     A                          XNEW,DFUN)
                   NFIX  = NFIXR
                   ICORR =-1
                   GOTO 309
 260       CONTINUE
C
C.... B›BO„ ƒPAHˆ—HO‰ TO—Kˆ, “TO—HEHHO‰ C TO—HOCTœž „O BE‹ˆ—ˆH›
C     MˆHˆMA‹œHOƒO ˜AƒA.
C
               DMOVE = HVMN
               IWRT  = 1
               IF (IPR .GE. 0)
     A             CALL WRTOUT (IWRT,ISTEP,IPRR,IT,IDIV,NCAN,NX,NDIM,
     B                          XOLD,DFUN)
               IEND = 4
               CALL WRTEND (IEND,IPR,ISTEP,NG,NFIX,NCAN,EPS,DMOVE)
               RETURN
 300    CONTINUE
 301  CONTINUE
C
C.... “TO—HEHˆE KOOP„ˆHAT O—EPE„HO‰ TO—Kˆ HA KPˆBO‰.
C
        IT = ITC
        IER= 0
C***B
#if defined(hib)
      IF (MESSAG.LE.-4) THEN
      PRINT 1041
 1041 FORMAT(/'  NEWTN starting at')
      PRINT 1042,(XNEW(I),I=1,NX)
 1042 FORMAT('     Xinit (XNEW)=',
     *     /('           ',3G23.15)) 
      ENDIF
#endif
C***E
        CALL NEWTN (NX,NDIM,NG,NFIX,NCAN,XNEW,EPS,EPSN,IT,IPR,FUN,DFDX,
     A              DFUN,IER,ISTOP)
C***B
#if defined(hib)
      IF (MESSAG.LE.-4) THEN
      PRINT 1043,IER,ISTOP
 1043 FORMAT(/'  NEWTN completed with IER=',I3,'  ISTOP=',I3)
      PRINT 1044,(XNEW(I),I=1,NX)
 1044 FORMAT('     Xfinal (XNEW)=',
     *     /('           ',3G23.15)) 
      PRINT 1045,EPSN,EPS,IT
 1045 FORMAT(/'    EPSN=',G11.4,'  EPS=',G11.4,'  IT=',I2)
      ENDIF
#endif
C***E
        IF (ISTOP .NE. 0) GOTO 550
        IF (IER   .EQ. 0) GOTO 310
C
C.... HE CO˜‹ˆCœ HœžTOHOBCKˆE ˆTEPA–ˆˆ.
C
 306     CONTINUE

C  ---     STEP IS REJECTED
C***B
#if defined(hib)
      IF (MESSAG.LE.-4) THEN
      PRINT 1046
 1046 FORMAT(/'    Point is rejected because of error return code')
      ENDIF
#endif
C***E

            IWRT  = 1 - IER
            CALL WRTOUT (IWRT,ISTEP,IPR,IT,IDIV,NCAN,NX,NDIM,XNEW,DFUN)
 309        CONTINUE
            INDSTP=-1
            CALL SHOWBE(INDSTP,DMOVE,DMOVE/2.D0)
            DMOVE = DMOVE/2.D0
            IDIV  = IDIV + 1
C***B
#if defined(hib)
      IF (MESSAG.LE.-4) THEN
      PRINT 1051
 1051 FORMAT(/'  STEP halving: ')
      PRINT 1052,DMOVE,IDIV
 1052 FORMAT('     DMOVE=',G15.6,'  IDIV=',I2)
      ENDIF
#endif
C***E
            ICORR =-1
            IF (DABS(DMOVE) .GE. HVMN
     A          .AND.XOLD(NFIX)+DMOVE*VMOVE(NFIX).NE.XOLD(NFIX))
     B                                 GOTO 230
                IF (IBOUND .EQ. 1) GOTO 260
                    IEND = -3
                    CALL WRTEND (IEND,IPR,ISTEP,NG,NFIX,NCAN,EPS,DMOVE)
                    RETURN
 310    CONTINUE
        IF (ITC1 .EQ. IT) ICORR = -1
C
C.... POBEPKA “TO—HEHHO‰ TO—Kˆ.
C
        COSI0 = 0.D0
        DIST  = 0.D0
        DO 320 I=1,NX
           DIFF  = XNEW(I) - XOLD(I)
           DIST  = DIST + DIFF*DIFF
           COSI0 = COSI0 + DIFF*VMOVE(I)
 320    CONTINUE
        DIST  = DSQRT(DIST)
        COSI0 = COSI0/DIST*DSIGN(1.D0,DMOVE)
C***B
        WRK   = 1.D0 - COSI0*COSI0
        SINI0 = DSQRT(DABS(WRK))
        SINI0 = DMAX1(1.D-10,SINI0)
        RAD0  = DIST/2.D0/SINI0
C***B
#if defined(hib)
      IF (MESSAG.LE.-4) THEN
      PRINT 1061
 1061 FORMAT(/'  TESTING new point (step 1):')
      PRINT 1062,COSI0,COSI1,DIST,RAD0
 1062 FORMAT('     COSI0=',G11.4,'  COSI1=',G11.4,'  DIST =',G11.4,
     *       ' RAD0=',G11.4) 
      ENDIF
#endif
C***E
C
C***E
C
C***B
        DDMOVE=DABS(DMOVE)
        FDM = 0.01
        IF (COSI0 .LT. COSI1*FDM*DDMOVE/(FDM*DDMOVE+EPS)
     *      .AND. RMODE.GE.0.) THEN
C  ---     STEP IS REJECTED
C***B
#if defined(hib)
      IF (MESSAG.LE.-4) THEN
      PRINT 1063
 1063 FORMAT(/'    Point is rejected')
      ENDIF
#endif
C***E
           GOTO 325
        ELSE
C***E
C  ---     STEP IS ACCEPTED
C***B
#if defined(hib)
      IF (MESSAG.LE.-4) THEN
      PRINT 1064
 1064 FORMAT(/'    Point is accepted')
      ENDIF
#endif
C***E
           GOTO 321
        ENDIF
 321  CONTINUE
C
C.... B›—ˆC‹EHˆE KACATE‹œHOƒO BEKTOPA B TEK“™E‰ TO—KE.
C
        IER = 0
C***B
#if defined(hib)
      IF (MESSAG.LE.-4) THEN
      PRINT 1071
 1071 FORMAT(/'  TANGNT starting...')
      ENDIF
#endif
C***E
        CALL TANGNT (NX,NDIM,NFIX,XNEW,F,VMOVN,FUN,DFDX,DFUN,
     *               IER,ISTOP)
C***B
#if defined(hib)
      IF (MESSAG.LE.-4) THEN
      PRINT 1072,IER,ISTOP
 1072 FORMAT(/'  TANGNT completed with IER=',I3,'  ISTOP=',I3)
      PRINT 1073,(VMOVN(I),I=1,NX)
 1073 FORMAT('     Tangent (VMOVN)=',
     *     /('           ',3G23.15)) 
      ENDIF
#endif
C***E
        IF (ISTOP .NE. 0) GOTO 550
        IF (IER   .EQ. 0) GOTO 322
C
C.... HE B›—ˆC‹EH KACATE‹œH›‰ BEKTOP.
C
            IWRT  = 5 - IER
            CALL WRTOUT (IWRT,ISTEP,IPR,IT,IDIV,NCAN,NX,NDIM,XNEW,DFUN)
            GOTO 309
C
C.... Ž„Ž‹†…ˆ… POBEPKˆ “TO—HEHHO‰ TO—Kˆ.
C
 322    CONTINUE
        COSI3 = 0.D0
        DIST3 = 0.D0
        DO 323 I=1,NX
           DIFF  = XNEW(I) - XOLD(I)
           DIST3 = DIST3 + DIFF*DIFF
           COSI3 = COSI3 + DIFF*VMOVN(I)
 323    CONTINUE
        DIST3 = DSQRT(DIST3)
        COSI3 = COSI3/DIST3*DSIGN(1.D0,DMOVE)
        SINI3 = DSQRT(DABS(1.D0 - COSI3*COSI3))
        SINI3 = DMAX1(1.D-10,SINI3)
        RAD3  = DIST3/2.D0/SINI3
C***B
#if defined(hib)
      IF (MESSAG.LE.-4) THEN
      PRINT 1081
 1081 FORMAT(/'  TESTING new point (step 2):')
      PRINT 1082,COSI3,COSI1,DIST3,RAD3
 1082 FORMAT('     COSI3=',G11.4,'  COSI1=',G11.4,'  DIST3=',G11.4,
     *       ' RAD3=',G11.4) 
      ENDIF
#endif
C***E
C
c        IF (RMODE.LT.0.) GOTO 330
         IF (COSI3 .LT. COSI1*FDM*DDMOVE/(FDM*DDMOVE+EPS)
     *       .AND. RMODE.GE.0.) THEN
C  ---     STEP IS REJECTED
C***B
#if defined(hib)
      IF (MESSAG.LE.-4) THEN
      PRINT 1083
 1083 FORMAT(/'    Point is rejected')
      ENDIF
#endif
C***E
           GOTO 325
        ELSE
C***E
C  ---     STEP IS ACCEPTED
C***B
#if defined(hib)
      IF (MESSAG.LE.-4) THEN
      PRINT 1084
 1084 FORMAT(/'    Point is accepted')
      ENDIF
#endif
C***E
           GOTO 324
        ENDIF
 324  CONTINUE
C***B
#if defined(hib)
      IF (MESSAG.LE.-4) THEN
      PRINT 1121
 1121 FORMAT(/'  TESTING new point (step 3):')
      ENDIF
#endif
C***E
C
      CALL CNEWTN (NX,NDIM,NFIX,NFIXC,XNEW,F,VMOVN,EPS,DFUN,IER,ISTOP)
C***B
#if defined(hib)
      IF (MESSAG.LE.-4) THEN
      PRINT 1122,EPSNAD,EPSNNF,NFIXC
 1122 FORMAT('     EPSNAD=',G11.4,'  EPSNNF=',G11.4,'  NFIXC=',I3)
      ENDIF
#endif
C***E
      IF (IER.NE.0 .AND. RMODE.GE.0.) THEN
C  ---     STEP IS REJECTED
C***B
#if defined(hib)
      IF (MESSAG.LE.-4) THEN
      PRINT 1123
 1123 FORMAT(/'    Point is rejected')
      ENDIF
#endif
C***E
           GOTO 325
        ELSE
C***E
C  ---     STEP IS ACCEPTED
C***B
#if defined(hib)
      IF (MESSAG.LE.-4) THEN
      PRINT 1124
 1124 FORMAT(/'    Point is accepted')
      ENDIF
#endif
C***E
           GOTO 330
        ENDIF
C


C
C.... “TO—HEHHAŸ TO—KA HA KPˆBO‰ HE ‹ˆ‡KA K TO—KE-POƒHO‡“.
C
 325        IWRT  = 9
            CALL WRTOUT (IWRT,ISTEP,IPR,IT,IDIV,NCAN,NX,NDIM,XNEW,DFUN)
            GOTO 309
 330    CONTINUE
C
C.... PE„BAPˆTE‹œHAŸ OPAOTKA TEK“™E‰ TO—Kˆ KPˆBO‰ O‹œ‡OBATE‹EM.
C
      IER = 0
      CALL CURNTP (NX,XNEW,IER,ISTOP)
      IF (ISTOP .EQ. 0) GOTO 334
          IEND =-5
          CALL WRTEND (IEND,IPR,ISTEP,NG,NFIX,NCAN,EPS,DMOVE)
          RETURN
 334  CONTINUE
      IF (IER .EQ. 0) GOTO 336
C
C.... PˆHŸTO PE˜EHˆE EPEC—ˆTATœ TO—K“ C O‹OBˆHH›M ˜AƒOM.
C
            IWRT  = 15
            CALL WRTOUT (IWRT,ISTEP,IPR,IT,IDIV,NCAN,NX,NDIM,XNEW,DFUN)
            GOTO 309
 336  CONTINUE
C
C.... POBEPKA KPˆBO‰ HA ‡AMKH“TOCTœ.
C
        IF(ISTEP .EQ. 1) GOTO 380
        IF ((XNEW(NFIX)-X0(NFIX))*(X0(NFIX)-XOLD(NFIX)).LT.0.D0)GOTO 380
C
C.... KOHTPO‹œ COƒ‹ACOBAHHOCTˆ HAPAB‹EHˆ‰ „Bˆ†EHˆŸ B POBEPŸEMO‰
C     ˆ EPBO‰ TO—KAX.
C
           VLOCK0= VTNG0(NFIX)*DMOVE0
           VLOCK = VMOVE(NFIX)*DMOVE
           IF (VLOCK0*VLOCK .LT. 0.D0) GOTO 380
C
C.... POBEPKA ‹ˆ‡OCTˆ POBEPŸEMO‰ ˆ EPBO‰ TO—EK HA KPˆBO‰.
C
               COSIL = 0.D0
               DIST  = 0.D0
               DO 340 I=1,NX
                  DIFF  = X0(I) - XOLD(I)
                  DIST  = DIST + DIFF*DIFF
                  COSIL = COSIL + DIFF*VMOVE(I)
 340           CONTINUE
               DIST  = DSQRT(DIST)
               COSIL = DABS(COSIL)/DIST
               IF (COSIL .LT. COSI1) GOTO 380
C
C.... POƒHO‡ ˆ “TO—HEHˆE TO—Kˆ C ‡A„AHHO‰ ”ˆKCˆPOBAHHO‰ KOOP„ˆHATO‰
C     XNEW(NFIX)=X0(NFIX).
C
                   DVLOCK = (X0(NFIX) - XOLD(NFIX))/VMOVE(NFIX)
                   DO 350 I=1,NX
                      XNEW(I) = XOLD(I) + DVLOCK*VMOVE(I)
 350               CONTINUE
                   IWRT = 12
                   CALL WRTOUT (IWRT,ISTEP,IPR,IT,IDIV,NCAN,NX,NDIM,
     A                          XNEW,DFUN)
                   IT    = ITC
                   IER   = 0
                   CALL NEWTN (NX,NDIM,NG,NFIX,NCAN,XNEW,EPS,EPSN,IT,
     A                         IPR,FUN,DFDX,DFUN,IER,ISTOP)
                   IF (ISTOP .NE. 0) GOTO 550
                   IF (IER   .EQ. 0) GOTO 360
C
C.... HE CO˜‹ˆCœ HœžTOHOBCKˆE ˆTEPA–ˆˆ.
C
                       DMOVE = DVLOCK
                       GOTO 306
 360               CONTINUE
                   IWRT  = 13
                   CALL WRTOUT (IWRT,ISTEP,IPR,IT,IDIV,NCAN,NX,NDIM,
     A                          XNEW,DFUN)
                   IWRT = 14
                   CALL WRTOUT (IWRT,ISTEP,IPR,IT,IDIV,NCAN,NX,NDIM,X0,
     A                          DFUN)
                   DO 370 I=1,NX
                      IF (DABS(XNEW(I) - X0(I)) .GT. EPSL) GOTO 380
 370               CONTINUE
C
C.... B›BO„ TO—Kˆ ‡AM›KAHˆŸ.
C
                   DMOVE  = DVLOCK
                   DMOVE1 = DVLOCK
                   IER    = 3
                   IWRT   = 1
                   IF (IPR .GE. 0)
     A                CALL WRTOUT (IWRT,ISTEP,IPRR,IT,IDIV,NCAN,NX,
     B                             NDIM,XNEW,DFUN)
                   DO 372 I=1,NX
                      VMOVE(I) = VTNG0(I)
 372               CONTINUE
                   CALL SERVCE (ISTEP,NX,NDIM,NG,NCAN,ICRS,NUF,IFUNS,
     A                          NUE,IVARS,IPRNT,XNEW,EPS,FUN,DFDX,OUT,
     B                          DFUN,ISTOP)
                   IEND = 3
                   CALL WRTEND(IEND,IPR,ISTEP,NG,NFIX,NCAN,EPS,DMOVE)
                   RETURN
 380    CONTINUE
        DO 400 I=1,NX
           VMOVE(I)=VMOVN(I)
 400    CONTINUE
C
C.... B›OP OTˆMA‹œHO‰ ”ˆKCˆPOBAHHO‰ EPEMEHHO‰.
C
        NFIX1 = NFIX
c        IF (DABS(VMOVE(NFIX)) .GT. VK) GOTO 430
c            NFIX  = 1
c            DO 410 I=1,NX
c               IF (DABS(VMOVE(I)) .GT. DABS(VMOVE(NFIX))) NFIX = I
c 410        CONTINUE
	    NFIX = NFIXC
            IF (NFIX .EQ. NFIX1) GOTO 430
C
C.... KOPPEK–ˆŸ HAPAB‹EHˆŸ „Bˆ†EHˆŸ, B›—ˆC‹EHˆE OPE„E‹ˆTE‹Ÿ (DET)
C     ˆ O–EHKˆ O“C‹OB‹EHHOCTˆ (COND) „‹Ÿ MATPˆ–› (DFUN) C “—ETOM BHOBœ
C     B›PAHHO‰ ”ˆKCˆPOBAHHO‰ EPEMEHHO‰.
C
              CALL CORR (NX,NDIM,NFIX1,NFIX,VMOVE,DMOVE,DFUN)
              IF (DET*DETL .LT. 0.D0) SGNDET=-SGNDET
 430  CONTINUE
C
C.... KOPPEK–ˆŸ BE‹ˆ—ˆH› TEK“™EƒO ˜AƒA.
C
        DMOVE1 = DMOVE

         IF (IDIV.GT.0) SFACT=1.125D0
         IF (SFACT.LT.3.D0) SFACT=2.D0*SFACT-1.D0
C***B
#if defined(hib)
      IF (MESSAG.LE.-4) THEN
      PRINT 1091
 1091 FORMAT(/'  CHOOSING fix variable:')
      PRINT 1092,NFIX,DMOVE
 1092 FORMAT('     NFIX='I2,'  DMOVE=',G11.4)
      ENDIF
#endif
C***E
C
C***B
        MODE=DABS(RMODE)
C
C        MODE=1     - ã¢¥«¨ç¥­¨¥ è £  ¢¤¢®¥ ¯p¨ ¬ «®¬ ã£«¥
C            =2     - ¢ëç¨á«¥­¨¥ è £  ç¥p¥§ p ¤¨ãá ªp¨¢¨§­ë
C            =3     - ª®pp¥ªæ¨ï è £  ¯® ®â«¨ç¨î ®â âp¥¡ã¥¬®£® ã£« 
C
        IF (MODE.LT.1.OR.MODE.GT.3) MODE=1
        IF (MODE.EQ.1) GO TO 437
        IF (MODE.GT.2) GO TO 433
        DMOVN=DABS(DMOVE)
        IF (MODE.EQ.2) DMOVN = RAD3*DENCRV
C        ª®­áâ ­âë 3 ¨ 0.3 ®£p ­¨ç¨¢ îâ á¢¥påã ¨ á­¨§ã ¨§¬¥­¥­¨¥ è £ 
        DMOVN = DMIN1(SFACT*DABS(DMOVE),DMOVN)
        DMOVN = DMAX1(0.3D0*DABS(DMOVE),DMOVN)
        DMOVN = DMIN1(HVMX,DMOVN)
        DMOVN = DMAX1(HVMN,DMOVN)
        IF (ICORR .EQ. -1 .AND. IT .GT. 1)  
     A                     DMOVN=DMIN1(DMOVN,DABS(DMOVE))
        DMOVE = DSIGN(DMOVN,DMOVE)
        GO TO 440
C
 433    CONTINUE
        ANG3=DATAN2(SINI3,COSI3)*180.0D0/3.14D0
        SINI2=DSQRT(1.D0-COSI2*COSI2)
        FACT = CDEMP*(ANG3-ANGCRV)
c       FACT = ANG3/ANGCRV-1.0D0
        FACT1=1.D0-FACT
C        ª®­áâ ­âë 3 ¨ 0.3 ®£p ­¨ç¨¢ îâ á¢¥påã ¨ á­¨§ã ¨§¬¥­¥­¨¥ è £ 
        FACT1=DMAX1(FACT1,0.3D0)
        FACT1=DMIN1(FACT1,SFACT)
        DMOVN=DABS(DMOVE)*FACT1
        DMOVN = DMIN1(HVMX,DMOVN)
        DMOVN = DMAX1(HVMN,DMOVN)
        IF (ICORR .EQ. -1 .AND. IT .GT. 1)  
     A                     DMOVN=DMIN1(DMOVN,DABS(DMOVE))
        DMOVE = DSIGN(DMOVN,DMOVE)
        GO TO 440
C   
 437    CONTINUE
C***E
        IF (ICORR .EQ. -1 .AND. IT .GT. 1)  GOTO 440
           DMOVEC = DABS(DMOVE)
           IF (COSI0.GT.COSI2 .AND. DMOVEC.LE.HCONTR)
     A                              DMOVE=DMOVE*DMIN1(2.D0,SFACT)
           IF (COSI0.GT.COSI2 .AND. DMOVEC.GT.HCONTR) 
     A                                   DMOVE=DSIGN(HVMX,DMOVE)
 440    CONTINUE
        IF (DABS(DMOVE*VMOVE(NFIX)).GT.HVMX)
     A                     DMOVE=DSIGN(HVMX/VMOVE(NFIX),DMOVE)
            INDSTP=1
            CALL SHOWBE(INDSTP,DMOVE1,DMOVE)
C
C.... B›—ˆC‹EHˆE ACO‹žTH›X ‡HA—EHˆ‰ MAKCˆMA‹œHOƒO ˆ MˆHˆMA‹œHOƒO
C     ˜AƒOB B„O‹œ KPˆBO‰.
C
C***B
#if defined(hib)
      IF (MESSAG.LE.-4) THEN
      PRINT 1101
 1101 FORMAT(/'  SETTING new step:')
      PRINT 1102,DMOVE
 1102 FORMAT('     DMOVE=',G11.4)
      ENDIF
#endif
C***E
        HABS = DABS (DMOVE1)
        IF (HABS .LT. HMIN) HMIN = HABS
        IF (HABS .GT. HMAX) HMAX = HABS
C
C.... B›BO„ O—EPE„HO‰ TO—Kˆ HA KPˆBO‰.
C
        IER = 0
        IWRT = 1
        CALL WRTOUT (IWRT,ISTEP,IPR,IT,IDIV,NCAN,NX,NDIM,XNEW,DFUN)
        CALL SERVCE (ISTEP,NX,NDIM,NG,NCAN,ICRS,NUF,IFUNS,NUE,IVARS,
     A               IPRNT,XNEW,EPS,FUN,DFDX,OUT,DFUN,ISTOP)
        IF (IBOUND .EQ. 1)  GOTO 480
        IF (ISTOP .EQ. 0) GOTO 454
            GOTO 550
 454    CONTINUE
        DO 460 I=1,NX
           XOLD(I) = XNEW(I)
 460    CONTINUE
 470  CONTINUE
      IEND  = 1
      CALL WRTEND (IEND,IPR,ISTEP,NG,NFIX,NCAN,EPS,DMOVE)
      RETURN
 480  CONTINUE
C
C.... “TO—HEHA ƒPAHˆ—HAŸ TO—KA.
C
      IEND = 2
      CALL WRTEND (IEND,IPR,ISTEP,NG,NFIX,NCAN,EPS,DMOVE)
      RETURN
C
 550  IEND =-5
      CALL WRTEND (IEND,IPR,ISTEP,NG,NFIX,NCAN,EPS,DMOVE)
      RETURN
      END
