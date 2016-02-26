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

      SUBROUTINE POISK (NX,NFIX,NCAN,IPR,X0,EPS,FUN,DFDX,IER,ISTOP)
C     3.05.90ƒ.
C     13.11.92
C     15.06.94
C* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C*                                                                     *
C*    POƒPAMMA  POISK  PE„HA‡HA—EHA „‹Ÿ “TO—HEHˆŸ EPBO‰ TO—Kˆ HA    *
C* KPˆBO‰ O ‡A„AHHOM“ HA—A‹œHOM“ Pˆ‹ˆ†EHˆž. CTPATEƒˆŸ “TO—HEHˆŸ     *
C* O—EHœ POCTA:O‹ACTœ ˆ‡MEHEHˆŸ B›„E‹EHHO‰ EPEMEHHO‰ X(NG)          *
C* OKP›BAETCŸ CETKO‰ C HEKOTOP›M ˜AƒOM. “CTœ X0<1>,X0<2>,...,X0<M>-  *
C* “‡‹› CETKˆ. HA—ˆHAŸ C HEKOTOPOƒO “‡‹A X0<K>=X0(NG) EPEˆPAžTCŸ     *
C* BCE “‡‹› B C‹E„“ž™EM OPŸ„KE:X0<K>,X0<K-1>,X0<K+1>,X0<K-2> ˆ T.„.   *
C* Pˆ—EM B KA†„OM "“‡‹E" POˆ‡BO„ˆTCŸ EPEOP BCEBO‡MO†H›X            *
C* ƒˆEP‹OCKOCTE‰ C“CKA „O TEX OP OKA TO—KA HA KPˆBO‰ HE “„ET     *
C* HA‰„EHA, ‹ˆO HE POBEPŸTCŸ BCE "“‡‹›" CETKˆ O EPEMEHHO‰ X(NG).   *
C* . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . *
C*                                                                     *
C*    CˆCOK ”OPMA‹œH›X APAMETPOB:                                    *
C*                                                                     *
C*    NX   -PA‡MEPHOCTœ POCTPAHCTBA EPEMEHH›X.                       *
C*    NFIX -HA BXO„E B POƒPAMM“ - HOMEP B›„E‹EHO‰ EPEMEHHO‰;         *
C*          HA B›XO„E-HOMEP ”ˆKCˆPOBAHHO‰ EPEMEHHO‰ “TO—HEHHO‰ TO—Kˆ. *
C*    X0   -HA—A‹œHOE Pˆ‹ˆ†EHHˆE K EPBO‰ TO—KE HA KPˆBO‰.           *
C*          (HA B›XO„E ˆ‡ POƒPAMM› KOOP„ˆHAT› “TO—HEHHO‰ TO—Kˆ)       *
C*    EPS  -TO—HOCTœ OPE„E‹EHˆŸ TO—Kˆ HA KPˆBO‰.                      *
C*    FUN  -ˆMŸ O„POƒPAMM› ”“HK–ˆˆ.                                  *
C*    DFDX -ˆMŸ POƒPAMM› B›—ˆC‹EHˆŸ MATPˆ–› —ACTH›X   Pˆ‡BO„H›X;     *
C*    IER  -KO„ BO‡BPATA (IER=0 -COOTBETCTB“ET HOPMA‹œHOM“             *
C*                        ‡ABEP˜EHˆž PAOT› POƒPAMM›).                *
C*    ISTOP-                                                           *
C* ................................................................... *
C*    B›‡›BAEM›E O„POƒPAMM›: NEWTN.                                  *
C* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C* Pˆ ˆ‡MEHEHˆˆ MAKCˆMA‹œHOƒO —ˆC‹A EPEMEHH›X NDIM -HEOXO„ˆMO ˆ‡ME- *
C* HˆTœ PA‡MEPHOCTˆ MACCˆBOB XOLD(NDIM),DFUN(NDIM,NDIM),FUNS(NDIM-1) ˆ *
C* ‡HA—EHˆE APAMETPA NDIM.                                            *
C***********************************************************************
C
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER*2 (I-N)
      include 'beetlebf.dim'
      DIMENSION X0(1), XOLD(NDIM), FUNS(NDIM1)
      EXTERNAL FUN, DFDX
      COMMON /ADD5A/ PXGMN, PXGMX
      COMMON /ADD6A/  XGMN,  XGMX
      COMMON /ALL2/ IPOISK
      COMMON /ALL3/  IBND
      COMMON /CNWT1/ IPNT1
C
      COMMON /WORKBE/ DFUN(NDIM,NDIM), ARR2(NDIM,NDIM)
C
      ISTOP = 0
      IPNT1 = 1
C
C.... OPE„E‹EHˆE BCOMOƒATE‹œH›X APAMETPOB.
C
      NG   = NFIX
      NFUN = NX - 1
      DO 100 I=1,NX
        XOLD(I)=X0(I)
 100  CONTINUE
      DELTA = 0.1D0*(PXGMX - PXGMN)
      XC    = X0(NG)
C
      IF (DELTA.NE.0.D0) GOTO 105  
        MEND = 1
        IND  = -1
        GOTO 108
 105  CONTINUE
C                
      ML    = (XC - PXGMN)/DELTA + 2
      MR    = (PXGMX - XC)/DELTA + 2
      MEND  = MAX0 (ML,MR)
      IND   = -1
      IF (ML .GT. MR) IND = 1
C
 108  CONTINUE
C
      NDR   = 2
      IEND  = 0
C
C.... O™ˆ‰ –ˆK‹ „‹Ÿ POXO†„EHˆŸ ˆHTEPBA‹A (PXGMN,PXGMX) C ˜AƒOM DELTA.
C
#if defined(hib)
      IF (IPR .GE. 1) WRITE (NCAN,2020)
#endif
      DO 170 I=1,MEND
        NEND = NDR
        IF (I .EQ. 1) NEND = 1
        DX0 = (I - 1)*DELTA
C
C.... –ˆK‹ “TO—HEHˆ‰ TO—Kˆ HA KPˆBO‰ Pˆ PA‡‹ˆ—H›X ‡HA—EHˆŸX X0(NG)
C     ˆ‡ ˆHTEPBA‹A (PXGMN,PXGMX) (O PA‡‹ˆ—H›M HAPAB‹EHˆŸM C“CKA Pˆ
C     KA†„OM ‡HA—EHˆˆ X0(NG)).
C
        DO 160 J=1,NEND
          X0NEW = XC + DX0*IND
          IF (IBND .EQ. 0) GOTO 110
C
C.... POBEPKA B›XO„A ‡A ƒPAHˆ–“.
C
          IF (XGMN .LT. X0NEW .AND. X0NEW .LT. XGMX) GOTO 110
C
C.... OˆCK TO—Kˆ B‹ˆ‡ˆ ƒPAHˆ–›.
C
              XBOUND = XGMN
              IF (X0NEW .GT. XGMX) XBOUND = XGMX
              IEND = IEND + 1
              NDR  = 1
              IND  =-1*IND
              X0NEW= XBOUND
 110      CONTINUE
C
C.... “TO—HEHˆE TO—Kˆ B ƒˆEP‹OCKOCTˆ X(NG)=CONSTANATA.
C
          NFIX   = NG
          X0(NG) = X0NEW
#if defined(hib)
          IF (IPR .GE. 1) WRITE (NCAN,2000)
          IF (IPR .GE. 1) WRITE (NCAN,2030) X0NEW
          IF (IPR .GE. 1) WRITE (NCAN,2040) NFIX
          IF (IPR .GE. 1) WRITE (NCAN,2050) X0(NFIX)
#endif
          IT     = 10
C
C.... OPA™EHˆE K PO–E„“PE HœžTOHA.
C
          IERN = 0
          CALL NEWTN (NX,NDIM,NG,NFIX,NCAN,X0,EPS,EPSN,IT,IPR,
     A                FUN,DFDX,DFUN,IERN,ISTOP)
          IF (ISTOP .NE. 0) RETURN
          IF (IERN  .EQ. 0) GOTO 180
C
C.... TO—KA HA KPˆBO‰ HE HA‰„EHA.
C
#if defined(hib)
              IF (IPR .GE. 1) WRITE (NCAN,2000)
              IF (IPR .GE. 1) WRITE (NCAN,2060)
              IF (IPR .GE. 1) WRITE (NCAN,2070)
              IF (IPR .GE. 1) WRITE (NCAN,2080)  (X0(IW),IW=1,NX)
              IF (IPR .GE. 1) WRITE (NCAN,2090)
#endif
C
C.... B›—ˆC‹ˆM ‡HA—EHˆŸ ”“HK–ˆ‰ B OC‹E„HE‰ HA‰„EHO‰ TO—KE.
C
              IERR = 0
              CALL FUN (X0,FUNS,IERR,ISTOP)
              IF (ISTOP .NE. 0) RETURN
              IF (IERR  .EQ. 0) GOTO 115
#if defined(hib)
                  IF (IPR .GE. 1) WRITE (NCAN,3000)
#endif
 115          CONTINUE
#if defined(hib)
              IF (IPR .GE. 1) WRITE (NCAN,2000)
              IF (IPR .GE. 1) WRITE (NCAN,2080)  (FUNS(IW),IW=1,NFUN)
              IF (IPR .GE. 1) WRITE (NCAN,2010)
#endif
              DO 120 K=1,NX
                 X0(K) = XOLD(K)
 120          CONTINUE
              X0(NG) = X0NEW
              IF (IPOISK .EQ. 0) GOTO 175
C
C.... –ˆK‹ EPEOPA BCEBO‡MO†H›X ƒˆEP‹OCKOCTE‰ C“CKA HA KPˆB“ž.
C
              DO 150 L=1,NX
                 IF (L .EQ. NG) GOTO 140
C
C.... “TO—HEHˆˆ TO—Kˆ B ƒˆEP‹OCKOCTˆ X0(NFIX)=CONSTANTA.
C
                     NFIX = L
                     IT   = 10
#if defined(hib)
                     IF (IPR .GE. 1) WRITE (NCAN,2000)
                     IF (IPR .GE. 1) WRITE (NCAN,2030) X0NEW
                     IF (IPR .GE. 1) WRITE (NCAN,2040) NFIX
                     IF (IPR .GE. 1) WRITE (NCAN,2050) X0(NFIX)
#endif
C
C.... OPA™EHˆE K PO–E„“PE HœžTOHA.
C
                     IERN = 0
                     CALL NEWTN (NX,NDIM,NG,NFIX,NCAN,X0,EPS,EPSN,IT,
     A                           IPR,FUN,DFDX,DFUN,IERN,ISTOP)
                     IF (ISTOP .NE. 0) RETURN
                     IF (IERN  .EQ. 0) GOTO 180
C
C.... TO—KA HA KPˆBO‰ HE HA‰„EHA.
C
#if defined(hib)
                         IF (IPR .GE. 1) WRITE (NCAN,2000)
                         IF (IPR .GE. 1) WRITE (NCAN,2060)
                         IF (IPR .GE. 1) WRITE (NCAN,2070)
                         IF (IPR .GE. 1) WRITE (NCAN,2080)
     A                                         (X0(IW),IW=1,NX)
                         WRITE (NCAN,2090)
#endif
C
C.... B›—ˆC‹ˆM ‡HA—EHˆŸ ”“HK–ˆ‰ B OC‹E„HE‰ HA‰„EHO‰ TO—KE.
C
                         IERR = 0
                         CALL FUN (X0,FUNS,IERR,ISTOP)
                         IF (ISTOP .NE. 0) RETURN
                         IF (IERR  .EQ. 0) GOTO 125
#if defined(hib)
                             IF (IPR .GE. 1) WRITE (NCAN,3000)
#endif
 125                     CONTINUE
#if defined(hib)
                         IF (IPR.GE.1) WRITE (NCAN,2000)
                         IF (IPR.GE.1 .AND. IERR.EQ.0) WRITE (NCAN,2080)
     A                                         (FUNS(IW),IW=1,NFUN)
                         IF (IPR .GE. 1) WRITE (NCAN,2000)
                         IF (IPR .GE. 1) WRITE (NCAN,2010)
#endif
                         DO 130 K=1,NX
                            X0(K) = XOLD(K)
 130                     CONTINUE
                         X0(NG) = X0NEW
 140             CONTINUE
 150          CONTINUE
C
C.... EPBAŸ TO—KA HA KPˆBO‰ HE HA‰„EHA.
C
              IER   = -1
              IPNT1 = 0
              IF (IEND .EQ. 2) RETURN
                  IF (IEND .EQ. 0) IND = -1*IND
 160    CONTINUE
 170  CONTINUE
 175  CONTINUE
      IER   = -1
      IPNT1 = 0
      RETURN
 180  CONTINUE
C
C.... EPBAŸ TO—KA HA KPˆBO‰ “CE˜HO HA‰„EHA.
C
#if defined(hib)
      IF (IPR .GE. 1) WRITE (NCAN,2000)
      IF (IPR .GE. 1) WRITE (NCAN,3010)
      IF (IPR .GE. 1) WRITE (NCAN,2000)
      IF (IPR .GE. 1) WRITE (NCAN,2080)  (X0(IW),IW=1,NX)
      IF (IPR .GE. 1) WRITE (NCAN,2000)
      IF (IPR .GE. 1) WRITE (NCAN,3020)
#endif
      IER   = 0
      IPNT1 = 0
      RETURN
#if defined(hib)
 2000 FORMAT
     A(1X)
C
 2010 FORMAT
     A(1X,120('-'))
C
 2020 FORMAT
     A('   “TO—HŸETCŸ EPBAŸ TO—KA HA KPˆBO‰.'/
     B '   --------------------------------- ')
C
 2030 FORMAT
     A('   ‡HA—EHˆE B›„E‹EHHO‰ EPEMEHHO‰:        ',G14.5)
C
 2040 FORMAT
     A('   HOMEP ”ˆKCˆPOBAHHO‰ EPEMEHHO‰:        ',I6)
C
 2050 FORMAT
     A('   ‡HA—EHˆE ”ˆKCˆPOBAHHO‰ EPEMEHHO‰:     ',G14.5)
C
 2060 FORMAT
     A('   TO—KA HA KPˆBO‰ HE HA‰„EHA.            '/
     B '   ---------------------------            ')
C
 2070 FORMAT
     A('   KOOP„ˆHAT› OC‹E„HE‰ B›—ˆC‹EHHO‰ TO—Kˆ:')
C
 2080 FORMAT
     A(10(2X,7(G14.5,1X)/))
C
 2090 FORMAT
     A('   ‡HA—EHˆŸ ”YHK–ˆ‰ B OC‹E„HE‰ TO—KE:    ')
C
 3000 FORMAT
     A('   ‡HA—EHˆŸ ”YHK–ˆ‰ HE B›—ˆC‹EH›.        ')
C
 3010 FORMAT
     A('   KOOP„ˆHAT› EPBO‰ TO—Kˆ HA KPˆBO‰:    '
     B/'   ----------------------------------    ')
C
 3020 FORMAT
     A(1X,120('='))
#endif
      END
