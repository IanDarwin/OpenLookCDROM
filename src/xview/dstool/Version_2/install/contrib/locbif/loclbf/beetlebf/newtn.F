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

      SUBROUTINE NEWTN (NX,NDIMM,NG,NFIX,NCAN,X0,EPS,EPSN,IT,IPR,FUN,
     A                  DFDX,DFUN,IER,ISTOP)
C     3.05.90ƒ.
c Rev.29.07.92 
C     18.11.92
C     15.06.94
C* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C*                                                                     *
C*    POƒPAMMA NEWTN PE„HA‡HA—EHA „‹Ÿ  “TO—HEHˆŸ PE˜EHˆŸ CˆCTEM›     *
C*    HE‹ˆHE‰H›X YPABHEHˆ‰  F(X) = 0  METO„OM HœžTOHA, Pˆ ”ˆKCˆPOBAH- *
C*    HOM ‡HA—EHˆˆ EPEMEHHO‰  X(NFIX) = X0(NFIX).                     *
C*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*
C*    APAMETP›:                                                       *
C*    =========                                                        *
C*    NX   - —ˆC‹O EPEMEHH›X;                                         *
C*    NDIM - ‡AŸB‹EHHAŸ CTPO—HAŸ PA‡MEPHOCTœ MATPˆ–› DFUN;             *
C*    NG   - HOMEP B›„E‹EHHO‰ EPEMEHHO‰;                              *
C*    NFIX - HOMEP ”ˆKCˆPOBAHHO‰ EPEMEHHO‰;                           *
C*    X0   - HA BXO„E POƒPAMM› - HA—A‹œHOE Pˆ‹ˆ†EHˆE K PE˜EHˆž;     *
C*           HA B›XO„E - “TO—HEHHOE Pˆ‹ˆ†EHˆE K PE˜EHˆž C ‡A„AHHO‰   *
C*           TO—HOCTœž.                                                *
C*    EPS  - TO—HOCTœ METO„A;                                          *
C*    EPSN - HA B›XO„E PEA‹œHO „OCTˆƒH“TAŸ TO—HOCTœ.                   *
C*    IT   - HA BXO„E POƒPAMM› - MAKCˆMA‹œHO „O“CTˆMOE —ˆC‹O ˆTEPA–ˆ‰;
C*           HA B›XO„E - —ˆC‹O C„E‹AHH›X ˆTEPA–ˆ‰;                     *
C*    IPR  - APAMETP “PAB‹EHˆŸ E—ATœž COO™EHˆŸ-POƒHO‡ O HœžTOH“  *
C*           B›˜E‹ ‡A ƒPAHˆ–“;                                         *
C*    FUN  - ˆMŸ O„POƒPAMM› B›—ˆC‹EHˆŸ PAB›X —ACTE‰;                *
C*    DFDX - ˆMŸ POƒPAMM› B›—ˆC‹EHˆŸ MATPˆ–› —ACTH›X POˆ‡BO„H›X;     *
C*    DFUN - „B“MEPH›‰ MACCˆB PA‡MEPHOCTˆ NDIM*NX, CO„EP†A™ˆ‰ MATPˆ–“  *
C*           EPB›X —ACTH›X POˆ‡BO„H›X.                               *
C*    IER  - APAMETP O˜ˆKˆ:                                          *
C*             0-ˆTEPA–ˆˆ “CE˜HO CO˜‹ˆCœ C TO—HOCTœž EPS;             *
C*            -1-TPE“EMAŸ TO—HOCTœ HE „OCTˆƒH“TA ‡A ITC ˆTEPA–ˆ‰.     *
C*            -2-HA O—EPE„HO‰ ˆTEPA–ˆˆ OPE„E‹ˆTE‹œ ‹ˆHE‰HO‰ CˆCTEM› =0;
C*            -3-HE B›—ˆC‹EH› ‡HA—EHˆŸ ”“HK–ˆ‰ B HœžTOHOBCKOM POƒHO‡E;*
C*            -4-HE B›—ˆC‹EHA MATPˆ–A —ACTH›X POˆ‡BO„H›X;             *
C*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*
C*           BMAS - BCOMOƒATE‹œH›‰ PAO—ˆ‰ MACCˆB.                    *
C*                                                                     *
C*           B›‡›BAEM›E O„POƒPAMM›:                                  *
C*              DERN, FUN, JACOB.                                      *
C* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C*    Pˆ ˆ‡MEHEHˆˆ MAKCˆMA‹œHOƒO —ˆC‹A (NDIM) EPEMEHH›X HEOXO„ˆMO   *
C*    ˆ‡MEHˆTœ PA‡MEPHOCTˆ MACCˆBOB:   A(NDIM,NDIM), BMAS(NDIM),       *
C*                                     ARR1(NDIM,NDIM)                 *
C* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER*2 (I-N)
      include 'beetlebf.dim'
      EXTERNAL FUN, DFDX
      DIMENSION X0(1), DFUN(NDIMM,NX), BMAS(NDIM),DFNORM(NDIM)
C
      COMMON /CNWT1/ IPNT1
C
      COMMON /ADD6A/ XGMN, XGMX
      COMMON /ADD7A/ EPSNWT
      COMMON /ADD7B/ ITNWT
C
      COMMON /ALL3/  IBND
C
      COMMON /WORKBE/ ARR1(NDIM,NDIM), A(NDIM,NDIM)
C
      COMMON /MESSG/ MESSAG
C
      ISTOP = 0
      ITMAX = IT
      IT=0
      NFUN  = NX - 1
      EPSN  = 100.D0*EPSNWT
      DO 99 J=1,NX
         DFUN(NX,J) = 0.D0
 99   CONTINUE
      DFUN(NX,NFIX) = 1.D0
      XREM          = X0(NFIX)

C.... O„PABˆM ”ˆKCˆPOBAHH“ž EPMEHH“ž.
C
c         X0(NFIX) = XREM
C
C.... B›—ˆC‹ŸEM ‡HA—EHˆŸ ”“HK–ˆ‰ FUN B TO—KE X0;
C     B›—ˆC‹EHH›E ‡HA—EHˆŸ PA‡ME™AEM B MACCˆBE BMAS.
C
        IER = 0
C***B
#if defined(hib)
      IF (MESSAG.LE.-6) THEN
      PRINT 1021
 1021 FORMAT(/'    Determining system evaluation starting...')
      ENDIF
#endif
C***E

        CALL FUN (X0,BMAS,IER,ISTOP)

C***B
#if defined(hib)
      IF (MESSAG.LE.-6) THEN
      PRINT 1022,IER,ISTOP
 1022 FORMAT(/'    Determining system evaluation completed with',
     *           ' IER=',I3,'  ISTOP=',I3)
      PRINT 1023,(BMAS(I),I=1,NX-1)
 1023 FORMAT('     Function values (BMAS)=',
     *     /('         ',3G23.15)) 
      ENDIF
#endif
C***E
        IF (ISTOP .NE. 0) RETURN
        IF (IER .NE. 0) THEN
            IER = -3
            RETURN
        ENDIF

      ANORMF=0.D0
      DO I=1,NX-1
        IF (ANORMF.LT.DABS(BMAS(I))) ANORMF=DABS(BMAS(I))
      ENDDO

        CALL SHOWIT(IT,EPSN,ANORMF)

      IF (IPNT1.EQ.0 .AND. ANORMF.GT.100.D0) THEN
            IER= -7
C***B
#if defined(hib)
      IF (MESSAG.LE.-6) THEN
      PRINT 1025
 1025 FORMAT(/'    Function values too large: IER=-7')
      ENDIF
#endif
C***E
            RETURN
      ENDIF


C
C.... OPƒAHˆ‡“EM –ˆK‹ O MAKCˆMA‹œHOM“ —ˆC‹“ „O“CTˆM›X ˆTEPA–ˆ‰.
C
      DO 440 IT = 1,ITMAX
C***B
#if defined(hib)
      IF (MESSAG.LE.-6) THEN
      PRINT 1011,IT
 1011 FORMAT(//'    Corrector iteration ',I3)
      PRINT 1012,(X0(I),I=1,NX)
 1012 FORMAT(  '    Starting point (X0)=',
     *     /(  '         ',3G23.15)) 
      ENDIF
#endif
C***E
C
      IF (IPNT1 .EQ. 1)                      GOTO 101
      IF (IT.GT.ITNWT) GOTO 105
 101  CONTINUE
C
C.... B›—ˆC‹ŸEM MATPˆ–“ POˆ‡BO„H›X DFUN.
C
      IER = 0
C***B
#if defined(hib)
      IF (MESSAG.LE.-6) THEN
      PRINT 1031
 1031 FORMAT(/'    Jacobian evaluation starting...')
      ENDIF
#endif
C***E

      CALL JACOB (NX,NDIMM,X0,FUN,DFDX,DFUN,BMAS,IER,ISTOP)

C***B
#if defined(hib)
      IF (MESSAG.LE.-6) THEN
      PRINT 1032,IER,ISTOP
 1032 FORMAT(/'    Jacobian evaluation completed with',
     *           ' IER=',I3,'  ISTOP=',I3)
      PRINT 1033, ((DFUN(I,J),J=1,NX),I=1,NX-1)
 1033 FORMAT('     Jacobian (DFUN)=',
     *     /('         ',3G23.15)) 
      ENDIF
#endif
C***E
      IF (ISTOP .NE. 0) RETURN
      IF (IER   .EQ. 0) GOTO 105
          IER = -4
          RETURN
 105  CONTINUE
C
C.... ”OPMˆP“EM BEKTOP-CTO‹E– (BMAS) ˆ MATPˆ–“ (A).
C
         BMAS(NX)  = 0.D0
         DO 120 I=1,NX
            BMAS(I) =-BMAS(I)
	    DFNORM(I)=0.D0
            DO 110 J=1,NX
               A(I,J)=DFUN(I,J)
	       DFNORM(I)=DFNORM(I)+DABS(DFUN(I,J))
 110        CONTINUE
 120     CONTINUE
C
C.... PE˜AEM CˆCTEM“ ‹ˆHE‰H›X “PABHEHˆ‰  A*Y = BMAS;
C     PE˜EHˆE Y PA‡ME™AETCŸ B MACCˆBE BMAS.
C
        IER = 0
C***B
#if defined(hib)
      IF (MESSAG.LE.-6) THEN
      PRINT 1041
 1041 FORMAT(/'    Solving linear system...')
      ENDIF
#endif
C***E
        CALL LEQV (A,BMAS,NDIMM,NX,IER)
C***B
#if defined(hib)
      IF (MESSAG.LE.-6) THEN
      PRINT 1042,IER
 1042 FORMAT(/'    Solving completed with IER=',I3)
      PRINT 1043,(BMAS(I),I=1,NX)
 1043 FORMAT('     Solution vector=',
     *     /('         ',3G23.15)) 
      ENDIF
#endif
C***E
        IF (IER .EQ. 0) GOTO 260
C
C.... MATPˆ–A A - B›PO†„EHA.
C
           IER =-2
           RETURN
 260    CONTINUE
C
C.... HAXO„ˆM O—EPE„HOE Pˆ‹ˆ†EHˆE.
C
        EPSN = 0.D0
        DO 320 I = 1,NX
           RR=X0(I)
           X0(I) = RR + BMAS(I)
           RR1   = DABS(BMAS(I))/(1.D0+DABS(RR))
	   IF (DABS(X0(I)).LT.1.D-5*RR1) X0(I)=0.D0
           IF(EPSN.LT.RR1) EPSN = RR1
 320    CONTINUE
C***B
#if defined(hib)
      IF (MESSAG.LE.-6) THEN
      PRINT 1051
 1051 FORMAT(/'    Test of convergence:')
      PRINT 1052,(X0(I),I=1,NX)
 1052 FORMAT('     New point (X0)=',
     *     /('         ',3G23.15)) 
      PRINT 1053,EPSN
 1053 FORMAT('     Accuracy achieved (EPSN)=',G11.4)
      ENDIF
#endif
C***E
        IF (IBND .EQ. 0) GOTO 400
C
C.... COO™EHˆE:POƒHO‡ O HœžTOH“ B›˜E‹ ‡A „O“CTˆM›E ƒPAHˆ–›.
C
        IWRT = 11
        IF (X0(NG) .LT. XGMN .OR. X0(NG) .GT. XGMX)
     A      CALL WRTOUT (IWRT,ISTEP,IPR,IT,IDIV,NCAN,NX,NDIMM,X0,DFUN)
C
C.... “CE—EHˆE HœžTOHOBCKOƒO POƒHO‡A „O ƒˆEP‹OCKOCTˆ
C     X(NG) = XGMN (ˆ‹ˆ X(NG) = XGMX)
C
        IF (X0(NG) .LT. XGMN) X0(NG) = XGMN
        IF (X0(NG) .GT. XGMX) X0(NG) = XGMX
C
C.... POBEPŸEM O—EPE„HOE Pˆ‹ˆ†EHˆE HA TO—HOCTœ.
C
 400  CONTINUE
C
C.... O„PABˆM ”ˆKCˆPOBAHH“ž EPMEHH“ž.
C
         X0(NFIX) = XREM
C
C.... B›—ˆC‹ŸEM ‡HA—EHˆŸ ”“HK–ˆ‰ FUN B TO—KE X0;
C     B›—ˆC‹EHH›E ‡HA—EHˆŸ PA‡ME™AEM B MACCˆBE BMAS.
C
        IER = 0
C***B
#if defined(hib)
      IF (MESSAG.LE.-6) THEN
      PRINT 1021
c 1021 FORMAT(/'    Determining system evaluation starting...')
      ENDIF
#endif
C***E

        CALL FUN (X0,BMAS,IER,ISTOP)

C***B
#if defined(hib)
      IF (MESSAG.LE.-6) THEN
      PRINT 1022,IER,ISTOP
c 1022 FORMAT(/'    Determining system evaluation completed with',
c     *           ' IER=',I3,'  ISTOP=',I3)
      PRINT 1023,(BMAS(I),I=1,NX-1)
c 1023 FORMAT('     Function values (BMAS)=',
c     *     /('         ',3G23.15)) 
      ENDIF
#endif
C***E
        IF (ISTOP .NE. 0) RETURN
        IF (IER .NE. 0) THEN
            IER = -3
            RETURN
        ENDIF

      ANORMF=0.D0
      ANRMF1=0.D0
      DO I=1,NX-1
         R1=DABS(BMAS(I)/(1.D0+DFNORM(I)))
         IF (ANORMF.LT.DABS(BMAS(I))) ANORMF=DABS(BMAS(I))
         IF (ANRMF1.LT.R1) ANRMF1=R1
      ENDDO
C***B
#if defined(hib)
      IF (MESSAG.LE.-6) THEN
      PRINT 1081,ANORMF,ANRMF1
 1081 FORMAT(/'    Function norms: ANORMF=',G11.4,'  ANRMF1=',G11.4)
      ENDIF
#endif
C***E

        CALL SHOWIT(IT,EPSN,ANORMF)

      IF (IPNT1.EQ.0 .AND. ANORMF.GT.100.D0) THEN
            IER= -7
C***B
#if defined(hib)
      IF (MESSAG.LE.-6) THEN
      PRINT 1025
c 1025 FORMAT(/'    Function values too large: IER=-7')
      ENDIF
#endif
C***E
            RETURN
      ENDIF



C
        IF (EPSN .GT. EPS .OR. ANORMF.GT.EPS) GOTO 420
            IER = 0
            RETURN
 420    CONTINUE
        IF (IPNT1.EQ.0 .AND. EPSN.GT.10.D0) THEN
            IER= -5
C***B
#if defined(hib)
      IF (MESSAG.LE.-6) THEN
      PRINT 1061
 1061 FORMAT(/'    Correction too large: IER=-5')
      ENDIF
#endif
C***E
            RETURN
        ENDIF
        IF (IT.GT.2 .AND. EPSN.GT. 10.0D0*EPSNOL) THEN
            IER= -6
C***B
#if defined(hib)
      IF (MESSAG.LE.-6) THEN
      PRINT 1071
 1071 FORMAT(/'    No progress in convergence: IER=-6')
      ENDIF
#endif
C***E
            RETURN
        ENDIF
        EPSNOL=EPSN
 440  CONTINUE
      IT  = ITMAX
      IER =-1
      RETURN
      END
