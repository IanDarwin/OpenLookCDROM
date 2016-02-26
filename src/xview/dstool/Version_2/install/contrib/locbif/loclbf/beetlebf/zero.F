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

      SUBROUTINE ZERO (ISTEP,NX,NDIMM,NG,NCAN,NUF,IFUNS,IPRZ,STX2,EPS,
     A                 FUN,DFDX,OUT,DFUN,ISTOP)
C     3.05.90ƒ.
C     7.08.1991
C     13.11.92
C     15.06.94
C* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C*                                                                     *
C*    POƒPAMMA  ZERO  PE„HA‡HA—EHA „‹Ÿ “TO—HEHˆŸ KOOP„ˆHAT H“‹E‰     *
C*    ‡A„AHH›X ”“HK–ˆ‰.                                                *
C*                                                                     *
C*     A P A M E T P ›:                                               *
C*    =================                                                *
C*                                                                     *
C*    ISTEP -—ˆC‹O ˜AƒOB, PO„E‹AHH›X B„O‹œ KPˆBO‰.                    *
C*    NX    -PA‡MEPHOCTœ POCTPAHCTBA EPMEHH›X.                       *
C*    NDIM  -‡AŸB‹EHHAŸ CTPO—HAŸ PA‡MEPHOCTœ MATPˆ–› DFUN.             *
C*    NG    -HOMEP ƒ‹ABHO‰ EPEMEHHO‰.                                 *
C*    NCAN  -HOMEP KAHA‹A „‹Ÿ B›BO„A PE‡“‹œTATOB.                      *
C*    IPRZ  -APAMETP, OPE„E‹Ÿž™ˆ‰ O‹HOT“ POTOKO‹A.                 *
C*    NUF   -—ˆC‹O ”“HK–ˆ‰, ˆCC‹E„“EM›X HA KPˆBO‰.                     *
C*    IFUNS -MACCˆB HOMEPOB ˆCC‹E„“EM›X ”“HK–ˆ‰.                       *
C*    STX2  -KOOP„ˆHAT› TEK“™E‰ TO—Kˆ HA KPˆBO‰.                       *
C*    EPS   -TO—HOCTœ OPE„E‹EHˆŸ TO—Kˆ HA KPˆBO‰ B METO„E HœžTOHA.    *
C*    FUN   -ˆMŸ O„POƒPAMM› B›—ˆC‹EHˆŸ OPE„E‹Ÿž™ˆX ”“HK–ˆ‰.         *
C*    DFDX  -ˆMŸ O„POƒPAMM› B›—ˆC‹EHˆŸ ŸKOˆAHA OPE„E‹Ÿž™ˆX ”“HK–ˆ‰.*
C*    DFUN  -MACCˆB, CO„EP†A™ˆ‰ ŸKOˆAH OPE„E‹Ÿž™ˆX ”“HK–ˆ‰.          *
C* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C*    Pˆ ˆ‡MEHEHˆˆ MAKCˆMA‹œHOƒO —ˆC‹A EPEMEHH›X  NDIM -HEOXO„ˆMO   *
C*    ˆ‡MEHˆTœ PA‡MEPHOCTˆ MACCˆBOB:                                   *
C*    STX1(*) STX2(*) X1(*) X2(*) XZERO(*) VMOVE(*)                    *
C* . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . *
C*    Pˆ ˆ‡MEHEHˆˆ MAKCˆMA‹œHOƒO —ˆC‹A ”“HK–ˆ‰, ˆCC‹E„“EM›X HA KPˆBO‰.*
C*    HEOXO„ˆMO ˆ‡MEHˆTœ PA‡MEPHOCTˆ MACCˆBOB:                        *
C*    FUNS1(*) IFUNS(*) IFUNSA(*) IERF(*) FUNS2(*).                    *
C* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER*2(I-N)
      include 'beetlebf.dim'
      CHARACTER*56 TEXT
      DIMENSION STX1(NDIM), STX2(NDIM), X1(NDIM), X2(NDIM),
     A          XZERO(NDIM), FUNS1(IFMAX), IFUNS(IFMAX),
     B          DFUN(NDIM,NX), INDEX(NPTYP)
      EXTERNAL FUN, DFDX, OUT
C
      COMMON /ADD3B/ EPSZER
C
      COMMON /OUT1/ FUNS2(IFMAX)
      COMMON /OUT2/ INDZ, IERF(IFMAX)
      COMMON /OUT4/ EPSZ, EPSR, FZERO
C
      COMMON /ZERO2/ IFUNSA(IFMAX)
      COMMON /WRTO1/ NFIX1, NFIX
      COMMON /WRTO2/ DMOVE1, DMOVE
      COMMON /WRTO4/ EPSC, EPSN
      COMMON /WRTO6/ ITC,IT2
      COMMON /WRTO7/ VMOVE(NDIM),VMOVN(NDIM)
      SAVE STX1,FUNS1,/OUT1/,/OUT2/,/OUT4/
C
      ISTOP = 0
      IF (NUF .EQ. 0) RETURN
          DO 100 I=1,NPTYP
             INDEX(I) = 0
 100      CONTINUE
          EPSC  = EPS
C
C.... B›—ˆC‹EHˆE ‡HA—EHˆ‰ BCEX ”“HK–ˆ‰ B TEK“™E‰ TO—KE HA KPˆBO‰.
C
          DO 200 IFUN=1,NUF
            INDF        = IFUNSA (IFUN)
            IER         = 0
            FUNS2(INDF) = FUNALL (INDF,NX,STX2,FUN,DFDX,IER,ISTOP)
            IF (ISTOP .NE. 0) RETURN
            IF (IER   .EQ. 0) GOTO 170
C
C.... HE B›—ˆC‹EHO ‡HA—EHˆE ”“HK–ˆˆ B TEK“™E‰ TO—KE HA KPˆBO‰.
C
                IF (ISTEP .EQ. 0) FUNS1(INDF) = -666.D+33
                                  FUNS2(INDF) = -666.D+33
                IERF(INDF) = IER
                GOTO 190
 170        CONTINUE
            IF (IERF(INDF) .EQ. 0) GOTO 180
C
C.... HE B›—ˆC‹EH› ‡HA—EHˆŸ ”“HK–ˆˆ B PE„›„“™ˆX TO—KAX HA KPˆBO‰;
C     B›—ˆC‹EHHOE ‡HA—EHˆE ”“HK–ˆˆ B TEK“™E‰ TO—KE HE POBEPŸETCŸ.
C
                FUNS1(INDF) = FUNS2(INDF)
                IERF(INDF)  = 0
 180        CONTINUE
 190        CONTINUE
 200      CONTINUE
C
C.... B›BO„ POTOKO‹A PAC—ETA ”“HK–ˆ‰.
C
          IPRTCL = 3
#if defined(hib)
          CALL WRTZER (IPRTCL,INDF,IWRT,ISTEP,IPRZ,IT,IDIV,NCAN,NX,
     A                 NDIM,ITER,NUF,IFUNS,EPSR,EPSZ,FUNS2,
     B                 X2,FX,DFUN)
#endif
C
C.... OPAOTKA O‹œ‡OBATE‹EM TEK“™E‰ TO—Kˆ HA KPˆBO‰ ˆ
C     B›—ˆC‹EHH›X ‡HA—EHˆ‰ ”“HK–ˆ‰.
C
          INDEX(1) = 1
          CALL OUT (ISTEP,INDEX,NX,NDIM,STX2,VMOVE,DFUN,ISTOP)
          IF (ISTOP .NE. 0) RETURN
          INDEX(1) = 0
          IF (ISTEP .EQ. 0) GOTO 390
              ITMAX = 10
              EPSZ  = EPSZER
C
C.... –ˆK‹ POBEPKˆ HA CMEH“ ‡HAKA BCEX B›—ˆC‹EHH›X ‡HA—EHˆ‰.
C
              DO 380 IFUN=1,NUF
                 INDZ  = 0
                 INDF = IFUNSA(IFUN)
C
C.... POBEPKA “C‹OBˆ‰:
C     1). IFUNS(IFUN) < 0 -H“‹œ HE “TO—HŸETCŸ;
C     2). IERF (INDF) .NE. 0 -‡HA—EHˆE ”“HK–ˆˆ HE B›—ˆC‹EHO;
C
                 IF (IFUNS(IFUN) .LT. 0) GOTO 370
                 IF (IERF (INDF) .NE. 0) GOTO 370
C
C.... O„ƒOTOBKA PAO—ˆX MACCˆBOB.
C
                 DO 210 I=1,NX
                    X1(I) = STX1(I)
                    X2(I) = STX2(I)
 210             CONTINUE
C
C.... POBEPKA “C‹OBˆŸ CMEH› ‡HAKA.
C
                 F1 = FUNS1(INDF)
                 F2 = FUNS2(INDF)
                 IF (F1*F2 .GT. 0.D0) GOTO 370
                 DF = DABS((F1-F2)/(X1(NFIX1)-X2(NFIX1)))
                 IF (DF.LT.0.1*EPSZ) GOTO 370
		 INDLOC = 0
C
C.... –ˆK‹ “TO—HEHˆŸ KOOP„ˆHAT H“‹Ÿ ”“HK–ˆˆ.
C
                     DO 300 ITER =1,ITMAX
C
C.... POƒHO‡ KOOP„ˆHAT H“‹Ÿ ”“HK–ˆˆ.
C
 215              CONTINUE
                  IF (INDLOC .EQ. 0) RX = F2/(F2-F1)
                  IF (INDLOC .EQ. 1) RX = 0.5D0
                  IF (INDLOC .EQ. 2) RX = 4.D0/9.D0
C
                        DO 220 I=1,NX
                           XZERO(I) = X2(I)-(X2(I)-X1(I))*RX
 220                    CONTINUE
                        IT  = ITC
                        IPR = 0
                        IER = 0
c          WRITE (TEXT,1001) ITER
c          WRITE (6,1001) ITER
c 1001     FORMAT(
c     *   '  ZERO: Iter=',I2,41X)
C         CALL Showte(TEXT)
                        CALL NEWTN (NX,NDIM,NG,NFIX1,NCAN,XZERO,EPS,
     A                            EPSN,IT,IPR,FUN,DFDX,DFUN,IER,ISTOP)
                        IF (ISTOP .NE. 0) RETURN
                        IF (IER   .EQ. 0) GOTO 230
                        INDLOC=INDLOC+1
                        IF (INDLOC .LE. 2) GOTO 215
C
C.... HE CO˜‹ˆCœ HœžTOHOBCKˆE ˆTEPA–ˆˆ.
C
 225                    CONTINUE
                            IPRTCL = 2
                            IWRT   = 1 - IER
                            GOTO 320
 230                    CONTINUE
        	R1 = 0.D0
		R2 = 0.D0
		DO 235 I=1,NX
                   R1 = R1+(XZERO(I)-STX1(I))*(STX2(I)-STX1(I))
                   R2 = R2+(XZERO(I)-STX2(I))*(STX2(I)-STX1(I))
 235            CONTINUE
                IF (R1*R2 .GT. 0.) GOTO 225
                        IER   = 0
                        FZERO = FUNALL(INDF,NX,XZERO,FUN,DFDX,IER,ISTOP)
c          WRITE (TEXT,1002) FZERO
c          WRITE (6,1002) FZERO
c 1002     FORMAT(
c     *   '  ZERO: Fzero=',G9.2,33X)
C         CALL Showte(TEXT)
                        IF (ISTOP .NE. 0) RETURN
                        IF (IER   .EQ. 0) GOTO 240
                        INDLOC=INDLOC+1
                        IF (INDLOC .LE. 2) GOTO 215
C
C.... HE B›—ˆC‹EH› ‡HA—EHˆŸ ”“HK–ˆˆ HA KPˆBO‰.
C
                            IPRTCL = 2
                            IWRT   = 0
                            GOTO 320
 240                    CONTINUE
C
C.... B›OP ˆHTEPBA‹A, CO„EP†A™EƒO H“‹œ ”“HK–ˆˆ.
C
                            IF (F1*FZERO .LT. 0.D0) GOTO 260
                                F1 = FZERO
                                DO 250 I=1,NX
                                   X1(I) = XZERO(I)
 250                            CONTINUE
                                GOTO 280
 260                        CONTINUE
                                DO 270 I=1,NX
                                   X2(I) = XZERO(I)
 270                            CONTINUE
                                F2 = FZERO
 280                         CONTINUE
C
C.... KOHTPO‹œ „OCTˆƒH“TO‰ TO—HOCTˆ.
C
                       EPSR = DABS(X2(NFIX1)-X1(NFIX1))
                       DF = DABS((F1-F2)/(X1(NFIX1)-X2(NFIX1)))
                       FN=    DABS (FZERO/DF)
                       IF (EPSR.LE.EPSZ .OR. FN.LE.0.1*EPSZ) GOTO 310
 300                 CONTINUE
                     ITER = ITMAX
                     INDZ = -1
 310                 CONTINUE
C
C.... B›„A—A POTOKO‹A: HA‰„EHO Pˆ‹ˆ†EHˆE K H“‹ž ”“HK–ˆˆ.
C
                     IPRTCL = 1
c                     CALL WRTZER (IPRTCL,INDF,IWRT,ISTEP,IPRZ,IT,IDIV,
c     A                            NCAN,NX,NDIM,ITER,NUF,IFUNS,
c     B                            EPSR,EPSZ,XZERO,X2,FZERO,DFUN)
                     INDEX(3) = INDF
                     CALL OUT (ISTEP,INDEX,NX,NDIM,XZERO,VMOVE,
     A                         DFUN,ISTOP)
                     IF (ISTOP .NE. 0) RETURN
                     GOTO 380
 320                 CONTINUE
C
C.... B›„A—A POTOKO‹A: HE CO˜‹ˆCœ HœžTOHOBCKˆE ˆTEPA–ˆˆ ˆ‹ˆ
C                       HE B›—ˆC‹EHO ‡HA—EHˆE ”“HK–ˆˆ.
C
                     EPSR = DABS(X2(NFIX1)-X1(NFIX1))
C
                     FZERO = F1
                     IF (DABS(F1) .LE. DABS(F2)) GOTO 340
                         FZERO = F2
                         DO 330 I=1,NX
                            X1(I) = X2(I)
 330                     CONTINUE
 340                 CONTINUE
                     DMOVER= DMOVE
                     DMOVE = DMOVE1
                     NFIXR = NFIX
                     NFIX  = NFIX1
#if defined(hib)
                     CALL WRTZER (IPRTCL,INDF,IWRT,ISTEP,IPRZ,IT,IDIV,
     A                            NCAN,NX,NDIM,ITER,NUF,IFUNS,
     B                            EPSR,EPSZ,X1,XZERO,FZERO,DFUN)
#endif
                     DMOVE = DMOVER
                     NFIX  = NFIXR
C
C.... „OO‹HˆTE‹œHAŸ OPAOTKA B›—ˆC‹EHH›X ‡HA—EHˆ‰ ”“HK–ˆ‰.
C
                     INDZ     =-1
                     INDEX(3) = INDF
                     CALL OUT (ISTEP,INDEX,NX,NDIM,X1,VMOVE,
     A                         DFUN,ISTOP)
                     IF (ISTOP .NE. 0) RETURN
 370             CONTINUE
 380          CONTINUE
 390      CONTINUE
C
C.... O„ƒOTOBKA K O—EPE„HOM“ ˜Aƒ“ B„O‹œ KPˆBO‰.
C
          DO 400 I=1,NX
            STX1(I) = STX2(I)
 400      CONTINUE
          DO 410 IFUN = 1,NUF
            INDF        =IFUNSA(IFUN)
            FUNS1(INDF) = FUNS2(INDF)
 410      CONTINUE
          RETURN
          END
