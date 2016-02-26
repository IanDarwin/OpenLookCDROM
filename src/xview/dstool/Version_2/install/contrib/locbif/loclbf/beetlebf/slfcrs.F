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

      SUBROUTINE SLFCRS (ISTEP,NX,NDIMM,NG,NCAN,ICRS,IPRCRS,STX2,EPS,
     A                   FUN,DFDX,OUT,DFUN,ISTOP)
C     3.05.90É
C     6.08.1991
C     26.08.1991
C     13.11.92
C     15.06.94
C* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C*                                                                     *
C*    èPOÉPAMMA  SLFCRS  èPEÑHAáHAóEHA Ñãü ìTOóHEHàü TOóEK             *
C*    CAMOèEPECEóEHàü KPàBOâ B NX-MEPHOM èPOCTAHCTBE.                  *
C*    TOóKA CAMOèEPECEóEHàü èPOâÑEHA, ECãà OèPEÑEãàTEãú MATPàñõ        *
C*    óACTHõX èPOàáBOÑHõX COOTBETCTBìûôàâ îàKCàPOBAHHOâ èEPEMEHHOâ     *
C*    MEHüET áHAK. HOãú OèEPEÑEãàTEãü ìTOóHüETCü METOÑOM CEKìôàX.      *
C*                                                                     *
C*    BXOÑHõE è A P A M E T P õ:                                       *
C*    --------------------------                                       *
C*    ISTEP -óàCãO òAÉOB, èPOâÑEHõX BÑOãú KPàBOâ.                      *
C*    NX    -óàCãO èEPEMEHHõX (X(1),...,X(NX)).                        *
C*    NDIM  -áAüBãEHHAü CTOPóHAü PAáMEPHOCTú MATPàñõ DFUN(NDIM,NX).    *
C*    NG    -HOMEP BõÑEãEHHOâ èEPEMEHHOâ.                              *
C*    NCAN  -HOMEP KAHAãA Ñãü BõBOÑA èPOTOKOãA ìTOóHEHàü TOóKà CAMOèEP.*
C*    ICRS  -èAPAMETP, ìKAáõBAûôàâ HA HEOÅXOÑàMOCTú ìTOóHEHàü TOóKà    *
C*           CAMOèEPECEóEHàü:                                          *
C*             0 - TOóKA CAMOèEPECEóEHàü HE ìTOóHüETCü;                *
C*             1 - TOóKA CAMOèEPECEóEHàü ìTOóHüETCü.                   *
C*    IPRCRS-èAPAMETP, ìKAáõBAûôàâ CTEèEHú èOÑPOÅHOCTà èPOTOKOãA:      *
C*            -1 - èPOTOKOã OTCìCTBìET;                                *
C*             0 - BõÑAûTCü KOOPÑàHATõ TOóKà CAMOèEPECEóEHàü;          *
C*             1 - BõÑAETCü HEKOTOPAü àHîOPMAñàü OÅ àTEPAñàüX.         *
C*    STX2  -KOOPÑàHATõ èOCãEÑHEâ TOóKà HA KPàBOâ;                     *
C*    EPS   -TOóHOCTú OèPEÑEãEHàü TOóKà HA KPàBOâ B METOÑE HúûTOHA.    *
C*    FUN   -àMü èOÑèPOÉPAMMõ BõóàCãEHàü èPABõX óACTEâ.                *
C*    DFDX  -àMü èOÑèPOÉPAMMõ BõóàCãEHàü MATPàñõ óACTHõX èPOàáBOÑHõX   *
C*           áAÑAHHOâ üBHõM OÅPAáOM.                                   *
C*    DFUN  -ÑBìMEPHõâ MACCàB PAáMEPHOCTà DFUN(NDIM,NX),               *
C*           COÑEPÜAôàâ MATPàñì óACTHõX èPOàáBOÑHõX.                   *
C* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C*   èPà àáMEHEHàà MAKCàMAãúHOÉO óàCãA  NDIM èEPEMEHHõXHEOÅXOÑàMO*     *
C*   àCèPABàTú PAáMEPHOCTà MACCàBOB:                                   *
C*   STX1(*) STX2(*) X1(*) X2(*) XCRS(*) XABD(*) VMOVE(*) 	       *
C*   FNCRS(*) VMOVE3(*)                                                *
C* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER*2 (I-N)
      include 'beetlebf.dim'
      EXTERNAL FUN, DFDX, OUT
      DIMENSION STX1(NDIM), STX2(1), X1(NDIM), X2(NDIM), XCRS(NDIM),
     A          XABD(NDIM), DFUN(NDIM,NX), 
     B          FNCRS(NDIM), VMOVE3(NDIM), INDEX(NPTYP)
      DIMENSION ITNWTZ(10)
C
      COMMON /ADD2B/ EPSCRS
C
      COMMON /OUT3/ EPSCR, EPSR, DETZ, CONDZ
      COMMON /OUT7/ INDC
C
      COMMON /LEQV1/  DETL, CONDL
      COMMON /CORR1/  DET,  COND, SGNDET
C
      COMMON /WRTO1/ NFIX1, NFIX
      COMMON /WRTO2/ DMOVE1, DMOVE
      COMMON /WRTO4/ EPSC, EPSN
      COMMON /WRTO6/ ITC,IT2
      COMMON /WRTO7/ VMOVE(NDIM),VMOVN(NDIM)
      SAVE DETOLD,CONDST,STX1

      ISTOP = 0
      IF (ICRS .EQ. 0) RETURN
      DET2       = DETL
      DET1       = DETOLD
      DETOLD     = DET
      COND2      = CONDL
      COND1      = CONDST
      CONDST     = COND
C
C.... KOHTPOãú à ìTOóHEHàE TOóKà CAMOèEPECEóEHàü.
C
c      WRITE (6,*) ' DET=',DET,'  DETL=',DETL,'  DETOLD=',DETOLD 
         IF (ISTEP .EQ. 0) GOTO 370
            DO 200 I=1,NX
               X1(I)   = STX1(I)
               X2(I)   = STX2(I)
c              STX1(I) = STX2(I)
 200        CONTINUE
            IF (DET1*DET2 .GT. 0.D0) GOTO 360
c                IF (DABS(DET2).LT.0.01*EPSCRS
c     A              .AND.DABS(DET1).LT.0.01*EPSCRS) GOTO 360
                 DF = DABS((DET2-DET1)/(X1(NFIX1)-X2(NFIX1)))
                 IF (DF.LT.0.1*EPSCRS) GOTO 360
c
C
C.... èOÑÉOTOBKA K àTEPAñàüM ìTOóHEHàü TOóKà CAMOèEPECEóEHàü.
C
               DETSTP = 5.D-06
               DO 100 I=1,4
                  INDEX(I) = 0
 100           CONTINUE
               INDC   = 0
               ITCREM = ITC
               ITC    = 5*ITC
               ITMAX  = 10
               EPSC   = EPS
	       EPSN   = 0.D0
               EPSCR  = EPSCRS
	       INDLOC = 0
C
C.... áHAóEHàü DETL,CONDL,DET,COND-èOPTüTCü B èPOñECCE àTEPAñàâ
C     èO HúûTOHì, èOùTOMì àX HEOÅXOÑàMO áAèOMHàTú.
C
               ITNWTZ(1)  = 0
C
C.... ñàKã ìTOóHEHàü TOóKà CAMOèEPECEóEHàü.
C
               DO 340 ITER =1,ITMAX
C
C.... èPOÉHOá TOóKà CAMOèEPECEóEHàü.
C
 205 		  CONTINUE
                  IF (INDLOC .EQ. 0) RX = DET2/(DET2-DET1)
                  IF (INDLOC .EQ. 1) RX = 0.5D0
                  IF (INDLOC .EQ. 2) RX = 4.D0/9.D0
C
                  DO 210 I=1,NX
                     XCRS(I) = X2(I)-(X2(I)-X1(I))*RX
 210              CONTINUE
                  ITEND = ITC
                  IPR   = 0
c      PRINT *,' SLFCRS:'
c      PRINT *,'  ITER=',ITER,'  INDLOC=',INDLOC
c      PRINT *,'   DET1=',DET1,'  DET2=',DET2
c      PRINT *,'   X1=',(X1(J), J=1,NX)
c      PRINT *,'   X2=',(X2(J), J=1,NX)
c      PRINT *,'   XC=',(XCRS(J), J=1,NX)
C
C.... ìTOóHEHàE TOóKà HA KPàBOâ METOÑOM HúûTOHA.
C
                  IER = 0
                  CALL NEWTN (NX,NDIM,NG,NFIX1,NCAN,XCRS,EPSC,EPSN,
     A                        ITEND,IPR,FUN,DFDX,DFUN,IER,ISTOP)
                  IF (ISTOP .NE. 0) RETURN
                  ITNWTZ(ITER) = ITEND
c      PRINT *,'   IER=',IER
c      PRINT *,'   XC=',(XCRS(J), J=1,NX)
                  IF (IER   .EQ. 0) GOTO 280
C
                  INDLOC=INDLOC+1
                  IF (INDLOC .LE. 2) GOTO 205
C
C.... HE COòãàCú HúûTOHOBCKàE àTEPAñàà.
C
 215              CONTINUE  
                     IWRT         = 1 - IER
                     IPRTCL       = 3
                     DO 220 I=1,NX
                        XABD(I) = XCRS(I)
 220                 CONTINUE
C
C.... BõÅEPEM TOóKì HA KPàBOâ, ÅãàÜAâòìû K TOóKE CAMOèEPECEóEHàü.
C
                     IF (DABS(DET1) .GT. DABS(DET2)) GOTO 240
                        DETZ   = DET1
                        CONDZ  = COND1
                        DO 230 I=1,NX
                           XCRS(I) = X1(I)
 230                    CONTINUE
                        GOTO 260
 240                 CONTINUE
                        DETZ   = DET2
                        CONDZ  = COND2
                        DO 250 I=1,NX
                           XCRS(I) = X2(I)
 250                    CONTINUE
 260                 CONTINUE
C
C.... KOHTPOãú ÑOCTàÉHìTOâ TOóHOCTà B TOóKE, ÅãàÜAâòEâ K TOóKE
C     CAMOèEPECEóEHàü.
C
                      EPSR = DABS(X2(NFIX1)-X1(NFIX1))
C
C.... àTEPAñàà áAKOHóEHõ-HE COòãàCú àTEPAñàà HúûTOHA.
C
                     DMOVER= DMOVE
                     DMOVE = DMOVE1
                     NFIXR = NFIX
                     NFIX  = NFIX1
#if defined(hib)
                     CALL WRTCRS (IPRTCL,IWRT,IPRCRS,ITER,ITNWTZ,ITEND,
     A                            NCAN,NX,NDIM,EPSCR,EPSR,DETZ,CONDZ,
     B                            XCRS,XABD,DFUN)
#endif
                     DMOVE = DMOVER
                     NFIX  = NFIXR
                     INDC  =-1
                     GOTO 355
 280              CONTINUE
C
        	R1 = 0.D0
		R2 = 0.D0
		DO 285 I=1,NX
                   R1 = R1+(XCRS(I)-STX1(I))*(STX2(I)-STX1(I))
                   R2 = R2+(XCRS(I)-STX2(I))*(STX2(I)-STX1(I))
 285            CONTINUE
                IF (R1*R2 .GT. 0.) GOTO 215
C
C.... áAèOMHàM HOBOE áHAóEHàE OèPEÑEãàTEãü (DETL).
C
                        IER = 0
                        CALL TANGNT (NX,NDIM,NFIX1,XCRS,FNCRS,VMOVE3,
     A                               FUN,DFDX,DFUN,IER,ISTOP)
                        IF (ISTOP .NE. 0) RETURN
c      PRINT *,'   IER (TANGNT)=',IER
                        IF (IER .NE. 0 .AND. IER .NE. -1) GOTO 215
                  DETZ         = DETL
                  CONDZ        = CONDL
C
C.... BõÅOP HìÜHOÉO àHTEPBAãA C HìãEM  DETZ = 0.
C
                  IF (DETZ*DET1 .LT. 0.D0) GOTO 300
                      DET1   = DETZ
                      COND1  = CONDZ
                      DO 290 I=1,NX
                         X1(I) = XCRS(I)
 290                  CONTINUE
                      GOTO 320
 300              CONTINUE
                      DET2   = DETZ
                      COND2  = CONDZ
                      DO 310 I=1,NX
                         X2(I) = XCRS(I)
 310                  CONTINUE
 320              CONTINUE
C
C.... KOHTPOãú ÑOCTàÉHìTOâ TOóHOCTà.
C
                  EPSR = DABS(X2(NFIX1)-X1(NFIX1))
                  DF = DABS((DET1-DET2)/(X1(NFIX1)-X2(NFIX1)))
                  FN=    DABS (DETZ/DF)
                  IF (EPSR.LE.EPSCR .OR. FN.LE.0.1*EPSCR) GOTO 350
 340           CONTINUE
C
C.... áAÑAHHAü TOóHOCTú HE ÑOCTàÉHìTA.
C
               INDC   =-1
               IPRTCL = 2
               ITER   = ITMAX
               GOTO 352
 350           CONTINUE
C
C.... TOóKA CAMOèEPECEóEHàü ìTOóHEHA C áAÑAHHOâ TOóHOCTúû.
C
               IPRTCL = 1
 352           CONTINUE
#if defined(hib)
c               CALL WRTCRS (IPRTCL,IWRT,IPRCRS,ITER,ITNWTZ,ITEND,NCAN,
c     A                      NX,NDIM,EPSCR,EPSR,DETZ,CONDZ,XCRS,XABD,
c     B                      DFUN)
#endif
 355           CONTINUE
C
C.... OÅPAÅOTKA ìTOóHEHHOâ TOóKà CAMOèEPECEóEHàü èOãúáOBATEãEM.
C
               INDEX(2) = 1
               CALL OUT (ISTEP,INDEX,NX,NDIM,XCRS,VMOVE,DFUN,ISTOP)
               ITC    = ITCREM
               IF (ISTOP .NE. 0) RETURN
 360        CONTINUE
 370     CONTINUE
C
C.... èOÑÉOTOBKA K èEPBOMì òAÉì BÑOãú KPàBOâ.
C
         DO 380 I=1,NX
            STX1(I) = STX2(I)
 380     CONTINUE
         RETURN
         END
