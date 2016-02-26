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

      SUBROUTINE JACOB (NX,NDIMM,X0,FUN,DFDX,DFUN,BMAS,IER,ISTOP)
C     3.05.90ƒ.
C     13.11.92
C     15.06.94
C* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C*                                                                     *
C*       POƒPAMMA JACOB PE„HA‡HA—EHA „‹Ÿ B›—ˆC‹EHˆŸ MATPˆ–› —ACTH›X  *
C*       POˆ‡BO„H›X. POˆ‡BO„H›E B›—ˆC‹ŸTCŸ —ˆC‹EHHO ˆ‹ˆ             *
C*       AHA‹ˆTˆ—ECKˆ, B ‡ABˆCˆMOCTˆ OT †E‹AHˆŸ O‹œ‡OBATE‹Ÿ.          *
C*                                                                     *
C*       APAMETP›:                                                    *
C*       =========                                                     *
C*                                                                     *
C*  NX    -PA‡MEPHOCTœ POCTPAHCTBA EPEMEHH›X (X(1),...,X(NX)).       *
C*  NDIM  -‡AŸB‹EHHAŸ CTPO—HAŸ PA‡MEPHOCTœ MATPˆ–› DFUN.               *
C*  X0    -KOOP„ˆHAT› TO—Kˆ, B KOTOPO‰ B›—ˆC‹ŸETCŸ ŸKOˆAH.            *
C*  FUN   -ˆMŸ O„POƒPAMM› ”“HK–ˆˆ.                                    *
C*  DFDX  -ˆMŸ POƒPAMM› B›—ˆC‹EHˆŸ MATPˆ–› —ACTH›X POˆ‡BO„H›X.       *
C*  DFUN  -MACCˆB B›—ˆC‹EHH›X —ACTH›X POˆ‡BO„H›X, DFUN=DFUN(NFUN,NX). *
C*  BMAS  -HA BXO„E -MACCˆB ‡HA—EHˆ‰ ”“HK–ˆ‰ B TO—KE X0: BMAS =F(X0).  *
C*  IER   -APAMETP O˜ˆKˆ:  IER = 0- B›—ˆC‹EH› BCE POˆ‡BO„H›E;       *
C*                           IER < 0- ŸKOˆAH HE B›—ˆC‹EH.             *
C* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C*  Pˆ ˆ‡MEHEHˆˆ MAKCˆMA‹œHOƒO —ˆC‹A EPEMEHH›X (NDIM) HEOXO„ˆMO     *
C*  ˆ‡MEHˆTœ PA‡MEPHOCTœ MACCˆBA:  DMAS(NDIM-1).                       *
C* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER*2 (I-N)
      include 'beetlebf.dim'
      EXTERNAL FUN, DFDX
      DIMENSION X0(1), DFUN(NDIM,NX), BMAS(1), DMAS(NDIM1)
      COMMON /DERN1/ VARX
      NFUN  = NX - 1
      IER   = 0
      ISTOP = 0
C
C.... ”OPMˆP“EM MATPˆ–“ EPB›X POˆ‡BO„H›X - DFUN.
C
      IF (VARX .LT. 0) GOTO 130
          DO 120 J = 1,NX
             IER = 0
             CALL DERN (NFUN,NX,J,X0,BMAS,DMAS,FUN,IER,ISTOP)
             IF (ISTOP .NE. 0) RETURN
             IF (IER   .EQ. 0) GOTO 104
                 IER = -4
                 RETURN
 104         CONTINUE
             DO 118 I = 1,NFUN
                DFUN(I,J) = DMAS(I)
 118         CONTINUE
 120      CONTINUE
          RETURN
 130  CONTINUE
      IER = 0
      CALL DFDX (X0,DFUN,NDIM,NX,IER,ISTOP)
      IF (ISTOP .NE. 0) RETURN
      IF (IER   .NE. 0) IER = -4
      RETURN
      END
