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

        SUBROUTINE DERN (NFUN,NX,NDX,X0,FX0,DF,FUN,IER,ISTOP)
C       3.05.90ƒ.
C       01.09.90
C       13.11.92
C       15.06.94
C* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C*                                                                     *
C*    POƒPAMMA DERN PE„HA‡HA—EHA „‹Ÿ —ˆC‹EHHOƒO „ˆ””EPEH–ˆPOBAHˆŸ    *
C*    BEKTOP-”“HK–ˆˆ F(X) O ‡A„AHHOM“ APƒ“MEHT“.                      *
C*                                                                     *
C*    APAMETP›:                                                       *
C*    =========                                                        *
C*    NFUN - —ˆC‹O KOMOHEHT BEKTOP-”“HK–ˆˆ F=(F(1),...,F(NFUN)).      *
C*    NX   - —ˆC‹O APƒ“MEHTOB BEKTOP-”“HK–ˆˆ:(X(1),...,X(NX)).         *
C*    NDX  - HOMEP APƒ“MEHTA, O KOTOPOM“ B›—ˆC‹ŸTCŸ POˆ‡BO„H›E.     *
C*    X0   - KOOP„ˆHAT› TO—Kˆ, B KOTOPO‰ B›—ˆC‹ŸTCŸ POˆ‡BO„H›E       *
C*           (X0: X0(1),...,X0(NX)).                                   *
C*    FX0  - BEKTOP ‡HA—EHˆ‰ KOMOHEHT BEKTOP-”“HK–ˆˆ B TO—KE X0.      *
C*    DF   - BEKTOP-POˆ‡BO„HAŸ OT BEKTOP-”“HK–ˆˆ.                     *
C*    FUN  - ˆMŸ POƒPAMM› B›—ˆC‹EHˆŸ BEKTOP-”“HK–ˆˆ.                  *
C* . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . *
C*                                                                     *
C*    B›‡›BAEM›E POƒPAMM›: FUN.                                       *
C* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C*    Pˆ ˆ‡MEHEHˆˆ MAKCˆMA‹œHOƒO —ˆC‹A EPEMEHH›X NDIM HEOXO„ˆMO     *
C*    ˆ‡MEHˆTœ PA‡MEPHOCTœ MACCˆBA FX1(NDIM-1).                        *
C* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
        IMPLICIT REAL*8 (A-H,O-Z)
        IMPLICIT INTEGER*2 (I-N)
	include 'beetlebf.dim'
        EXTERNAL FUN
        DIMENSION X0(1), FX0(1), DF(1), FX1(NDIM1)
        COMMON /DERN1/ DX0
C
        ISTOP   = 0
        IER     = 0
        R       = X0(NDX)
        DXSC    = DX0*(1.D0+DABS(X0(NDX)))
        X0(NDX) = X0(NDX) + DXSC
C
        CALL FUN (X0,FX1,IER,ISTOP)
        IF (ISTOP .NE. 0) RETURN
        IF (IER   .NE. 0) RETURN
C
        DO 100 I = 1,NFUN
          DF(I) = (FX1(I) - FX0(I))/DXSC
100     CONTINUE
        X0(NDX) = R
        RETURN
        END
