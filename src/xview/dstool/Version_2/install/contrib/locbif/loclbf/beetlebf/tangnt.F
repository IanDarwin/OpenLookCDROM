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

        SUBROUTINE TANGNT(NX,NDIMM,NFIX,XTNG,FTNG,VTNG,FUN,DFDX,DFUN,
     A                    IER,ISTOP)
C       3.05.90ƒ.
C       31.07.1991
C       13.11.92
C       15.06.94
C* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C*                                                                     *
C*    POƒPAMMA  TANGNT  PE„HA‡HA—EHA „‹Ÿ HAXO†„EHˆŸ BEKTOPA          *
C*    E„ˆHˆ—HO‰ „‹ˆH› - VTNG, KACATE‹œHOƒO K KPˆBO‰ : FUN(X)=0         *
C*    B TO—KE XTNG.                                                    *
C*    PE„O‹AƒAETCŸ, —TO B KA—ECTBE APAMETPA HA KPˆBO‰ B›PAHA       *
C*    NFIX-KOMOHEHTA BEKTOPA X: X(NFIX).                              *
C*    KACATE‹œH›‰ BEKTOP OPˆEHTˆP“ETCŸ TAK, —TO› „Bˆ†EHˆE O KACATE‹œ-*
C*    HOM“ BEKTOP“ COOTBETCTBOBA‹O BO‡PACTAHˆž APAMETPA, T.E.         *
C*    BCEƒ„A B›O‹HŸETCŸ HEPABEHCTBO: VTNG(NFIX) GT. 0.D0              *
C*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*
C*                                                                     *
C*    APAMETP›:                                                       *
C*    =========                                                        *
C*    NX   - —ˆC‹O EPEMEHH›X;                                         *
C*    NDIM - ‡AŸB‹EHHAŸ CTPO—HAŸ PA‡MEPHOCTœ MATPˆ–› DFUN;             *
C*    NFIX - HOMEP ”ˆKCˆPOBAHHO‰ EPEMEHHO‰;                           *
C*    XTNG - MACCˆB KOOP„ˆHAT TO—Kˆ HA KPˆBO‰, B KOTOPO‰ B›—ˆC‹ŸETCŸ   *
C*           KACATE‹œH›‰ BEKTOP.                                       *
C*    FTNG - ARRAY FOR FUNCTION VALUES                                 *
C*    VTNG - MACCˆB KOOP„ˆHAT KACATE‹œHOƒO BEKTOPA.                    *
C*    FUN  - ˆMŸ POƒPAMM› B›—ˆC‹EHˆŸ APB›X —ACTE‰ - F(X).            *
C*    DFDX - ˆMŸ POƒPAMM› B›—ˆC‹EHˆŸ MATPˆ–› —ACTH›X POˆ‡BO„H›X.     *
C*    DFUN - „B“MEPH›‰ MACCˆB PA‡MEPHOCTˆ  NDIM*NX, CO„EP†A™ˆ‰         *
C*           MATPˆ–“ EPB›X —ACTH›X POˆ‡BO„H›X;                       *
C*    IER  - APAMETP O˜ˆKˆ:                                          *
C*              0 - KACATE‹œH›‰ BEKTOP “CE˜HO HA‰„EH;                 *
C*             -1 - ‹ˆHE‰HAŸ CˆCTEMA “PABHEHˆ‰, OPE„E‹Ÿž™AŸ           *
C*                  KACATE‹œH›‰ BEKTOP - B›PO†„EHA;                    *
C*             -2 - HE B›—ˆC‹EH› ‡HA—EHˆŸ ”“HK–ˆ‰ B TO—KE  XTNG;       *
C*             -3 - HE B›—ˆC‹EH› POˆ‡BO„H›E B TO—KE       XTNG;       *
C* . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . *
C*                                                                     *
C*    B›‡›BAEM›E POƒPAMM›: DERN, FUN, JACOB.                          *
C* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C*    Pˆ HEOXO„ˆMOCTˆ ˆ‡MEHEHˆŸ MAKCˆMA‹œHOƒO —ˆC‹A NDIM             *
C*    EPEMEHH›X, C‹E„“ET ˆ‡MEHˆTœ PA‡MEPHOCTœ MACCˆBA A(NDIM,NDIM),   *
C*                                                   ARR1(NDIM,NDIM)   *
C* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER*2 (I-N)
      include 'beetlebf.dim'
      EXTERNAL FUN, DFDX
      DIMENSION XTNG(1), FTNG(1), VTNG(1), DFUN(NDIM,NX)
C
      COMMON /WORKBE/ ARR1(NDIM,NDIM), A(NDIM,NDIM)
C
C.... B›—ˆC‹ŸEM ‡HA—EHˆŸ ”“HK–ˆ‰ F(X) B TO—KE XTNG;
C     B›—ˆC‹EHH›E ‡HA—EHˆŸ PA‡ME™AžTCŸ B MACCˆBE  VTNG.
C
      ISTOP = 0
      IER   = 0
      CALL FUN (XTNG,FTNG,IER,ISTOP)
      IF (ISTOP .NE. 0) RETURN
      IF (IER   .EQ. 0) GOTO 102
          IER = -2
          RETURN
 102  CONTINUE
C
C.... B›—ˆC‹ŸEM MATPˆ–“ —ACTH›X POˆ‡BO„H›X.
C
      DO 200 J=1,NX
         DFUN(NX,J) = 0.D0
 200  CONTINUE
      DFUN(NX,NFIX) = 1.D0
      IER = 0
      CALL JACOB (NX,NDIM,XTNG,FUN,DFDX,DFUN,FTNG,IER,ISTOP)
      IF (ISTOP .NE. 0) RETURN
      IF (IER   .EQ. 0) GOTO 215
          IER =-3
          RETURN
 215  CONTINUE
C
C.... ”OPMˆP“EM BEKTOP PAB›X —ACTE‰ (VTNG) ˆ MATPˆ–“ (A).
C
      DO 255 I=1,NX
         VTNG(I) = 0.0D0
         DO 250 J = 1,NX
            A(I,J) = DFUN(I,J)
 250     CONTINUE
 255  CONTINUE
      VTNG(NX)  = 1.D0
C
C.... PE˜AEM CˆCTEM“ ‹ˆHE‰H›X “PABHEHˆ‰  A*Y=VTNG;
C
      IER = 0
      CALL LEQV (A,VTNG,NDIM,NX,IER)
      IF (IER .EQ. 0) GOTO 260
C
C.... MATPˆ–A  DFUN - B›PO†„EHA.
C
          IER = -1
          RETURN
 260  CONTINUE
C
C.... HOPMˆP“EM „‹ˆH“ KACATE‹œHOƒO BEKTOPA  VTNG  HA E„ˆHˆ–“.
C
      DLEN = 0.D0
      DO 360 I = 1,NX
        ELEM = VTNG(I)
        DLEN = DLEN + ELEM*ELEM
 360  CONTINUE
      DLEN = DSQRT(DLEN)
      DO 380 I = 1,NX
        VTNG(I) = VTNG(I)/DLEN
 380  CONTINUE
      IER = 0
      RETURN
      END
