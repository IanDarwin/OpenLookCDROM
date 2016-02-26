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

      SUBROUTINE SOLVE (NDIM,N,A,B,IPVT)
C     3.05.90ƒ.
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER*2 (I-N)
      DIMENSION A(NDIM,N), B(N), IPVT(N)
C
C     PE˜EHˆE ‹ˆHE‰HO‰ CˆCTEM› A*X=B
C     O„POƒPAMM“ HE C‹E„“ET ˆCO‹œ‡OBATœ,
C     EC‹ˆ DECOMP OHAP“†ˆ‹A B›PO†„EHHOCTœ.
C
C     (POƒPAMMA OˆCAHA B KHˆƒE:
C
C           „†.”OPCA‰T, M.MA‹œKOM, K.MO“‹EP
C           "MA˜ˆHH›E METO„› MATEMATˆ—ECKˆX B›—ˆC‹EHˆ‰"
C            M.:MˆP,1980)
C
C     BXO„HAŸ ˆH”OPMA–ˆŸ..
C
C       NDIM=‡AŸB‹EHHAŸ CTPO—HAŸ PA‡MEPHOCTœ MAC-
C       CˆBA, CO„EP†A™EƒO A.
C
C       N=OPŸ„OK MATPˆ–›.
C
C       A=”AKTOPˆ‡OBAHHAŸ MATPˆ–A, O‹“—EHHAŸ ˆ‡
C       DECOMP.
C
C       B=BEKTOP PAB›X —ACTE‰.
C
C       IPVT=BEKTOP BE„“™ˆX ‹EMEHTOB, O‹“—EHH›‰
C       ˆ‡ DECOMP.
C
C     B›XO„HAŸ ˆH”OPMA–ˆŸ..
C
C       B=BEKTOP PE˜EHˆŸ.
C
C
C     PŸMO‰ XO„.
C
      IF (N .EQ. 1) GOTO 50
      NM1 = N - 1
      DO 20 K=1,NM1
         KP1 = K + 1
         M   = IPVT(K)
         T   = B(M)
         B(M)= B(K)
         B(K)= T
         DO 10 I=KP1,N
            B(I) = B(I) + A(I,K)*T
 10      CONTINUE
 20   CONTINUE
C
C     OPATHAŸ O„CTAHOBKA
C
      DO 40 KB=1,NM1
         KM1 = N - KB
         K   = KM1 + 1
         B(K)=B(K)/A(K,K)
         T   = -B(K)
         DO 30 I=1,KM1
            B(I) = B(I) + A(I,K)*T
 30      CONTINUE
 40   CONTINUE
 50   B(1) = B(1)/A(1,1)
      RETURN
      END
