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

      SUBROUTINE TRFLD(NK,X,DTRAN,IER)
C
C   COMPUTING THE FUNCTION CORRESPONDING NONTRANSVERSAL CASE 
C   FOR FOLD CURVE (NONTRANSVERSALITY OVER PARAMETER P(IP(1))  )
C         ITERATED MAPS
C        22.02.1990
C        31.08.1990
C        03.12.1991
C        16.11.92
C
      IMPLICIT REAL*8(A-H,O-Z)
      IMPLICIT INTEGER*2(I-N)
      include 'linfpbf.dim'
      parameter (NDXP=(NFMAX+1)*(NFMAX+1))
      DIMENSION X(NK)
      COMMON/LINBF1/DHF
      COMMON/PPAR/P(NPARMX)
      COMMON/IPPAR/IP(NPARMX)
      COMMON/LINBF/NF,NAP,ILN,NF2,ITP
      COMMON/WORK/ DXP(NDXP)
      EXTERNAL FUNM
      IER=0
      DO 10 I=1,NAP
        K=IP(I)
        P(K)=X(NF+I)
 10   CONTINUE
      NFP=NF+1
      NFP2=NFP**2
      CALL DP1FLD(NFP,NFP2,-DHF,X,DXP,FUNM,IER)
        IF (IER.NE.0) RETURN
      L=1
      DO 15 I=1,NF
         DXP(L)=DXP(L)-1.D0
         L=L+NFP+1
 15   CONTINUE    
      R1=0.D0
      DO 20 I=1,NFP
         R1=R1+DXP(NFP*I)**2
 20   CONTINUE
      R1=DSQRT(R1)
      IF (R1.EQ.0) GO TO 40      
      DO 30 I=1,NFP
         DXP(NFP*I)=DXP(NFP*I)/R1
 30   CONTINUE
 40   CONTINUE
      CALL DETBF(DXP,NFP,DTRAN)
      RETURN
      END
 
