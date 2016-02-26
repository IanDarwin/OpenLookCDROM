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

      SUBROUTINE DETNTV(NK,X,DNTV,IER)
C
C   COMPUTING THE FUNCTION CORRESPONDING NONTRANSVERSAL CASE 
C   FOR NEUTRALITY CURVE (NONTRANSVERSALITY OVER PARAMETER P(IP(1))  )
C        22.02.1990
C        15.11.92
C
      IMPLICIT REAL*8(A-H,O-Z)
      IMPLICIT INTEGER*2(I-N)
      include 'epbf.dim'
      parameter (NDXP=(NFMAX+1)*(NFMAX+1))
      DIMENSION X(NK)
      COMMON/LINBF1/DHF
      COMMON/PPAR/P(NPARMX)
      COMMON/IPPAR/IP(NPARMX)
      COMMON/LINBF/NF,NAP,ILN,NF2,ITP
      COMMON/WORK/ DXP(NDXP)
      EXTERNAL FUNS
      IER=0
      DO 10 I=1,NAP
        K=IP(I)
        P(K)=X(NF+I)
 10   CONTINUE
      NFP=NF+1
      NFP2=NFP**2
      CALL DXP1NT(NFP,NFP2,DHF,X,DXP,FUNS,IER)
        IF (IER.NE.0) RETURN
      CALL DETBF(DXP,NFP,DNTV)
      RETURN
      END
 
