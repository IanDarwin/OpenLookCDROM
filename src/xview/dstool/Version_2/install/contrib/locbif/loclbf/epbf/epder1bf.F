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

      SUBROUTINE epDER1BF(NK,DH,X,DER,FUNS,IER)
C
C   COMPUTING OR RESTORING JACOBIAN MATRIX
C           21.02.90
C           15.11.92
C           10.07.94
C
      IMPLICIT REAL*8(A-H,O-Z)
      IMPLICIT INTEGER*2(I-N)
      include 'epbf.dim'
      DIMENSION DER(1),X(NK)
      COMMON/PPAR/P(NPARMX)
      COMMON/IPPAR/IP(NPARMX)
      COMMON/LINBF/NF,NAP,ILN,NF2,ITP
      COMMON/LINBFM/DHOLD,DEROLD(NFMAX2),XOLD(NDIM)
      COMMON/LINBFN/IOLD
      EXTERNAL FUNS
      IER=0
      NF2=NF*NF
      IF (IOLD.EQ.0) GO TO 30
      IF (DHOLD.NE.DH) GO TO 30
      DO 10 I=1,NK
        IF (X(I).NE.XOLD(I)) GO TO 30
 10   CONTINUE
      DO 20 I=1,NF2
        DER(I)=DEROLD(I)
 20   CONTINUE
      GO TO 99
 30   IOLD=0
      DO 40 I=1,NAP
        K=IP(I)
        P(K)=X(NF+I)
 40   CONTINUE
      CALL DER1L(NF,NF2,DH,X,DER,FUNS,IER)
      IF (IER.NE.0) GO TO 99
      DHOLD=DH
      DO 50 I=1,NK
        XOLD(I)=X(I)
 50   CONTINUE
      DO 60 I=1,NF2
        DEROLD(I)=DER(I)
 60   CONTINUE
      IOLD=1
 99   CONTINUE
      RETURN
      END
 
