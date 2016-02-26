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

      SUBROUTINE SECANT(NK,X,IER)
C
C         23.05.1991
C         14.11.92
C
      IMPLICIT REAL*8(A-H,O-Z)
      IMPLICIT INTEGER*2(I-N)
      include 'linbf.dim'
      parameter (NFMAX1=NFMAX+1)
      DIMENSION X(NK)
      COMMON/SECPL/ PL(NFMAX1)
      COMMON/PPAR/P(NPARMX)
      COMMON/IPPAR/IP(NPARMX)
      COMMON/LINBF/NF,NAP,ILN,NF2,ITP
      COMMON /IERFS/ IERFS,IERDUM
      IER=0
      DO  I=1,NAP
        K=IP(I)
        P(K)=X(NF+I)
      ENDDO
      CALL FUNS(X,PL)
      IF (IERFS.NE.0) THEN
         IER=-1
      ELSE
         RR=0.D0
         DO I=1,NF
            RR=RR+PL(I)*X(I)
         ENDDO
         PL(NF+1)=-RR
      ENDIF
      RETURN
      END

