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

      SUBROUTINE FUNTS(N,M,X,YY,GG,FUNS,IER)
C
C   LINEAR TRANSFORMATION OF VARIABLES
C   AND INVERSE TRANSFORMATION OF FUNCTIONS
C         21.02.90
C         15.11.92          
C
      IMPLICIT REAL*8(A-H,O-Z)
      IMPLICIT INTEGER*2(I-N)
      include 'epbf.dim'
      DIMENSION YY(NFMAX,M),GG(NFMAX,M),X(N)
      COMMON/PQ/P(NFMAX,NFMAX),Q(NFMAX,NFMAX)
      COMMON /IERFS/ IERFS,IERDUM
      EXTERNAL FUNS
      IER=0
      DO 3 J=1,M
        DO 2 I=1,N
          R=0.
          DO 1 K=1,N
  1         R=R+Q(I,K)*YY(K,J)
  2     GG(I,J)=R+X(I)
  3   CONTINUE
      DO 4 J=1,M
  4   CALL FUNS(GG(1,J),YY(1,J))
      IER=IERFS
      IF (IER.NE.0) RETURN 
      DO 7 J=1,M
        DO 6 I=1,N
          R=0.
          DO 5 K=1,N
  5       R=R+P(I,K)*YY(K,J)
  6     GG(I,J)=R
  7   CONTINUE
      RETURN
      END
 
