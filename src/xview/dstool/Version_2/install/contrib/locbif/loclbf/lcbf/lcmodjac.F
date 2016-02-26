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

      SUBROUTINE LCMODJAC(NK,X,D,IER)
C
C   MODIFICATION OF THE JACOBY MATRIX TO STUDY LIMIT CYCLE BIFURCATIONS
C        20.02.1991
C        16.11.92
C        18.10.93
C        08.07.94
C
C   FORMAL PARAMETERS:
C    NK     - DIMENSION OF THE CONTINUATION PROBLEM
C    X      - VECTOR OF THE STATE VARIABLES (LENGTH NK)
C    D      - REAL ARRAY OF THE LENGTH NF*NF 
C    IER    - ERROR CONDITION
C                 0 - NORMAL VALUE
C
      IMPLICIT REAL*8(A-H,O-Z)
      IMPLICIT INTEGER*2(I-N)
      include 'lcbf.dim'
      DIMENSION X(NK),D(1)
      DIMENSION F(NFMAX),HF(NFMAX)
      COMMON/PPAR/P(NPARMX)
      COMMON/IPPAR/IP(NPARMX)
      COMMON/LINBF/NF,NAP,ILN,NF2,ITP
      COMMON/IERFS/ IERFS,IERDUM
      COMMON/FINPNT/ H(NFMAX)
      IER=0
      DO 10 I=1,NAP
        K=IP(I)
        P(K)=X(NF+I)
 10   CONTINUE
      CALL FUNS(X,F)
c      WRITE (*,*) ' x=',X, ' f =',F,' ier=',IERFS
      IER=IERFS
      IF (IER.NE.0) RETURN
      CALL FUNS(H,HF)
c      WRITE (*,*) ' h=',H, ' hf=',HF,' ier=',IERFS
      IER=IERFS
      IF (IER.NE.0) RETURN
C
      R1=0.D0
      DO I=1,NF
        R1=R1+F(I)*HF(I)
      ENDDO
c      WRITE (*,*) ' r1=',r1
      IF (R1 .LE. 1.D-10) THEN
        IER=-1
        RETURN
      ENDIF
C
      DO 50 I=1,NF
        L=NF*(I-1)
        R=0.D0
        DO 30 J=1,NF
C          R=R+D(I,J)*F(J)
          R=R+D(L+J)*F(J)
 30     CONTINUE
        DO 40 J=1,NF
C          D(I,J)=D(I,J)-R*HF(J)/R1
          D(L+J)=D(L+J)-R*HF(J)/R1
 40     CONTINUE
 50   CONTINUE
C
      RETURN
      END
 
