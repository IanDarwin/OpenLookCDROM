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

      SUBROUTINE KOEEV1(A,N,NA,P,IER)
C
C   Compute coefficients of the characteristic polynomial
C
C   Rev. 2.08.92
C        17.11.92
C
      IMPLICIT REAL*8(A-H,O-Z)
      IMPLICIT INTEGER*2(I-N)
      include 'algebrbf.dim'
      parameter (NFMAXd=NFMAX*2)
      DIMENSION A(NA,1),P(N)
      DIMENSION Y(NFMAXd)
      COMMON/WORK1/ B(NFMAXd,NFMAXd)
      IF(N.GT.0.AND.N.LE.NFMAX) GO TO 11
      IER=1
      RETURN
11    IER=0
      S=0.D0
      STR=0.D0
      DO 99 I=1,N
         S=S+A(I,I)
         STR=STR+DABS(A(I,I))
 99   CONTINUE
c     STR=DSQRT(STR)
c      WRITE (*,*) '  STR=',STR  
      P(1)=-S
      IF(N.EQ.1) RETURN
c---
      SC=1.D0
      SC=N/(1.D-2+STR)
      P(1)=P(1)*SC
      DO 61 I=1,N
         DO 60 J=1,N
           A(I,J)=SC*A(I,J)
 60      CONTINUE
 61   CONTINUE   
c---
      DO 1 I=1,N
      DO 1 J=1,N
      S1=0.D0
      DO 2 K=1,N
      IF(K.EQ.J) GO TO 3
          S1=S1+A(I,K)*A(K,J)
      GO TO 2
3        S1=S1+A(I,K)*(A(K,K)+P(1))
2     CONTINUE
1     B(I,J)=S1
      S=0
      DO 4 I=1,N
4     S=S+B(I,I)
      P(2)=-S/2.D0
      IF(N.EQ.2) RETURN
      DO 9 L=3,N
      DO 5 J=1,N
          DO 6 I=1,N
          S=0.
      DO 7 K=1,N
      IF(K.EQ.J) GO TO 8
      S=S+A(I,K)*B(K,J)
      GO TO 7
8     S=S+A(I,K)*(B(J,J)+P(L-1))
7     CONTINUE
6     Y(I)=S
      DO 10 I=1,N
10     B(I,J)=Y(I)
5     CONTINUE
      S=0
      DO 12 I=1,N
12    S=S+B(I,I)
      P(L)=-S/L
9     CONTINUE
c          PRINT 902,(P(I),I=1,6)
c 902      FORMAT (/2X,'P=',/(2X,6G13.6))      
c
      RETURN
C
      R=(DABS(P(N)))**(1.D0/N)
      R=1.D0/(R+1.D0)
      R1=1.D0
      DO 35 I=1,N
        R1=R1*R
        P(I)=P(I)*R1
 35   CONTINUE
      RETURN
      END


