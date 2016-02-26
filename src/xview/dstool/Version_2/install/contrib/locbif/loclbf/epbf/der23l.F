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

      SUBROUTINE DER23L(N,ND,DH,X,D,FUNS,IER)
C
C   COMPUTING THE SECOND AND THIRD DERIVATIVES FOR LYAPUNOV VALUE
C        21.02.90
C        15.11.92
C
      IMPLICIT REAL*8(A-H,O-Z)
      IMPLICIT INTEGER*2(I-N)
      include 'epbf.dim'
      parameter (MMAX=13+(NFMAX-2)*8)
      DIMENSION D(ND),X(N)
      COMMON/WORK4/ YY(NFMAX,MMAX),GG(NFMAX,MMAX)
      EXTERNAL FUNS
      IER=0
      M=13+(N-2)*8
      DO 20 I=1,N
        DO 10 J=1,M
 10     YY(I,J)=0.
 20   CONTINUE
      D2H=2.*DH
      DH2=DH**2
      D4H2=4.D0*DH2
      D2H3=2.D0*DH*DH2
      YY(1,2)=DH
      YY(1,3)=-DH
      YY(2,4)=DH
      YY(2,5)=-DH
      YY(1,6)=DH
      YY(2,6)=DH
      YY(1,7)=-DH
      YY(2,7)=DH
      YY(1,8)=DH
      YY(2,8)=-DH
      YY(1,9)=-DH
      YY(2,9)=-DH
      YY(1,10)=D2H
      YY(1,11)=-D2H
      YY(2,12)=D2H
      YY(2,13)=-D2H
      IF (N.EQ.2) GO TO 50
      L=14
      DO 40 I=1,2
        DO 30 J=3,N
          YY(I,L)=DH
          YY(J,L)=DH
          YY(I,L+1)=-DH
          YY(J,L+1)=DH
          YY(I,L+2)=DH
          YY(J,L+2)=-DH
          YY(I,L+3)=-DH
          YY(J,L+3)=-DH
 30     L=L+4
 40   CONTINUE
 50   CALL FUNTS(N,M,X,YY,GG,FUNS,IER)
      IF (IER.NE.0) RETURN
      NN=N+N
      DO 60 I=1,N
        D(I)=(GG(I,2)+GG(I,3)-2.*GG(I,1))/DH2
        D(N+I)=(GG(I,4)+GG(I,5)-2.*GG(I,1))/DH2
        D(NN+I)=(GG(I,6)-GG(I,7)-GG(I,8)+GG(I,9))/D4H2
 60   CONTINUE
      D(3*N+1)=(GG(1,10)-2.*GG(1,2)+2.*GG(1,3)-GG(1,11))/D2H3
      D(3*N+2)=((GG(1,6)+GG(1,8)-2.*GG(1,2))-
     -(GG(1,7)+GG(1,9)-2.*GG(1,3)))/D2H3
      D(3*N+3)=((GG(2,6)+GG(2,7)-2.*GG(2,4))-
     -(GG(2,8)+GG(2,9)-2.*GG(2,5)))/D2H3
      D(3*N+4)=(GG(2,12)-2.*GG(2,4)+2.*GG(2,5)-GG(2,13))/D2H3
      IF (N.EQ.2) RETURN
      L1=2*(N-2)
      K=3*N+5
      DO 80 I=1,2
        L=14
        DO 70 J=1,L1
        D(K)=(GG(I,L)-GG(I,L+1)-GG(I,L+2)+GG(I,L+3))/D4H2
        L=L+4
 70     K=K+1
 80   CONTINUE
      RETURN
      END
 
