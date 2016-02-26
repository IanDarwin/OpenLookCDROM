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

      SUBROUTINE DHQR(NM,N,LOW,IGH,H,WR,WI,IERR)
      IMPLICIT REAL*8(A-H,O-Z)
      IMPLICIT INTEGER*2(I-N)
      REAL*8 MACHEP
      INTEGER*2 I,J,K,L,M,N,EN,LL,MM,NA,NM,IGH,ITS,LOW,MP2,ENM2,IERR
      LOGICAL NOTLAS
      DIMENSION H(NM,N),WR(N),WI(N)
c
c     this subroutine is a translation of the algol procedure hqr,
c     num. math. 14, 219-231(1970) by martin, peters, and wilkinson.
c     handbook for auto. comp., vol.ii-linear algebra, 359-371(1971).
c
c     this subroutine finds the eigenvalues of a real
c     upper hessenberg matrix by the qr method.
c
c     on input
c
c        nm must be set to the row dimension of two-dimensional
c          array parameters as declared in the calling program
c          dimension statement.
c
c        n is the order of the matrix.
c
c        low and igh are integers determined by the balancing
c          subroutine  balanc.  if  balanc  has not been used,
c          set low=1, igh=n.
c
c        h contains the upper hessenberg matrix.  information about
c          the transformations used in the reduction to hessenberg
c          form by  elmhes  or  orthes, if performed, is stored
c          in the remaining triangle under the hessenberg matrix.
c
c     on output
c
c        h has been destroyed.  therefore, it must be saved
c          before calling  hqr  if subsequent calculation and
c          back transformation of eigenvectors is to be performed.
c
c        wr and wi contain the real and imaginary parts,
c          respectively, of the eigenvalues.  the eigenvalues
c          are unordered except that complex conjugate pairs
c          of values appear consecutively with the eigenvalue
c          having the positive imaginary part first.  if an
c          error exit is made, the eigenvalues should be correct
c          for indices ierr+1,...,n.
c
c        ierr is set to
c          zero       for normal return,
c          j          if the limit of 30*n iterations is exhausted
c                     while the j-th eigenvalue is being sought.
c
c     questions and comments should be directed to burton s. garbow,
c     mathematics and computer science div, argonne national laboratory
c
c     this version dated august 1983.
c
c     ------------------------------------------------------------------
c
C     adaptation to double precision at Inst.Appl. Math., USSR Acad. Sci.
C     ------------------------------------------------------------------
C
C     **********MACHEP - machine-dependent constant********
C
          MACHEP=.11102230246251565D-15
C
      IERR=0
      ANORM = 0.0
      K = 1
c     .......... store roots isolated by balanc
c                and compute matrix norm ..........
      DO 50 I=1,N
C
      DO 40 J = K,N
   40 ANORM = ANORM +DABS(H(I,J))
C
      K = I
      IF(I.GE.LOW.AND.I.LE.IGH)GO TO 50
      WR(I)=H(I,I)
      WI(I)=0.0
   50 CONTINUE
C
      EN=IGH
      T=0.0
c     .......... search for next eigenvalues ..........
   60 IF(EN.LT.LOW)GO TO 1001
      ITS=0
      NA=EN-1
      ENM2=NA-1
c     .......... look for single small sub-diagonal element
c                for l=en step -1 until low do -- ..........
   70 DO 80 LL=LOW,EN
      L=EN+LOW-LL
      IF(L.EQ.LOW)GO TO 100
      S = DABS(H(L-1,L-1)) +DABS(H(L,L))
      IF(S .EQ. 0.0) S = ANORM
      IF(DABS(H(L,L-1)).LE.MACHEP*S) GO TO 100
   80 CONTINUE
c     .......... form shift ..........
  100 X=H(EN,EN)
      IF(L.EQ.EN)GO TO 270
      Y=H(NA,NA)
      W=H(EN,NA)*H(NA,EN)
      IF(L.EQ.NA)GO TO 280
      IF(ITS.EQ.30)GO TO 1000
      IF(ITS.NE.10.AND.ITS.NE.20)GO TO 130
c     .......... form exceptional shift ..........
      T=T+X
C
      DO 120 I=LOW,EN
  120 H(I,I)=H(I,I)-X
C
      S=DABS(H(EN,NA))+DABS(H(NA,ENM2))
      X=0.75*S
      Y=X
      W=-0.4375*S*S
  130 ITS=ITS+1
c     .......... look for two consecutive small
c                sub-diagonal elements.
c                for m=en-2 step -1 until l do -- ..........
      DO 140 MM=L,ENM2
      M=ENM2+L-MM
      ZZ=H(M,M)
      R=X-ZZ
      S=Y-ZZ
      P=(R*S-W)/H(M+1,M)+H(M,M+1)
      Q=H(M+1,M+1)-ZZ-R-S
      R=H(M+2,M+1)
      S=DABS(P)+DABS(Q)+DABS(R)
      P=P/S
      Q=Q/S
      R=R/S
      IF(M.EQ.L)GO TO 150
      IF(DABS(H(M,M-1))*(DABS(Q)+DABS(R)).LE.MACHEP*DABS(P)
     **(DABS(H(M-1,M-1))+DABS(ZZ)+DABS(H(M+1,M+1))))GO TO 150
  140 CONTINUE
C
  150 MP2=M+2
C
      DO 160 I=MP2,EN
      H(I,I-2)=0.0
      IF(I.EQ.MP2)GO TO 160
      H(I,I-3)=0.0
  160 CONTINUE
c     .......... double qr step involving rows l to en and
c                columns m to en ..........
      DO 260 K=M,NA
      NOTLAS=K.NE.NA
      IF(K.EQ.M)GO TO 170
      P=H(K,K-1)
      Q=H(K+1,K-1)
      R=0.0
      IF(NOTLAS)R=H(K+2,K-1)
      X=DABS(P)+DABS(Q)+DABS(R)
      IF(X.EQ.0.0)GO TO 260
      P=P/X
      Q=Q/X
      R=R/X
170   S=DSIGN(DSQRT(P*P+Q*Q+R*R),P)
      IF(K.EQ.M)GO TO 180
      H(K,K-1)=-S*X
      GO TO 190
180   IF(L.NE.M) H(K,K-1)=-H(K,K-1)
190   P=P+S
      X=P/S
      Y=Q/S
      ZZ=R/S
      Q=Q/P
      R=R/P
c     .......... row modification ..........
      DO 210 J=K,EN
        P=H(K,J)+Q*H(K+1,J)
        IF(.NOT.NOTLAS) GO TO 200
        P=P+R*H(K+2,J)
        H(K+2,J)=H(K+2,J)-P*ZZ
200     H(K+1,J)=H(K+1,J)-P*Y
        H(K,J)=H(K,J)-P*X
210   CONTINUE
C
      J=MIN0(EN,K+3)
c     .......... column modification ..........
      DO 230 I=L,J
      P=X*H(I,K)+Y*H(I,K+1)
      IF(.NOT.NOTLAS)GO TO 220
      P=P+ZZ*H(I,K+2)
      H(I,K+2)=H(I,K+2)-P*R
  220 H(I,K+1)=H(I,K+1)-P*Q
      H(I,K)=H(I,K)-P
  230 CONTINUE
C
  260 CONTINUE
C
      GO TO 70
c     .......... one root found ..........
  270 WR(EN)=X+T
      WI(EN)=0.0
      EN=NA
      GO TO 60
c     .......... two roots found ..........
  280 P=(Y-X)/2.0
      Q=P*P+W
      ZZ=DSQRT(DABS(Q))
      X=X+T
      IF(Q.LT.0.0)GO TO 320
c     .......... real pair ..........
      ZZ=P+DSIGN(ZZ,P)
      WR(NA)=X+ZZ
      WR(EN)=WR(NA)
      IF(ZZ.NE.0.0)WR(EN)=X-W/ZZ
      WI(NA)=0.0
      WI(EN)=0.0
      GO TO 330
c     .......... complex pair ..........
  320 WR(NA)=X+P
      WR(EN)=X+P
      WI(NA)=ZZ
      WI(EN)=-ZZ
  330 EN=ENM2
      GO TO 60
c     .......... set error -- all eigenvalues have not
c                converged after 30*n iterations ..........
 1000 IERR=EN
 1001 RETURN
      END


