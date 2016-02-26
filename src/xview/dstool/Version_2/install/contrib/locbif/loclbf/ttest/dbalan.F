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

      SUBROUTINE DBALAN(NM,N,A,LOW,IGH,SCALE)
      IMPLICIT REAL*8(A-H,O-Z)
      IMPLICIT INTEGER*2(I-N)
C
      DIMENSION A(NM,N),SCALE(N)
C
C
      LOGICAL NOCONV
c
c     this subroutine is a translation of the algol procedure balance,
c     num. math. 13, 293-304(1969) by parlett and reinsch.
c     handbook for auto. comp., vol.ii-linear algebra, 315-326(1971).
c
c     this subroutine balances a real matrix and isolates
c     eigenvalues whenever possible.
c
c     on input
c
c        nm must be set to the row dimension of two-dimensional
c          array parameters as declared in the calling program
c          dimension statement.
c
c        n is the order of the matrix.
c
c        a contains the input matrix to be balanced.
c
c     on output
c
c        a contains the balanced matrix.
c
c        low and igh are two integers such that a(i,j)
c          is equal to zero if
c           (1) i is greater than j and
c           (2) j=1,...,low-1 or i=igh+1,...,n.
c
c        scale contains information determining the
c           permutations and scaling factors used.
c
c     suppose that the principal submatrix in rows low through igh
c     has been balanced, that p(j) denotes the index interchanged
c     with j during the permutation step, and that the elements
c     of the diagonal matrix used are denoted by d(i,j).  then
c        scale(j) = p(j),    for j = 1,...,low-1
c                 = d(j,j),      j = low,...,igh
c                 = p(j)         j = igh+1,...,n.
c     the order in which the interchanges are made is n to igh+1,
c     then 1 to low-1.
c
c     note that 1 is returned for igh if igh is zero formally.
c
c     the algol procedure exc contained in balance appears in
c     balanc  in line.  (note that the algol roles of identifiers
c     k,l have been reversed.)
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
C     **********RADIX - machine-dependent constant********************
      RADIX=16.
C
      B2=RADIX*RADIX
      K=1
      L=N
      GO TO 100
c     .......... in-line procedure for row and
c                column exchange ..........
   20 SCALE(M)=J
      IF(J.EQ.M)GOTO 50
C
      DO 30 I=1,L
      F=A(I,J)
      A(I,J)=A(I,M)
      A(I,M)=F
   30 CONTINUE
C
      DO 40 I=K,N
      F=A(J,I)
      A(J,I)=A(M,I)
      A(M,I)=F
   40 CONTINUE
C
   50 GO TO(80,130),IEXC
c     .......... search for rows isolating an eigenvalue
c                and push them down ..........
   80 IF(L.EQ.1)GOTO 280
      L=L-1
c     .......... for j=l step -1 until 1 do -- ..........
  100 DO 120 JJ=1,L
      J=L+1-JJ
C
      DO 110 I=1,L
      IF(I.EQ.J)GO TO 110
      IF(A(J,I).NE.0.0)GOTO 120
  110 CONTINUE
C
      M=L
      IEXC=1
      GOTO 20
  120 CONTINUE
C
      GOTO 140
c     .......... search for columns isolating an eigenvalue
c                and push them left ..........
  130 K=K+1
C
  140 DO 170 J=K,L
C
      DO 150 I=K,L
      IF(I.EQ.J)GOTO 150
      IF(A(I,J).NE.0.0)GOTO 170
  150 CONTINUE
C
      M=K
      IEXC=2
      GOTO 20
  170 CONTINUE
c     .......... now balance the submatrix in rows k to l ..........
      DO 180 I=K,L
  180 SCALE(I)=1.0
c     .......... iterative loop for norm reduction ..........
  190 NOCONV=.FALSE.
C
      DO 270 I=K,L
      C=0.D0
      R=0.D0
C
      DO 200 J=K,L
      IF(J.EQ.I)GOTO 200
      C=C+DABS(A(J,I))
      R=R+DABS(A(I,J))
  200 CONTINUE
c     .......... guard against zero c or r due to underflow ..........
      IF (C .EQ. 0.D0.OR. R .EQ. 0.D0) GO TO 270
      G=R/RADIX
      F=1.D0
      S=C+R
  210 IF(C.GE.G)GOTO 220
      F=F*RADIX
      C=C*B2
      GOTO 210
  220 G=R*RADIX
  230 IF(C.LT.G)GOTO 240
      F=F/RADIX
      C=C/B2
      GOTO 230
c     .......... now balance ..........
  240 IF((C+R)/F.GE.0.95D0*S)GOTO 270
      G=1.D0/F
      SCALE(I)=SCALE(I)*F
      NOCONV=.TRUE.
C
      DO 250 J=K,N
  250 A(I,J)=A(I,J)*G
C
      DO 260 J=1,L
  260 A(J,I)=A(J,I)*F
C
  270 CONTINUE
C
      IF(NOCONV)GOTO 190
C
  280 LOW=K
      IGH=L
      RETURN
      END
