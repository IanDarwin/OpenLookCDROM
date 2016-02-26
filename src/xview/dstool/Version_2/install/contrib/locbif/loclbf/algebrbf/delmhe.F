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

      SUBROUTINE DELMHE(NM,N,LOW,IGH,A,INT)
      IMPLICIT REAL*8(A-H,O-Z)
      IMPLICIT INTEGER*2(I-N)
C
      DIMENSION A(NM,N),INT(IGH)
C
c
c     this subroutine is a translation of the algol procedure elmhes,
c     num. math. 12, 349-368(1968) by martin and wilkinson.
c     handbook for auto. comp., vol.ii-linear algebra, 339-358(1971).
c
c     given a real general matrix, this subroutine
c     reduces a submatrix situated in rows and columns
c     low through igh to upper hessenberg form by
c     stabilized elementary similarity transformations.
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
c        a contains the input matrix.
c
c     on output
c
c        a contains the hessenberg matrix.  the multipliers
c          which were used in the reduction are stored in the
c          remaining triangle under the hessenberg matrix.
c
c        int contains information on the rows and columns
c          interchanged in the reduction.
c          only elements low through igh are used.
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
      LA=IGH-1
      KP1=LOW+1
      IF(LA.LT.KP1)GOTO 200
C
      DO 180 M=KP1,LA
      MM1=M-1
      X=0.D0
      I=M
C
      DO 100 J=M,IGH
      IF(DABS(A(J,MM1)).LE.DABS(X))GOTO 100
      X=A(J,MM1)
      I=J
  100 CONTINUE
C
      INT(M)=I
C
      INT(M)=I
      IF(I.EQ.M)GOTO 130
c     .......... interchange rows and columns of a ..........
      DO 110 J=MM1,N
      Y=A(I,J)
      A(I,J)=A(M,J)
      A(M,J)=Y
  110 CONTINUE
C
      DO 120 J=1,IGH
      Y=A(J,I)
      A(J,I)=A(J,M)
      A(J,M)=Y
  120 CONTINUE
c     .......... end interchange ..........
  130 IF(X.EQ.0.D0)GOTO 180
      MP1=M+1
C
      DO 160 I=MP1,IGH
      Y=A(I,MM1)
      IF(Y.EQ.0.D0)GOTO 160
      Y=Y/X
      A(I,MM1)=Y
C
      DO 140 J=M,N
  140 A(I,J)=A(I,J)-Y*A(M,J)
C
      DO 150 J=1,IGH
  150 A(J,M)=A(J,M)+Y*A(J,I)
C
  160 CONTINUE
C
  180 CONTINUE
C
  200 RETURN
      END
