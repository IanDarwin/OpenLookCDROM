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

      SUBROUTINE ORTG2(N,NA,Z)
C
C   COMPUTING BASIS IN A SUBSPACE ORTHOGONAL TO TWO GIVEN VECTORS
C           23.06.81
C     Rev.  25.07.92
C           17.11.92
C
      IMPLICIT REAL*8(A-H,O-Z)
      IMPLICIT INTEGER*2(I-N)
      DIMENSION Z(NA,N)
      R1=0.
      R2=0.
      R3=0.
      DO 10 I=1,N
        R1=R1+Z(I,1)**2
        R2=R2+Z(I,2)**2
        R3=R3+Z(I,1)*Z(I,2)
 10   CONTINUE
      R1=1.D0/DSQRT(R1)
      R2=1.D0/DSQRT(R2)
      R3=R3*R1*R2
      R4=1.D0/(1.D0-R3*R3)
      DO 20 I=1,N
        Z(I,1)=R1*Z(I,1)
        Z(I,2)=R2*Z(I,2)
 20   CONTINUE
      DO 40 I=1,N
        DO 30 J=3,N
 30     Z(I,J)=0.
 40   CONTINUE
      I1=1
      R1=DABS(Z(1,1))
      DO 43 I=2,N 
         R=DABS(Z(I,1))
         IF (R.LE.R1) GO TO 43
            R1=R
            I1=I
 43   CONTINUE 
C
      R2=Z(I1,2)/Z(I1,1)
      R1=DABS(Z(1,2)-R2*Z(1,1))      
      I2=1
      DO 44 I=2,N
         R=DABS(Z(I,2)-R2*Z(I,1))      
         IF (R.LE.R1) GO TO 44
            R1=R
            I2=I
 44   CONTINUE
      L=3
      DO 60 I=1,N
        IF ((I-I1)*(I-I2).EQ.0) GO TO 60
          Z(I,L)=1.
          R1=(Z(I,1)-Z(I,2)*R3)*R4
          R2=(Z(I,2)-Z(I,1)*R3)*R4
          DO 50 J=1,N
 50       Z(J,L)=Z(J,L)-R1*Z(J,1)-R2*Z(J,2)
          L=L+1
 60   CONTINUE
      RETURN
      END
