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

      SUBROUTINE DSIMQ(A,B,N,KS)
      IMPLICIT INTEGER*2(I-N)
      DIMENSION A(1),B(1)
      DOUBLE PRECISION A,B,TOL,BIGA,SAVE
      TOL=0.D0
      KS=0
      JJ=-N
      DO 65 J=1,N
      JY=J+1
      JJ=JJ+N+1
      BIGA=0.D0
      IT=JJ-J
      DO 30 I=J,N
      IJ=IT+I
      IF(DABS(BIGA)-DABS(A(IJ)))20,30,30
   20 BIGA=A(IJ)
      IMAX=I
   30 CONTINUE
      IF(DABS(BIGA)-TOL)35,35,40
   35 KS=1
      RETURN
   40 I1=J+N*(J-2)
      IT=IMAX-J
      DO 50 I=J,N
      I1=I1+N
      I2=I1+IT
      SAVE=A(I1)
      A(I1)=A(I2)
      A(I2)=SAVE
   50 A(I1)=A(I1)/BIGA
      SAVE=B(IMAX)
      B(IMAX)=B(J)
      B(J)=SAVE/BIGA
      IF(J-N)55,70,55
   55 I=N*(J-1)
      DO 65 IX=JY,N
      I1=I+IX
      IT=J-IX
      DO 60 JX=JY,N
      IJ=N*(JX-1)+IX
      I2=IJ+IT
   60 A(IJ)=A(IJ)-(A(I1)*A(I2))
   65 B(IX)=B(IX)-(B(J)*A(I1))
   70 JY=N-1
      IT=N*N
      DO 80 J=1,JY
      I=IT-J
      I1=N-J
      I2=N
      DO 80 JJ=1,J
      B(I1)=B(I1)-A(I)*B(I2)
      I=I-N
   80 I2=I2-1
      RETURN
      END
 
