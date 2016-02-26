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

      SUBROUTINE ORBITD(N,X,IDIR,IEND)
C
C          18.09.90
C          23.08.91
C          14.11.92
C
      IMPLICIT REAL*8(A-H,O-Z)
      IMPLICIT INTEGER*2(I-N)
      include 'linbf.dim'
      DIMENSION X(N),Y(NFMAX),F(NFMAX)
      COMMON /LINBF/ NF,NAP,ILN,NF2,ITP
      COMMON /ITMAP/ ITMAP
      COMMON /IERFS/ IERFS,IERDUM
      COMMON /LINBFQ/ IPT(NPTYP1), IVAL(NVALPT), ISTP
      COMMON /LINBFR/ VALPT(NVALPT)
      COMMON /INTOUT/ IOUT,IDIREC
      DATA NPMAX/10000/
      NF=N
      NAP=0
      ILN=-2
      CALL INITBF
      DO 1 I=1,NPTYP1
        IPT(I)=0
 1    CONTINUE
      DO 2 I=1,NVALPT
        IVAL(I)=0
        VALPT(I)=0.
 2    CONTINUE
      IERFS=0
      IEND=0
      ISTOP=0
      IOUT=0
      IDIREC=IDIR
      DO 10 I=1,N
        Y(I)=X(I)
 10   CONTINUE
      NP=0
      NIT=0
C
 20   NP=NP+1
      IPT(2)=1
      ANIT=NIT
C      PRINT 101,(Y(I),I=1,N)
C 101  FORMAT(1X,'Y=',5G13.4)
      CALL OUTPT(N,Y,NP,ANIT,ISTOP)
      IF (ISTOP.NE.0) GO TO 40
      IF (NP.GE.NPMAX) GO TO 50
       CALL FUNM(Y,F)
       IF (IERFS.NE.0) GO TO 99
      DO 30 I=1,N
        Y(I)=F(I)
 30   CONTINUE
      NIT=NIT+ITMAP 
      GO TO 20      
C
 40   CONTINUE
      IEND=-5
      RETURN
C
 50   CONTINUE
      IEND=1
      RETURN
C
 99   CONTINUE
      IEND=-5 
      RETURN
      END
