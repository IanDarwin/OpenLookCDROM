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

      SUBROUTINE fpFUNM(X,F)
C
C   COMPUTING MAP'S ITERATIONS 
C          31.08.90
C          16.11.92
C          10.07.94
C
      IMPLICIT REAL*8(A-H,O-Z)
      IMPLICIT INTEGER*2(I-N)
      include 'fpbf.dim'
      DIMENSION X(1),F(1),Y(NFMAX)
      COMMON /IERFS/ IERFS,IERDUM
      COMMON /LINBF/ NF,NAP,ILN,NF2,ITP
      COMMON/ITMAP/ITMAP
      IERFS=0
      CALL FUNS(X,F)
      IF (ITMAP.LE.1) RETURN
      IF (IERFS.NE.0) RETURN
      DO 20 J=2,ITMAP
        DO 10 I=1,NF
          Y(I)=F(I)
 10     CONTINUE
      CALL FUNS(Y,F) 
      IF (IERFS.NE.0) RETURN
 20   CONTINUE
      RETURN
      END
