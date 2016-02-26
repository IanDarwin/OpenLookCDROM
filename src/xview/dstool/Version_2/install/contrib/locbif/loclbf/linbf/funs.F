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

      SUBROUTINE FUNS(X,F)
C
C   INTERFACE TO FUNT(T,X,F)
C         05.10.90
C
      IMPLICIT REAL*8(A-H,O-Z)
      IMPLICIT INTEGER*2(I-N)
      DIMENSION X(1),F(1)
      INTEGER*4 NFUNS
      COMMON /NFUNS/  NFUNS
      COMMON /IFLASH/ IFLASH
C
      T=0.D0
C      L=MAX0(1,IFLASH)
C      L=MOD((NFUNS-1),L)
C      IF (L.EQ.0) CALL FLASH
C 
      CALL FUNT(T,X,F)
C      CALL TERMNT
C      
C      NFUNS=NFUNS+1
      RETURN
      END
 
