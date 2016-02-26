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

C-----------------------------------------------------------
      SUBROUTINE WLINE(ISTEP,INDEX,NK,NDIMM,X,VMOVE,DFUN,ISTOP)
C
C     14.11.92
C
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER*2(I-N)
      INTEGER*4 NFUNS
      include 'linbf.dim'
      DIMENSION X(1),VMOVE(1),INDEX(NPTYP),DFUN(NDIM,1)
      COMMON/NFUNS/NFUNS
      COMMON/NJAC/NJAC,NJACN,NJACB
#if defined(hib)
      PRINT 101,NFUNS,NJAC
 101  FORMAT(//1X,'NFUNS=',I9,'  NJAC=',I8)
#endif
      RETURN
      END
 
