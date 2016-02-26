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

      SUBROUTINE DFDX (XX,DFUN,NDIM,NX,IER,ISTOP)
C
C* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C                                                                      *
C*    èPOÉPAMMA  DFDX3  èPEÑHAáHAóEHA Ñãü AHAãàTàóECKOÉO BõóàCãEHHü    *
C*    MATPàñõ óACTHõX èPOàáBOÑHõX.                                     *
C*                                                                     *
C* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
      IMPLICIT REAL*8(A-H,O-Z)
      IMPLICIT INTEGER*2 (I-N)
      DIMENSION XX(1), DFUN(NDIM,NX)
C
      IER   = 0
      ISTOP = 0
C
      RETURN
      END
