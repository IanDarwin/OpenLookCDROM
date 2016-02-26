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

      subroutine dfunbf(X,DFUN,NDIMM,NX,IER,ISTOP)

C  interface routine for dfunbf    

C  last change:  07/08/94  (ak)
 
      IMPLICIT REAL*8(A-H,O-Z)
      IMPLICIT INTEGER*2(I-N)
      integer*2 mflag
      DIMENSION X(1),DFUN(NDIMM,NX)
      common /mswtch/ mflag
 

      if (mflag.eq.1) then
        call epdfunbf(X,DFUN,NDIMM,NX,IER,ISTOP)
      else if (mflag.eq.2) then
        call fpdfunbf(X,DFUN,NDIMM,NX,IER,ISTOP)
      else if (mflag.eq.3) then
        call fpdfunbf(X,DFUN,NDIMM,NX,IER,ISTOP)
      else if (mflag.eq.4) then
        call fpdfunbf(X,DFUN,NDIMM,NX,IER,ISTOP)
      endif

      return
      end
