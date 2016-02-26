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

      subroutine modjac(NK,X,D,IER)

C  interface routine for modjac    

C  last change:  07/08/94  (ak)
 
      IMPLICIT REAL*8(A-H,O-Z)
      IMPLICIT INTEGER*2(I-N)
      integer*2 mflag
      DIMENSION X(NK),D(1)
      common /mswtch/ mflag
 

      if (mflag.eq.1) then
        return
      else if (mflag.eq.2) then
        call fpmodjac(NK,X,D,IER)
      else if (mflag.eq.3) then
        call lcmodjac(NK,X,D,IER)
      else if (mflag.eq.4) then
        call psmodjac(NK,X,D,IER)
      endif

      return
      end
