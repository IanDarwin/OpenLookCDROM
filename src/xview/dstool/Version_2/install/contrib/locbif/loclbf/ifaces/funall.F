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

      function funall(INDF,NX,X,FUN,DFDX,IER,ISTOP)

C  interface routine for funall    

C  last change:  07/08/94  (ak)
 
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER*2(I-N)
      integer*2 mflag
      DIMENSION X(1)
      common /mswtch/ mflag
      EXTERNAL FUN, DFDX
 

      if (mflag.eq.1) then
        funall = epfunall(INDF,NX,X,FUN,DFDX,IER,ISTOP)
      else if (mflag.eq.2) then
        funall = fpfunall(INDF,NX,X,FUN,DFDX,IER,ISTOP)
      else if (mflag.eq.3) then
        funall = fpfunall(INDF,NX,X,FUN,DFDX,IER,ISTOP)
      else if (mflag.eq.4) then
        funall = fpfunall(INDF,NX,X,FUN,DFDX,IER,ISTOP)
      endif

      return
      end
