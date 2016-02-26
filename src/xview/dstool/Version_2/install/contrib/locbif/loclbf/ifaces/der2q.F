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

      subroutine der2q(N,ND,DH,X,DER,FUNS,IER)

C  interface routine for der2q    

C  last change:  08/16/94  (ak)
 
      IMPLICIT REAL*8(A-H,O-Z)
      IMPLICIT INTEGER*2(I-N)
      integer*2 mflag
      DIMENSION DER(N,N),X(N)
      common /mswtch/ mflag
      EXTERNAL FUNS
 

      if (mflag.eq.1) then
        call epder2q(N,ND,DH,X,DER,FUNS,IER)
      else if (mflag.eq.2) then
        call fpder2q(N,ND,DH,X,DER,FUNS,IER)
      else if (mflag.eq.3) then
        call fpder2q(N,ND,DH,X,DER,FUNS,IER)
      else if (mflag.eq.4) then
        call fpder2q(N,ND,DH,X,DER,FUNS,IER)
      endif

      return
      end
