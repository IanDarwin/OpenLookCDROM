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

      subroutine linebf(N,M,L,X,P,Line,ActPar,H,E)

C  interface routine for linebf    

C  last change:  07/08/94  (ak)
 
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER*2 (I-N)
      integer*2 mflag
      INTEGER*2 N,M,L,Line
      REAL*8    X(N),P(M)
      LOGICAL*2 ActPar(M)
      common /mswtch/ mflag
 

      if (mflag.eq.1) then
        call eplinebf(N,M,L,X,P,Line,ActPar,H,E)
      else if (mflag.eq.2) then
        call fplinebf(N,M,L,X,P,Line,ActPar,H,E)
      else if (mflag.eq.3) then
        call lclinebf(N,M,L,X,P,Line,ActPar,H,E)
      else if (mflag.eq.4) then
        call pslinebf(N,M,L,X,P,Line,ActPar,H,E)
      endif

      return
      end
