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

      subroutine bdata

C  interface routine for bdata    

C  last change:  07/08/94  (ak)
 
      integer*2 mflag
      common /mswtch/ mflag
 


      if (mflag.eq.1) then
        call epbdata
      else if (mflag.eq.2) then
        call fpbdata
      else if (mflag.eq.3) then
        call lcbdata
      else if (mflag.eq.4) then
        call psbdata
      endif

      return
      end
