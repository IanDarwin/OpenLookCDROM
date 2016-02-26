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

      subroutine InitNM

C  interface routine for initnm    

C  last change:  07/08/94  (ak)
 
      integer*2 mflag
      common /mswtch/ mflag
 

      if (mflag.eq.1) then
        call epInitNM
      else if (mflag.eq.2) then
        call fpInitNM
      else if (mflag.eq.3) then
        call lcInitNM
      else if (mflag.eq.4) then
        call psInitNM
      endif

      return
      end
