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

      subroutine outpt(NK,XP,NPC,T,ISTOP)

C  interface routine for outpt    

C  last change:  07/08/94  (ak)
 
      IMPLICIT INTEGER*2 (I-N)
      IMPLICIT REAL*8 (A-H,O-Z)
      integer*2 mflag
      DIMENSION XP(1)
      common /mswtch/ mflag
 

      if (mflag.eq.1) then
        call epoutpt(NK,XP,NPC,T,ISTOP)
      else if (mflag.eq.2) then
        call fpoutpt(NK,XP,NPC,T,ISTOP)
      else if (mflag.eq.3) then
        call lcoutpt(NK,XP,NPC,T,ISTOP)
      else if (mflag.eq.4) then
        call psoutpt(NK,XP,NPC,T,ISTOP)
      endif

      return
      end
