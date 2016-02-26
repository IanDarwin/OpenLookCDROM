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

      subroutine messng (LineType,PointType,BifVal,TEXT)

C  interface routine for messng    

C  last change:  07/08/94  (ak)
 
      IMPLICIT INTEGER*2 (I-N)
      integer*2 mflag
      INTEGER*2      LineType,PointType
      REAL*8         BifVal(1)
      CHARACTER*56   TEXT
      common /mswtch/ mflag
 

      if (mflag.eq.1) then
        call epmessng(LineType,PointType,BifVal,TEXT)
      else if (mflag.eq.2) then
        call fpmessng(LineType,PointType,BifVal,TEXT)
      else if (mflag.eq.3) then
        call lcmessng(LineType,PointType,BifVal,TEXT)
      else if (mflag.eq.4) then
        call psmessng(LineType,PointType,BifVal,TEXT)
      endif

      return
      end
