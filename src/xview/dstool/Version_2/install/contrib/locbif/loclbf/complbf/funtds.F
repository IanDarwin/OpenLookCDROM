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

      subroutine funtdemo (t,x,f)

      implicit integer*2 (i-n)
      implicit real*8 (a-h,o-z)

      dimension x(1),f(1)

      common/ierfs/ ierfs,ierdum
 
      external getfv  !$pragma C( getfv  )
      external getufv !$pragma C( getufv )

      call getfv(x, t, f)
      return
 
      entry usrfdemo (ifn,t,x,funres)

      ierfs=0
      call getufv(ifn,x,t,funres) 
      return
      end
