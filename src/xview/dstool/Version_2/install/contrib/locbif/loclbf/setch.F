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


c  This routine is used to set the character arrays in locbif
c  using data from the appropriate dstool postmaster objects.
c  The act of stuffing CHARACTER common areas is complicated
c  by problems of alignment -  see Sun FORTRAN-C interface 
c  document on the relevant issues.  To skirt these problems,
c  we let the FORTRAN compiler generate code to set common
c  blocks.

c  last change:  05/18/92 (mrm)


      subroutine setch( strng, cmntyp, index )

      integer*4     cmntyp, index	! match normal C int type
      character*6   strng, nc(251)
      character*13  lim(4), vc(251), pc(40)
      
      common /char/ nc, vc, pc, lim

      if (cnmtyp .eq.0) then		! stuff nc common array entry
        nc(index) = strng
      else if (cnmtyp .eq. 1) then	! stuff vc common array entry
        vc(index) = strng
      else if (cnmtyp .eq. 2) then	! stuff pc common array entry
        pc(index) = strng
      end if

      return
      end
