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

        SUBROUTINE TERMNT
        IMPLICIT INTEGER*2 (I-N)
C
c---        INCLUDE 'KEYDEF.DAT'	
C	
C
	LOGICAL*2 WAIT,KEYEV1
        INTEGER*2 EVENT
        INTEGER*4 NFUNS,L
C
        INTEGER*2      KEYFLG,KEYCODE

	integer halt
        external loclbf_halt !$pragma C( loclbf_halt )

        COMMON/KEYFLG/ KEYFLG,KEYCODE
C
	COMMON/IERFS/  IERFS, IERDUM
        COMMON /NFUNS/  NFUNS
        COMMON /IABORT/ IABORT
        COMMON /IFLASH/ IFLASH

	call loclbf_halt(halt)
	if (halt .ne. 0) then
	  IERFS=-1
        endif

        RETURN
        END



