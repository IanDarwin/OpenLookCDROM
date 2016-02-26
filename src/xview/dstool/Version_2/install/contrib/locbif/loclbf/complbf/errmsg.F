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

      SUBROUTINE ErrMsg(what)
c
c  Error messages for RHS or user's functions computations
c
c---      INCLUDE 'KEYDEF.DAT'	
        INTEGER*2 what
c---        LOGICAL*2    KEYEVE,WAIT
        INTEGER*2    IERFS,IERDUM
	COMMON/IERFS/  IERFS,IERDUM
        CHARACTER*56 Error(0:12)
      DATA Error /
     -  ' Error :  unknown type                                  ',
     -  ' Error :  square root of negative number                ',
     -  ' Error :  zerodivide                                    ',
     -  ' Error :  overflow                                      ',
     -  ' Error :  underflow                                     ',
     -  ' Error :  logarithm of negative number                  ',
     -  ' Error :  real degree of negative number                ',
     -  ' Error :  index out of range                            ',
     -  ' Can not open file IR.DAT                               ',
     -  ' File IR.DAT has invalid structure                      ',
     -  ' Run-time data memory size is too small                 ',
     -  ' Run-time code memory size is too small                 ',
     -  ' Error :  invalid operation                             '/
c
c---	CALL ShowTe (Error(what))
c***
	WRITE (6,101) Error(what)
 101    FORMAT(1X,A56)
c***
           IERFS=what+1
c---	   IF (IERDUM.NE.0) THEN
c---              WAIT=.TRUE.
c---	      IF (KEYEVE(WAIT,EVNT)) CONTINUE
c---	      IERDUM=0
c---	      IF (EVNT.EQ.KEscape) THEN
c---                 IERFS=-1
c---              ENDIF
c---           ENDIF
      RETURN
      END
