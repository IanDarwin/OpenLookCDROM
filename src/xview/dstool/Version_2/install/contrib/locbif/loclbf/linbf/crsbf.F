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

      SUBROUTINE CRSBF(NK,X,INDC)
C-----------------------------------------------------------
C       20.05.1990
C       14.11.92
C-----------------------------------------------------------
C
      IMPLICIT REAL*8(A-H,O-Z)
      IMPLICIT INTEGER*2(I-N)
      include 'linbf.dim'
      DIMENSION X(NK)
C----
      COMMON/LINBF/NF,NAP,ILN,NF2,ITP
      COMMON /LINBFQ/ IPT(NPTYP1), INDER, IVAL(NVALPT)
      COMMON /LINBFR/ VALPT(NVALPT)
C----
      IPT(2)=2
      INDER =INDC
C----
#if defined(hib)
      PRINT 930
      PRINT 901
      IF (INDC.NE.0) PRINT 902
#endif
      CALL PRNTBF(NK,X)
      IF (ILN.LT.0) GO TO 100
      CALL EIGLBF(NK,X,IER)
 100  CONTINUE
#if defined(hib)
      PRINT 930
#endif
      RETURN
#if defined(hib)
 901  FORMAT(/3X,'’—Š€ ‘€Œ……‘…—…ˆŸ')
 902  FORMAT(/5X,'(ˆ “’—…ˆˆ ’—Šˆ ‘Œ……‘…—…ˆŸ ',
     A   '’…“…Œ€Ÿ ’—‘’œ … „‘’ˆƒ“’€)'/)
 930  FORMAT(/1X,120('-')/)
#endif
      END
