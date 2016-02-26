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

C-----------------------------------------------------------
      SUBROUTINE fpREGPBF(NK,X,ILN,NUF,IFUNS,FUNG,IERF,ISTEP)
C
C  Processing regular points (iterated maps)
C     16.11.92
C     10.07.94

C
      IMPLICIT REAL*8(A-H,O-Z)
      IMPLICIT INTEGER*2(I-N)
      include 'linfpbf.dim'
      DIMENSION X(NK),FUNG(1),IERF(1),IFUNS(1)
C----
      COMMON /LINBFQ/ IPT(NPTYP), INDER, IVAL(NVALPT),ISTP
      COMMON /LINBFR/ VALPT(NVALPT)
      IPT(2)=1
      INDER =0
C----
      NT=ISTEP+1
#if defined(hib)
      PRINT 105,NT
#endif
      CALL PRNTBF(NK,X)
      IF (ILN.LT.0) GO TO 90
      CALL EIGLBF(NK,X,IER)
      IF (ILN.NE.1.AND.ILN.NE.11.AND.ILN.NE.13) GO TO 10
 5      GTKR=FUNG(5)
        JTKR=IERF(5)
#if defined(hib)
        IF (JTKR.EQ.0) PRINT 101,GTKR
        IF (JTKR.NE.0) PRINT 102,JTKR
#endif
C----
	VALPT(1)=GTKR
	IVAL (1)=JTKR
C----
        GO TO 90
 10   IF (ILN.NE.2) GO TO 20
C 15     GLV=FUNG(5)
C        JLV=IERF(5)
C$if defined(hib)
C        IF (JLV.EQ.0) PRINT 103,GLV
C        IF (JLV.NE.0) PRINT 104,JLV
C$endif
C----
C	VALPT(1)=GLV
C	IVAL (1)=JLV
		      IPT(3)= 1
C	IF (JLV.NE.0) IPT(3)= 2 
C	IF (JLV.EQ.3) IPT(3)=-1
C
C	      IPT(3)=1,2 - Hopf case, IPT(3)=-1 - saddle case 
C----
        GO TO 90
 20     CONTINUE
 90     CONTINUE
#if defined(hib)
        IF (NUF.EQ.0) GO TO 99
        PRINT 110
        IF (NUF.EQ.1)
     A  PRINT 111,IFUNS(1),FUNG(IFUNS(1))
        IF (NUF.EQ.2)
     A  PRINT 112,(IFUNS(I),FUNG(IFUNS(I)),I=1,2  )
        IF (NUF.EQ.3)
     A  PRINT 113,(IFUNS(I),FUNG(IFUNS(I)),I=1,3  )
        IF (NUF.EQ.4)
     A  PRINT 114,(IFUNS(I),FUNG(IFUNS(I)),I=1,4  )
        IF (NUF.GE.5)
     A  PRINT 115,(IFUNS(I),FUNG(IFUNS(I)),I=1,NUF)
99      CONTINUE
#endif
        RETURN
#if defined(hib)
 101  FORMAT(/4X,'Š””ˆ–ˆ…’ ˆ Š‚€„€’ˆ—Œ —‹…… ‚ “€‚…ˆˆ ',
     A    '€‡‚…’‚‹…ˆŸ =',G13.6)
 102  FORMAT(/4X,'Š””ˆ–ˆ…’ ˆ Š‚€„€’ˆ—Œ —‹…… ‚ “€‚…ˆˆ ',
     A    '€‡‚…’‚‹…ˆŸ … ‚›—ˆ‘‹…:  IER=',I3)
 103  FORMAT(/4X,'…‰’€‹œ‘’œ ”Š“‘ƒ ’ˆ€ - ',
     A   '…‚€Ÿ ‹Ÿ“‚‘Š€Ÿ ‚…‹ˆ—ˆ€=',G13.6)
 104  FORMAT(/4X,'…‰’€‹œ‘’œ ‘…„‹‚ƒ ’ˆ€  (IERLV=',I3,')')
 105  FORMAT(/1X,'’—Š€ Œ…',I5)
 110  FORMAT(/4X,'”“Š–ˆˆ € Šˆ‚‰:')
 111  FORMAT( (3X,'G(',I2,')=',G11.4))
 112  FORMAT( (3X,2('G(',I2,')=',G11.4,2X)))
 113  FORMAT( (3X,3('G(',I2,')=',G11.4,2X)))
 114  FORMAT( (3X,4('G(',I2,')=',G11.4,2X)))
 115  FORMAT( (3X,5('G(',I2,')=',G11.4,2X)))
 199  FORMAT(1X,120('-'))
#endif
      END
