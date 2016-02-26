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

	SUBROUTINE Messag (TEXT,Npt,LineType,PointType,
     *                     N,M,L,BifVal,IEND)
C
C       19.10.94
C
        IMPLICIT INTEGER*2 (I-N)
	include 'complbf.dim'
C
        INTEGER*2  N,M,L
	INTEGER*2  Npt,LineType,PointType,IEND,NumPt
	REAL*8     BifVal(1)
	CHARACTER*6    NC(NRVC)
	CHARACTER*13   VC(NRVC),PC(NRPC),LIM(4)
	CHARACTER*56   TEXT
	INTEGER*2  MAXNPT
c

	COMMON/CHAR/   NC,VC,PC,LIM
        COMMON/NPT/    NumPt
        COMMON/MAXNPT/ MAXNPT
c
           TEXT=
     *    '                                                        '
C
 	NumPt=Npt
C============================================================================
C   First and last points
C
        IF (Npt.EQ.1.OR.Npt.EQ.-1) THEN
           TEXT=
     *    ' The first point                                        '
            GO TO 999
        ENDIF
C***
	IF (ABS(Npt).EQ.MAXNPT) THEN
	    IEND=-1
            TEXT=
     *    ' Last point in buffer                                   '
	   RETURN
	ENDIF
C============================================================================
C  Regular point
C
        IF (PointType.EQ.0) THEN
           WRITE (TEXT,10) Npt
10          FORMAT (' Npt =',I6,44X)
            GO TO 999
	ENDIF
C----------------------------------------------------------------------------
	IPT=PointType/100
        GOTO (1000,2000,3000,4000),IPT
	GO TO 1000
2000	CONTINUE
C============================================================================
C  Selfcrossing
C	
         IF (PointType.EQ.201) THEN
	   TEXT=
     *    ' Selfcrossing                                           '
            GO TO 999
	ENDIF
         IF (PointType.EQ.211) THEN
	   TEXT=
     *    ' Selfcrossing (?)                                       '
            GO TO 999
	ENDIF
3000     CONTINUE
C============================================================================
C  Singular point
C
	CALL Messng (LineType,PointType,BifVal,TEXT)
        GO TO 999
C	RETURN
C============================================================================   
C  Extremum
C
4000     CONTINUE 
	IF (PointType.EQ.401) THEN
            IEXT=BifVal(1)+N
            WRITE (TEXT,13) NC(IEXT),BifVal(2)
13          FORMAT
     *(' Maximum of parameter  ',A6,' is ',G13.6,10X)
            GO TO 999
	ENDIF
	IF (PointType.EQ.402) THEN
            IEXT=BifVal(1)+N
            WRITE (TEXT,14) NC(IEXT),BifVal(2)
14          FORMAT
     *(' Minimum of parameter  ',A6,' is ',G13.6,10X)
            GO TO 999
	ENDIF
	IF (PointType.EQ.411) THEN
            IEXT=BifVal(1)+N
            WRITE (TEXT,15) NC(IEXT),BifVal(2)
15          FORMAT
     *(' Maximum (?) of parameter  ',A6,' is ',G13.6,6X)
            GO TO 999
	ENDIF
	IF (PointType.EQ.412) THEN
            IEXT=BifVal(1)+N
            WRITE (TEXT,16) NC(IEXT),BifVal(2)
16          FORMAT
     *(' Minimum (?) of parameter  ',A6,' is ',G13.6,6X)
            GO TO 999
	ENDIF
C============================================================================
C  Special points
C
1000     CONTINUE
	IF (PointType.EQ.5) THEN
	    TEXT=
     *    ' Limit of parameter region                              '
            GO TO 998
	ENDIF
	IF (PointType.EQ.4) THEN
            TEXT=
     *    ' Closed curve                                           '
            GO TO 998
	ENDIF
	IF (PointType.EQ.6) THEN
            TEXT=
     *    ' Cannot continue numerical integration                  '
            GO TO 998
	ENDIF
	IF (PointType.EQ.-1) THEN
            TEXT=
     *    ' Last point in the direction                            '
            GO TO 998
	ENDIF
	IF (PointType.EQ.-3) THEN
            TEXT=
     *    ' Current step size is too small                         '
            GO TO 998
	ENDIF
	IF (PointType.EQ.-5) THEN
            TEXT=
     *    ' Computations terminated                                '
            GO TO 998
C	   RETURN
	ENDIF
C
	RETURN
999     CONTINUE
        RETURN
C
998     CONTINUE
        IEND=-1

	RETURN
	END
