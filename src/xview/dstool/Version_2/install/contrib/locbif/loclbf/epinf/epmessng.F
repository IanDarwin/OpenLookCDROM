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

	SUBROUTINE epMessng (LineType,PointType,BifVal,TEXT)
C
C  Singular point messages
C       case: Equilibrium points of ODEs
C
C     10.07.94
C 
        IMPLICIT INTEGER*2 (I-N)
	INTEGER*2      LineType,PointType
        REAL*8         BifVal(1)
	CHARACTER*56   TEXT
C
            TEXT=
     *	  ' Undefined point type                                   '
C
C  Singular point 
C

	GOTO (3251,3031,3001,3011,3021,3041,3111,3121,3131,3141,3151,
     *         3161,3171,3211,3221,3231,3241), LineType
C---------------------------------------------------------------------------
C    Orbit
C
 3251	CONTINUE
C  Period point
	IF (PointType.EQ.300) THEN
           WRITE (TEXT,17) BifVal(1)
17	   FORMAT (' Pause: t=',G13.6,33X)
	ENDIF
	RETURN
C----------------------------------------------------------------------------
C    Curve 
C
3031	CONTINUE
	RETURN
C----------------------------------------------------------------------------  
C   Equilibrium curve
C
3001	CONTINUE
C   Zero eigenvalue
	IF (PointType.EQ.311) THEN
            WRITE (TEXT,1) BifVal(1)
1	    FORMAT (' Zero eigenvalue :  a=',G13.6,21X)	
	ENDIF
	IF (PointType.EQ.312) THEN
            WRITE (TEXT,2) BifVal(1)
2	    FORMAT (' Zero eigenvalue(?) a=',G13.6,21X)
	ENDIF
	IF (PointType.EQ.313) THEN
            TEXT=
     *	  ' Zero eigenvalue :  a=?                                 '
	ENDIF
	IF (PointType.EQ.314) THEN
	   TEXT=
     *    ' Zero eigenvalue(?) a=?                                 '
	ENDIF
C  Hopf
	IF (PointType.EQ.321) THEN
            WRITE (TEXT,3)  BifVal(1)
3	    FORMAT (' Hopf : L1=',G13.6,32X)
	ENDIF
	IF (PointType.EQ.322) THEN
            WRITE (TEXT,4)  BifVal(1)
4	    FORMAT (' Hopf(?) L1=',G13.6,32X)
	ENDIF
	IF (PointType.EQ.323) THEN
	   TEXT=
     *    ' Hopf : L1=?                                            '
	ENDIF
	IF (PointType.EQ.324) THEN
	   TEXT=
     *    ' Hopf(?) L1=?                                           '
	ENDIF
	IF (PointType.EQ.325) THEN
	   TEXT=
     *    ' Neutral saddle                                         '
	ENDIF
	IF (PointType.EQ.326) THEN
	   TEXT=
     *    ' Neutral saddle (?)                                     '
	ENDIF
C  Double eigenvalue
	IF (PointType.EQ.331) THEN
	   TEXT=
     *    ' Double eigenvalue                                      '
	ENDIF
	IF (PointType.EQ.332) THEN
	   TEXT=
     *    ' Double eigenvalue (?)                                  '
	ENDIF
	RETURN
C----------------------------------------------------------------------------
C   Fold curve
C
3011	CONTINUE
C   Bogdanov
	IF (PointType.EQ.311) THEN
            WRITE (TEXT,5) BifVal(1)
5	    FORMAT (' Neutrality : a=',G13.6,27X)
	ENDIF
	IF (PointType.EQ.312) THEN
           WRITE (TEXT,6) BifVal(1)
6	   FORMAT (' Neutrality(?) a=',G13.6,26X)
	ENDIF
	IF (PointType.EQ.313) THEN
	   TEXT=
     *    ' Neutrality  : a=?                                      '
	ENDIF
	IF (PointType.EQ.314) THEN
	   TEXT=
     *    ' Neutrality(?) a=?                                      '
	ENDIF
C   Cusp
	IF (PointType.EQ.321) THEN
	   TEXT=
     *	  ' Cusp                                                   '
	ENDIF
	IF (PointType.EQ.322) THEN
	   TEXT=
     *	  ' Cusp(?)                                                '
	ENDIF
C  Double eigenvalue
	IF (PointType.EQ.331) THEN
           WRITE (TEXT,7) BifVal(1)
7	   FORMAT (' Double eigenvalue : a=',G13.6,20X)
	ENDIF
	IF (PointType.EQ.332) THEN
           WRITE (TEXT,8) BifVal(1)
8	   FORMAT (' Double eigenvalue(?) a=',G13.6,19X)
	ENDIF
	IF (PointType.EQ.333) THEN
	   TEXT=
     *	  ' Double eigenvalue : a=?                                '
	ENDIF
	IF (PointType.EQ.334) THEN
	   TEXT=
     *	  ' Double eigenvalue(?) a=?                               '
	ENDIF
	RETURN
C----------------------------------------------------------------------------
C   Hopf curve
C
3021	CONTINUE
C   Zero eigenvalue
	IF (PointType.EQ.311) THEN
           WRITE (TEXT,1) BifVal(1)
	ENDIF
	IF (PointType.EQ.312) THEN
            WRITE (TEXT,2) BifVal(1)
	ENDIF
	IF (PointType.EQ.313) THEN
	   TEXT=
     *    ' Zero eigenvalue : a=?                                  '
	ENDIF
	IF (PointType.EQ.314) THEN
	   TEXT=
     *    ' Zero eigenvalue(?) a=?                                 '
	ENDIF
C   L1=0
	IF (PointType.EQ.321) THEN
	   TEXT=
     *    ' Zero Lyapunov value                                    '
	ENDIF
	IF (PointType.EQ.322) THEN
	   TEXT=
     *    ' Zero Lyapunov value(?)                                 '
	ENDIF
C   Double eigenvalue
	IF (PointType.EQ.331) THEN
	   TEXT=
     *    ' Double eigenvalue                                      '
	ENDIF
	IF (PointType.EQ.332) THEN
	   TEXT=
     *    ' Double eigenvalue(?)                                   '
	ENDIF
	RETURN
C----------------------------------------------------------------------------	
C   Double eigenvalue (focus-node) curve
C
 3041	CONTINUE
C  Zero eigenvalue
         IF (PointType.EQ.311) THEN
            WRITE (TEXT,1) BifVal(1)
	ENDIF
	IF (PointType.EQ.312) THEN
            WRITE (TEXT,2) BifVal(1)
	ENDIF
	IF (PointType.EQ.313) THEN
	   TEXT=						   
     *    ' Zero eigenvalue : a=?                                  '
	ENDIF
	IF (PointType.EQ.314) THEN
	   TEXT=
     *    ' Zero eigenvalue(?) a=?                                 '
	ENDIF
C  Neutrality
	IF (PointType.EQ.321) THEN
	   TEXT=
     *    ' Neutrality                                             '
         ENDIF  
	IF (PointType.EQ.322) THEN
	   TEXT=
     *    ' Neutrality(?)                                          '
         ENDIF
	 RETURN  
C----------------------------------------------------------------------------
C   Double Zero curve
C
3111	CONTINUE
C   Cusp
	IF (PointType.EQ.311) THEN
            TEXT=
     *    ' Cusp                                                   '
	ENDIF
	IF (PointType.EQ.312) THEN
	   TEXT=
     *    ' Cusp(?)                                                '
	ENDIF
	RETURN
C----------------------------------------------------------------------------
C  Fold + Hopf curve
C
3121	CONTINUE
C  Cusp
	IF (PointType.EQ.311) THEN
	   TEXT=
     *    ' Cusp                                                   '
	ENDIF
	IF (PointType.EQ.312) THEN
	   TEXT=
     *    ' Cusp(?)                                                '
	ENDIF
	RETURN
C----------------------------------------------------------------------------
C   Double Hopf curve
C
3131	CONTINUE
	RETURN
C----------------------------------------------------------------------------
C   Cusp curve
C
3141	CONTINUE
C   Neutrality
	IF (PointType.EQ.311) THEN
	   TEXT=
     *    ' Neutrality                                             '
	ENDIF
	IF (PointType.EQ.312) THEN
	   TEXT=
     *    ' Neutrality(?)                                          '
	ENDIF
	RETURN
C----------------------------------------------------------------------------
C   Hopf + L1=0  curve
C
3151	CONTINUE
	RETURN
C----------------------------------------------------------------------------
C   Fold + Extr  curve
C
3161	CONTINUE
C   Nutrality
	IF (PointType.EQ.311) THEN
            WRITE (TEXT,5) BifVal(1)
	ENDIF
	IF (PointType.EQ.312) THEN
            WRITE (TEXT,6) BifVal(1)
	ENDIF
	IF (PointType.EQ.313) THEN
	   TEXT=
     *    ' Neutrality  : a=?                                      '
	ENDIF
	IF (PointType.EQ.314) THEN
	   TEXT=
     *    ' Neutrality(?) a=?                                      '
	ENDIF
	RETURN
C----------------------------------------------------------------------------	
C   Hopf + Extr  curve
C
3171	CONTINUE
C   Fold
         IF (PointType.EQ.311) THEN
            WRITE (TEXT,1) BifVal(1)
	ENDIF
	IF (PointType.EQ.312) THEN
            WRITE (TEXT,2) BifVal(1)
	ENDIF
	IF (PointType.EQ.313) THEN
	   TEXT=
     *    ' Zero eigenvalue : a=?                                  '
	ENDIF
	IF (PointType.EQ.314) THEN
	   TEXT=
     *    ' Zero eigenvalue(?) a=?                                 '
	ENDIF
3211	CONTINUE
3221	CONTINUE
3231	CONTINUE
3241	CONTINUE
	RETURN
        END 
