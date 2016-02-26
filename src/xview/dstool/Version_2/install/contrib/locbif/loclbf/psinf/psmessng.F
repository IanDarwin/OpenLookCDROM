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

	SUBROUTINE psMessng (LineType,PointType,BifVal,TEXT)
C
C  Singular point messages
C       case: Fixed points, Periodic solutions, Limit cycles
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
	GOTO (3015,3016,3001,3002,3003,3004,3005,3006,3007,3008,3009,
     *         3010,3011,3012,3013,3014), LineType
C----------------------------------------------------------------------------  
C    Orbit
C
 3015	CONTINUE
C  Period point
	IF (PointType.EQ.300) THEN
           WRITE (TEXT,17) BifVal(1)
17	   FORMAT (' Pause: t=',G13.6,33X)
	ENDIF
	RETURN
C----------------------------------------------------------------------------
C    Curve 
C
 3016   CONTINUE
        RETURN
C----------------------------------------------------------------------------
C   Equilibrium curve
C
3001	CONTINUE
C   Multiplier = 1
	IF (PointType.EQ.311) THEN
            WRITE (TEXT,1) BifVal(1)
1	    FORMAT (' Multiplier = 1 :  a=',G13.6,21X)	
	ENDIF
	IF (PointType.EQ.312) THEN
            WRITE (TEXT,2) BifVal(1)
2	    FORMAT (' Multiplier = 1(?) a=',G13.6,21X)
	ENDIF
	IF (PointType.EQ.313) THEN
            TEXT=
     *	  ' Multiplier = 1 :  a=?                                  '
	ENDIF
	IF (PointType.EQ.314) THEN
	   TEXT=
     *    ' Multiplier = 1(?) a=?                                  '
	ENDIF
C  Hopf
	IF (PointType.EQ.321) THEN
            WRITE (TEXT,3)
3	    FORMAT (' Hopf   ',48X)
	ENDIF
	IF (PointType.EQ.322) THEN
            WRITE (TEXT,4) 
4	    FORMAT (' Hopf (?)',47X)
	ENDIF
	IF (PointType.EQ.323) THEN
            WRITE (TEXT,3)
	ENDIF
	IF (PointType.EQ.324) THEN
            WRITE (TEXT,4) 
	ENDIF
	IF (PointType.EQ.325) THEN
	   TEXT=
     *    ' Neutral saddle                                         '
	ENDIF
	IF (PointType.EQ.326) THEN
	   TEXT=
     *    ' Neutral saddle (?)                                     '
	ENDIF
C  Multiplier = -1
	IF (PointType.EQ.331) THEN
	   TEXT=
     *    ' Multiplier = -1                                        '
	ENDIF
	IF (PointType.EQ.332) THEN
	   TEXT=
     *    ' Multiplier = -1 (?)                                    '
	ENDIF
C  Double multiplier
	IF (PointType.EQ.341) THEN
	   TEXT=
     *    ' Double multiplier                                      '
	ENDIF
	IF (PointType.EQ.342) THEN
	   TEXT=
     *    ' Double multiplier (?)                                  '
	ENDIF
	RETURN
C----------------------------------------------------------------------------
C   Fold curve
C
3002	CONTINUE
C   Neutrality
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
C   Multiplier = -1
	IF (PointType.EQ.321) THEN
            WRITE (TEXT,9) BifVal(1)
9	    FORMAT (' Multiplier = -1 :  a=',G13.6,20X)	
	ENDIF
	IF (PointType.EQ.322) THEN
            WRITE (TEXT,10) BifVal(1)
10          FORMAT (' Multiplier = -1(?) a=',G13.6,20X)
	ENDIF
	IF (PointType.EQ.323) THEN
            TEXT=
     *	  ' Multiplier = -1 : a=?                                  '
	ENDIF
	IF (PointType.EQ.324) THEN
	   TEXT=
     *    ' Multiplier = -1(?) a=?                                 '
	ENDIF
C   Cusp
	IF (PointType.EQ.331) THEN
	   TEXT=
     *	  ' Cusp                                                   '
	ENDIF
	IF (PointType.EQ.332) THEN
	   TEXT=
     *	  ' Cusp(?)                                                '
	ENDIF
C  Double multiplier
	IF (PointType.EQ.341) THEN
           WRITE (TEXT,7) BifVal(1)
7	   FORMAT (' Double multiplier : a=',G13.6,20X)
	ENDIF
	IF (PointType.EQ.342) THEN
           WRITE (TEXT,8) BifVal(1)
8	   FORMAT (' Double multiplier(?) a=',G13.6,19X)
	ENDIF
	IF (PointType.EQ.343) THEN
	   TEXT=
     *	  ' Double multiplier : a=?                                '
	ENDIF
	IF (PointType.EQ.344) THEN
	   TEXT=
     *	  ' Double multiplier(?) a=?                               '
	ENDIF
	RETURN
C----------------------------------------------------------------------------
C   Hopf curve
C
3003	CONTINUE
C   Multiplier = 1
	IF (PointType.EQ.311) THEN
           WRITE (TEXT,1) BifVal(1)
	ENDIF
	IF (PointType.EQ.312) THEN
            WRITE (TEXT,2) BifVal(1)
	ENDIF
	IF (PointType.EQ.313) THEN
	   TEXT=
     *	  ' Multiplier = 1 :  a=?                                  '
	ENDIF
	IF (PointType.EQ.314) THEN
	   TEXT=
     *	  ' Multiplier = 1(?) a=?                                  '
	ENDIF
C  Multiplier = -1
	IF (PointType.EQ.321) THEN
	   TEXT=
     *    ' Multiplier = -1                                        '
	ENDIF
	IF (PointType.EQ.322) THEN
	   TEXT=
     *    ' Multiplier = -1 (?)                                    '
	ENDIF
C  Double multiplier
	IF (PointType.EQ.331) THEN
	   TEXT=
     *    ' Double multiplier                                      '
	ENDIF
	IF (PointType.EQ.332) THEN
	   TEXT=
     *    ' Double multiplier (?)                                  '
	ENDIF
	RETURN
C----------------------------------------------------------------------------
C   Flip curve
C
 3004 	CONTINUE
C  Multiplier = 1     
	IF (PointType.EQ.311) THEN
            WRITE (TEXT,1) BifVal(1)
	ENDIF
	IF (PointType.EQ.312) THEN
            WRITE (TEXT,2) BifVal(1)
	ENDIF
	IF (PointType.EQ.313) THEN
            TEXT=
     *	  ' Multiplier = 1 :  a=?                                  '
	ENDIF
	IF (PointType.EQ.314) THEN
	   TEXT=
     *    ' Multiplier = 1(?) a=?                                  '
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
C  Double multiplier
	IF (PointType.EQ.331) THEN
	   TEXT=
     *    ' Double multiplier                                      '
	ENDIF
	IF (PointType.EQ.332) THEN
	   TEXT=
     *    ' Double multiplier (?)                                  '
	ENDIF
	RETURN
C----------------------------------------------------------------------------	
C  Double Multiplier curve
C
 3005	CONTINUE
C  Multiplier = 1
         IF (PointType.EQ.311) THEN
            WRITE (TEXT,1) BifVal(1)
	ENDIF
	IF (PointType.EQ.312) THEN
            WRITE (TEXT,2) BifVal(1)
	ENDIF
	IF (PointType.EQ.313) THEN
            TEXT=
     *	  ' Multiplier = 1 :  a=?                                  '
	ENDIF
	IF (PointType.EQ.314) THEN
	   TEXT=
     *    ' Multiplier = 1(?) a=?                                  '
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
C  Multiplier = -1
	IF (PointType.EQ.331) THEN
	   TEXT=
     *    ' Multiplier = -1                                        '
	ENDIF
	IF (PointType.EQ.332) THEN
	   TEXT=
     *    ' Multiplier = -1 (?)                                    '
	ENDIF
        RETURN  
C----------------------------------------------------------------------------
C   Double (+1) curve
C
3006	CONTINUE
C   Cusp
	IF (PointType.EQ.311) THEN
            TEXT=
     *    ' Cusp                                                   '
	ENDIF
	IF (PointType.EQ.312) THEN
	   TEXT=
     *    ' Cusp(?)                                                '
	ENDIF
C  Multiplier = -1
	IF (PointType.EQ.321) THEN
	   TEXT=
     *    ' Multiplier = -1                                        '
	ENDIF
	IF (PointType.EQ.322) THEN
	   TEXT=
     *    ' Multiplier = -1 (?)                                    '
	ENDIF
	RETURN
C----------------------------------------------------------------------------
C   Double (-1) curve
C
3007	CONTINUE
C   Multiplier = 1
	IF (PointType.EQ.311) THEN
           WRITE (TEXT,1) BifVal(1)
	ENDIF
	IF (PointType.EQ.312) THEN
            WRITE (TEXT,2) BifVal(1)
	ENDIF
	IF (PointType.EQ.313) THEN
	   TEXT=
     *	  ' Multiplier = 1 :  a=?                                  '
	ENDIF
	IF (PointType.EQ.314) THEN
	   TEXT=
     *	  ' Multiplier = 1(?) a=?                                  '
	ENDIF
	RETURN
C----------------------------------------------------------------------------
C  Fold + Hopf curve
C
3008	CONTINUE
C   Multiplier = -1
	IF (PointType.EQ.311) THEN
            WRITE (TEXT,9) BifVal(1)
	ENDIF
	IF (PointType.EQ.312) THEN
            WRITE (TEXT,10) BifVal(1)
	ENDIF
	IF (PointType.EQ.313) THEN
            TEXT=
     *	  ' Multiplier = -1 : a=?                                  '
	ENDIF
	IF (PointType.EQ.314) THEN
	   TEXT=
     *    ' Multiplier = -1(?) a=?                                 '
	ENDIF
C  Cusp
	IF (PointType.EQ.321) THEN
	   TEXT=
     *    ' Cusp                                                   '
	ENDIF
	IF (PointType.EQ.322) THEN
	   TEXT=
     *    ' Cusp(?)                                                '
	ENDIF
	RETURN
C----------------------------------------------------------------------------
C   Flip + Hopf curve
C
3009	CONTINUE
C  Multiplier = 1
         IF (PointType.EQ.311) THEN
            WRITE (TEXT,1) BifVal(1)
	ENDIF
	IF (PointType.EQ.312) THEN
            WRITE (TEXT,2) BifVal(1)
	ENDIF
	IF (PointType.EQ.313) THEN
            TEXT=
     *	  ' Multiplier = 1 :  a=?                                  '
	ENDIF
	IF (PointType.EQ.314) THEN
	   TEXT=
     *    ' Multiplier = 1(?) a=?                                  '
	ENDIF
	RETURN
C----------------------------------------------------------------------------
C   Fold + Flip curve
C
 3010	CONTINUE
C   Neutrality
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
C  Cusp
	IF (PointType.EQ.321) THEN
	   TEXT=
     *    ' Cusp                                                   '
	ENDIF
	IF (PointType.EQ.322) THEN
	   TEXT=
     *    ' Cusp(?)                                                '
	ENDIF
	RETURN
C----------------------------------------------------------------------------
C   Cusp curve
C
3011	CONTINUE
C   Nutrality
	IF (PointType.EQ.311) THEN
	   TEXT=
     *    ' Neutrality                                             '
	ENDIF
	IF (PointType.EQ.312) THEN
	   TEXT=
     *    ' Neutrality(?)                                          '
	ENDIF
C  Multiplier = -1
	IF (PointType.EQ.321) THEN
	   TEXT=
     *    ' Multiplier = -1                                        '
	ENDIF
	IF (PointType.EQ.322) THEN
	   TEXT=
     *    ' Multiplier = -1 (?)                                    '
	ENDIF
	RETURN
	RETURN
C----------------------------------------------------------------------------
C   Fold + Extr  curve
C
3012	CONTINUE
C   Neutrality
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
C   Multiplier = -1
	IF (PointType.EQ.321) THEN
            WRITE (TEXT,9) BifVal(1)
	ENDIF
	IF (PointType.EQ.322) THEN
            WRITE (TEXT,10) BifVal(1)
	ENDIF
	IF (PointType.EQ.323) THEN
            TEXT=
     *	  ' Multiplier = -1 : a=?                                  '
	ENDIF
	IF (PointType.EQ.324) THEN
	   TEXT=
     *    ' Multiplier = -1(?) a=?                                 '
	ENDIF
C   Cusp
	IF (PointType.EQ.331) THEN
	   TEXT=
     *	  ' Cusp                                                   '
	ENDIF
	IF (PointType.EQ.332) THEN
	   TEXT=
     *	  ' Cusp(?)                                                '
	ENDIF
C  Double multiplier
	IF (PointType.EQ.341) THEN
           WRITE (TEXT,7) BifVal(1)
	ENDIF
	IF (PointType.EQ.342) THEN
           WRITE (TEXT,8) BifVal(1)
	ENDIF
	IF (PointType.EQ.343) THEN
	   TEXT=
     *	  ' Double multiplier : a=?                                '
	ENDIF
	IF (PointType.EQ.344) THEN
	   TEXT=
     *	  ' Double multiplier(?) a=?                               '
	ENDIF
	RETURN
C----------------------------------------------------------------------------	
C   Hopf + Extr  curve
C
3013	CONTINUE
C   Multiplier = 1
	IF (PointType.EQ.311) THEN
           WRITE (TEXT,1) BifVal(1)
	ENDIF
	IF (PointType.EQ.312) THEN
            WRITE (TEXT,2) BifVal(1)
	ENDIF
	IF (PointType.EQ.313) THEN
	   TEXT=
     *	  ' Multiplier = 1 :  a=?                                  '
	ENDIF
	IF (PointType.EQ.314) THEN
	   TEXT=
     *	  ' Multiplier = 1(?) a=?                                  '
	ENDIF
C  Multiplier = -1
	IF (PointType.EQ.321) THEN
	   TEXT=
     *    ' Multiplier = -1                                        '
	ENDIF
	IF (PointType.EQ.322) THEN
	   TEXT=
     *    ' Multiplier = -1 (?)                                    '
	ENDIF
C  Double multiplier
	IF (PointType.EQ.331) THEN
	   TEXT=
     *    ' Double multiplier                                      '
	ENDIF
	IF (PointType.EQ.332) THEN
	   TEXT=
     *    ' Double multiplier (?)                                  '
	ENDIF
	RETURN
C----------------------------------------------------------------------------
C   Flip + Extr curve
C
 3014	CONTINUE
C  Multiplier = 1     
	IF (PointType.EQ.311) THEN
            WRITE (TEXT,1) BifVal(1)
	ENDIF
	IF (PointType.EQ.312) THEN
            WRITE (TEXT,2) BifVal(1)
	ENDIF
	IF (PointType.EQ.313) THEN
            TEXT=
     *	  ' Multiplier = 1 :  a=?                                  '
	ENDIF
	IF (PointType.EQ.314) THEN
	   TEXT=
     *    ' Multiplier = 1(?) a=?                                  '
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
C  Double multiplier
	IF (PointType.EQ.331) THEN
	   TEXT=
     *    ' Double multiplier                                      '
	ENDIF
	IF (PointType.EQ.332) THEN
	   TEXT=
     *    ' Double multiplier (?)                                  '
	ENDIF
	RETURN
        END 
