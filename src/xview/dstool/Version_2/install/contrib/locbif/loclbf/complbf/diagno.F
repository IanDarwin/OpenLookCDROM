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

	SUBROUTINE Diagno (IEND)
C
C       19.10.94
C
        IMPLICIT INTEGER*2 (I-N)
	include 'complbf.dim'

	REAL*8	        RVC(NRVC),RPC(NRPC),RLIM(4)
	REAL*8         X(NFMAX),P(NPARMX),G(IUFMAX),RR(NFMAX),
     *	               RI(NFMAX),BifVal(NBFVL),T
	CHARACTER*56 TEXT
        INTEGER*2  N,M,L
	INTEGER*2  IEND
        COMMON/NMLIT/    N,M,L,IT
	COMMON/REAL/   RVC,RPC,RLIM
	COMMON/BFVL/   BifVal
        COMMON/NPT/    Npt
c
            TEXT=
     *    '                                                        '
C
	DO 10 I=1,N
	   X(I)=RVC(I)
 10     CONTINUE
	DO 20 I=1,M
	   P(I)=RVC(I+N)
 20     CONTINUE
	IF (L.GT.0) THEN
	   DO 30 I=1,L
	      G(I)=RVC(I+N+M)
 30        CONTINUE
	ENDIF
	T=RVC(N+M+L+1)
	DO 40 I=1,N
	   RR(I)=RVC(1+I+N+M+L)
	   RI(I)=RVC(1+I+2*N+M+L)
 40     CONTINUE
C
	IF (IEND.EQ.4.OR.IEND.EQ.2) THEN
            TEXT=
     *    ' Limit of parameter region                              '
           GO TO 100
	ENDIF
	IF (IEND.EQ.3) THEN
            TEXT=
     *    ' Closed curve                                           '
           GO TO 100
	ENDIF
	IF (IEND.EQ.1) THEN
            TEXT=
     *    ' Cannot continue numerical integration                  '

           GO TO 100
	ENDIF
	IF (IEND.EQ.-1) THEN
            TEXT=
     *    ' Error in initial data                                  '
           GO TO 100
	ENDIF
	IF (IEND.EQ.-2) THEN
            TEXT=
     *    ' Cannot find the first point                            '
           GO TO 100
	ENDIF
	IF (IEND.EQ.-3) THEN
            TEXT=
     *    ' Current step size is too small                         '

           GO TO 100
	ENDIF
	IF (IEND.EQ.-4) THEN
            TEXT=
     *    ' Cannot find tangent vector at the first point          '
           GO TO 100
	ENDIF
	IF (IEND.EQ.-5)  THEN
	    TEXT=
     *    ' Computations terminated                                '
           GO TO 100
	ENDIF
	IF (IEND.EQ.-6) THEN
            TEXT=
     *    ' Incorrect number of active parameters                  '
           GO TO 100
	ENDIF
	IF (IEND.EQ.-7) THEN
            TEXT=
     *    ' Undefined function values at the initial point         '
           GO TO 100
	ENDIF
	IF (IEND.EQ.-8) THEN
            TEXT=
     *    ' Cannot find Jacoby matrix at the initial point         '
           GO TO 100
	ENDIF
	IF (IEND.EQ.-9) THEN
            TEXT=
     *    ' Cannot find tangent vector at the initial point        '
           GO TO 100
	ENDIF
        RETURN
C
100     CONTINUE
        CALL EndMes (TEXT,IEND,
     *               N,M,L,X,P,G,T,RR,RI,BifVal)	
	RETURN
	END

