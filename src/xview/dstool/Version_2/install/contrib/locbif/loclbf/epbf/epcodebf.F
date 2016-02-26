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

      SUBROUTINE epCODEBF(IPTYPE,BIFVAL)
C
C   In this subprogram we produce special code
C      IPTYPE for every point on a curve     
C          29.05.1989
C          15.11.92
C          10.07.94
C
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER*2(I-N)
      include 'epbf.dim'
      DIMENSION BIFVAL(NBFVL)
C----
      COMMON /LINBFQ/ IPT(NPTYP), INDER, IVAL(NVALPT),ISTP
      COMMON /LINBFR/ VALPT(NVALPT)
C----
      IPT1=IPT(1)
      IPT2=IPT(2)
      IPT3=IPT(3)
      IPT4=IPT(4)
      DO 1 I=1,NBFVL
        BIFVAL(I)=VALPT(I)
 1    CONTINUE
      GO TO (1000,2000,3000,4000), IPT2
C
C===================================================================
 1000 CONTINUE
C   Regular point
      IPTYPE=100
      IF (IPT1.NE.2) GO TO 1100
C			    Hopf line
        IF (IPT3.EQ. 1) IPTYPE=101
C				 focus case, L1=BV(1)
        IF (IPT3.EQ. 2) IPTYPE=102  
C				 focus case, L1=?
        IF (IPT3.EQ.-1) IPTYPE=103
C				 saddle case
 1100 IF (IPT1.NE.1.OR.IPT1.NE.11) RETURN
C			    Mult2 or Bogdanov
        IF (IVAL(1).EQ.0)  IPTYPE=101
C				 a=BV(1)
        IF (IVAL(1).NE.0)  IPTYPE=102
C				 a=?
      RETURN
C
C===================================================================
 2000 CONTINUE
C   Selfcrossing
        IF (INDER.EQ.0)  IPTYPE=201
C			    O'K
        IF (INDER.NE.0)  IPTYPE=211
C			    Warning
      RETURN
C
C===================================================================
 3000 CONTINUE
C   Singular point
      IPTYPE=300
      IF (IPT1.EQ. 0) GO TO 3001
      IF (IPT1.EQ. 1) GO TO 3011
      IF (IPT1.EQ. 2) GO TO 3021
      IF (IPT1.EQ. 3) GO TO 3031
      IF (IPT1.EQ. 4) GO TO 3041
      IF (IPT1.EQ.11) GO TO 3111
      IF (IPT1.EQ.12) GO TO 3121
      IF (IPT1.EQ.13) GO TO 3131
      IF (IPT1.EQ.14) GO TO 3141
      IF (IPT1.EQ.15) GO TO 3151
      IF (IPT1.EQ.16) GO TO 3161
      IF (IPT1.EQ.17) GO TO 3171
      IF (IPT1.EQ.21) GO TO 3211
      IF (IPT1.EQ.22) GO TO 3221
      IF (IPT1.EQ.23) GO TO 3231
      IF (IPT1.EQ.24) GO TO 3241
      RETURN
C
 3001 CONTINUE
C---   Equilibrium   ---------------------------------
      IF (IPT3.NE.1) GO TO 3002
C		    Multiplicity condition
	IF (IVAL(1).EQ.0.AND.INDER.EQ.0) IPTYPE=311
C				   a=BV(1)
	IF (IVAL(1).EQ.0.AND.INDER.NE.0) IPTYPE=312
C				   Warning, a=BV(1)
	IF (IVAL(1).NE.0.AND.INDER.EQ.0) IPTYPE=313
C				   a=?
	IF (IVAL(1).NE.0.AND.INDER.NE.0) IPTYPE=314
C                                Warning, a=?
         RETURN
C
 3002 IF (IPT3.NE.2) GO TO 3005
C		    Nutrality condition
	IF (IPT4.EQ. 1.AND.INDER.EQ.0) IPTYPE=321
C				   focus case, L1=BV(1)
	IF (IPT4.EQ. 1.AND.INDER.NE.0) IPTYPE=322
C				   warning, focus case, L1=BV(1)
	IF (IPT4.EQ. 2.AND.INDER.EQ.0) IPTYPE=323
C				   focus case, L1=?
	IF (IPT4.EQ. 2.AND.INDER.NE.0) IPTYPE=324
C				   warning, focus case, L1=?
	IF (IPT4.EQ.-1.AND.INDER.EQ.0) IPTYPE=325
C				   saddle case
	IF (IPT4.EQ.-1.AND.INDER.NE.0) IPTYPE=326
C				   warning, saddle case
	RETURN
C
 3005 IF (IPT3.NE.3) RETURN
C		    Multiple eigenvalues
	IF (INDER.EQ.0) IPTYPE=331
C				   O'K
	IF (INDER.NE.0) IPTYPE=332
C				   Warning
	RETURN
C----------------------------------------------------
 3011 CONTINUE
C---   Mult 2    ------------------------------------
       IF (IPT3.NE.1) GO TO 3012
C		    Nutrality condition
	IF (IVAL(1).EQ.0.AND.INDER.EQ.0) IPTYPE=311
C				   a=BV(1)
	IF (IVAL(1).EQ.0.AND.INDER.NE.0) IPTYPE=312
C				   Warning, a=BV(1)
	IF (IVAL(1).NE.0.AND.INDER.EQ.0) IPTYPE=313
C				   a=?
	IF (IVAL(1).NE.0.AND.INDER.NE.0) IPTYPE=314
C                                Warning, a=?
         RETURN
 3012 IF (IPT3.NE.2) GO TO 3013
C		    Mult 3
         IF (INDER.EQ.0) IPTYPE=321
C				   O'K
	IF (INDER.NE.0) IPTYPE=322
C				   Warning
	RETURN
 3013 IF (IPT3.NE.3) RETURN
C		    Multiple eigenvalues
	IF (IVAL(1).EQ.0.AND.INDER.EQ.0) IPTYPE=331
C				   a=BV(1)
	IF (IVAL(1).EQ.0.AND.INDER.NE.0) IPTYPE=332
C				   Warning, a=BV(1)
	IF (IVAL(1).NE.0.AND.INDER.EQ.0) IPTYPE=333
C				   a=?
	IF (IVAL(1).NE.0.AND.INDER.NE.0) IPTYPE=334
C                                Warning, a=?
	RETURN
C----------------------------------------------------
 3021 CONTINUE
C---   Hopf   ---------------------------------------
       IF (IPT3.NE.1) GO TO 3022
C		    Multiplicity condition
	IF (IVAL(1).EQ.0.AND.INDER.EQ.0) IPTYPE=311
C				   a=BV(1)
	IF (IVAL(1).EQ.0.AND.INDER.NE.0) IPTYPE=312
C				   Warning, a=BV(1)
	IF (IVAL(1).NE.0.AND.INDER.EQ.0) IPTYPE=313
C				   a=?
	IF (IVAL(1).NE.0.AND.INDER.NE.0) IPTYPE=314
C                                Warning, a=?
         RETURN
 3022 IF (IPT3.NE.2) GO TO 3023
C		    L1=0
         IF (INDER.EQ.0) IPTYPE=321
C				   O'K
	IF (INDER.NE.0) IPTYPE=322
C				   Warning
	RETURN
 3023 IF (IPT3.NE.3) RETURN
C		    Multiple eigenvalues
	IF (INDER.EQ.0) IPTYPE=331
C				   O'K
	IF (INDER.NE.0) IPTYPE=332
C				   Warning
	RETURN
C
C----------------------------------------------------
 3031 CONTINUE
C---   L1=0   ---------------------------------------
      IF (IPT3.NE.1) RETURN
C		    Nutrality condition
         IF (INDER.EQ.0) IPTYPE=311
C				   O'K
	IF (INDER.NE.0) IPTYPE=312
C				   Warning
	RETURN
C----------------------------------------------------
 3041 CONTINUE
C---   Multiple eigenvalues	-----------------------
       IF (IPT3.NE.1) GO TO 3042
C		    Multiplicity condition
	IF (IVAL(1).EQ.0.AND.INDER.EQ.0) IPTYPE=311
C				   a=BV(1)
	IF (IVAL(1).EQ.0.AND.INDER.NE.0) IPTYPE=312
C				   Warning, a=BV(1)
	IF (IVAL(1).NE.0.AND.INDER.EQ.0) IPTYPE=313
C				   a=?
	IF (IVAL(1).NE.0.AND.INDER.NE.0) IPTYPE=314
C                                Warning, a=?
         RETURN
C
 3042 IF (IPT3.NE.2) RETURN
C		    Nutrality condition
         IF (INDER.EQ.0) IPTYPE=321
C				   O'K
	IF (INDER.NE.0) IPTYPE=322
C				   Warning
	RETURN
C----------------------------------------------------
 3111 CONTINUE
C---   Bogdanov	  -----------------------------------
      IF (IPT3.NE.1) RETURN
C		    Mult 3
         IF (INDER.EQ.0) IPTYPE=311
C				   O'K
	IF (INDER.NE.0) IPTYPE=312
C				   Warning
	RETURN
C----------------------------------------------------
 3121 CONTINUE
C---   Mult2 + Hopf  --------------------------------
      IF (IPT3.NE.1) RETURN
C		    Mult 3
         IF (INDER.EQ.0) IPTYPE=311
C				   O'K
	IF (INDER.NE.0) IPTYPE=312
C				   Warning
	RETURN
C
C----------------------------------------------------
 3131 CONTINUE
C---   Hopf 2	-------------------------------------
	RETURN
C----------------------------------------------------
 3141 CONTINUE
C---   Mult 3	-------------------------------------
      IF (IPT3.NE.1) RETURN
C		    Nutrality condition
         IF (INDER.EQ.0) IPTYPE=311
C				   O'K
	IF (INDER.NE.0) IPTYPE=312
C				   Warning
	RETURN
C
C----------------------------------------------------
 3151 CONTINUE
C---   Hopf + L1=0   --------------------------------
	RETURN
C----------------------------------------------------
 3161 CONTINUE
C---Mult 2 + Degeneracy
      IF (IPT3.NE.1) RETURN
C		    Nutrality condition
	IF (IVAL(1).EQ.0.AND.INDER.EQ.0) IPTYPE=311
C				   a=BV(1)
	IF (IVAL(1).EQ.0.AND.INDER.NE.0) IPTYPE=312
C				   Warning, a=BV(1)
	IF (IVAL(1).NE.0.AND.INDER.EQ.0) IPTYPE=313
C				   a=?
	IF (IVAL(1).NE.0.AND.INDER.NE.0) IPTYPE=314
C                                Warning, a=?
	RETURN
C----------------------------------------------------
 3171 CONTINUE
C---   Hopf + Degeneracy   --------------------------
      IF (IPT3.NE.1) RETURN
C		    Multiplicity condition
	IF (IVAL(1).EQ.0.AND.INDER.EQ.0) IPTYPE=311
C				   a=BV(1)
	IF (IVAL(1).EQ.0.AND.INDER.NE.0) IPTYPE=312
C				   Warning, a=BV(1)
	IF (IVAL(1).NE.0.AND.INDER.EQ.0) IPTYPE=313
C				   a=?
	IF (IVAL(1).NE.0.AND.INDER.NE.0) IPTYPE=314
C                                Warning, a=?
	RETURN
C----------------------------------------------------
 3211 CONTINUE
C---   Bogdanov + Mult 3   --------------------------
	RETURN
C----------------------------------------------------
 3221 CONTINUE
C---   Hopf + Mult 3	-----------------------------
	RETURN
C----------------------------------------------------
 3231 CONTINUE
C---   Bogdanov + Extr   ----------------------------
	RETURN
C----------------------------------------------------
 3241 CONTINUE
C---   Hopf + Mult 2 +Extr   ------------------------
         RETURN
C===================================================================
 4000 CONTINUE
C   Extremum
C	      Parameter's number - BV(1),  Extremal value - BV(2)
        IF (IPT3.GT.0.AND.INDER.EQ.0)  IPTYPE=401
C			   Maximum
        IF (IPT3.LT.0.AND.INDER.EQ.0)  IPTYPE=402
C			   Minimum
        IF (IPT3.GT.0.AND.INDER.NE.0)  IPTYPE=411
C			   Maximum , warning
        IF (IPT3.LT.0.AND.INDER.NE.0)  IPTYPE=412
C			   Minimum , warning
      RETURN
      END
 
