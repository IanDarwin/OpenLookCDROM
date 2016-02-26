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

      SUBROUTINE FUNADD(FUNNAM,IERR,X,FUNRES)
C         26.08.1991  
C         14.11.92
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER*2 (I-N)
      include 'linbf.dim'
      DIMENSION X(1)
      CHARACTER*6    FUNNAM
      COMMON /LINBFW/ FUNH(IFMAX),ARCLNG,RNPT
      COMMON /WRTO1/  NFIX1, NFIX
      COMMON /WRTO2/  DMOVE1, DMOVE
      COMMON /WRTO3/  COSI1, COSI2, COSI0, COSI3
      COMMON /WRTO4/  EPSC, EPSN
      COMMON /WRTO6/  ITC,ITNEWT
      COMMON /CORR1/  DET,COND,SGNDET
C
      IF (FUNNAM.EQ.'BIFCRS') THEN
C   BIFFN1
         FUNRES=DET*SGNDET
         GOTO 100       
      ENDIF 
C
      IF (FUNNAM.EQ.'BIFFN1') THEN
C   BIFFN1
         FUNRES=FUNH(1)
         GOTO 100       
      ENDIF 
C
      IF (FUNNAM.EQ.'BIFFN2') THEN
C   BIFFN2
         FUNRES=FUNH(2)
         GOTO 100
      ENDIF 
C
      IF (FUNNAM.EQ.'BIFFN3') THEN
C   BIFFN3
         FUNRES=FUNH(3)
         GOTO 100
      ENDIF 
C
      IF (FUNNAM.EQ.'ARCLNG') THEN
C   Arclengh
         FUNRES=ARCLNG
         GOTO 100
      ENDIF 
C
      IF (FUNNAM.EQ.'NPOINT') THEN
C   Number of a point on a curve 
         FUNRES=RNPT
         GOTO 100
      ENDIF 
C
      IF (FUNNAM.EQ.'NFIX  ') THEN
C   Current continuation index 
         FUNRES=NFIX1
         GOTO 100
      ENDIF 
C
      IF (FUNNAM.EQ.'NITER ') THEN
C   Number of newton iterations 
         FUNRES=ITNEWT
         GOTO 100
      ENDIF 
C
      IF (FUNNAM.EQ.'ACCNWT') THEN
C   Achived accuracy of newton procedure at current point 
         FUNRES=EPSN
         GOTO 100
      ENDIF 
C
      IF (FUNNAM.EQ.'HCUR  ') THEN
C   Current step-size 
         FUNRES=DMOVE1
         GOTO 100
      ENDIF 
C
      IF (FUNNAM.EQ.'HNEW  ') THEN
C   Chosen step-size 
         FUNRES=DMOVE
         GOTO 100
      ENDIF 
C
      IF (FUNNAM.EQ.'INT00 ' .OR. FUNNAM.EQ.'SINT00') THEN
C   Integral 0
	 IIIFUN=0
 	 RNRMIN=FUNCVL(IIIFUN)
         FUNRES=RNRMIN
         IF (FUNNAM.EQ.'SINT00' ) FUNRES=DSQRT(DABS(FUNRES))
         GOTO 100
      ENDIF 
C
      IF (FUNNAM.EQ.'INT01 ' .OR. FUNNAM.EQ.'SINT01') THEN
C   Integral 1
	 IIIFUN=1
 	 RNRMIN=FUNCVL(IIIFUN)
         FUNRES=RNRMIN
         IF (FUNNAM.EQ.'SINT01' ) FUNRES=DSQRT(DABS(FUNRES))
         GOTO 100
      ENDIF 
C
      IF (FUNNAM.EQ.'INT02 ' .OR. FUNNAM.EQ.'SINT02') THEN
C   Integral 2
	 IIIFUN=2
 	 RNRMIN=FUNCVL(IIIFUN)
         FUNRES=RNRMIN
         IF (FUNNAM.EQ.'SINT02' ) FUNRES=DSQRT(DABS(FUNRES))
         GOTO 100
      ENDIF 
C
      IF (FUNNAM.EQ.'INT03 ' .OR. FUNNAM.EQ.'SINT03') THEN
C   Integral 3
	 IIIFUN=3
 	 RNRMIN=FUNCVL(IIIFUN)
         FUNRES=RNRMIN
         IF (FUNNAM.EQ.'SINT03' ) FUNRES=DSQRT(DABS(FUNRES))
         GOTO 100
      ENDIF 
C
      IF (FUNNAM.EQ.'INT04 ' .OR. FUNNAM.EQ.'SINT04') THEN
C   Integral 4
	 IIIFUN=4
 	 RNRMIN=FUNCVL(IIIFUN)
         FUNRES=RNRMIN
         IF (FUNNAM.EQ.'SINT04' ) FUNRES=DSQRT(DABS(FUNRES))
         GOTO 100
      ENDIF 
C
      IF (FUNNAM.EQ.'INT05 ' .OR. FUNNAM.EQ.'SINT05') THEN
C   Integral 5
	 IIIFUN=5
 	 RNRMIN=FUNCVL(IIIFUN)
         FUNRES=RNRMIN
         IF (FUNNAM.EQ.'SINT05' ) FUNRES=DSQRT(DABS(FUNRES))
         GOTO 100
      ENDIF 
C
      IF (FUNNAM.EQ.'INT06 ' .OR. FUNNAM.EQ.'SINT06') THEN
C   Integral 6
	 IIIFUN=6
 	 RNRMIN=FUNCVL(IIIFUN)
         FUNRES=RNRMIN
         IF (FUNNAM.EQ.'SINT06' ) FUNRES=DSQRT(DABS(FUNRES))
         GOTO 100
      ENDIF 
C
      IF (FUNNAM.EQ.'INT07 ' .OR. FUNNAM.EQ.'SINT07') THEN
C   Integral 7
	 IIIFUN=7
 	 RNRMIN=FUNCVL(IIIFUN)
         FUNRES=RNRMIN
         IF (FUNNAM.EQ.'SINT07' ) FUNRES=DSQRT(DABS(FUNRES))
         GOTO 100
      ENDIF 
C
      IF (FUNNAM.EQ.'INT08 ' .OR. FUNNAM.EQ.'SINT08') THEN
C   Integral 8
	 IIIFUN=8
 	 RNRMIN=FUNCVL(IIIFUN)
         FUNRES=RNRMIN
         IF (FUNNAM.EQ.'SINT08' ) FUNRES=DSQRT(DABS(FUNRES))
         GOTO 100
      ENDIF 
C
      IF (FUNNAM.EQ.'INT09 ' .OR. FUNNAM.EQ.'SINT09') THEN
C   Integral 9
	 IIIFUN=9
 	 RNRMIN=FUNCVL(IIIFUN)
         FUNRES=RNRMIN
         IF (FUNNAM.EQ.'SINT09' ) FUNRES=DSQRT(DABS(FUNRES))
         GOTO 100
      ENDIF 
C
      IF (FUNNAM.EQ.'INT10 ' .OR. FUNNAM.EQ.'SINT10') THEN
C   Integral 10
	 IIIFUN=10
 	 RNRMIN=FUNCVL(IIIFUN)
         FUNRES=RNRMIN
         IF (FUNNAM.EQ.'SINT10' ) FUNRES=DSQRT(DABS(FUNRES))
         GOTO 100
      ENDIF 
C
C   CASE DEFAULT
      IERR=-1
      RETURN
C
 100  CONTINUE
      IERR=0
C
C      WRITE (6,101) N,M,L,IT,FUNNAM,FUNRES,(FUNH(I),I=1,5)
C 101  FORMAT(' N,M,L,IT=',4I4,2X,A6,2X,G11.4,/'  funh=',5G13.4)      
      RETURN
      END
