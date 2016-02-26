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

C
	SUBROUTINE ShowIt (NITER,EPSN,ANORMF)
        IMPLICIT INTEGER*2 (I-N)
C
c        INCLUDE 'KEYDEF.DAT'	
C
        REAL*8       EPSN,ANORMF
        INTEGER*2    NITER,MESSAG
        INTEGER*2    EVNT
        LOGICAL*2    KEYEVE,WAIT
	CHARACTER*56 TEXT

        INTEGER*2      KEYFLG,KEYCODE
        COMMON/KEYFLG/ KEYFLG,KEYCODE

        COMMON/MESSG/ MESSAG

        IF (MESSAG.EQ.0) RETURN
C
C	CALL ClearMsg
c
        TEXT=
     *   '                                                        ' 

C    Output the message

        IF (NITER.EQ.0) THEN
            WRITE (TEXT,1) ANORMF
 1          FORMAT
     *('   Newton iterations started:  FNORM = ',G9.2,8X)
        ELSE          
            WRITE (TEXT,2) NITER,EPSN,ANORMF
 2          FORMAT
     *('   Iteration ',I2,'  Accuracy =',G9.2,'  FNORM =',G9.2,2X)
        ENDIF
c---         CALL ShowTe(TEXT)

c---         IF (MESSAG.EQ.2) THEN
c---            WAIT=.TRUE.
c---            IF (KEYEVE(WAIT,EVNT)) RETURN
c---         ENDIF

c         WAIT=.FALSE.
c 10      IF (MESSAG.EQ.2) WAIT=.TRUE.
c         IF (KEYEVE(WAIT,EVNT)) CONTINUE
c         SELECT CASE (EVNT)
c            CASE (KNonStop) 
c               MESSAG=1
c               RETURN
c            CASE (KYesStop)
c               MESSAGE=2
c               GOTO 10 
c            CASE DEFAULT
c               KEYFLG=1
c               KEYCODE=EVNT
c         END SELECT

         RETURN
         END	
C
	SUBROUTINE ShowBe (INDSTP,HCUR,HNEW)
        IMPLICIT INTEGER*2 (I-N)
C
c        INCLUDE 'KEYDEF.DAT'	
C
        REAL*8       HCUR,HNEW
        INTEGER*2    INDSTP,MESSAG
        INTEGER*2    EVNT
        LOGICAL*2    KEYEVE,WAIT
	CHARACTER*56 TEXT

        INTEGER*2      KEYFLG,KEYCODE
        COMMON/KEYFLG/ KEYFLG,KEYCODE

        COMMON/MESSG/ MESSAG

        IF (MESSAG.EQ.0) RETURN
C
C	CALL ClearMsg
c
        TEXT=
     *   '                                                        ' 

C    Output the message

        IF (INDSTP.GT.0) THEN
            WRITE (TEXT,1) HCUR,HNEW
 1          FORMAT
     *('   Step is accepted:  Hold=',G11.4,'  Hnew=',G11.4)
        ELSE
            WRITE (TEXT,2) HCUR,HNEW
 2          FORMAT
     *('   Step is rejected:  Hold=',G11.4,'  Hnew=',G11.4)
        ENDIF

c---        CALL ShowTe(TEXT)

c---         IF (MESSAG.EQ.2) THEN
c---            WAIT=.TRUE.
c---            IF (KEYEVE(WAIT,EVNT)) RETURN
c---         ENDIF

c         WAIT=.FALSE.
c 10      IF (MESSAG.EQ.2) WAIT=.TRUE.
c         IF (KEYEVE(WAIT,EVNT)) CONTINUE
c         SELECT CASE (EVNT)
c            CASE (KNonStop) 
c               MESSAG=1
c               RETURN
c            CASE (KYesStop)
c               MESSAGE=2
c               GOTO 10 
c            CASE DEFAULT
c               KEYFLG=1
c               KEYCODE=EVNT
c         END SELECT

         RETURN
         END	

