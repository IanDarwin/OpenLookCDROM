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

	SUBROUTINE EndMes (TEXT,IEND,
     *                     N,M,L,X,P,G,T,RR,RI,BifVal)
C
C       19.10.94
C
        IMPLICIT INTEGER*2 (I-N)
	include 'complbf.dim'

c---	INCLUDE 'KEYDEF.DAT'
c---        INTEGER*2    Ptype
	REAL*8       X(N),P(M),G(L),RR(N),RI(N),BifVal(5),T
	CHARACTER*56 TEXT
	INTEGER*2    MAXNPT,EVNT,PointType
	LOGICAL*2    ACTIVE(NRVC),STATE,INST
	LOGICAL*2    KEYEVE,WAIT,COMP,STOP,AUTO,WRT,SOS
	LOGICAL*2    UPDT

        COMMON/MAXNPT/ MAXNPT
	COMMON/LOGIC/  ACTIVE,STATE,INST
	COMMON/COMP/ COMP,STOP,AUTO
        COMMON/NPT/ Npt
	COMMON/WRT/ WRT
	COMMON/SOS/ SOS
        COMMON/UPDT/   UPDT
C
        IF (WRT) CALL ProtTe(TEXT)
c---
c---        IF (SOS) THEN
c---           Ptype=1
c---           CALL MSound(Ptype)
c---	ENDIF
c---
c---        CALL ShowT1(TEXT)
c---C
c---           IF (.NOT.UPDT) THEN
c---              CALL Trans
c---              CALL UpDate
c---           ENDIF
c---
c---	   WAIT=.FALSE.
c---	   IF (IEND.EQ.-3.OR.IEND.EQ.-1.OR.IEND.EQ.3
c---     *     .OR.IEND.EQ.1) WAIT=.TRUE.
c--- 100	   CONTINUE
c---	   IF (KEYEVE(WAIT,EVNT)) CONTINUE
c---	   IF (WAIT) CALL ClearMsg
c---	   IF (IEND.EQ.-5) EVNT=KEscape
c---c	   IF (EVNT.EQ.KEscape.OR.EVNT.EQ.KInsert) THEN
c---              IF (EVNT.EQ.KInsert) INST=.TRUE. 
c---C***	      IF (COMP.AND.ABS(Npt).LT.MAXNPT) THEN
c---	      IF (COMP.AND.ABS(Npt).LE.MAXNPT) THEN
                 IF (Npt.GT.0) THEN
	            NCAN=7
                 ELSE
	            NCAN=8
	         ENDIF
	         PointType=-1
                 CALL Store (NCAN,PointType,Npt,N,M,L,
     *                       X,P,G,T,RR,RI,BifVal,IERR)
c---	      ENDIF
c---c           ENDIF 
c---        CALL FixPos
	IEND=1
        RETURN
	END		
