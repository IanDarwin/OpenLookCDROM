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
	SUBROUTINE OUTP (TYP,N,M,L,X,P,G,T,RR,RI,BifVal,
     *                    Line,ActPar,PointType,Npt,IEND)
C
C       19.10.94
C
        IMPLICIT INTEGER*2 (I-N)
	  include 'complbf.dim'
C
c---	INCLUDE 'KEYDEF.DAT'
C
        INTEGER*2 Data(5),SOS,SOLDOT
c---	INTEGER*2 PIX,PIY,x1,y1
	INTEGER*2 TYP,N,M,L,Line,PointType,Npt,IEND,Stype
	INTEGER*2 MAXNPT
	REAL*8    X(1),P(1),G(1),RR(1),RI(1),BifVal(1)
	REAL*8    BfVl(NBFVL),T
	LOGICAL*2 ActPar(M)
	LOGICAL*2 UPDT
	INTEGER*2 EVNT

C
        INTEGER*2      NP1,NP2,NP3
	INTEGER*2      K,ILine,NVar,ID(NRVC),LINETYPE,IX,IY
	CHARACTER*60   TEXT,HINT,TEXTCM

	REAL*8	       RVC(NRVC),RPC(NRPC),RLIM(4)
	LOGICAL*2      ACTIVE(NRVC),STATE,INST,KEYEVE,WAIT,COMP,STOP,
     *                 WRT,AUTO
	COMMON/INTEG/  K,ILine,NVar,ID,LINETYPE,IX,IY
	COMMON/REAL/   RVC,RPC,RLIM
	COMMON/BFVL/   BfVl
	COMMON/LOGIC/  ACTIVE,STATE,INST
	COMMON/COMP/   COMP,STOP,AUTO
        COMMON/WRT/    WRT
        COMMON/MAXNPT/ MAXNPT
	COMMON/SOS/    SOS
        COMMON/UPDT/   UPDT
        COMMON/SOLDOT/ SOLDOT
        COMMON/MESSG/  MESS
        COMMON/NP123/   NP1,NP2,NP3
        COMMON/MESBLK/ TEXTCM
	DATA HINT /
     *   '            Enter: Continue  F2: Options  Ins: Get point'/
C
C                                                    Write protocol (modified: mrm)
 	IF (WRT .and. (PointType.ne.0) ) 				
     *	   CALL Protoc (N,M,L,X,P,G,T,RR,RI,BifVal,
     *                  Line,ActPar,PointType,Npt,IEND)

C						      Modify common /REAL/
         IEND=0
	   DO 10 I=1,N
	      RVC(I)=X(I)
 10        CONTINUE
	   DO 20 I=1,M
              RVC(I+N)=P(I)
 20        CONTINUE
	   IF (L.GT.0) THEN
               DO 30 I=1,L
                  RVC(I+N+M)=G(I)
 30            CONTINUE
           ENDIF    
	   RVC(N+M+L+1)=T
           DO 40 I=1,N
	      RVC(1+I+N+M+L)=RR(I)
	      RVC(1+I+2*N+M+L)=RI(I)
c	      write(*,*) '---------'
c	      write(*,*) RVC(1+I+N+M+L), RVC(1+I+2*N+M+L)
 40        CONTINUE
	   DO 50 I=1,NBFVL
	      BfVl(I)=BifVal(I)
 50        CONTINUE
C	                    			     Write to files FSTOR
         IF (TYP.EQ.0) THEN
	    IF (Npt.GT.0) THEN
	       NCAN=7
            ELSE       
	       NCAN=8
	    ENDIF
            CALL Store (NCAN,PointType,Npt,N,M,L,
     *                  X,P,G,T,RR,RI,BifVal,IEND)
            IF (IEND.EQ.1) RETURN 
         ENDIF
C                                              Output of the curve
c---        CALL CurPlt(TYP,PointType,Npt,RLIM,SOLDOT,
c---     *              RVC(IX),RVC(IY),x1,y1,PIX,PIY)
C            
c---        UPDT=.FALSE.
C
C  Sound
C
c---      IF (SOS.NE.0) THEN
c---        Stype=0
c---        IF (PointType .NE. 0) Stype=1
c---        CALL MSound(Stype)
c---      ENDIF
C
C  Diagnostics
C
        IF (ABS(Npt).EQ.1 .AND. PointType.EQ.-1) THEN
           IEND=1
           RETURN
        ENDIF
C
	CALL Messag (TEXT,Npt,LineType,PointType,
     *                      N,M,L,BifVal,IEND)
	TEXTCM = TEXT
	TEXTCM(57:58) = '\0'

        IF (IEND.EQ.-1) THEN
           CALL EndMes (TEXT,IEND,N,M,L,X,P,G,T,RR,RI,BifVal)	
           RETURN
        ENDIF
C
C    Write message in protocol
C
        IF (WRT .and. (PointType.ne.0) ) CALL ProtTe(TEXT)
C
C    Draw marker
C
        IF (PointType.NE.0) THEN
c        IF (PointType.NE.0 .OR. ABS(Npt).EQ.1) THEN
c---           CALL Marker(PIX+x1,PIY+y1)
        ENDIF
c
C    Output the message

c---        IF (MESS.EQ.0) THEN
c---           CALL ShowTe (TEXT)
c---        ELSE
c---           IF (ABS(Npt).EQ.1 .OR. PointType. NE. 0)
c---     *     CALL ShowTe (TEXT)
c---        ENDIF
C
C    Symbol output
C
c---	IF (STATE .OR. STOP .OR. 
c---     *      PointType.NE.0 .OR. ABS(Npt).EQ.1) THEN
c---           IF ( .NOT. UPDT) THEN
c---              UPDT=.TRUE.
c---              CALL Trans
c---              CALL UpDate
c---              CALL MVTo (PIX,PIY)
c---           ENDIF
c---	ENDIF
c	IF (IEND.EQ.1) RETURN
C
C							Key processing
c---      WAIT=.FALSE.	
c---      IF (KEYEVE(WAIT,EVNT)) THEN
c---C:Escape or Insert
c---         IF (EVNT.EQ.KEscape.OR.EVNT.EQ.KInsert) GO TO 700
c---C:Pause
c---         IF (EVNT.EQ.KStop.OR.EVNT.EQ.KComp2) THEN
c---            IF (.NOT.UPDT) THEN
c---               CALL Trans
c---               CALL UpDate
c---            ENDIF
c---            IF (PointType.EQ.0.AND.EVNT.EQ.KStop) THEN
c---               WRITE (TEXT,1003) Npt
c--- 1003	       FORMAT (' Npt=',I4,45X)	
c---	       CALL ShowTe (TEXT)
c---	       CALL ShowHint (HINT)
c---            ENDIF
c---            WAIT=.TRUE.
c---	    IF (EVNT.EQ.KComp2) GO TO 11
c---         ENDIF
c---      ENDIF
c---C:Stop 
c---      IF ( .NOT. (PointType.EQ.0 .OR. AUTO)) WAIT=.TRUE.
c---      IF ( .NOT. (ABS(Npt).NE.1 .OR. AUTO)) WAIT=.TRUE.
c---      IF (STOP) WAIT=.TRUE.
c---C
c---C  Waiting and key processing
c---C
c---      IF ( .NOT. WAIT) RETURN
c--- 10   CONTINUE
c---      CALL MVTo (PIX,PIY)
c---      IF (KEYEVE(WAIT,EVNT)) THEN
c---C
c---C:Escape or Insert
c---         IF (EVNT.EQ.KEscape.OR.EVNT.EQ.KInsert) GO TO 700
c---	 IF (EVNT.EQ.KContinue) RETURN
c---C
c---         IF (TYP.EQ.0) THEN
c---            IF (Npt.GT.0 .AND.
c---     *         (EVNT.EQ.KComp4 .OR.
c---     *          EVNT.EQ.KContinue)) RETURN
c---            IF (Npt.LT.0 .AND.
c---     *         (EVNT.EQ.KComp_4 .OR.
c---     *          EVNT.EQ.KCtrlContinue .OR.
c---     *          EVNT.EQ.KContinue)) RETURN
c---         ELSE
c---            IF (Npt.GT.0 .AND.
c---     *         (EVNT.EQ.KBrowse1 .OR.
c---     *          EVNT.EQ.KContinue)) RETURN
c---            IF (Npt.LT.0 .AND.
c---     *         (EVNT.EQ.KBrowse_1 .OR.
c---     *          EVNT.EQ.KCtrlContinue .OR.
c---     *          EVNT.EQ.KContinue)) RETURN
c---         ENDIF
c--- 11	 CONTINUE	
c---	 IF (EVNT.EQ.KComp2) THEN 
c---
c---            Data (1) = 3
c---            IF (AUTO) Data (1) = 1
c---            IF (STOP) Data (1) = 2
c---            Data (2) = 1
c---            IF (SOLDOT .EQ. 1) Data(2) = 2
c---            Data (3) = 1
c---            IF (STATE)  Data(3) = 2
c---            Data (4) = 1
c---            IF (SOS .EQ. 1) Data(4) = 2
c---            Data (5) = 1
c---            IF (WRT) Data(5) = 2
c---            Set = 1
c---
c---	    CALL PauseOpt (Data,EVNT,WAIT,PIX,PIY)
c---
c---            STOP = .FALSE.
c---            IF (Data(1) .EQ. 2) STOP = .TRUE.
c---            AUTO = .FALSE.
c---            IF (Data(1) .EQ. 1) AUTO = .TRUE.
c---            SOLDOT = 1
c---            IF (Data(2) .EQ. 1) SOLDOT = 0               
c---            RPC(NP1+NP2+1) = SOLDOT
c---            SOS = 0
c---            IF (Data(4) .EQ. 2) SOS = 1
c---            RPC(NP1+NP2+2) = SOS
c---            WRT = .FALSE.
c---            IF (Data(5) .EQ. 2) WRT = .TRUE.
c---
c---            SELECT CASE (EVNT)
c---	       CASE (KComp4,KComp_4,KBrowse1,KBrowse_1)
c---	          RETURN
c---	       CASE DEFAULT
c---            END SELECT
c---	    CALL ShowHint (HINT)
c---	 ENDIF
c---      ENDIF
c---      GO TO 10
c---C
c---C                                       Escape or Insert processing
c---C
c--- 700  CONTINUE
c---      IF (EVNT.EQ.KInsert) INST=.TRUE.
c---      IF (ABS(Npt).LT.MAXNPT .AND. TYP.EQ.0) THEN
c---         IF (Npt.GT.0) THEN
c---            NCAN=7
c---         ELSE
c---            NCAN=8
c---         ENDIF
c---         PointType=-1
c---         CALL Store (NCAN,PointType,Npt,N,M,L,
c---     *               X,P,G,T,RR,RI,BifVal,IEND)
c---      ENDIF
c---      IEND=1
C
      RETURN
      END	
