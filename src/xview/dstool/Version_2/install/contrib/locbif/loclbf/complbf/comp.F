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

	SUBROUTINE Comput (IDIR)
C
C       15.11.92
C
        IMPLICIT INTEGER*2 (I-N)
	include 'complbf.dim'
C
C   IDIR=+1 - compute forward
C   IDIR=-1 - compute backward
C
	INTEGER*2 I
C
	INTEGER*2 N,M,L,IT
	REAL*8    X(NFMAX),P(NPARMX),H,E,EPSCRV,H0CRV
	LOGICAL*2 ActPar(NPARMX)
C
	INTEGER*2      IDIR,K,ILine,NVar,ID(NRVC),Line,IX,IY,INIT
c---	INTEGER*2      J,POS,J0,POS0
	CHARACTER*6    NC(NRVC)
	CHARACTER*13   VC(NRVC),PC(NRPC),LIM(4)
	REAL*8	        RVC(NRVC),RPC(NRPC),RLIM(4)
	LOGICAL*2      ACTIVE(NRVC),STATE,INST
C	
	COMMON/NMLIT/    N,M,L,IT
c---	COMMON/JPOS/   J,POS
	COMMON/INTEG/  K,ILine,NVar,ID,Line,IX,IY
	COMMON/CHAR/   NC,VC,PC,LIM
	COMMON/REAL/   RVC,RPC,RLIM
	COMMON/LOGIC/  ACTIVE,STATE,INST
	COMMON/H/      H
	COMMON/IH/     IH
	COMMON/EPSH0/  EPSCRV,H0CRV
        COMMON/INITV/  INIT
C 
        IF (INIT.NE.0) THEN
c---	      CALL TransA
	      CALL ReComp
c---	      J0=J
c---	      POS0=POS
c---c	      CALL OffSet
c---              CALL Trans
c---              CALL UpDate
c---	      J=J0
c---	      POS=POS0
c---c	      CALL Offset
c---              CALL ClearMsg
        ENDIF
C
	DO 10 I=1,N
	   X(I)=RVC(I)
 10     CONTINUE
	DO 20 I=1,M
	   P(I)=RVC(N+I)
	   ActPar(I)=ACTIVE(N+I)
 20     CONTINUE
	H=H0CRV*IDIR
	E=EPSCRV
	IH=IDIR
C
         CALL LineBF (N,M,L,X,P,Line,ActPar,H,E)

	RETURN
	END
	
