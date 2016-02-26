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
        SUBROUTINE lcOUTPT(NK,XP,NPC,T,ISTOP)
C
C       16.11.92
C       10.09.93
C       10.07.94
C
        IMPLICIT INTEGER*2 (I-N)
	IMPLICIT REAL*8 (A-H,O-Z)
	include 'lcinf.dim'
	DIMENSION XP(1)
	DIMENSION X(0:NFMAX),X0(NFMAX),G(IUFMAX),BifVal(NBFVL)
	INTEGER*2      K,ILine,NVar,ID(NRVC),Line,IX,IY,PointType
	LOGICAL*2      ACTIVE(NRVC),STATE,INST,ActPar(NPARMX)
        CHARACTER*6    NC(NRVC)
	CHARACTER*13   VC(NRVC),PC(NRPC),LIM(4)
        CHARACTER*6    FUNNAM
C
	COMMON/NMLIT/    N,M,L,IT
	COMMON/PPAR/   P(NPARMX)
C  Really RR and RI are RMOD and ARG
	COMMON/LINBEE/ RR(NFMAX),RI(NFMAX)
	COMMON/INTEG/  K,ILine,NVar,ID,Line,IX,IY
	COMMON/LOGIC/  ACTIVE,STATE,INST
	COMMON/H/      H
	COMMON/IH/     IH
        COMMON /CHAR/   NC,VC,PC,LIM
C
	X(0)=T
	DO 10 I=1,N
	   X(I)=XP(I)
	   X0(I)=XP(I)
 10     CONTINUE
	DO 20 J=1,M
	   ActPar(J)=ACTIVE(J+N)
 20     CONTINUE
	DO 30 I=0,L-1
           FUNNAM=NC(N+M+I+1)
           CALL FUNADD(FUNNAM,IEND,X,G(I+1))
           IF (IEND.NE.0) CALL USERFUN(I,IEND,X,G(I+1))
 30     CONTINUE
C
	CALL CODEBF (PointType,BifVal)
	IF (PointType/100.EQ.1) PointType=0
C
	Npt=NPC
	IF (IH.LE.0) Npt=-Npt
C	
	CALL OUTP (0,N,M,L,X0,P,G,T,RR,RI,BifVal,
     *                    Line,ActPar,PointType,Npt,IEND)
	IF (IEND.EQ.1) ISTOP=100
C
c        CALL ClearMsg
	RETURN
	END
