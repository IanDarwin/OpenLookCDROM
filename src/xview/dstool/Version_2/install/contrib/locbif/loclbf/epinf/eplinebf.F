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
	SUBROUTINE epLineBF (N,M,L,X,P,Line,ActPar,H,E)
C
C	15.11.92
C       10.07.94
C
	IMPLICIT REAL*8 (A-H,O-Z)
        IMPLICIT INTEGER*2 (I-N)
	include 'epinf.dim'
	INTEGER*2 N,M,L,Line,IKRS(17),IKR,IAP(NACTMX)
	REAL*8    X(N),P(M),H,E,PMIN(NPARMX),PMAX(NPARMX),PP(NPARMX)
	LOGICAL*2 ActPar(M)
	COMMON/PPAR/ PP
        COMMON/LINBFE/ RR(NFMAX),RI(NFMAX),IPDUMM
	COMMON/IH/     IH
 
	DATA IKRS/-2,-1,0,1,2,4,11,12,13,14,15,16,17,21,22,23,24/
C
	IKR=IKRS(Line)
        NACT=0
	DO 10 I=1,M
	   IF (ActPar(I)) NACT=NACT+1
 10     CONTINUE
        NAP1=NACT
	IEND=0

	IF (IKR.EQ.-2) THEN
C
C  ORBIT Call
           IF (NACT.NE.0) THEN
               IEND=-6
               CALL Diagno (IEND)  
               RETURN
           ENDIF	       

	   DO 20 I=1,M
	      PP(I)=P(I)
 20        CONTINUE
	   DO 30 I=1,N
	      RR(I)=0.0D0
	      RI(I)=0.0D0
 30        CONTINUE
           CALL ORBITC (N,X,IH,IEND)
	ELSE		
C
C  LINLBF Call
	   I=1
	   DO 40 J=1,M
	      IF (ActPar(J)) THEN
                 IAP(I)=J
	         I=I+1
	      ENDIF
 40        CONTINUE
	   DO 50 I=1,M
	      PMIN(I)=-1.0D32
	      PMAX(I)= 1.0D32
 50        CONTINUE
         CALL LINLBF (IKR,N,X,M,P,PMIN,PMAX,NAP1,IAP,H,E,IEND)    
        ENDIF
	CALL Diagno (IEND)
	RETURN
	END
	      		   
	
