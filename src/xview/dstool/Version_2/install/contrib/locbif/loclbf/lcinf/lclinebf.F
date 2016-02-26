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
	SUBROUTINE lcLineBF (N,M,L,X,P,Line,ActPar,H,E)
C
C   case: Periodic solutions
C
C
C       16.11.92
C       10.07.94
C
	IMPLICIT REAL*8 (A-H,O-Z)
        IMPLICIT INTEGER*2 (I-N)
	include 'lcinf.dim'
        INTEGER*2 Iorbit
	INTEGER*2 N,M,L,Line,IKRS(16),IKR,IAP(NACTMX)
	REAL*8    X(N),P(M),H,E,PMIN(NPARMX),PMAX(NPARMX),PP(NPARMX)
	LOGICAL*2 ActPar(M)
	COMMON/PPAR/ PP
        COMMON/LINBEE/ RR(NFMAX),RI(NFMAX)
        COMMON/NSTI/   NSTI
	COMMON/IORBIT/ Iorbit
        COMMON/IH/     IH

	DATA IKRS/-2,-1,0,1,2,3,4,11,12,13,14,15,16,17,18,19/
C
	IKR=IKRS(Line)
        NACT=0
	DO I=1,M
	   IF (ActPar(I)) NACT=NACT+1
	ENDDO
        NAP1=NACT
 
	IF (IKR.EQ.-2) THEN
C
C  ORBIT Call
           IF (NACT.NE.0) THEN
	      IEND=-6
              CALL Diagno (IEND)  
	      RETURN
           ENDIF	       

	   DO I=1,M
	      PP(I)=P(I)
	   ENDDO
	   DO I=1,N
	      RR(I)=0.0D0
	      RI(I)=0.0D0
	   ENDDO
           IF (Iorbit.EQ.-1) THEN
              CALL ORBITD(N,X,IH,IEND)
           ELSE
              CALL ORBITC(N,X,IH,IEND)
           ENDIF
	ELSE		
C
C  LINLBF Call
	   I=1
	   DO J=1,M
	      IF (ActPar(J)) THEN
                 IAP(I)=J
	         I=I+1
	      ENDIF
	   ENDDO
	   DO I=1,M
	      PMIN(I)=-1.0D32
	      PMAX(I)= 1.0D32
	   ENDDO
	   CALL LINLBF (IKR,N,X,M,P,PMIN,PMAX,NAP1,IAP,H,E,IEND)    
        ENDIF
	CALL Diagno (IEND)
	RETURN
	END
	      		   
	
