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

      SUBROUTINE RdInit (  filename )
C
C       19.10.94
C
      
      IMPLICIT INTEGER*2 (I-N)
      IMPLICIT REAL*8 (A-H,O-Z)
	include 'complbf.dim'
	parameter (NHRVC=NFMAX+NPARMX+IUFMAX+1)

      character*80   filename
      
      INTEGER*2 SOLDOT
      INTEGER*2      I,N,M,L,IT
      INTEGER*2      K,ILine,NVar,ID(NRVC),LINE,IX,IY,NCPAR
      INTEGER*2      ColorType,FillColor,IACT(NRVC),ISTATE,ISTOP
      CHARACTER*6    NC(NRVC),dummy
      CHARACTER*13   VC(NRVC),PC(NRPC),LIM(4)
      REAL*8         RVC(NRVC),RPC(NRPC),RLIM(4),HRVC(NHRVC)
      LOGICAL*2      ACTIVE(NRVC),STATE,COMP,STOP,AUTO,INST
       
      COMMON/IORBIT/ IORBIT
      COMMON/SOLDOT/ SOLDOT
      COMMON/SOS/ ISOUND
      COMMON/MAXNPT/ MAXNPT
      COMMON/IFLASH/ IFLASH
      COMMON/MESSG/  MESSAG
      COMMON/INITV/  INIT
      COMMON/EPSH0/  EPSCRV,H0CRV
      COMMON/NP123/  NP1,NP2,NP3
      COMMON/NMLIT/    N,M,L,IT	
      COMMON/INTEG/  K,ILine,NVar,ID,LINE,IX,IY
      COMMON/CHAR/   NC,VC,PC,LIM
      COMMON/REAL/   RVC,RPC,RLIM
      COMMON/LOGIC/  ACTIVE,STATE,INST
      COMMON/COMP/   COMP,STOP,AUTO
      COMMON/COLOR/  ColorType,FillColor
      COMMON/PARNUM/ NCPAR
 
C number of phase variables and their names

c     OPEN (1,FILE='ts.dat')
c     READ (1,'(I3)') N
c     DO 10 i=1,N
c10     READ (1,'(A6)') NC(i)

C number of parameters and their names

c     READ (1,'(I3)') M
c     DO 20 i=1,M
c20     READ (1,'(A6)') NC(i+N)

C number of functions and their names

c     READ (1,'(I3)') L
c     IF (L.GT.0) THEN
c       DO 30 i=1,L
c30       READ (1,'(A6)') NC(i+N+M)
c     ENDIF

C number of common variables and their names

c     READ (1,'(I3)') IC
c     IF (IC.GT.0) THEN
c       DO 40 i=1,IC
c40       READ (1,'(A6)') dummy
c     ENDIF

C time variable and its name

c     READ (1,'(I3)') IT
c     IF (IT.GT.0) THEN
c        READ (1,'(A6)') NC(N+M+L+1) 
c     ENDIF	
c     CLOSE (1)
 
      DO 50 I=1,NRVC
 50 	 RVC(I)=0.0

      OPEN (1,FILE=filename)
      READ (1,101) HRVC
      DO 60 I=1,N
 60      RVC(I)=HRVC(I)
 
      DO 70 I=1,M
 70      RVC(I+N)=HRVC(I+NFMAX)
 
      IF (L.GT.0) THEN
	 DO 80 I=1,L
 80         RVC(I+N+M)=HRVC(I+NFMAX+NFMAX)
      ENDIF

      RVC(N+M+L+1)=HRVC(NHRVC)
      READ (1,102) RPC
      READ (1,103) RLIM
      READ (1,104) LINE
      READ (1,105) IACT

      DO 90 I=1,NRVC
	ACTIVE(I)=.FALSE.
	IF (IACT(I).EQ.1) ACTIVE(I)=.TRUE.
 90   CONTINUE

      READ (1,106) IX,IY
      READ (1,107) K
      READ (1,108) ID
      READ (1,109) ColorType,FillColor
      READ (1,110) ISTATE
      STATE=.FALSE.
      IF (ISTATE.EQ.1) STATE=.TRUE.  
      READ (1,111) ISTOP
      STOP=.FALSE.
      IF (ISTOP.EQ.1) STOP=.TRUE.  
      CLOSE (1)
 
C   Write initial values to the COMMON blocks
 
      CALL InitNM
      DO 210 I=1,3*N+M+L+1
 210     WRITE (VC(I),'(G13.7)') RVC(I)
 
      DO 220 I=1,NCPAR
 220	 WRITE (PC(I),'(G13.7)') RPC(I)
 
      DO 230 I=1,4
 230	 WRITE (LIM(I),'(G13.7)') RLIM(I)

      H0CRV =RPC(1)
      EPSCRV=RPC(8)
      IORBIT=RPC(NP1+11)
      SOLDOT=RPC(NP1+NP2+1)
      ISOUND=RPC(NP1+NP2+2)
      IFLASH=RPC(NP1+NP2+3)
      MESSAG=RPC(NP1+NP2+4)
      MAXNPT=RPC(NP1+NP2+5)
      INIT  =RPC(NP1+NP2+6)

      RETURN
 
 101  FORMAT(1X,3G25.17)
 102  FORMAT(1X,3G25.17)
 103  FORMAT(1X,3G25.17)
 104  FORMAT(1X,I3)
 105  FORMAT(1X,20I3)
 106  FORMAT(1X,2I3)
 107  FORMAT(1X,I3)
 108  FORMAT(1X,20I3)
 109  FORMAT(1X,2I3)
 110  FORMAT(1X,I3)
 111  FORMAT(1X,I3)

      END	

