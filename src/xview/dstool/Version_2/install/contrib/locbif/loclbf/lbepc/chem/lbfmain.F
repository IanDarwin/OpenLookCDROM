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
      integer*2 mflag
      common /mswtch/ mflag
C
C   Version for Equilibrium Points (ep)
      mflag=1
C
      CALL InitData
        CALL Locbif
      STOP
      END
 	SUBROUTINE InitData
C
C      07.12.92
C
        IMPLICIT INTEGER*2 (I-N)
	include 'lbepc.dim'
        parameter (Maxltp=17)
C
	INTEGER*2      I,N,M,L,IT
C
	INTEGER*2      K,ID(NRVC),LINE,IX,IY,IC
        INTEGER*2      ColorType,FillColor,IACT(NRVC),ISTATE,ISTOP
	CHARACTER*6    NC(NRVC),dummy
        CHARACTER*1    CHAR1,IPACT(NPARMX)
	CHARACTER*27   ddummy
	REAL*8         RVC(NRVC),RPC(NRPC),RLIM(4)
C
      RLIM(1)=0.
      RLIM(2)=0.
      RLIM(3)=0.
      RLIM(4)=0.
      K=1
      IX=1
      IY=1
      ColorType=1
      FillColor=1
      ISTATE=0
      ISTOP=0
C
cC number of phase variables and their names
c      OPEN (1,FILE='ts.dat')
c      READ (1,'(I3)') N
c      DO 10 i=1,N
c        READ (1,'(A6)') NC(i)
c 10   CONTINUE
cC number of parameters and their names
c      READ (1,'(I3)') M
c      DO 20 i=1,M
c        READ (1,'(A6)') NC(i+N)
c 20   CONTINUE
cC number of functions and their names
c      READ (1,'(I3)') L
c      IF (L.GT.0) THEN
c        DO 30 i=1,L
c          READ (1,'(A6)') NC(i+N+M)
c 30     CONTINUE
c      ENDIF
cC number of common variables and their names
c      READ (1,'(I3)') IC
c      IF (IC.GT.0) THEN
c        DO 40 i=1,IC
c          READ (1,'(A6)') dummy
c 40     CONTINUE
c      ENDIF
cC time variable and its name
c      READ (1,'(I3)') IT
c      IF (IT.GT.0) THEN
c         READ (1,'(A6)') NC(N+M+L+1) 
c      ENDIF	
c      CLOSE (1)
C
	DO 50 I=1,NRVC
	   RVC(I)=0.0
 50     CONTINUE
	DO 55 I=1,NRPC
	   RPC(I)=0.0
 55     CONTINUE
      OPEN (1,FILE='initds.dat')
c     OPEN (2,FILE='initds.tst')
           READ (1,1011) N
c          write (2,1011) N
 1011      FORMAT(22X,I3)
        DO 60 I=1,N
           READ (1,1001) NC(I),RVC(I)
c          write (2,1001) NC(I),RVC(I)
 1001      FORMAT(3X,A6,1X,G25.17)
 60     CONTINUE
           READ (1,1011) M 
c          write (2,1011) M 
        DO 70 I=1,M
           READ (1,1002) NC(N+I),RVC(NFMAX+I),IPACT(I)
c          write (2,1002) NC(N+I),RVC(NFMAX+I),IPACT(I)
 1002      FORMAT(3X,A6,1X,G25.17,3X,A1)
 70     CONTINUE
           READ (1,1011) L 
c          write (2,1011) L 
 	IF (L.GT.0) THEN
 	   DO 80 I=1,L
              READ (1,1003) NC(N+M+I)
c             write (2,1003) NC(N+M+I)
 1003      FORMAT(3X,A6)
 80        CONTINUE
 	ENDIF
           READ (1,*) 
c          write (2,*) 
           READ (1,1004) NC(N+M+L+1),RVC(NRVC)
c          write (2,1004) NC(N+M+L+1),RVC(NRVC)
 1004      FORMAT(3X,A6,1X,G25.17)
           READ (1,*) 
c          write (2,*) 
c          LINE=0
           DO 85 I=1,Maxltp
              READ (1,1005) ddummy,CHAR1
c             write (2,1005) ddummy,CHAR1
 1005         FORMAT(1X,A27,A1)
              IF (CHAR1.EQ.'+') LINE=I
 85        CONTINUE
c	READ (1,104) LINE
  104    FORMAT(1X,I3)
	DO 90 I=1,NRVC
           ID(I)=I
	   IACT(I)=0
	   IF (I.LE.N .OR. I.GT.N+M) GOTO 90
	   IF (IPACT(I-N).EQ.'+') IACT(I)=1
 90     CONTINUE
           READ (1,*) 
c          write (2,*) 
	   DO 95 I=1,13
	      READ (1,1001) dummy,RPC(I)
c             write (2,1001) dummy,RPC(I)
 95     CONTINUE
           READ (1,*)
c          write (2,*)
           DO 96 I=14,24
              READ (1,1001) dummy,RPC(I)
c             write (2,1001) dummy,RPC(I)
 96     CONTINUE
           READ (1,*) 
c          write (2,*) 
           DO 97 I=25,30
              READ (1,1001) dummy,RPC(I)
c             write (2,1001) dummy,RPC(I)
 97     CONTINUE
        CLOSE (1)
c       CLOSE (2)
C
C number of phase variables and their names
      OPEN (1,FILE='ts.dat')
      WRITE (1,'(I3)') N
      DO 10 i=1,N
        WRITE (1,'(A6)') NC(i)
 10   CONTINUE
C number of parameters and their names
      WRITE (1,'(I3)') M
      DO 20 i=1,M
        WRITE (1,'(A6)') NC(i+N)
 20   CONTINUE
C number of functions and their names
      WRITE (1,'(I3)') L
      IF (L.GT.0) THEN
        DO 30 i=1,L
          WRITE (1,'(A6)') NC(i+N+M)
 30     CONTINUE
      ENDIF
C number of common variables and their names
      IC=0
      WRITE (1,'(I3)') IC
c      IF (IC.GT.0) THEN
c        DO 40 i=1,IC
c          WRITE (1,'(A6)') dummy
c 40     CONTINUE
c      ENDIF
C time variable and its name
      IT=1
      WRITE (1,'(I3)') IT
      IF (IT.GT.0) THEN
         WRITE (1,'(A6)') NC(N+M+L+1)
      ENDIF
      CLOSE (1)
C
	OPEN (1,FILE='init.dat')
        WRITE (1,101) (RVC(I),I=1,NFMAX+NPARMX+IUFMAX+1)
 101    FORMAT(1X,3G25.17)
        WRITE (1,102) RPC
 102    FORMAT(1X,3G25.17)
        WRITE (1,103) RLIM
 103    FORMAT(1X,3G25.17)
	WRITE (1,104) LINE
        WRITE (1,105) IACT
 105    FORMAT(1X,20I3)
        WRITE (1,106) IX,IY
 106    FORMAT(1X,2I3)
        WRITE (1,107) K
 107    FORMAT(1X,I3)
        WRITE (1,108) ID
 108    FORMAT(1X,20I3)
        WRITE (1,109) ColorType,FillColor
 109    FORMAT(1X,2I3)
        WRITE (1,110) ISTATE
 110    FORMAT(1X,I3)
        WRITE (1,111) ISTOP
 111    FORMAT(1X,I3)
	CLOSE (1)
c	RETURN
C
	END	

