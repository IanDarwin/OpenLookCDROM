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

	SUBROUTINE ReComp
C
C       19.10.94
C
        IMPLICIT INTEGER*2 (I-N)
        IMPLICIT REAL*8(A-H,O-Z)
          include 'complbf.dim'

        INTEGER*2      IFN,IERR
        INTEGER*2      IERFS,IERDUM
        REAL*8         FUNRES,PCOORD(0:NFMAX)
	REAL*8         RVC(NRVC),RPC(NRPC),RLIM(4)
	REAL*8         P(NPARMX)
        CHARACTER*56   TEXT
	COMMON/NMLIT/    N,M,L,IT	
	COMMON/PPAR/   P
	COMMON/REAL/   RVC,RPC,RLIM
        COMMON/IERFS/  IERFS,IERDUM

	IERFS=0
	IFN=10
	DO 10 I=1,N
	   PCOORD(I)=RVC(I)
 10     CONTINUE
	PCOORD(0)=RVC(N+M+L+1)
	DO 20 I=1,M
	   P(I)=RVC(I+N)
 20     CONTINUE
	CALL USERFUN(IFN,IERR,PCOORD,FUNRES)
	IF (IERR.NE.-2. AND. IERFS.EQ.0) THEN
           IF (FUNRES.EQ.0.0D0) THEN
	      TEXT=
     *     ' Values initialized                                     '
c---              CALL ShowTe(TEXT)
c***
      WRITE (6,101) TEXT
 101  FORMAT(1X,A56)
c***          
              DO 30 I=1,N
                 RVC(I)=PCOORD(I)
 30           CONTINUE
              DO 40 I=1,M
                 RVC(I+N)=P(I)
 40           CONTINUE
              RVC(N+M+L+1)=PCOORD(0)
           ELSE
	      TEXT=
     *     ' Values not initialized                                  '
c---              CALL ShowTe(TEXT)
c***
      WRITE (6,101) TEXT
c***          
	   ENDIF
	ENDIF

      	RETURN
	END

