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

	SUBROUTINE Protoc (N,M,L,X,P,G,T,RR,RI,BifVal,
     *                    Line,ActPar,PointType,Npt,IERR)
C
C       15.11.92
C
        IMPLICIT INTEGER*2 (I-N)
	include 'complbf.dim'
C
	INTEGER*2 N,M,L,Line,PointType,Npt,IERR,NCPAR,NAP
	REAL*8    X(1),P(1),G(1),RR(1),RI(1),BifVal(1),PACT(NACTMX)
	REAL*8    BfVl(NBFVL),T
        INTEGER*2 NCANP
	LOGICAL*2 ActPar(M)
        CHARACTER*7  NAMPAR1(NRPC)
        CHARACTER*20 LineType
	CHARACTER*56 TEXT
C
	CHARACTER*6    NC(NRVC),NACT(NACTMX)
	CHARACTER*13   VC(NRVC),PC(NRPC),LIM(4)
	REAL*8	       RVC(NRVC),RPC(NRPC),RLIM(4)
	LOGICAL*2      ACTIVE(NRVC),STATE,INST
	COMMON/CHAR/   NC,VC,PC,LIM
	COMMON/REAL/   RVC,RPC,RLIM
	COMMON/BFVL/   BfVl
	COMMON/LOGIC/  ACTIVE,STATE,INST
        COMMON/PARNUM/ NCPAR
        COMMON/NAMPAR/ NAMPAR1
C
        SAVE NCANP
        DATA NCANP/9/
C
        WRITE (NCANP,1001) Npt
1001    FORMAT (80('-'),/,' Npt=',I4,/)
        WRITE (NCANP,1002) (NC(I),X(I),I=1,N)
C
	NAP=0
	DO 10 I1=1,M
	   IF (ActPar(I1)) THEN
	      NAP=NAP+1
	      NACT(NAP)=NC(I1+N)
	      PACT(NAP)=P(I1)
            ENDIF
 10     CONTINUE
	WRITE (NCANP,1002) (NACT(I),PACT(I),I=1,NAP)
	IF (L.GT.0) WRITE (NCANP,1002) (NC(I+N+M),G(I),I=1,L)
	WRITE (NCANP,1004) NC(N+M+L+1),T
1004	FORMAT (1X,A6,'=',G13.5)
	WRITE (NCANP,1002) (NC(1+I+N+M+L),RR(I),I=1,N)
	WRITE (NCANP,1002) (NC(1+I+2*N+M+L),RI(I),I=1,N)
1002    FORMAT (3(1X,A6,'=',G13.5),/,3(1X,A6,'=',G13.5),/,
     *          3(1X,A6,'=',G13.5),/,1X,A6,'=',G13.5)       
        RETURN
C----------------------------------------------------------------
C
        ENTRY ProtTe(TEXT)
C
	WRITE (NCANP,101) TEXT
 101	FORMAT (1X,A56)
c***
	WRITE (6,101) TEXT
c***
        RETURN
C----------------------------------------------------------------
C
        ENTRY ProtO (N,M,L,LineType,IERR)
C
        OPEN (NCANP,FILE='result',
     *        ACCESS='APPEND',
     *        STATUS='UNKNOWN')
        WRITE (NCANP,700)
 700    FORMAT (80('*'))
        WRITE (NCANP,701) LineType,N,M,L
 701    FORMAT ('   Line type: ',A20,//,
     *          '   Number of variables:  N =',I2,/,
     *          '   Number of parameters: M =',I2,/,
     *          '   Number of functions:  L =',I2,/)    
        WRITE (NCANP,714)
 714    FORMAT ('   Initial point:',/)
	WRITE (NCANP,702) (NC(I),RVC(I),I=1,N)
	WRITE (NCANP,702) (NC(I+N),RVC(I+N),I=1,M)
 702    FORMAT (3(1X,A6,'=',G13.6),/,3(1X,A6,'=',G13.6),/,
     *          3(1X,A6,'=',G13.6),/,1X,A6,'=',G12.5)       
	NAP=0
	DO 20 I1=1,M
	   IF (ACTIVE(N+I1)) THEN
	      NAP=NAP+1
	      NACT(NAP)=NC(I1+N)
           ENDIF
 20     CONTINUE
        WRITE (NCANP,703) (NACT(I),I=1,NAP)
 703    FORMAT (/,'   Active parameters:',4(1X,A6))
        WRITE (NCANP,704) (NAMPAR1(I),RPC(I),I=1,NCPAR)
 704    FORMAT (/,'   Computational parameters:',//,
     *         (3(1X,A7,G12.4),/))
        RETURN   
C----------------------------------------------------------------
        ENTRY ProtC
C
        CLOSE (NCANP)
        RETURN
        END 
