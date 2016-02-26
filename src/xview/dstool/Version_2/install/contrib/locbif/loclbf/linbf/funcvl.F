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

      REAL*8 FUNCTION FUNCVL(IIIFUN)
C
C          23.08.1991
C          14.11.92
C
      IMPLICIT REAL*8(A-H,O-Z)
      IMPLICIT INTEGER*2(I-N)
      include 'linbf.dim'
      parameter (NFMAX1=NFMAX+1)
      DIMENSION Y(NFMAX1)
      COMMON /ITMAP/ ITMAP
      COMMON /IERFS/ IERFS,IERDUM
      COMMON/PPAR/P(NPARMX)
      COMMON/IPPAR/IP(NPARMX)
      COMMON /LINBF/ NF,NAP,ILN,NF2,ITP
      COMMON/INTGR/TMAXI,HI,EPSAI,EPSRI,DHI,HMXI,HMNI,SOLVER,RMXSTP
	COMMON /ADRHS/ IADRHS,IFNINT
	COMMON /CURPNT/ XCUR(NDIM)
c      PRINT *,' FUNCVL:--------------------------------'
c      PRINT *,'  ILN',ILN,'  IADRHS=',IADRHS,'  IIIFUN=',IIIFUN
c      PRINT *,'  XCUR=',(XCUR(I),I=1,6) 
      IF (ILN.EQ.-2) THEN
         FUNCVL=0
c      PRINT *,' FUNCVL:********************************'
         RETURN
      ENDIF
C
      IADRHS=1
      IFNINT=IIIFUN
      IERFS=0
      IEND=0
      DO  I=1,NAP
        K=IP(I)
        P(K)=XCUR(NF+I)
      ENDDO
      DO 10 I=1,NF
        Y(I)=XCUR(I)
 10   CONTINUE
      Y(NF+1)=0.
      T=0.D0
      TMAX=DABS(TMAXI*ITMAP)
      H=HI
      EPSABS=DABS(EPSAI)
      EPSREL=DABS(EPSRI)
      DH=DHI
      HMAX=DABS(HMXI)
      IF (HMAX.GT.0.1D0*TMAX) HMAX=0.1D0*TMAX
      MODE=SOLVER
      NMAX=RMXSTP
C
       CALL INTEGR(NF+1,T,TMAX,Y,H,HMAX,EPSABS,EPSREL,
     *             DH,NMAX,MODE,IEND)
      IADRHS=0
      H=HI
      IF (IEND.NE.0) GO TO 99
      FUNCVL=Y(NF+1)
c      PRINT *,'  FUNCVL=',FUNCVL
c      PRINT *,' FUNCVL:********************************'
      RETURN
C
 99   CONTINUE
      FUNCVL=-(10.)**30 
c      PRINT *,'  FUNCVL=',FUNCVL
c      PRINT *,' FUNCVL:********************************'
      RETURN
      END
C
        SUBROUTINE FADRHS(T,X,F)
C --- COMPUTING THE LAST RHS VALUE
        IMPLICIT REAL*8 (A-H,O-Z)
        IMPLICIT INTEGER*2(I-N)
	include 'linbf.dim'
        DIMENSION X(1),F(1)
	DIMENSION XX(0:NFMAX)
	COMMON /IERFS/ IERFS,IERDUM
        COMMON /IABORT/ IABORT
	COMMON /LINBF/ NF,NAP,ILN,NF2,ITP
	COMMON /ADRHS/ IADRHS,IFNINT
c      PRINT *,' FADRHS:--------------------------------'
c      PRINT *,'  T=',T,'  IFNINT=',IFNINT,'  NF=',NF
	XX(0)=T
	DO 10 I=1,NF
	   XX(I)=X(I)
 10     CONTINUE
	F(NF+1)=0.
        IF (IFNINT.EQ.0) THEN
	   R=0.
	   DO 20 I=1,NF
    	      R=R+X(I)**2
 20        CONTINUE
           F(NF+1)=R
c	   PRINT *,'  F(NF+1)=',F(NF+1)
c           PRINT *,' FADRHS:  1*************************'
           RETURN 
	ELSE
           JFUN=IABS(IFNINT)-1
	   IEND=0
           CALL USERFUN(JFUN,IEND,XX,G)
           IF (IEND.NE.0) GOTO 100
           F(NF+1)=G
c        F(NF+1)=(X(1)-X(3))**2+(X(2)-X(4))**2
c	   PRINT *,'  F(NF+1)=',F(NF+1)
c           PRINT *,' FADRHS:  2*************************'
           RETURN
	ENDIF
C   ERROR PROCESSING
 100    CONTINUE
        IF (IEND.LT.0) THEN
	   IERFS=102
           IABORT=1
        ELSE 
           IERFS=103
        ENDIF
        RETURN
        END
