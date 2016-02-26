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

      SUBROUTINE INTEGR(N,T,TMAX,X,H,HMAX,EPSABS,EPSREL,DH,
     *                  NMAX,MODE,IEND)
C
C   CALL FOR ODEs SOLVERS 
C
C       MODE =  1 - DOPRI5
C            =  2 - DOPRI5
C            =  3 - DOPRI5
C            = -1 - RADAU5
C
C          27.09.90
C          31.01.91
C          16.02.91
C	   19.08.91
C          14.11.92
C
      IMPLICIT REAL*8(A-H,O-Z)
      IMPLICIT INTEGER*2(I-N)
      include 'integrbf.dim'
      parameter (NF1=NFMAX+1, NWORK=4*NF1*NF1+9*NF1+7)
      DIMENSION X(N)
      DIMENSION F(NF1),WORK(NWORK)
      COMMON /INTOUT/ IOUT,IDIREC
      EXTERNAL FUNTN,OUTDUM
      IEND=0
      TEND=T+DSIGN(TMAX,H*IDIREC)
      HMIN=1.0D-10
      HINT=DABS(H)
      EPSA=EPSABS
      EPSR=EPSREL
      NMXSTP=NMAX
      DO 10 I=1,N
        F(I)=1.D0/N
 10   CONTINUE
c     SELECT CASE
c         CASE (1,2,3)
          IF (MODE.EQ.1 .OR. MODE.EQ.2 .OR.MODE.EQ.3) THEN
            CALL CLDOP5(N,T,TEND,X,HINT,HMIN,HMAX,EPSA,EPSR,NMXSTP,
     *                  MODE,IEND,WORK)
            IF (IEND.NE.1) GO TO 90
            IEND=0
            GO TO 30
      ENDIF
c         CASE (4)
          IF (MODE.EQ.4) THEN
            CALL DRKTS1(N,T,TEND,X,HINT,HMIN,HMAX,EPSA,
     *                  FUNTN,OUTDUM,IEND,F,WORK)
            IF (IEND.NE.1) GO TO 90
            IEND=0
            GO TO 30
      ENDIF
c         CASE (-1)
          IF (MODE.EQ.-1) THEN
            CALL CLRAD5(N,T,TEND,X,HINT,HMIN,HMAX,EPSA,EPSR,NMXSTP,
     *                  MODE,IEND,WORK)
            IF (IEND.NE.1) GO TO 90
            IEND=0
            GO TO 30
      ENDIF
c         CASE DEFAULT
            IEND=-1
c      END SELECT

C
 30   CONTINUE
      RETURN
C
 90   IF (IEND.NE.-5) IEND=-99
      RETURN
      END
        SUBROUTINE OUTDUM(N,T,X,H,ICODE,ISTOP)
        IMPLICIT REAL*8(A-H,O-Z)
	IMPLICIT INTEGER*2 (I-N)
        DIMENSION X(1)
        COMMON /INTOUT/ IOUT,IDIREC
        COMMON /NPNT/ NP
        ISTOP=0
        IF (IOUT.EQ.0) RETURN
        IF (ICODE.NE.1) RETURN
        NP=NP+1
        CALL OUTPT(N,X,NP,T,ISTOP)
        RETURN
        END          
C
        SUBROUTINE FUNTN(N,T,X,F)
C --- CALL FOR FUNT
        IMPLICIT REAL*8 (A-H,O-Z)
        IMPLICIT INTEGER*2(I-N)
        DIMENSION X(N),F(N)
        COMMON /IERFS/  IERFS,IERDUM
        COMMON /IABORT/ IABORT
        COMMON /ADRHS/  IADRHS,IFNINT
        CALL FUNT(T,X,F)
C --- CALL FOR VALUE OF ADDITIONAL RHS
        IF (IADRHS.NE.0) THEN
           IF (IERFS.EQ.0 .AND. IABORT.EQ.0) CALL FADRHS(T,X,F)
	ENDIF
        RETURN
        END
