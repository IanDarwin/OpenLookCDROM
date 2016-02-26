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

      SUBROUTINE CLDOP5(N,T,TEND,X,HINT,HMIN,HMAX,EPSA,EPSR,NMXSTP,
     *                  MODE,IEND,WORK)
C * * * * * * * * * * * * * * * * * * * * * * * * *
C --- CALL DOPRI5 FOR LOCBIF 
C * * * * * * * * * * * * * * * * * * * * * * * * *
C
C     14.11.92
C--------------------------------------------------
C
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER*2(I-N)
      include 'integrbf.dim'
      parameter (NF1=NFMAX+1, LWORK=5*NF1+6, LRCONT=5*NF1+2, LIWORK=2)
        DIMENSION X(1),WORK(1),IWORK(LIWORK)
        COMMON/STAT/NFCN,NSTEP,NACCPT,NREJCT,NDEC,NSOL
        COMMON /CONTD/ICONT(4)
        COMMON /CONTD1/RCONT(LRCONT)
        EXTERNAL FUNTN,SOLOUT
C      PRINT 101,MODE,NMXSTP,EPSA,EPSR,IWORK(2),IWORK(1),ATOL,RTOL
C --- OUTPUT ROUTINE IS USED DURING INTEGRATION
        IOUT=1
C --- REQUIRED (RELATIVE) TOLERANCE
        ITOL=0
        ATOL=EPSA
        RTOL=EPSR
C --- DEFAULT VALUES FOR PARAMETERS
        DO 10 I=1,5
  10    WORK(I)=0.D0
        IWORK(1)=NMXSTP
        IWORK(2)=MODE
        WORK(6) =HMAX
C      PRINT 101,MODE,NMXSTP,EPSA,EPSR,IWORK(2),IWORK(1),ATOL,RTOL
C 101  FORMAT(/'   MODE,NMXSTP,EPSA,EPSR=',2I8,2G13.4,
C     *        '                        =',2I8,2G13.4)
C --- CALL OF THE SUBROUTINE DOPRI5
        CALL DOPRI5(N,FUNTN,T,X,TEND,HINT,
     &                  RTOL,ATOL,ITOL,
     &                  SOLOUT,IOUT,
     &                  WORK,LWORK,IWORK,LIWORK,LRCONT,IDID)
C
      IEND=IDID
C
C --- PRINT FINAL SOLUTION
C --- PRINT STATISTICS
#if defined(hib)
c        WRITE (6,91) NFCN,NSTEP,NACCPT,NREJCT
c 91     FORMAT(' fcn=',I5,' step=',I4,' accpt=',I4,' rejct=',I3)
#endif
      RETURN
      END
C
C
        SUBROUTINE SOLOUT (NR,TOLD,T,X,N,IRTRN)
C --- CALL FOR OUTDUM (OUTPUT SUBROUTINE)
CCC --- PRINTS SOLUTION AT EQUIDISTANT OUTPUT-POINTS
CCC --- BY USING "CONTD5", THE CONTINUOUS COLLOCATION SOLUTION
        IMPLICIT REAL*8 (A-H,O-Z)
        IMPLICIT INTEGER*2(I-N)
        DIMENSION X(N)
C        COMMON /INTERN/XOUT
C
      H=T-TOLD
      IF (NR.EQ.1) THEN
         ICODE=0
      ELSE
         ICODE=1
      ENDIF
      CALL OUTDUM(N,T,X,H,ICODE,ISTOP)
      IF (ISTOP.NE.0) IRTRN=-5
C        IF (NR.EQ.1) THEN
C           WRITE (6,99) X,Y(1),Y(2),NR-1
C           XOUT=X+0.1D0
C        ELSE
C 10        CONTINUE
C           IF (X.GE.XOUT) THEN
C              WRITE (6,99) XOUT,CONTD5(1,XOUT),CONTD5(2,XOUT),NR-1
C              XOUT=XOUT+0.1D0
C              GOTO 10
C           END IF
C        END IF                 
C 99     FORMAT(1X,'X =',F5.2,'    Y =',2E18.10,'    NSTEP =',I4)
        RETURN
        END
C
c        SUBROUTINE FUNTN(N,T,X,F)
C --- CALL FOR FUNT
c        IMPLICIT REAL*8 (A-H,O-Z)
c        IMPLICIT INTEGER*2(I-N)
c        DIMENSION X(N),F(N)
c        CALL FUNT(T,X,F)
c        RETURN
c        END
      SUBROUTINE DOPRI5(N,FCN,X,Y,XEND,H,
     &                  RTOL,ATOL,ITOL,
     &                  SOLOUT,IOUT,
     &                  WORK,LWORK,IWORK,LIWORK,LRCONT,IDID)
C ----------------------------------------------------------
C     NUMERICAL SOLUTION OF A SYSTEM OF FIRST 0RDER
C     ORDINARY DIFFERENTIAL EQUATIONS  Y'=F(X,Y).
C     THIS IS AN EXPLICIT RUNGE-KUTTA METHOD OF ORDER (4)5
C     DUE TO DORMAND & PRINCE (WITH STEPSIZE CONTROL).
C     C.F. SECTION II.4
C
C     AUTHORS: E. HAIRER AND G. WANNER
C              UNIVERSITE DE GENEVE, DEPT. DE MATHEMATIQUES
C              CH-1211 GENEVE 24, SWITZERLAND
C              E-MAIL:  HAIRER@CGEUGE51.BITNET,  WANNER@CGEUGE51.BITNET
C
C     THIS CODE IS AN EXTENSION OF THE CODE DESCRIBED IN:
C         E. HAIRER, S.P. NORSETT AND G. WANNER, SOLVING ORDINARY
C         DIFFERENTIAL EQUATIONS I. NONSTIFF PROBLEMS.
C         SPRINGER SERIES IN COMPUTATIONAL MATHEMATICS 8,
C         SPRINGER-VERLAG (1987)
C
C     VERSION OF APRIL 19, 1989
C****
C     ADAPTATION BY A.I.KHIBNIK
C                03.01.1991
C****
C
C     INPUT PARAMETERS
C     ----------------
C     N           DIMENSION OF THE SYSTEM
C
C     FCN         NAME (EXTERNAL) OF SUBROUTINE COMPUTING THE
C                 VALUE OF F(X,Y):
C                    SUBROUTINE FCN(N,X,Y,F)
C                    REAL*8 X,Y(N),F(N)
C                    F(1)=...   ETC.
C
C     X           INITIAL X-VALUE
C
C     Y(N)        INITIAL VALUES FOR Y
C
C     XEND        FINAL X-VALUE (XEND-X MAY BE POSITIVE OR NEGATIVE)
C
C     H           INITIAL STEP SIZE GUESS;
C                 H=1.D0/(NORM OF F'), USUALLY 1.D-1 OR 1.D-3, IS GOOD.
C                 THIS CHOICE IS NOT VERY IMPORTANT, THE CODE QUICKLY
C                 ADAPTS ITS STEP SIZE. WHEN YOU ARE NOT SURE, THEN
C                 STUDY THE CHOSEN VALUES FOR A FEW
C                 STEPS IN SUBROUTINE "SOLOUT".
C                 (IF H=0.D0, THE CODE PUTS H=1.D-6).
C
C     RTOL,ATOL   RELATIVE AND ABSOLUTE ERROR TOLERANCES. THEY
C                 CAN BE BOTH SCALARS OR ELSE BOTH VECTORS OF LENGTH N.
C
C     ITOL        SWITCH FOR RTOL AND ATOL:
C                   ITOL=0: BOTH RTOL AND ATOL ARE SCALARS.
C                     THE CODE KEEPS, ROUGHLY, THE LOCAL ERROR OF
C                     Y(I) BELOW RTOL*ABS(Y(I))+ATOL
C                   ITOL=1: BOTH RTOL AND ATOL ARE VECTORS.
C                     THE CODE KEEPS THE LOCAL ERROR OF Y(I) BELOW
C                     RTOL(I)*ABS(Y(I))+ATOL(I).
C
C     SOLOUT      NAME (EXTERNAL) OF SUBROUTINE PROVIDING THE
C                 NUMERICAL SOLUTION DURING INTEGRATION.
C                 IF IOUT=1, IT IS CALLED AFTER EVERY SUCCESSFUL STEP.
C                 SUPPLY A DUMMY SUBROUTINE IF IOUT=0.
C                 IT MUST HAVE THE FORM
C                    SUBROUTINE SOLOUT (NR,XOLD,X,Y,N,IRTRN)
C                    REAL*8 X,Y(N)
C                    ....
C                 SOLOUT FURNISHES THE SOLUTION "Y" AT THE NR-TH
C                    GRID-POINT "X" (THEREBY THE INITIAL VALUE IS
C                    THE FIRST GRID-POINT).
C                 "XOLD" IS THE PRECEEDING GRID-POINT.
C                 "IRTRN" SERVES TO INTERRUPT THE INTEGRATION. IF IRTRN
C                    IS SET <0, DOPRI5 RETURNS TO THE CALLING PROGRAM.
C
C          -----  CONTINUOUS OUTPUT: -----
C                 DURING CALLS TO "SOLOUT", A CONTINUOUS SOLUTION
C                 FOR THE INTERVAL [XOLD,X] IS AVAILABLE THROUGH
C                 THE FUNCTION
C                        >>>   CONTD5(I,S)   <<<
C                 WHICH PROVIDES AN APPROXIMATION TO THE I-TH
C                 COMPONENT OF THE SOLUTION AT THE POINT S. THE VALUE
C                 S SHOULD LIE IN THE INTERVAL [XOLD,X].
C
C     IOUT        SWITCH FOR CALLING THE SUBROUTINE SOLOUT:
C                    IOUT=0: SUBROUTINE IS NEVER CALLED
C                    IOUT=1: SUBROUTINE IS AVAILABLE FOR OUTPUT.
C
C     WORK        ARRAY OF WORKING SPACE OF LENGTH "LWORK".
C                 "LWORK" MUST BE AT LEAST  5*N+6
C
C     LWORK       DECLARED LENGHT OF ARRAY "WORK".
C
C     IWORK       INTEGER WORKING SPACE OF LENGHT "LIWORK".
C                 IWORK(1) AND IWORK(2) SERVE AS PARAMETERS
C                 FOR THE CODE. FOR STANDARD USE, SET IWORK(1) AND
C                 IWORK(2) TO ZERO BEFORE CALLING.
C                 "LIWORK" MUST BE AT LEAST 2 .
C
C     LIWORK      DECLARED LENGHT OF ARRAY "IWORK".
C
C     LRCONT      DECLARED LENGTH OF COMMON BLOCK
C                  >>>  COMMON /CONTD/ICONT(4),RCONT(LRCONT)  <<<
C                 WHICH MUST BE DECLARED IN THE CALLING PROGRAM.
C                 "LRCONT" MUST BE AT LEAST
C                             5*N+2 .
C                 THIS IS USED FOR STORING THE COEFFICIENTS OF THE
C                 CONTINUOUS SOLUTION AND MAKES THE CALLING LIST FOR THE
C                 FUNCTION "CONTD5" AS SIMPLE AS POSSIBLE.
C
C-----------------------------------------------------------------------
C
C     SOPHISTICATED SETTING OF PARAMETERS
C     -----------------------------------
C              SEVERAL PARAMETERS OF THE CODE ARE TUNED TO MAKE IT WORK
C              WELL. THEY MAY BE DEFINED BY SETTING WORK(1),..,WORK(6)
C              AS WELL AS IWORK(1) AND IWORK(2) DIFFERENT FROM ZERO.
C              FOR ZERO INPUT, THE CODE CHOOSES DEFAULT VALUES:
C
C    IWORK(1)  THIS IS THE MAXIMAL NUMBER OF ALLOWED STEPS.
C              THE DEFAULT VALUE (FOR IWORK(2)=0) IS 10000.
C
C    IWORK(2)  SWITCH FOR THE CHOICE OF THE COEFFICIENTS
C              IF IWORK(2).EQ.1  METHOD DOPRI5 OF DORMAND AND PRINCE
C              (TABLE 4.6 OF SECTION II.4).
C              IF IWORK(2).EQ.2  HIGHAM & HALL'S FIRST METHOD
C              (TABLE 2.1 OF IV.2).
C              IF IWORK(2).EQ.3  THE THIRD METHOD OF HIGHAM & HALL
C              THE DEFAULT VALUE (FOR IWORK(2)=0) IS IWORK(2)=1.
C
C    WORK(1)   UROUND, THE ROUNDING UNIT, DEFAULT 1.D-16.
C
C    WORK(2)   THE SAFETY FACTOR IN STEP SIZE PREDICTION,
C              DEFAULT 0.9D0.
C
C    WORK(3), WORK(4)   PARAMETERS FOR STEP SIZE SELECTION
C              THE NEW STEP SIZE IS CHOSEN SUBJECT TO THE RESTRICTION
C                 WORK(3) <= HNEW/HOLD <= WORK(4)
C              DEFAULT VALUES: WORK(3)=0.2D0, WORK(4)=10.D0
C
C    WORK(5)   IS THE "BETA" FOR STABILIZED STEP SIZE CONTROL
C              (SEE SECTION IV.2). LARGER VALUES OF BETA MAKE
C              THE STEP SIZE CONTROL MORE STABLE. DOPRI5 NEEDS
C              A LARGER BETA THAN HIGHAM & HALL. NEGATIVE WORK(5)
C              PROVOKE BETA=0.
C              DEFAULT 0.13D0.
C
C    WORK(6)   MAXIMAL STEP SIZE, DEFAULT XEND-X.
C
C----------------------------------------------------------------------C
C     OUTPUT PARAMETERS
C     -----------------
C     X           X-VALUE FOR WHICH THE SOLUTION HAS BEEN COMPUTED
C                 (AFTER SUCCESSFUL RETURN X=XEND).
C
C     Y(N)        NUMERICAL SOLUTION AT X
C
C     H           PREDICTED STEP SIZE OF THE LAST ACCEPTED STEP
C
C     IDID        REPORTS ON SUCCESSFULNESS UPON RETURN:
C                   IDID=1  COMPUTATION SUCCESSFUL,
C                   IDID<0  COMPUTATION UNSUCCESSFUL.
C
C-----------------------------------------------------------------------
C *** *** *** *** *** *** *** *** *** *** *** *** ***
C          DECLARATIONS
C *** *** *** *** *** *** *** *** *** *** *** *** ***
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER*2(I-N)
      DIMENSION Y(N),ATOL(1),RTOL(1),WORK(LWORK),IWORK(LIWORK)
      LOGICAL ARRET
      EXTERNAL FCN,SOLOUT
      COMMON /ierfs/ierfs,ierdum
      COMMON /iabort/iabort
      COMMON/STAT/NFCN,NSTEP,NACCPT,NREJCT,NDEC,NSOL
C --- COMMON STAT CAN BE INSPECTED FOR STATISTICAL PURPOSES:
C ---    NFCN      NUMBER OF FUNCTION EVALUATIONS
C ---    NSTEP     NUMBER OF COMPUTED STEPS
C ---    NACCPT    NUMBER OF ACCEPTED STEPS
C ---    NREJCT    NUMBER OF REJECTED STEPS (AFTER AT LEAST ONE STEP
C                  HAS BEEN ACCEPTED)
C *** *** *** *** *** *** ***
C        SETTING THE PARAMETERS
C *** *** *** *** *** *** ***
      ierfs=0
      iabort=0
      NFCN=0
      NSTEP=0
      NACCPT=0
      NREJCT=0
      ARRET=.FALSE.
C -------- NMAX , THE MAXIMAL NUMBER OF STEPS -----
      IF(IWORK(1).EQ.0)THEN
         NMAX=10000
      ELSE
         NMAX=IWORK(1)
         IF(NMAX.LE.0)THEN
#if defined(hib)
            WRITE(6,*)' WRONG INPUT IWORK(1)=',IWORK(1)
#endif
            ARRET=.TRUE.
         END IF
      END IF
C -------- METH   COEFFICIENTS OF THE METHOD
      IF(IWORK(2).EQ.0)THEN
         METH=1
      ELSE
         METH=IWORK(2)
         IF(METH.LE.0.OR.METH.GE.4)THEN
#if defined(hib)
            WRITE(6,*)' CURIOUS INPUT IWORK(2)=',IWORK(2)
#endif
            ARRET=.TRUE.
         END IF
      END IF
C -------- UROUND   SMALLEST NUMBER SATISFYING 1.D0+UROUND>1.D0
      IF(WORK(1).EQ.0.D0)THEN
         UROUND=1.D-16
      ELSE
         UROUND=WORK(1)
         IF(UROUND.LE.1.D-35.OR.UROUND.GE.1.D0)THEN
#if defined(hib)
            WRITE(6,*)' WHICH MACHINE DO YOU HAVE? YOUR UROUND WAS:'
     &                            ,WORK(1)
#endif
            ARRET=.TRUE.
         END IF
      END IF
C -------  SAFETY FACTOR -------------
      IF(WORK(2).EQ.0.D0)THEN
         SAFE=0.9D0
      ELSE
         SAFE=WORK(2)
         IF(SAFE.GE.1.D0.OR.SAFE.LE.1.D-4)THEN
#if defined(hib)
            WRITE(6,*)' CURIOUS INPUT FOR SAFETY FACTOR WORK(2)='
     &                            ,WORK(2)
#endif
            ARRET=.TRUE.
         END IF
      END IF
C -------  FAC1,FAC2     PARAMETERS FOR STEP SIZE SELECTION
      IF(WORK(3).EQ.0.D0)THEN
         FAC1=0.2D0
      ELSE
         FAC1=WORK(3)
      END IF
      IF(WORK(4).EQ.0.D0)THEN
         FAC2=10.D0
      ELSE
         FAC2=WORK(4)
      END IF
C --------- BETA FOR STEP CONTROL STABILIZATION -----------
      IF(WORK(5).EQ.0.D0)THEN
         BETA=0.13D0
      ELSE
         IF(WORK(5).LT.0.D0)THEN
            BETA=0.D0
         ELSE
            BETA=WORK(5)
         END IF
      END IF
C -------- MAXIMAL STEP SIZE
      IF(WORK(6).EQ.0.D0)THEN
         HMAX=XEND-X
      ELSE
         HMAX=WORK(6)
      END IF
C ------- PREPARE THE ENTRY-POINTS FOR THE ARRAYS IN WORK -----
      IEK1=7
      IEK2=IEK1+N
      IEK3=IEK2+N
      IEK4=IEK3+N
      IEK5=IEK4+N
C ------ TOTAL STORAGE REQUIREMENT -----------
      ISTORE=IEK5+N-1
      IF(ISTORE.GT.LWORK)THEN
#if defined(hib)
         WRITE(6,*)' INSUFFICIENT STORAGE FOR WORK, MIN. LWORK=',ISTORE
#endif
         ARRET=.TRUE.
      END IF
C --------- CONTROL OF LENGTH OF COMMON BLOCK "CONT" -------
      IF(LRCONT.LT.(5*N+2))THEN
#if defined(hib)
         WRITE(6,*)' INSUFF. STORAGE FOR RCONT, MIN. LRCONT=',5*N+2
#endif
         ARRET=.TRUE.
      END IF
C ------ WHEN A FAIL HAS OCCURED, WE RETURN WITH IDID=-1
      IF (ARRET) THEN
         IDID=-1
         RETURN
      END IF
C -------- CALL TO CORE INTEGRATOR ------------
      CALL DOPCOR(N,FCN,X,Y,XEND,HMAX,H,RTOL,ATOL,ITOL,
     &   SOLOUT,IOUT,IDID,NMAX,UROUND,METH,SAFE,BETA,FAC1,FAC2,
     &   WORK(IEK1),WORK(IEK2),WORK(IEK3),WORK(IEK4),WORK(IEK5))
C ----------- RETURN -----------
      RETURN
      END
C
C
C
C  ----- ... AND HERE IS THE CORE INTEGRATOR  ----------
C
      SUBROUTINE DOPCOR(N,FCN,X,Y,XEND,HMAX,H,RTOL,ATOL,ITOL,
     &   SOLOUT,IOUT,IDID,NMAX,UROUND,METH,SAFE,BETA,FAC1,FAC2,
     &   K1,K2,K3,K4,K5)
C ----------------------------------------------------------
C     CORE INTEGRATOR FOR DOPRI5
C     PARAMETERS SAME AS IN DOPRI5 WITH WORKSPACE ADDED
C ----------------------------------------------------------
C         DECLARATIONS
C ----------------------------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER*2(I-N)
      REAL*8 Y(N),K1(N),K2(N),K3(N),K4(N),K5(N)
      REAL*8 ATOL(1),RTOL(1)
      LOGICAL REJECT
      COMMON/STAT/NFCN,NSTEP,NACCPT,NREJCT,NDEC,NSOL
      COMMON /CONTD/NN,NN2,NN3,NN4
      COMMON /CONTD1/XOLD,HOUT,CONT(1)
C  
      COMMON /ierfs/ierfs,ierdum
      COMMON /iabort/iabort

C *** *** *** *** *** *** ***
C  INITIALISATIONS
C *** *** *** *** *** *** ***
      NN=N
      NN2=2*N
      NN3=3*N
      NN4=4*N
      IF (METH.EQ.1) CALL CDOPRI(C2,C3,C4,C5,E1,E3,E4,E5,E6,E7,
     &                    A21,A31,A32,A41,A42,A43,A51,A52,A53,A54,
     &                    A61,A62,A63,A64,A65,A71,A73,A74,A75,A76,
     &                    D21,D23,D24,D25,D26,D31,D33,D34,D35,D36,
     &                    D41,D43,D44,D45,D46)
      IF (METH.EQ.2) CALL CHIHAA(C2,C3,C4,C5,E1,E3,E4,E5,E6,E7,
     &                    A21,A31,A32,A41,A42,A43,A51,A52,A53,A54,
     &                    A61,A62,A63,A64,A65,A71,A73,A74,A75,A76,
     &                    D21,D23,D24,D25,D26,D31,D33,D34,D35,D36,
     &                    D41,D43,D44,D45,D46)
      IF (METH.EQ.3) CALL CHIHAC(C2,C3,C4,C5,E1,E3,E4,E5,E6,E7,
     &                    A21,A31,A32,A41,A42,A43,A51,A52,A53,A54,
     &                    A61,A62,A63,A64,A65,A71,A73,A74,A75,A76,
     &                    D21,D23,D24,D25,D26,D31,D33,D34,D35,D36,
     &                    D41,D43,D44,D45,D46)
      FACOLD=1.D-4
      FACC1=1.D0/FAC1
      FACC2=1.D0/FAC2
      POSNEG=DSIGN(1.D0,XEND-X)
C --- INITIAL PREPARATIONS
      HMAX=DABS(HMAX)
      H=DMIN1(DMAX1(1.D-6,DABS(H)),HMAX)
      H=DSIGN(H,POSNEG)
      REJECT=.FALSE.
      XOLD=X
      IF (IOUT.NE.0) THEN
          IRTRN=1
          HOUT=H
          CALL SOLOUT(NACCPT+1,XOLD,X,Y,N,IRTRN)
          IF (IRTRN.LT.0) GOTO 79
      END IF
      CALL FCN(N,X,Y,K1)
        IF (ierfs.ne.0)	GOTO 51
      NFCN=NFCN+1
C --- BASIC INTEGRATION STEP
C***   1  IF(NSTEP.GT.NMAX.OR.X+.1D0*H.EQ.X)GOTO 79
   1  IF(NSTEP.GT.NMAX) GOTO 89
      RR=X+.1D0*H
c      RR=X+H
      IF(RR.EQ.X) GOTO 89
      IF((X-XEND)*POSNEG+UROUND.GT.0.D0) THEN
          H=HNEW
          IDID=1
          RETURN
      END IF
c      RR=X+H-XEND
      RR=X+1.1D0*H-XEND
      IF(RR*POSNEG.GT.0.D0)H=XEND-X
      NSTEP=NSTEP+1
C --- THE FIRST 6 STAGES
      DO 22 I=1,N
  22  CONT(I)=Y(I)+H*A21*K1(I)
      CALL FCN(N,X+C2*H,CONT,K2)
        IF (ierfs.ne.0)	GOTO 51
      DO 23 I=1,N
  23  CONT(I)=Y(I)+H*(A31*K1(I)+A32*K2(I))
      CALL FCN(N,X+C3*H,CONT,K3)
        IF (ierfs.ne.0)	GOTO 51
      DO 24 I=1,N
  24  CONT(I)=Y(I)+H*(A41*K1(I)+A42*K2(I)+A43*K3(I))
      CALL FCN(N,X+C4*H,CONT,K4)
        IF (ierfs.ne.0)	GOTO 51
      DO 25 I=1,N
  25  CONT(I)=Y(I)+H*(A51*K1(I)+A52*K2(I)+A53*K3(I)+A54*K4(I))
      CALL FCN(N,X+C5*H,CONT,K5)
        IF (ierfs.ne.0)	GOTO 51
      DO 26 I=1,N
  26  CONT(I)=Y(I)+H*(A61*K1(I)+A62*K2(I)+A63*K3(I)+A64*K4(I)+A65*K5(I))
      XPH=X+H
      CALL FCN(N,XPH,CONT,K2)
        IF (ierfs.ne.0)	GOTO 51
      DO 27 I=1,N
  27  CONT(I)=Y(I)+H*(A71*K1(I)+A73*K3(I)+A74*K4(I)+A75*K5(I)+A76*K2(I))
C --- COMPUTE INTERMEDIATE SUM TO SAVE MEMORY
      IF (IOUT.NE.0) THEN
          DO 63 I=1,N
          CONT(I+NN)=K1(I)
          CONT(I+NN2)=D21*K1(I)+D23*K3(I)+D24*K4(I)+D25*K5(I)+D26*K2(I)
          CONT(I+NN3)=D31*K1(I)+D33*K3(I)+D34*K4(I)+D35*K5(I)+D36*K2(I)
          CONT(I+NN4)=D41*K1(I)+D43*K3(I)+D44*K4(I)+D45*K5(I)+D46*K2(I)
  63      CONTINUE
      END IF
      DO 61 I=1,N
  61  K2(I)=E1*K1(I)+E3*K3(I)+E4*K4(I)+E5*K5(I)+E6*K2(I)
C --- THE LAST STAGE
      CALL FCN(N,XPH,CONT,K3)
        IF (ierfs.ne.0)	GOTO 51
      DO 28 I=1,N
  28  K4(I)=(K2(I)+E7*K3(I))*H
      NFCN=NFCN+6
C --- ERROR ESTIMATION
      ERR=0.D0
      DO 41 I=1,N
      IF (ITOL.EQ.0) THEN
         SK=ATOL(1)+RTOL(1)*DMAX1(DABS(Y(I)),DABS(CONT(I)))
      ELSE
         SK=ATOL(I)+RTOL(I)*DMAX1(DABS(Y(I)),DABS(CONT(I)))
      END IF
      RR=K4(I)/SK
      IF (DABS(RR).GT.1.0D20) GOTO 51
      ERR=ERR+RR**2
  41  CONTINUE
C***  41  ERR=ERR+(K4(I)/SK)**2
      ERR=DSQRT(ERR/N)
C --- COMPUTATION OF HNEW
      FAC11=ERR**0.2D0
C --- LUND-STABILIZATION
      FAC=FAC11/FACOLD**BETA
C --- WE REQUIRE  FAC1 <= HNEW/H <= FAC2
      FAC=DMAX1(FACC2,DMIN1(FACC1,FAC/SAFE))
      HNEW=H/FAC
      IF(ERR.LE.1.D0)THEN
C --- STEP IS ACCEPTED
         FACOLD=DMAX1(ERR,1.D-4)
         NACCPT=NACCPT+1
         DO 44 I=1,N
         K1(I)=K3(I)
         SAFE1=Y(I)
         Y(I)=CONT(I)
  44     CONT(I)=SAFE1
         XOLD=X
         X=XPH
         IF (IOUT.NE.0) THEN
            HOUT=H
            CALL SOLOUT(NACCPT+1,XOLD,X,Y,N,IRTRN)
            IF (IRTRN.LT.0) GOTO 79
         END IF
         IF(ABS(HNEW).GT.HMAX)HNEW=POSNEG*HMAX
         IF(REJECT)HNEW=POSNEG*DMIN1(DABS(HNEW),DABS(H))
         REJECT=.FALSE.
      ELSE
C --- STEP IS REJECTED
         HNEW=H/DMIN1(FACC1,FAC11/SAFE)
         REJECT=.TRUE.
         IF(NACCPT.GE.1)NREJCT=NREJCT+1
      END IF
      H=HNEW
      GOTO 1
C**** RHS FAIL PROCESSING
  51  CONTINUE
      IF (ierfs.LT.0 .OR. iabort.NE.0 .OR. NFCN.EQ.0) THEN
C**** COMPUTATION INTERRUPTED   
         GOTO 79
      ELSE
C**** STEP IS REJECTED
         HNEW=H/2.D0
         REJECT=.TRUE.
         IF(NACCPT.GE.1)NREJCT=NREJCT+1
      END IF
      H=HNEW
      GOTO 1
C --- FAIL EXIT
  79  IDID=-5
      GOTO 200
  89  IDID=-3
 200  CONTINUE
#if defined(hib)
c      WRITE(6,979)X,IDID
c 979  FORMAT(' EXIT OF DOPRI5 AT X=',E11.4,'  IDID=',I3)
#endif
      RETURN
      END
C
      FUNCTION CONTD5(I,X)
C ----------------------------------------------------------
C     THIS FUNCTION CAN BE USED FOR CONINUOUS OUTPUT IN CONECTION
C     WITH THE OUTPUT-SUBROUTINE FOR DOPRI5. IT PROVIDES AN
C     APPROXIMATION TO THE I-TH COMPONENT OF THE SOLUTION AT X.
C ----------------------------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER*2(I-N)
c      COMMON /CONTD/N,N2,N3,N4,XOLD,H,CONT(1)
      COMMON /CONTD/N,N2,N3,N4
      COMMON /CONTD1/XOLD,H,CONT(1)
      S=(X-XOLD)/H
      CONTD5=CONT(I)+H*S*(CONT(I+N)+S*(CONT(I+N2)+S*(CONT(I+N3)
     &      +S*CONT(I+N4))))
      RETURN
      END
C
      SUBROUTINE CDOPRI(C2,C3,C4,C5,E1,E3,E4,E5,E6,E7,
     &                    A21,A31,A32,A41,A42,A43,A51,A52,A53,A54,
     &                    A61,A62,A63,A64,A65,A71,A73,A74,A75,A76,
     &                    D21,D23,D24,D25,D26,D31,D33,D34,D35,D36,
     &                    D41,D43,D44,D45,D46)
C ----------------------------------------------------------
C     THESE ARE THE COEFFICIENTS OF THE RK=METHOD DOPRI5
C     (IN THE BOOK ON PAGE 171)
C ----------------------------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER*2(I-N)
      C2=0.2D0
      C3=0.3D0
      C4=0.8D0
      C5=8.D0/9.D0
      A21=0.2D0
      A31=3.D0/40.D0
      A32=9.D0/40.D0
      A41=44.D0/45.D0
      A42=-56.D0/15.D0
      A43=32.D0/9.D0
      A51=19372.D0/6561.D0
      A52=-25360.D0/2187.D0
      A53=64448.D0/6561.D0
      A54=-212.D0/729.D0
      A61=9017.D0/3168.D0
      A62=-355.D0/33.D0
      A63=46732.D0/5247.D0
      A64=49.D0/176.D0
      A65=-5103.D0/18656.D0
      A71=35.D0/384.D0
      A73=500.D0/1113.D0
      A74=125.D0/192.D0
      A75=-2187.D0/6784.D0
      A76=11.D0/84.D0
      E1=71.D0/57600.D0
      E3=-71.D0/16695.D0
      E4=71.D0/1920.D0
      E5=-17253.D0/339200.D0
      E6=22.D0/525.D0
      E7=-1.D0/40.D0
      D21=-1337.D0/480.D0
      D23=105400.D0/27825.D0
      D24=-135.D0/80.D0
      D25=-54675.D0/212000.D0
      D26=66.D0/70.D0
      D31=1039.D0/360.D0
      D33=-468200.D0/83475.D0
      D34=9.D0/2.D0
      D35=400950.D0/318000.D0
      D36=-638.D0/210.D0
      D41=-1163.D0/1152.D0
      D43=37900.D0/16695.D0
      D44=-415.D0/192.D0
      D45=-674325.D0/508800.D0
      D46=374.D0/168.D0
      RETURN
      END
C
      SUBROUTINE CHIHAA(C2,C3,C4,C5,E1,E3,E4,E5,E6,E7,
     &                    A21,A31,A32,A41,A42,A43,A51,A52,A53,A54,
     &                    A61,A62,A63,A64,A65,A71,A73,A74,A75,A76,
     &                    D21,D23,D24,D25,D26,D31,D33,D34,D35,D36,
     &                    D41,D43,D44,D45,D46)
C ----------------------------------------------------------
C     HIGHAM & HALL'S FIRST METHOD (TABLE 2.1 OF IV.2)
C ----------------------------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER*2(I-N)
        C2=2.D0/9.D0
        C3=1.D0/3.D0
        C4=1.D0/2.D0
        C5=3.D0/5.D0
        A21=2.D0/9.D0
        A31=1.D0/12.D0
        A32=1.D0/4.D0
        A41=1.D0/8.D0
        A42=0.D0
        A43=3.D0/8.D0
        A51=91.D0/500.D0
        A52=-27.D0/100.D0
        A53=78.D0/125.D0
        A54=8.D0/125.D0
        A61=-11.D0/20.D0
        A62=27.D0/20.D0
        A63=12.D0/5.D0
        A64=-36.D0/5.D0
        A65=5.D0
        A71=1.D0/12.D0
        A73=27.D0/32.D0
        A74=-4.D0/3.D0
        A75=125.D0/96.D0
        A76=5.D0/48.D0
        E1=1.D0/20.D0
        E3=-81.D0/160.D0
        E4=6.D0/5.D0
        E5=-25.D0/32.D0
        E6=-1.D0/16.D0
        E7=1.D0/10.D0
        D21=-15.D0/4.D0
        D23=459.D0/32.D0
        D24=-22.D0
        D25=375.D0/32.D0
        D26=-5.D0/16.D0
        D31=16.D0/3.D0
        D33=-243.D0/8.D0
        D34=152.D0/3.D0
        D35=-625.D0/24.D0
        D36=5.D0/12.D0
        D41=-5.D0/2.D0
        D43=135.D0/8.D0
        D44=-30.D0
        D45=125.D0/8.D0
        D46=0.D0
      RETURN
      END
C
      SUBROUTINE CHIHAC(C2,C3,C4,C5,E1,E3,E4,E5,E6,E7,
     &                    A21,A31,A32,A41,A42,A43,A51,A52,A53,A54,
     &                    A61,A62,A63,A64,A65,A71,A73,A74,A75,A76,
     &                    D21,D23,D24,D25,D26,D31,D33,D34,D35,D36,
     &                    D41,D43,D44,D45,D46)
C ----------------------------------------------------------
C     THE THIRD METHOD OF HIGHAM & HALL
C ----------------------------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER*2(I-N)
        C2=11.D0/45.D0
        C3=11.D0/30.D0
        C4=55.D0/56.D0
        C5=9.D0/10.D0
        A21=11.D0/45.D0
        A31=11.D0/120.D0
        A32=11.D0/40.D0
        A41=106865.D0/87808.D0
        A42=-408375.D0/87808.D0
        A43=193875.D0/43904.D0
        A51=79503.D0/121000.D0
        A52=-1053.D0/440.D0
        A53=147753.D0/56870.D0
        A54=27048.D0/710875.D0
        A61=89303.D0/78045.D0
        A62=-2025.D0/473.D0
        A63=994650.D0/244541.D0
        A64=-2547216.D0/28122215.D0
        A65=475.D0/2967.D0
        A71=1247.D0/10890.D0
        A73=57375.D0/108053.D0
        A74=-1229312.D0/1962015.D0
        A75=125.D0/207.D0
        A76=43.D0/114.D0
        E1=16.D0/10285.D0
        E3=-12150.D0/1836901.D0
        E4=-6322176.D0/11118085.D0
        E5=50.D0/391.D0
        E6=1763.D0/3230.D0
        E7=-1.D0/10.D0
        D21=-4517.D0/1870.D0
        D23=604125.D0/166991.D0
        D24=-1361024.D0/1010735.D0
        D25=-1375.D0/391.D0
        D26=2365.D0/646.D0
        D31=210446.D0/92565.D0
        D33=-9267750.D0/1836901.D0
        D34=8517376.D0/1450185.D0
        D35=1250.D0/153.D0
        D36=-10922.D0/969.D0
        D41=-4588.D0/6171.D0
        D43=3597750.D0/1836901.D0
        D44=-11458944.D0/2223617.D0
        D45=-4750.D0/1173.D0
        D46=2580.D0/323.D0
      RETURN
      END

