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
#define full 0
C
      SUBROUTINE CLRAD5(N,T,TEND,X,HINT,HMIN,HMAX,EPSA,EPSR,NMXSTP,
     *                  MODE,IEND,WORK)
C * * * * * * * * * * * * * * * * * * * * * * * * *
C --- CALL RADAU5 FOR LOCBIF 
C * * * * * * * * * * * * * * * * * * * * * * * * *
C
C     14.11.92
C--------------------------------------------------
C
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER*2(I-N)
      include 'integrbf.dim'
C --- PARAMETERS FOR RADAU5 (FULL JACOBIAN)
      parameter (NF1=NFMAX+1, LWORK=4*NF1*NF1+9*NF1+7, 
     A           LIWORK=3*NF1+7, LRCONT=4*NF1+4)
      COMMON /CONT/ICONT(4),RCONT(LRCONT)
c        COMMON /CONT1/RCONT(48)
C --- DECLARATIONS
        DIMENSION X(1),WORK(1),IWORK(LIWORK)
        COMMON /NJAC/ NJAC,NJACN,NJACB
        COMMON/STAT/NFCN,NSTEP,NACCPT,NREJCT,NDEC,NSOL
        COMMON /CDOP5/RELTOL,RMETH,ALG
        EXTERNAL FUNTN,JFUN,SOLOU1,MASARR
C --- COMPUTE THE JACOBIAN NUMERICALLY
        IJAC=0
C --- JACOBIAN IS A FULL MATRIX
        MLJAC=N
        MUJAC=N
C --- DIFFERENTIAL EQUATION IS IN EXPLICIT OR IMPLICIT FORM
        IMAS=0
        IF (ALG.NE.0.) IMAS=1
        MLMAS=0
        MUMAS=0
C --- OUTPUT ROUTINE IS USED DURING INTEGRATION
        IOUT=1
C --- REQUIRED TOLERANCE
        ITOL=0
        ATOL=EPSA
        RTOL=EPSR
C --- DEFAULT VALUES FOR PARAMETERS
        DO 10 I=1,7
           IWORK(I)=0
           WORK(I)=0.D0
 10     CONTINUE
        IWORK(2)=NMXSTP
        WORK(7) =HMAX
C      PRINT 102,MODE,NMXSTP,EPSA,EPSR,IWORK(1),IWORK(2),ATOL,RTOL
C 102  FORMAT(/'   MODE,NMXSTP,EPSA,EPSR=',2I8,2G13.4,
C     *        '                        =',2I8,2G13.4)
C --- CALL OF THE SUBROUTINE RADAU5 
        CALL RADAU5(N,FUNTN,T,X,TEND,HINT,
     &                  RTOL,ATOL,ITOL,
     &                  JFUN,IJAC,MLJAC,MUJAC,
     &                  MASARR,IMAS,MLMAS,MUMAS,
     &                  SOLOU1,IOUT,
     &                  WORK,LWORK,IWORK,LIWORK,LRCONT,IDID)
C
      IEND=IDID
C
C --- PRINT FINAL SOLUTION
C --- PRINT STATISTICS
#if defined(hib)
        WRITE (6,90) ATOL,RTOL
 90     FORMAT('      Atol=',G13.6,'   Rtol=',G13.6)
        WRITE (6,91) NFCN,NJAC,NSTEP,NACCPT,NREJCT,NDEC,NSOL
 91     FORMAT(' fcn=',I5,' jac=',I4,' step=',I4,
     &        ' accpt=',I4,' rejct=',I3,' dec=',I4,
     &        ' sol=',I5)
#endif
      RETURN
      END
C
        SUBROUTINE SOLOU1 (NR,TOLD,T,X,N,IRTRN)
C --- CALL FOR OUTDUM (OUTPUT SUBROUTINE)
CCC --- PRINTS SOLUTION AT EQUIDISTANT OUTPUT-POINTS
CCC --- BY USING "CONTR5", THE CONTINUOUS COLLOCATION SOLUTION
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
C              WRITE (6,99) XOUT,CONTR5(1,XOUT),CONTR5(2,XOUT),NR-1
C              XOUT=XOUT+0.1D0
C              GOTO 10
C           END IF
C        END IF
C 99     FORMAT(1X,'X =',F5.2,'    Y =',2E18.10,'    NSTEP =',I4)
        RETURN
        END
C
c        SUBROUTINE FUNTN1(N,T,X,F)
C --- CALL FOR FUNT
c        IMPLICIT REAL*8 (A-H,O-Z)
c        IMPLICIT INTEGER*2(I-N)
c        DIMENSION X(N),F(N)
c        CALL FUNT(T,X,F)
c        RETURN
c        END
C
        SUBROUTINE JFUN(N,T,X,DFX,LDFX)
C --- EMPTY PROGRAM FOR JACOBIAN
        IMPLICIT REAL*8 (A-H,O-Z)
        IMPLICIT INTEGER*2(I-N)
        DIMENSION X(N),DFX(LDFX,N)
        RETURN
        END
C
        SUBROUTINE MASARR(N,AM,LMAS)
C --- MASS MATRIX IN DIAGONAL FORM 
        IMPLICIT REAL*8 (A-H,O-Z)
        IMPLICIT INTEGER*2(I-N)
        DIMENSION AM(LMAS,N)
        COMMON /CDOP5/RELTOL,RMETH,ALG
C        DO 20 I=1,N
C          DO 10 J=1,N
C            AM(I,J)=0.D0
C 10       CONTINUE
C 20     CONTINUE
C        DO 30 I=1,N
C          IF (I.GT.ALG.AND.N-I+1.GT.-ALG) AM(I,I)=1.D0
C 30     CONTINUE
        DO 40 I=1,N
          AM(1,I)=0.D0
          IF (I.GT.ALG.AND.N-I+1.GT.-ALG) AM(1,I)=1.D0
 40     CONTINUE
        RETURN
        END
C
      SUBROUTINE RADAU5(N,FCN,X,Y,XEND,H,
     &                  RTOL,ATOL,ITOL,
     &                  JAC ,IJAC,MLJAC,MUJAC,
     &                  MAS ,IMAS,MLMAS,MUMAS,
     &                  SOLOUT,IOUT,
     &                  WORK,LWORK,IWORK,LIWORK,LRCONT,IDID)
C ----------------------------------------------------------
C     NUMERICAL SOLUTION OF A STIFF (OR DIFFERENTIAL ALGEBRAIC)
C     SYSTEM OF FIRST 0RDER ORDINARY DIFFERENTIAL EQUATIONS
C                     M*Y'=F(X,Y).
C     THE SYSTEM CAN BE (LINEARLY) IMPLICIT (MASS-MATRIX M .NE. I)
C     OR EXPLICIT (M=I).
C     THE METHOD USED IS AN IMPLICIT RUNGE-KUTTA METHOD (RADAU IIA)
C     OF ORDER 5 WITH STEP SIZE CONTROL AND CONTINUOUS OUTPUT.
C     C.F. SECTION IV.8
C
C     AUTHORS: E. HAIRER AND G. WANNER
C              UNIVERSITE DE GENEVE, DEPT. DE MATHEMATIQUES
C              CH-1211 GENEVE 24, SWITZERLAND
C              E-MAIL:  HAIRER@CGEUGE51.BITNET,  WANNER@CGEUGE51.BITNET
C
C     THIS CODE IS PART OF THE BOOK:
C         E. HAIRER AND G. WANNER, SOLVING ORDINARY DIFFERENTIAL
C         EQUATIONS II. STIFF AND DIFFERENTIAL-ALGEBRAIC PROBLEMS.
C         SPRINGER SERIES IN COMPUTATIONAL MATHEMATICS,
C         SPRINGER-VERLAG (1990)
C
C     VERSION OF NOVEMBER 14, 1989
C****
C     ADAPTATION BY A.I.KHIBNIK
C                14.01.1991 - 31.01.1991
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
C                 FOR STIFF EQUATIONS WITH INITIAL TRANSIENT,
C                 H=1.D0/(NORM OF F'), USUALLY 1.D-3 OR 1.D-5, IS GOOD.
C                 THIS CHOICE IS NOT VERY IMPORTANT, THE CODE QUICKLY
C                 ADAPTS ITS STEP SIZE. STUDY THE CHOSEN VALUES FOR A FEW
C                 STEPS IN SUBROUTINE "SOLOUT", WHEN YOU ARE NOT SURE.
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
C     JAC         NAME (EXTERNAL) OF THE SUBROUTINE WHICH COMPUTES
C                 THE PARTIAL DERIVATIVES OF F(X,Y) WITH RESPECT TO Y
C                 (THIS ROUTINE IS ONLY CALLED IF IJAC=1; SUPPLY
C                 A DUMMY SUBROUTINE IN THE CASE IJAC=0).
C                 FOR IJAC=1, THIS SUBROUTINE MUST HAVE THE FORM
C                    SUBROUTINE JAC(N,X,Y,DFY,LDFY)
C                    REAL*8 X,Y(N),DFY(LDFY,N)
C                    DFY(1,1)= ...
C                 LDFY, THE COLUMN-LENGTH OF THE ARRAY, IS
C                 FURNISHED BY THE CALLING PROGRAM.
C                 IF (MLJAC.EQ.N) THE JACOBIAN IS SUPPOSED TO
C                    BE FULL AND THE PARTIAL DERIVATIVES ARE
C                    STORED IN DFY AS
C                       DFY(I,J) = PARTIAL F(I) / PARTIAL Y(J)
C                 ELSE, THE JACOBIAN IS TAKEN AS BANDED AND
C                    THE PARTIAL DERIVATIVES ARE STORED
C                    DIAGONAL-WISE AS
C                       DFY(I-J+MUJAC+1,J) = PARTIAL F(I) / PARTIAL Y(J).
C
C     IJAC        SWITCH FOR THE COMPUTATION OF THE JACOBIAN:
C                    IJAC=0: JACOBIAN IS COMPUTED INTERNALLY BY FINITE
C                       DIFFERENCES, SUBROUTINE "JAC" IS NEVER CALLED.
C                    IJAC=1: JACOBIAN IS SUPPLIED BY SUBROUTINE JAC.
C
C     MLJAC       SWITCH FOR THE BANDED STRUCTURE OF THE JACOBIAN:
C                    MLJAC=N: JACOBIAN IS A FULL MATRIX. THE LINEAR
C                       ALGEBRA IS DONE BY FULL-MATRIX GAUSS-ELIMINATION.
C                    0<=MLJAC<N: MLJAC IS THE LOWER BANDWITH OF JACOBIAN
C                       MATRIX (>= NUMBER OF NON-ZERO DIAGONALS BELOW
C                       THE MAIN DIAGONAL).
C
C     MUJAC       UPPER BANDWITH OF JACOBIAN  MATRIX (>= NUMBER OF NON-
C                 ZERO DIAGONALS ABOVE THE MAIN DIAGONAL).
C                 NEED NOT BE DEFINED IF MLJAC=N.
C
C     ----   MAS,IMAS,MLMAS, AND MUMAS HAVE ANALOG MEANINGS      -----
C     ----   FOR THE "MASS MATRIX" (THE MATRIX "M" OF SECTION IV.8): -
C
C     MAS         NAME (EXTERNAL) OF SUBROUTINE COMPUTING THE MASS-
C                 MATRIX M.
C                 IF IMAS=0, THIS MATRIX IS ASSUMED TO BE THE IDENTITY
C                 MATRIX AND NEEDS NOT TO BE DEFINED;
C                 SUPPLY A DUMMY SUBROUTINE IN THIS CASE.
C                 IF IMAS=1, THE SUBROUTINE MAS IS OF THE FORM
C                    SUBROUTINE MAS(N,AM,LMAS)
C                    REAL*8 AM(LMAS,N)
C                    AM(1,1)= ....
C                    IF (MLMAS.EQ.N) THE MASS-MATRIX IS STORED
C                    AS FULL MATRIX LIKE
C                         AM(I,J) = M(I,J)
C                    ELSE, THE MATRIX IS TAKEN AS BANDED AND STORED
C                    DIAGONAL-WISE AS
C                         AM(I-J+MUMAS+1,J) = M(I,J).
C
C     IMAS       GIVES INFORMATION ON THE MASS-MATRIX:
C                    IMAS=0: M IS SUPPOSED TO BE THE IDENTITY
C                       MATRIX, MAS IS NEVER CALLED.
C                    IMAS=1: MASS-MATRIX  IS SUPPLIED.
C
C     MLMAS       SWITCH FOR THE BANDED STRUCTURE OF THE MASS-MATRIX:
C                    MLMAS=N: THE FULL MATRIX CASE. THE LINEAR
C                       ALGEBRA IS DONE BY FULL-MATRIX GAUSS-ELIMINATION.
C                    0<=MLMAS<N: MLMAS IS THE LOWER BANDWITH OF THE
C                       MATRIX (>= NUMBER OF NON-ZERO DIAGONALS BELOW
C                       THE MAIN DIAGONAL).
C                 MLMAS IS SUPPOSED TO BE .LE. MLJAC.
C
C     MUMAS       UPPER BANDWITH OF MASS-MATRIX (>= NUMBER OF NON-
C                 ZERO DIAGONALS ABOVE THE MAIN DIAGONAL).
C                 NEED NOT BE DEFINED IF MLMAS=N.
C                 MUMAS IS SUPPOSED TO BE .LE. MUJAC.
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
C                    IS SET <0, RADAU5 RETURNS TO THE CALLING PROGRAM.
C
C          -----  CONTINUOUS OUTPUT: -----
C                 DURING CALLS TO "SOLOUT", A CONTINUOUS SOLUTION
C                 FOR THE INTERVAL [XOLD,X] IS AVAILABLE THROUGH
C                 THE REAL*8 FUNCTION
C                        >>>   CONTR5(I,S)   <<<
C                 WHICH PROVIDES AN APPROXIMATION TO THE I-TH
C                 COMPONENT OF THE SOLUTION AT THE POINT S. THE VALUE
C                 S SHOULD LIE IN THE INTERVAL [XOLD,X].
C
C     IOUT        SWITCH FOR CALLING THE SUBROUTINE SOLOUT:
C                    IOUT=0: SUBROUTINE IS NEVER CALLED
C                    IOUT=1: SUBROUTINE IS AVAILABLE FOR OUTPUT.
C
C     WORK        ARRAY OF WORKING SPACE OF LENGTH "LWORK".
C                 WORK(1), WORK(2),.., WORK(7) SERVE AS PARAMETERS
C                 FOR THE CODE. FOR STANDARD USE OF THE CODE
C                 WORK(1),..,WORK(7) MUST BE SET TO ZERO BEFORE
C                 CALLING. SEE BELOW FOR A MORE SOPHISTICATED USE.
C                 WORK(8),..,WORK(LWORK) SERVE AS WORKING SPACE
C                 FOR ALL VECTORS AND MATRICES.
C                 "LWORK" MUST BE AT LEAST
C                             N*(LJAC+LMAS+3*LE+8)+7
C                 WHERE
C                    LJAC=N              IF MLJAC=N (FULL JACOBIAN)
C                    LJAC=MLJAC+MUJAC+1  IF MLJAC<N (BANDED JAC.)
C                 AND
C                    LMAS=0              IF IMAS=0
C                    LMAS=N              IF IMAS=1 AND MLMAS=N (FULL)
C                    LMAS=MLMAS+MUMAS+1  IF MLMAS<N (BANDED MASS-M.)
C                 AND
C                    LE=N               IF MLJAC=N (FULL JACOBIAN)
C                    LE=2*MLJAC+MUJAC+1 IF MLJAC<N (BANDED JAC.)
C
C                 IN THE USUAL CASE WHERE THE JACOBIAN IS FULL AND THE
C                 MASS-MATRIX IS THE INDENTITY (IMAS=0), THE MINIMUM
C                 STORAGE REQUIREMENT IS
C                             LWORK = 4*N*N+8*N+7.
C
C     LWORK       DECLARED LENGHT OF ARRAY "WORK".
C
C     IWORK       INTEGER WORKING SPACE OF LENGHT "LIWORK".
C                 IWORK(1),IWORK(2),...,IWORK(7) SERVE AS PARAMETERS
C                 FOR THE CODE. FOR STANDARD USE, SET IWORK(1),..,
C                 IWORK(7) TO ZERO BEFORE CALLING.
C                 IWORK(8),...,IWORK(LIWORK) SERVE AS WORKING AREA.
C                 "LIWORK" MUST BE AT LEAST 3*N+7.
C
C     LIWORK      DECLARED LENGHT OF ARRAY "IWORK".
C
C     LRCONT      DECLARED LENGTH OF COMMON BLOCK
C                  >>>  COMMON /CONT/ICONT(4),RCONT(LRCONT)  <<<
C                 WHICH MUST BE DECLARED IN THE CALLING PROGRAM.
C                 "LRCONT" MUST BE AT LEAST
C                             4*N+4 .
C                 THIS IS USED FOR STORING THE COEFFICIENTS OF THE
C                 CONTINUOUS SOLUTION AND MAKES THE CALLING LIST FOR THE
C                 FUNCTION "CONTR5" AS SIMPLE AS POSSIBLE.
C
C ----------------------------------------------------------------------
C
C     SOPHISTICATED SETTING OF PARAMETERS
C     -----------------------------------
C              SEVERAL PARAMETERS OF THE CODE ARE TUNED TO MAKE IT WORK
C              WELL. THEY MAY BE DEFINED BY SETTING WORK(1),..,WORK(7)
C              AS WELL AS IWORK(1),..,IWORK(7) DIFFERENT FROM ZERO.
C              FOR ZERO INPUT, THE CODE CHOOSES DEFAULT VALUES:
C
C    IWORK(1)  IF IWORK(1).NE.0, THE CODE TRANSFORMS THE JACOBIAN
C              MATRIX TO HESSENBERG FORM. THIS IS PARTICULARLY
C              ADVANTAGEOUS FOR LARGE SYSTEMS WITH FULL JACOBIAN.
C              IT DOES NOT WORK FOR BANDED JACOBIAN (MLJAC<N)
C              AND NOT FOR IMPLICIT SYSTEMS (IMAS=1). IT IS
C              ALSO NOT GOOD FOR SPARSE JACOBIANS.
C
C    IWORK(2)  THIS IS THE MAXIMAL NUMBER OF ALLOWED STEPS.
C              THE DEFAULT VALUE (FOR IWORK(2)=0) IS 10000.
C
C    IWORK(3)  THE MAXIMUM NUMBER OF NEWTON ITERATIONS FOR THE
C              SOLUTION OF THE IMPLICIT SYSTEM IN EACH STEP.
C              THE DEFAULT VALUE (FOR IWORK(3)=0) IS 7.
C
C    IWORK(4)  IF IWORK(4).EQ.0 THE EXTRAPOLATED COLLOCATION SOLUTION
C              IS TAKEN AS STARTING VALUE FOR NEWTON'S METHOD.
C              IF IWORK(4).NE.0 ZERO STARTING VALUES ARE USED.
C              THE LATTER IS RECOMMENDED IF NEWTON'S METHOD HAS
C              DIFFICULTIES WITH CONVERGENCE (THIS IS THE CASE WHEN
C              NSTEP IS LARGER THAN NACCPT + NREJCT).
C              DEFAULT IS IWORK(4)=0.
C
C       THE FOLLOWING 3 PARAMETERS ARE IMPORTANT FOR
C       DIFFERENTIAL-ALGEBRAIC SYSTEMS OF INDEX > 1.
C       THE FUNCTION-SUBROUTINE SHOULD BE WRITTEN SUCH THAT
C       THE INDEX 1,2,3 VARIABLES APPEAR IN THIS ORDER.
C       IN ESTIMATING THE ERROR THE INDEX 2 VARIABLES ARE
C       MULTIPLIED BY H, THE INDEX 3 VARIABLES BY H**2.
C
C    IWORK(5)  DIMENSION OF THE INDEX 1 VARIABLES (MUST BE > 0). FOR
C              ODE'S THIS EQUALS THE DIMENSION OF THE SYSTEM.
C              DEFAULT IWORK(5)=N.
C
C    IWORK(6)  DIMENSION OF THE INDEX 2 VARIABLES. DEFAULT IWORK(6)=0.
C
C    IWORK(7)  DIMENSION OF THE INDEX 3 VARIABLES. DEFAULT IWORK(7)=0.
C
C
C    WORK(1)   UROUND, THE ROUNDING UNIT, DEFAULT 1.D-16.
C
C    WORK(2)   THE SAFETY FACTOR IN STEP SIZE PREDICTION,
C              DEFAULT 0.9D0.
C
C    WORK(3)   DECIDES WHETHER THE JACOBIAN SHOULD BE RECOMPUTED;
C              INCREASE WORK(3), TO 0.1 SAY, WHEN JACOBIAN EVALUATIONS
C              ARE COSTLY. FOR SMALL SYSTEMS WORK(3) SHOULD BE SMALLER
C              (0.001D0, SAY). NEGATIV WORK(3) FORCES THE CODE THE
C              COMPUTE THE JACOBIAN AFTER EVERY ACCEPTED STEP.
C              DEFAULT 0.001D0.
C
C    WORK(4)   STOPPING CRITERION FOR NEWTON'S METHOD, USUALLY CHOSEN <1.
C              SMALLER VALUES OF WORK(4) MAKE THE CODE SLOWER, BUT SAFER.
C              DEFAULT 0.03D0.
C
C    WORK(5) AND WORK(6) : IF WORK(5) < HNEW/HOLD < WORK(6), THEN THE
C              STEP SIZE IS NOT CHANGED. THIS SAVES, TOGETHER WITH A
C              LARGE WORK(3), LU-DECOMPOSITIONS AND COMPUTING TIME FOR
C              LARGE SYSTEMS. FOR SMALL SYSTEMS ONE MAY HAVE
C              WORK(5)=1.D0, WORK(6)=1.2D0, FOR LARGE FULL SYSTEMS
C              WORK(5)=0.99D0, WORK(6)=2.D0 MIGHT BE GOOD.
C              DEFAULTS WORK(5)=1.D0, WORK(6)=1.2D0 .
C
C    WORK(7)   MAXIMAL STEP SIZE, DEFAULT XEND-X.
C
C-----------------------------------------------------------------------
C
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
C                   IDID<0 COMPUTATION UNSUCCESSFUL.
C
C-----------------------------------------------------------------------
C *** *** *** *** *** *** *** *** *** *** *** *** ***
C          DECLARATIONS
C *** *** *** *** *** *** *** *** *** *** *** *** ***
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER*2(I-N)
      DIMENSION Y(N),ATOL(1),RTOL(1),WORK(LWORK),IWORK(LIWORK)
      LOGICAL IMPLCT,JBAND,ARRET,STARTN
      EXTERNAL FCN,JAC,MAS,SOLOUT
      COMMON /ierfs/ierfs,IERDUM
      COMMON /iabort/iabort
      COMMON /NJAC/ NJAC,NJACN,NJACB
      COMMON/STAT/NFCN,NSTEP,NACCPT,NREJCT,NDEC,NSOL
C --- COMMON STAT CAN BE INSPECTED FOR STATISTICAL PURPOSES:
C ---    NFCN      NUMBER OF FUNCTION EVALUATIONS (THOSE FOR NUMERICAL
C                  EVALUATION OF THE JACOBIAN ARE NOT COUNTED)
C ---    NJAC      NUMBER OF JACOBIAN EVALUATIONS (EITHER ANALYTICALLY
C                  OR NUMERICALLY)
C ---    NSTEP     NUMBER OF COMPUTED STEPS
C ---    NACCPT    NUMBER OF ACCEPTED STEPS
C ---    NREJCT    NUMBER OF REJECTED STEPS (DUE TO ERROR TEST),
C                  (STEP REJECTIONS IN THE FIRST STEP ARE NOT COUNTED)
C ---    NDEC      NUMBER OF LU-DECOMPOSITIONS OF BOTH MATRICES
C ---    NSOL      NUMBER OF FORWARD-BACKWARD SUBSTITUTIONS, OF BOTH
C                  SYSTEMS; THE NSTEP FORWARD-BACKWARD SUBSTITUTIONS,
C                  NEEDED FOR STEP SIZE SELECTION, ARE NOT COUNTED
C *** *** *** *** *** *** ***
C        SETTING THE PARAMETERS
C *** *** *** *** *** *** ***
       ierfs=0
       iabort=0
       NFCN=0
       NJAC=0
       NSTEP=0
       NACCPT=0
       NREJCT=0
       NDEC=0
       NSOL=0
       ARRET=.FALSE.
C -------- SWITCH FOR TRANSFORMATION OF JACOBIAN TO HESSIAN FORM ---
      NHESS=IWORK(1)
      IF (N.LE.2) NHESS=0
C -------- NMAX , THE MAXIMAL NUMBER OF STEPS -----
      IF(IWORK(2).EQ.0)THEN
         NMAX=10000
      ELSE
         NMAX=IWORK(2)
         IF(NMAX.LE.0)THEN
#if defined(hib)
            WRITE(6,*)' WRONG INPUT IWORK(2)=',IWORK(2)
#endif
            ARRET=.TRUE.
         END IF
      END IF
C -------- NIT    MAXIMAL NUMBER OF NEWTON ITERATIONS
      IF(IWORK(3).EQ.0)THEN
         NIT=7
      ELSE
         NIT=IWORK(3)
         IF(NIT.LE.0)THEN
#if defined(hib)
            WRITE(6,*)' CURIOUS INPUT IWORK(3)=',IWORK(3)
#endif
            ARRET=.TRUE.
         END IF
      END IF
C -------- STARTN  SWITCH FOR STARTING VALUES OF NEWTON ITERATIONS
      IF(IWORK(4).EQ.0)THEN
         STARTN=.FALSE.
      ELSE
         STARTN=.TRUE.
      END IF
C -------- PARAMETER FOR DIFFERENTIAL-ALGEBRAIC COMPONENTS
      NIND1=IWORK(5)
      NIND2=IWORK(6)
      NIND3=IWORK(7)
      IF (NIND1.EQ.0) NIND1=N
      IF (NIND1+NIND2+NIND3.NE.N)THEN
#if defined(hib)
        WRITE(6,*)' CURIOUS INPUT FOR WORK(5,6,7)=',NIND1,NIND2,NIND3
#endif
        ARRET=.TRUE.
      END IF
C -------- UROUND   SMALLEST NUMBER SATISFYING 1.D0+UROUND>1.D0
      IF(WORK(1).EQ.0.D0)THEN
         UROUND=1.D-16
      ELSE
         UROUND=WORK(1)
         IF(UROUND.LE.1.D-19.OR.UROUND.GE.1.D0)THEN
#if defined(hib)
            WRITE(6,*)' COEFFICIENTS HAVE 20 DIGITS, UROUND=',WORK(1)
#endif
            ARRET=.TRUE.
         END IF
      END IF
C --------- SAFE     SAFETY FACTOR IN STEP SIZE PREDICTION
      IF(WORK(2).EQ.0.D0)THEN
         SAFE=0.9D0
      ELSE
         SAFE=WORK(2)
         IF(SAFE.LE..001D0.OR.SAFE.GE.1.D0)THEN
#if defined(hib)
            WRITE(6,*)' CURIOUS INPUT FOR WORK(2)=',WORK(2)
#endif
            ARRET=.TRUE.
         END IF
      END IF
C ------ THET     DECIDES WHETHER THE JACOBIAN SHOULD BE RECOMPUTED;
      IF(WORK(3).EQ.0.D0)THEN
         THET=0.001D0
      ELSE
         THET=WORK(3)
      END IF
C --- FNEWT  STOPPING CRITERION FOR NEWTON'S METHOD, USUALLY CHOSEN <1.
      IF(WORK(4).EQ.0.D0)THEN
         FNEWT=0.03D0
      ELSE
         FNEWT=WORK(4)
      END IF
C --- QUOT1 AND QUOT2: IF QUOT1 < HNEW/HOLD < QUOT2, STEP SIZE = CONST.
      IF(WORK(5).EQ.0.D0)THEN
         QUOT1=1.D0
      ELSE
         QUOT1=WORK(5)
      END IF
      IF(WORK(6).EQ.0.D0)THEN
         QUOT2=1.2D0
      ELSE
         QUOT2=WORK(6)
      END IF
C -------- MAXIMAL STEP SIZE
      IF(WORK(7).EQ.0.D0)THEN
         HMAX=XEND-X
      ELSE
         HMAX=WORK(7)
      END IF
C --------- CHECK IF TOLERANCES ARE O.K.
      IF (ITOL.EQ.0) THEN
          IF (ATOL(1).LE.0.D0.OR.RTOL(1).LE.10.D0*UROUND) THEN
#if defined(hib)
              WRITE (6,*) ' TOLERANCES ARE TOO SMALL'
#endif
              ARRET=.TRUE.
          END IF
      ELSE
          DO 15 I=1,N
          IF (ATOL(I).LE.0.D0.OR.RTOL(I).LE.10.D0*UROUND) THEN
#if defined(hib)
              WRITE (6,*) ' TOLERANCES(',I,') ARE TOO SMALL'
#endif
              ARRET=.TRUE.
          END IF
  15      CONTINUE
      END IF
C *** *** *** *** *** *** *** *** *** *** *** *** ***
C         COMPUTATION OF ARRAY ENTRIES
C *** *** *** *** *** *** *** *** *** *** *** *** ***
C ---- IMPLICIT, BANDED OR NOT ?
      IMPLCT=IMAS.NE.0
      JBAND=MLJAC.NE.N
C -------- COMPUTATION OF THE ROW-DIMENSIONS OF THE 2-ARRAYS ---
C -- JACOBIAN  AND  MATRICES E1, E2
      IF(JBAND)THEN
         LDJAC=MLJAC+MUJAC+1
         LDE1=2*MLJAC+MUJAC+1
      ELSE
         LDJAC=N
         LDE1=N
      END IF
C -- MASS MATRIX
      IF (IMPLCT) THEN
          IF (MLMAS.NE.N) THEN
              LDMAS=MLMAS+MUMAS+1
          ELSE
              LDMAS=N
          END IF
C ------ BANDWITH OF "MAS" NOT LARGER THAN BANDWITH OF "JAC"
          IF (MLMAS.GT.MLJAC.OR.MUMAS.GT.MUJAC) THEN
#if defined(hib)
              WRITE (6,*) 'BANDWITH OF "MAS" NOT LARGER THAN BANDWITH OF
     & "JAC"'
#endif
            ARRET=.TRUE.
          END IF
      ELSE
          LDMAS=0
      END IF
      LDMAS2=MAX(1,LDMAS)
C ------ HESSENBERG OPTION ONLY FOR EXPLICIT EQU. WITH FULL JACOBIAN
      IF ((IMPLCT.OR.JBAND).AND.NHESS.NE.0) THEN
#if defined(hib)
         WRITE(6,*)' HESSENBERG OPTION ONLY FOR EXPLICIT EQUATIONS WITH
     &FULL JACOBIAN'
#endif
         ARRET=.TRUE.
      END IF
C ------- PREPARE THE ENTRY-POINTS FOR THE ARRAYS IN WORK -----
      IEZ1=8
      IEZ2=IEZ1+N
      IEZ3=IEZ2+N
      IEY0=IEZ3+N
      IESCAL=IEY0+N
      IEF1=IESCAL+N
      IEF2=IEF1+N
      IEF3=IEF2+N
      IEJAC=IEF3+N
      IEMAS=IEJAC+N*LDJAC
      IEE1=IEMAS+N*LDMAS
      IEE2R=IEE1+N*LDE1
      IEE2I=IEE2R+N*LDE1
C ------ TOTAL STORAGE REQUIREMENT -----------
      ISTORE=IEE2I+N*LDE1-1
      IF(ISTORE.GT.LWORK)THEN
#if defined(hib)
         WRITE(6,*)' INSUFFICIENT STORAGE FOR WORK, MIN. LWORK=',ISTORE
#endif
         ARRET=.TRUE.
      END IF
C ------- ENTRY POINTS FOR INTEGER WORKSPACE -----
      IEIP1=8
      IEIP2=IEIP1+N
      IEIPH=IEIP2+N
C --------- TOTAL REQUIREMENT ---------------
      ISTORE=IEIPH+N-1
      IF(ISTORE.GT.LIWORK)THEN
#if defined(hib)
         WRITE(6,*)' INSUFF. STORAGE FOR IWORK, MIN. LIWORK=',ISTORE
#endif
         ARRET=.TRUE.
      END IF
C --------- CONTROL OF LENGTH OF COMMON BLOCK "CONT" -------
      IF(LRCONT.LT.(4*N+4))THEN
#if defined(hib)
         WRITE(6,*)' INSUFF. STORAGE FOR RCONT, MIN. LRCONT=',4*N+4
#endif
         ARRET=.TRUE.
      END IF
C ------ WHEN A FAIL HAS OCCURED, WE RETURN WITH IDID=-1
      IF (ARRET) THEN
         IDID=-1
         RETURN
      END IF
C -------- CALL TO CORE INTEGRATOR ------------
      CALL RADCOR(N,FCN,X,Y,XEND,HMAX,H,RTOL,ATOL,ITOL,
     &   JAC,IJAC,MLJAC,MUJAC,MAS,IMAS,MLMAS,MUMAS,SOLOUT,IOUT,IDID,
     &   NMAX,UROUND,SAFE,THET,FNEWT,QUOT1,QUOT2,NIT,NHESS,STARTN,
     &   NIND1,NIND2,NIND3,
     &   IMPLCT,JBAND,LDJAC,LDE1,LDMAS2,WORK(IEZ1),WORK(IEZ2),
     &   WORK(IEZ3),WORK(IEY0),WORK(IESCAL),WORK(IEF1),WORK(IEF2),
     &   WORK(IEF3),WORK(IEJAC),WORK(IEE1),WORK(IEE2R),WORK(IEE2I),
     &   WORK(IEMAS),IWORK(IEIP1),IWORK(IEIP2),IWORK(IEIPH))
C ----------- RETURN -----------
      RETURN
      END
C
C
C
C  ----- ... AND HERE IS THE CORE INTEGRATOR  ----------
C
      SUBROUTINE RADCOR(N,FCN,X,Y,XEND,HMAX,H,RTOL,ATOL,ITOL,
     &   JAC,IJAC,MLJAC,MUJAC,MAS,IMAS,MLMAS,MUMAS,SOLOUT,IOUT,IDID,
     &   NMAX,UROUND,SAFE,THET,FNEWT,QUOT1,QUOT2,NIT,NHESS,STARTN,
     &   NIND1,NIND2,NIND3,IMPLCT,BANDED,LDJAC,LDE1,LDMAS,Z1,Z2,Z3,
     &   Y0,SCAL,F1,F2,F3,FJAC,E1,E2R,E2I,FMAS,IP1,IP2,IPHES)
C ----------------------------------------------------------
C     CORE INTEGRATOR FOR RADAU5
C     PARAMETERS SAME AS IN RADAU5 WITH WORKSPACE ADDED
C ----------------------------------------------------------
C         DECLARATIONS
C ----------------------------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER*2(I-N)
      REAL*8 Y(N),Z1(N),Z2(N),Z3(N),Y0(N),SCAL(N),F1(N),F2(N),F3(N)
      REAL*8 FJAC(LDJAC,N),FMAS(LDMAS,N)
      REAL*8 E1(LDE1,N),E2R(LDE1,N),E2I(LDE1,N)
      REAL*8 ATOL(1),RTOL(1)
      INTEGER*2 IP1(N),IP2(N),IPHES(N)
      COMMON /NJAC/ NJAC,NJACN,NJACB
      COMMON/STAT/NFCN,NSTEP,NACCPT,NREJCT,NDEC,NSOL
c      COMMON /CONT/NN,NN2,NN3,NDUM
c      COMMON /CONT1/XSOL,HSOL,C2M1,C1M1,CONT(1)
      COMMON /CONT/NN,NN2,NN3,NDUM,XSOL,HSOL,C2M1,C1M1,CONT(1)
C***
      COMMON /ierfs/ierfs,IERDUM
      COMMON /iabort/iabort
C***
      LOGICAL REJECT,FIRST,IMPLCT,BANDED,CALJAC,STARTN
      LOGICAL INDEX1,INDEX2,INDEX3
C *** *** *** *** *** *** ***
C  INITIALISATIONS
C *** *** *** *** *** *** ***
C***
      IONE=1
C***
C --------- DUPLIFY N FOR COMMON BLOCK CONT -----
      NN=N
      NN2=2*N
      NN3=3*N
C -------- CHECK THE INDEX OF THE PROBLEM -----
      INDEX1=NIND1.NE.0
      INDEX2=NIND2.NE.0
      INDEX3=NIND3.NE.0
C ------- COMPUTE MASS MATRIX FOR IMPLICIT CASE ----------
      IF(IMPLCT)CALL MAS(N,FMAS,LDMAS)
C ---------- CONSTANTS ---------
      SQ6=DSQRT(6.D0)
      C1=(4.D0-SQ6)/10.D0
      C2=(4.D0+SQ6)/10.D0
      C1M1=C1-1.D0
      C2M1=C2-1.D0
      C1MC2=C1-C2
      DD1=-(13.D0+7.D0*SQ6)/3.D0
      DD2=(-13.D0+7.D0*SQ6)/3.D0
      DD3=-1.D0/3.D0
      U1=(6.D0+81.D0**(1.D0/3.D0)-9.D0**(1.D0/3.D0))/30.D0
      ALPH=(12.D0-81.D0**(1.D0/3.D0)+9.D0**(1.D0/3.D0))/60.D0
      BETA=(81.D0**(1.D0/3.D0)+9.D0**(1.D0/3.D0))*DSQRT(3.D0)/60.D0
      CNO=ALPH**2+BETA**2
      T11=9.1232394870892942792D-02
      T12=-0.14125529502095420843D0
      T13=-3.0029194105147424492D-02
      T21=0.24171793270710701896D0
      T22=0.20412935229379993199D0
      T23=0.38294211275726193779D0
      T31=0.96604818261509293619D0
      TI11=4.3255798900631553510D0
      TI12=0.33919925181580986954D0
      TI13=0.54177053993587487119D0
      TI21=-4.1787185915519047273D0
      TI22=-0.32768282076106238708D0
      TI23=0.47662355450055045196D0
      TI31=-0.50287263494578687595D0
      TI32=2.5719269498556054292D0
      TI33=-0.59603920482822492497D0
      POSNEG=DSIGN(1.D0,XEND-X)
      HMAXN=DMIN1(DABS(HMAX),DABS(XEND-X))
      IF (DABS(H).LE.10.D0*UROUND) H=1.0D-6
      H=DMIN1(DABS(H),HMAXN)
      H=DSIGN(H,POSNEG)
      HOLD=H
      REJECT=.FALSE.
      FIRST=.TRUE.
      FACCON=1.D0
      CFAC=SAFE*(1+2*NIT)
      NSING=0
      XOLD=X
      IF (IOUT.NE.0) THEN
          IRTRN=1
          NRSOL=1
          XOSOL=XOLD
          XSOL=X
          DO 7 I=1,N
  7       CONT(I)=Y(I)
          NSOLU=N
          HSOL=HOLD
          CALL SOLOUT(NRSOL,XOSOL,XSOL,CONT,NSOLU,IRTRN)
          IF (IRTRN.LT.0) GOTO 79
      END IF
      MLE=MLJAC
      MUE=MUJAC
      MBJAC=MLJAC+MUJAC+1
      MBB=MLMAS+MUMAS+1
      MDIAG=MLE+MUE+1
      MDIFF=MLE+MUE-MUMAS
      MBDIAG=MUMAS+1
      N2=2*N
      N3=3*N
      IF (ITOL.EQ.0) THEN
          DO 8 I=1,N
   8      SCAL(I)=ATOL(1)+RTOL(1)*DABS(Y(I))
      ELSE
          DO 9 I=1,N
   9      SCAL(I)=ATOL(I)+RTOL(I)*DABS(Y(I))
      END IF
      HHFAC=H
      CALL FCN(N,X,Y,Y0)
        if (ierfs.ne.0) goto 78
      NFCN=NFCN+1
C --- BASIC INTEGRATION STEP
  10  CONTINUE
C *** *** *** *** *** *** ***
C  COMPUTATION OF THE JACOBIAN
C *** *** *** *** *** *** ***
      NJAC=NJAC+1
      IF (IJAC.EQ.0) THEN
C --- COMPUTE JACOBIAN MATRIX NUMERICALLY
          IF (BANDED) THEN
#if (full)
C --- JACOBIAN IS BANDED
              MUJACP=MUJAC+1
              MD=MIN(MBJAC,N)
              DO 16 K=1,MD
              J=K
 12           F1(J)=Y(J)
C***              F2(J)=DSQRT(UROUND*DMAX1(1.D-5,DABS(Y(J))))
              F2(J)=DSQRT(UROUND*DMAX1(1.D0,DABS(Y(J))))
              Y(J)=Y(J)+F2(J)
              J=J+MD
              IF (J.LE.N) GOTO 12
              CALL FCN(N,X,Y,CONT)
                if (ierfs.ne.0) goto 78
              J=K
              LBEG=MAX(1,J-MUJAC)
 14           LEND=MIN(N,J+MLJAC)
              Y(J)=F1(J)
              MUJACJ=MUJACP-J
              DO 15 L=LBEG,LEND
 15           FJAC(L+MUJACJ,J)=(CONT(L)-Y0(L))/F2(J)
              J=J+MD
              LBEG=LEND+1
              IF (J.LE.N) GOTO 14
 16           CONTINUE
#endif
          ELSE
C --- JACOBIAN IS FULL
              DO 18 I=1,N
              YSAFE=Y(I)
C              DELT=DSQRT(UROUND*DMAX1(1.D-5,ABS(YSAFE)))
              DELT=DSQRT(UROUND*DMAX1(1.D0,DABS(YSAFE)))
              Y(I)=YSAFE+DELT
              CALL FCN(N,X,Y,CONT)
              Y(I)=YSAFE
                if (ierfs.ne.0) goto 78
              DO 17 J=1,N
  17          FJAC(J,I)=(CONT(J)-Y0(J))/DELT
  18          CONTINUE
          END IF
      ELSE
C --- COMPUTE JACOBIAN MATRIX ANALYTICALLY
          CALL JAC(N,X,Y,FJAC,LDJAC)
      END IF
      CALJAC=.TRUE.
#if (full)
      IF (NHESS.NE.0) CALL DELMHE (LDJAC,N,IONE,N,FJAC,IPHES)
#endif
  20  CONTINUE
C *** *** *** *** *** *** ***
C  COMPUTE THE MATRICES E1 AND E2 AND THEIR DECOMPOSITIONS
C *** *** *** *** *** *** ***
      FAC1=1.D0/(H*U1)
      ALPHN=ALPH/(H*CNO)
      BETAN=BETA/(H*CNO)
      IF (IMPLCT) THEN
          IF (BANDED) THEN
C --- THE MATRIX E (B IS A BANDED MATRIX, JACOBIAN A BANDED MATRIX)
#if (full)
              DO 127 J=1,N
              I1J=MAX0(1,MUJAC+2-J)
              I2J=MIN(MBJAC,MUJAC+1-J+N)
              DO 125 I=I1J,I2J
              FJ=-FJAC(I,J)
              IMLE=I+MLE
              E1(IMLE,J)=FJ
              E2R(IMLE,J)=FJ
 125          E2I(IMLE,J)=0.D0
              I1B=MAX0(1,MUMAS+2-J)
              I2B=MIN0(MBB,MUMAS+1-J+N)
              DO 126 I=I1B,I2B
              IB=I+MDIFF
              BB=FMAS(I,J)
              E1(IB,J)=E1(IB,J)+FAC1*BB
              E2R(IB,J)=E2R(IB,J)+ALPHN*BB
 126          E2I(IB,J)=BETAN*BB
 127          CONTINUE
              CALL DECB(N,LDE1,E1,MLE,MUE,IP1,IER)
              IF (IER.NE.0) GOTO 78
              CALL DECBC(N,LDE1,E2R,E2I,MLE,MUE,IP2,IER)
              IF (IER.NE.0) GOTO 78
#endif
          ELSE
              IF (MLMAS.NE.N) THEN
C --- THE MATRIX E (B IS A BANDED MATRIX, JACOBIAN A FULL MATRIX)
                  DO 225 J=1,N
                  DO 225 I=1,N
                  FJ=-FJAC(I,J)
                  E1(I,J)=FJ
                  E2R(I,J)=FJ
 225              E2I(I,J)=0.D0
                  DO 226 J=1,N
                  I1=MAX0(1,J-MUMAS)
                  I2=MIN0(N,J+MLMAS)
                  DO 226 I=I1,I2
                  BB=FMAS(I-J+MBDIAG,J)
                  E1(I,J)=E1(I,J)+FAC1*BB
                  E2R(I,J)=E2R(I,J)+ALPHN*BB
 226              E2I(I,J)=BETAN*BB
                  CALL DEC(N,LDE1,E1,IP1,IER)
                  IF (IER.NE.0) GOTO 78
                  CALL DECC(N,LDE1,E2R,E2I,IP2,IER)
                  IF (IER.NE.0) GOTO 78
              ELSE
C --- THE MATRIX E (B IS A FULL MATRIX, JACOBIAN A FULL MATRIX)
                  IF (MLJAC.EQ.N) THEN
                      DO 324 J=1,N
                      DO 324 I=1,N
                      FJ=-FJAC(I,J)
                      BB=FMAS(I,J)
                      E1(I,J)=BB*FAC1+FJ
                      E2R(I,J)=BB*ALPHN+FJ
 324                  E2I(I,J)=BB*BETAN
                      CALL DEC(N,LDE1,E1,IP1,IER)
                      IF (IER.NE.0) GOTO 78
                      CALL DECC(N,LDE1,E2R,E2I,IP2,IER)
                      IF (IER.NE.0) GOTO 78
                  ELSE
C --- THE MATRIX E (B IS A FULL MATRIX, JACOBIAN A BANDED MATRIX)
                  END IF
              END IF
          END IF
      ELSE
          IF (BANDED) THEN
#if (full)
C --- THE MATRIX E (B=IDENTITY, JACOBIAN A BANDED MATRIX)
              DO 427 J=1,N
              I1J=MAX0(1,MUJAC+2-J)
              I2J=MIN(MBJAC,MUJAC+1-J+N)
              DO 425 I=I1J,I2J
              FJ=-FJAC(I,J)
              IMLE=I+MLE
              E1(IMLE,J)=FJ
              E2R(IMLE,J)=FJ
 425          E2I(IMLE,J)=0.D0
              E1(MDIAG,J)=E1(MDIAG,J)+FAC1
              E2R(MDIAG,J)=E2R(MDIAG,J)+ALPHN
              E2I(MDIAG,J)=BETAN
 427          CONTINUE
              CALL DECB(N,LDE1,E1,MLE,MUE,IP1,IER)
              IF (IER.NE.0) GOTO 78
              CALL DECBC(N,LDE1,E2R,E2I,MLE,MUE,IP2,IER)
              IF (IER.NE.0) GOTO 78
#endif
          ELSE
C --- THE MATRIX E (B=IDENTITY, JACOBIAN A FULL MATRIX)
              IF (NHESS.EQ.0) THEN
                  DO 526 J=1,N
                  DO 525 I=1,N
                  FJ=-FJAC(I,J)
                  E1(I,J)=FJ
                  E2R(I,J)=FJ
 525              E2I(I,J)=0.D0
                  E1(J,J)=E1(J,J)+FAC1
                  E2R(J,J)=E2R(J,J)+ALPHN
 526              E2I(J,J)=BETAN
                  CALL DEC(N,LDE1,E1,IP1,IER)
                  IF (IER.NE.0) GOTO 78
                  CALL DECC(N,LDE1,E2R,E2I,IP2,IER)
                  IF (IER.NE.0) GOTO 78
              ELSE
#if (full)
                  DO 624 J=1,N-1
                  J1=J+1
                  FJ=-FJAC(J1,J)
                  E1(J1,J)=FJ
                  E2R(J1,J)=FJ
 624              E2I(J1,J)=0.D0
                  DO 626 J=1,N
                  DO 625 I=1,J
                  FJ=-FJAC(I,J)
                  E1(I,J)=FJ
                  E2I(I,J)=0.D0
 625              E2R(I,J)=FJ
                  E1(J,J)=E1(J,J)+FAC1
                  E2R(J,J)=E2R(J,J)+ALPHN
 626              E2I(J,J)=BETAN
                  CALL DECH(N,LDE1,E1,IONE,IP1,IER)
                  IF (IER.NE.0) GOTO 78
                  CALL DECHC(N,LDE1,E2R,E2I,IONE,IP2,IER)
                  IF (IER.NE.0) GOTO 78
#endif
              END IF
          END IF
      END IF
      NDEC=NDEC+1
  30  CONTINUE
      NSTEP=NSTEP+1
C***  IF (NSTEP.GT.NMAX.OR.X+.1D0*H.EQ.X.OR.DABS(H).LE.UROUND) GOTO 89
      IF (NSTEP.GT.NMAX) GOTO 89
      RR0=X+.1D0*H
      IF (RR0.EQ.X.OR.DABS(H).LE.UROUND) GOTO 89
          IF (INDEX2) THEN
             DO 465 I=NIND1+1,NIND1+NIND2
 465         SCAL(I)=SCAL(I)/HHFAC
          END IF
          IF (INDEX3) THEN
             DO 466 I=NIND1+NIND2+1,NIND1+NIND2+NIND3
 466         SCAL(I)=SCAL(I)/(HHFAC*HHFAC)
          END IF
      XPH=X+H
C *** *** *** *** *** *** ***
C  STARTING VALUES FOR NEWTON ITERATION
C *** *** *** *** *** *** ***
      IF (FIRST.OR.STARTN) THEN
         DO 32 I=1,N
         Z1(I)=0.D0
         Z2(I)=0.D0
         Z3(I)=0.D0
         F1(I)=0.D0
         F2(I)=0.D0
  32     F3(I)=0.D0
      ELSE
         C3Q=H/HOLD
         C1Q=C1*C3Q
         C2Q=C2*C3Q
         DO 35 I=1,N
         AK1=CONT(I+N)
         AK2=CONT(I+N2)
         AK3=CONT(I+N3)
         Z1I=C1Q*(AK1+(C1Q-C2M1)*(AK2+(C1Q-C1M1)*AK3))
         Z2I=C2Q*(AK1+(C2Q-C2M1)*(AK2+(C2Q-C1M1)*AK3))
         Z3I=C3Q*(AK1+(C3Q-C2M1)*(AK2+(C3Q-C1M1)*AK3))
         Z1(I)=Z1I
         Z2(I)=Z2I
         Z3(I)=Z3I
         F1(I)=TI11*Z1I+TI12*Z2I+TI13*Z3I
         F2(I)=TI21*Z1I+TI22*Z2I+TI23*Z3I
  35     F3(I)=TI31*Z1I+TI32*Z2I+TI33*Z3I
      END IF
C *** *** *** *** *** *** ***
C  LOOP FOR THE SIMPLIFIED NEWTON ITERATION
C *** *** *** *** *** *** ***
            NEWT=0
            FACCON=DMAX1(FACCON,UROUND)**0.8D0
            THETA=DABS(THET)
  40        CONTINUE
            IF (NEWT.GE.NIT) GOTO 78
C ---     COMPUTE THE RIGHT-HAND SIDE
            DO 41 I=1,N
  41        CONT(I)=Y(I)+Z1(I)
            CALL FCN(N,X+C1*H,CONT,Z1)
              if (ierfs.ne.0) goto 78
            DO 42 I=1,N
  42        CONT(I)=Y(I)+Z2(I)
            CALL FCN(N,X+C2*H,CONT,Z2)
              if (ierfs.ne.0) goto 78
            DO 43 I=1,N
  43        CONT(I)=Y(I)+Z3(I)
            CALL FCN(N,XPH,CONT,Z3)
              if (ierfs.ne.0) goto 78
            NFCN=NFCN+3
C ---     SOLVE THE LINEAR SYSTEMS
            IF (IMPLCT) THEN
                IF (MLMAS.NE.N) THEN
                    DO 146 I=1,N
                    S1=0.0D0
                    S2=0.0D0
                    S3=0.0D0
                    J1B=MAX0(1,I-MLMAS)
                    J2B=MIN0(N,I+MUMAS)
                    DO 145 J=J1B,J2B
                    BB=FMAS(I-J+MBDIAG,J)
                    S1=S1-BB*F1(J)
                    S2=S2-BB*F2(J)
 145                S3=S3-BB*F3(J)
                    A1=Z1(I)
                    A2=Z2(I)
                    A3=Z3(I)
                    Z1(I)=S1*FAC1          +TI11*A1+TI12*A2+TI13*A3
                    Z2(I)=S2*ALPHN-S3*BETAN+TI21*A1+TI22*A2+TI23*A3
 146                Z3(I)=S3*ALPHN+S2*BETAN+TI31*A1+TI32*A2+TI33*A3
                    IF (BANDED) THEN
#if (full)
                        CALL SOLB(N,LDE1,E1,MLE,MUE,Z1,IP1)
                        CALL SOLBC(N,LDE1,E2R,E2I,MLE,MUE,Z2,Z3,IP2)
#endif
                    ELSE
                        CALL SOL(N,LDE1,E1,Z1,IP1)
                        CALL SOLC(N,LDE1,E2R,E2I,Z2,Z3,IP2)
                    END IF
                ELSE
                    DO 246 I=1,N
                    S1=0.0D0
                    S2=0.0D0
                    S3=0.0D0
                    DO 245 J=1,N
                    BB=FMAS(I,J)
                    S1=S1-BB*F1(J)
                    S2=S2-BB*F2(J)
 245                S3=S3-BB*F3(J)
                    A1=Z1(I)
                    A2=Z2(I)
                    A3=Z3(I)
                    Z1(I)=S1*FAC1          +TI11*A1+TI12*A2+TI13*A3
                    Z2(I)=S2*ALPHN-S3*BETAN+TI21*A1+TI22*A2+TI23*A3
 246                Z3(I)=S3*ALPHN+S2*BETAN+TI31*A1+TI32*A2+TI33*A3
                    CALL SOL(N,LDE1,E1,Z1,IP1)
                    CALL SOLC(N,LDE1,E2R,E2I,Z2,Z3,IP2)
                END IF
            ELSE
                DO 345 I=1,N
                A1=Z1(I)
                A2=Z2(I)
                A3=Z3(I)
                S2=-F2(I)
                S3=-F3(I)
                Z1(I)=-F1(I)*FAC1      +TI11*A1+TI12*A2+TI13*A3
                Z2(I)=S2*ALPHN-S3*BETAN+TI21*A1+TI22*A2+TI23*A3
 345            Z3(I)=S3*ALPHN+S2*BETAN+TI31*A1+TI32*A2+TI33*A3
                IF (BANDED) THEN
#if (full)
                    CALL SOLB(N,LDE1,E1,MLE,MUE,Z1,IP1)
                    CALL SOLBC(N,LDE1,E2R,E2I,MLE,MUE,Z2,Z3,IP2)
#endif
                ELSE
                    IF (NHESS.EQ.0) THEN
                        CALL SOL(N,LDE1,E1,Z1,IP1)
                        CALL SOLC(N,LDE1,E2R,E2I,Z2,Z3,IP2)
                    ELSE
#if (full)
                        DO 140 MM=N-2,1,-1
                        MP=N-MM
                        MP1=MP-1
                        I=IPHES(MP)
                        IF (I.EQ.MP) GOTO 110
                        ZSAFE=Z1(MP)
                        Z1(MP)=Z1(I)
                        Z1(I)=ZSAFE
                        ZSAFE=Z2(MP)
                        Z2(MP)=Z2(I)
                        Z2(I)=ZSAFE
                        ZSAFE=Z3(MP)
                        Z3(MP)=Z3(I)
                        Z3(I)=ZSAFE
 110                    CONTINUE
                        DO 100 I=MP+1,N
                        E1IMP=FJAC(I,MP1)
                        Z1(I)=Z1(I)-E1IMP*Z1(MP)
                        Z2(I)=Z2(I)-E1IMP*Z2(MP)
 100                    Z3(I)=Z3(I)-E1IMP*Z3(MP)
 140                    CONTINUE
                           CALL SOLH(N,LDE1,E1,IONE,Z1,IP1)
                           CALL SOLHC(N,LDE1,E2R,E2I,IONE,Z2,Z3,IP2)
                        DO 240 MM=1,N-2
                        MP=N-MM
                        MP1=MP-1
                        DO 200 I=MP+1,N
                        E1IMP=FJAC(I,MP1)
                        Z1(I)=Z1(I)+E1IMP*Z1(MP)
                        Z2(I)=Z2(I)+E1IMP*Z2(MP)
 200                    Z3(I)=Z3(I)+E1IMP*Z3(MP)
                        I=IPHES(MP)
                        IF (I.EQ.MP) GOTO 240
                        ZSAFE=Z1(MP)
                        Z1(MP)=Z1(I)
                        Z1(I)=ZSAFE
                        ZSAFE=Z2(MP)
                        Z2(MP)=Z2(I)
                        Z2(I)=ZSAFE
                        ZSAFE=Z3(MP)
                        Z3(MP)=Z3(I)
                        Z3(I)=ZSAFE
 240                    CONTINUE
#endif
                    END IF
                END IF
            END IF
            DO 51 I=1,N
            F1(I)=F1(I)+Z1(I)
            F2(I)=F2(I)+Z2(I)
  51        F3(I)=F3(I)+Z3(I)
            NSOL=NSOL+1
            NEWT=NEWT+1
            DYNO=0.D0
            DO 57 I=1,N
            DENOM=SCAL(I)
  57        DYNO=DYNO+(Z1(I)/DENOM)**2+(Z2(I)/DENOM)**2
     &          +(Z3(I)/DENOM)**2
            DYNO=DSQRT(DYNO/N3)
C ---     BAD CONVERGENCE OR NUMBER OF ITERATIONS TO LARGE
            IF (NEWT.GE.2.AND.NEWT.LT.NIT) THEN
                THETA=DYNO/DYNOLD
                IF (THETA.LT.0.99D0) THEN
                    FACCON=THETA/(1.0D0-THETA)
                    DYTH=FACCON*DYNO*THETA**(NIT-1-NEWT)
                    QNEWT=DMAX1(1.0D-4,DMIN1(20.0D0,DYTH/FNEWT))
                    IF (QNEWT.GE.1.0D0) THEN
                         HHFAC=.8D0*QNEWT**(-1.0D0/(4.0D0+NIT-1-NEWT))
                         H=HHFAC*H
                         REJECT=.TRUE.
                         IF (CALJAC) GOTO 20
                         GOTO 10
                    END IF
                ELSE
                    GOTO 78
                END IF
            END IF
            DYNOLD=DMAX1(DYNO,UROUND)
            DO 58 I=1,N
            F1I=F1(I)
            F2I=F2(I)
            F3I=F3(I)
            Z1(I)=T11*F1I+T12*F2I+T13*F3I
            Z2(I)=T21*F1I+T22*F2I+T23*F3I
  58        Z3(I)=T31*F1I+    F2I
            IF (FACCON*DYNO.GT.FNEWT) GOTO 40
C *** *** *** *** *** *** ***
C  ERROR ESTIMATION
C *** *** *** *** *** *** ***
      HEE1=DD1/H
      HEE2=DD2/H
      HEE3=DD3/H
      IF (IMPLCT) THEN
          DO 359 I=1,N
 359      F1(I)=HEE1*Z1(I)+HEE2*Z2(I)+HEE3*Z3(I)
          IF (MLMAS.EQ.N) THEN
              DO 361 I=1,N
              SUM=0.D0
              DO 360 J=1,N
 360          SUM=SUM+FMAS(I,J)*F1(J)
 361          F2(I)=SUM
          ELSE
              DO 363 I=1,N
              SUM=0.D0
              J1B=MAX0(1,I-MLMAS)
              J2B=MIN0(N,I+MUMAS)
              DO 362 J=J1B,J2B
 362          SUM=SUM+FMAS(I-J+MBDIAG,J)*F1(J)
 363          F2(I)=SUM
          END IF
      ELSE
          DO 461 I=1,N
 461      F2(I)=HEE1*Z1(I)+HEE2*Z2(I)+HEE3*Z3(I)
      END IF
      DO 462 I=1,N
 462  CONT(I)=F2(I)+Y0(I)
      IF (BANDED) THEN
#if (full)
          CALL SOLB(N,LDE1,E1,MLE,MUE,CONT,IP1)
#endif
      ELSE
          IF (NHESS.EQ.0) THEN
              CALL SOL(N,LDE1,E1,CONT,IP1)
          ELSE
#if (full)
              DO 340 MM=N-2,1,-1
              MP=N-MM
              I=IPHES(MP)
              IF (I.EQ.MP) GOTO 310
              ZSAFE=CONT(MP)
              CONT(MP)=CONT(I)
              CONT(I)=ZSAFE
 310          CONTINUE
              DO 300 I=MP+1,N
 300          CONT(I)=CONT(I)-FJAC(I,MP-1)*CONT(MP)
 340          CONTINUE
                CALL SOLH(N,LDE1,E1,IONE,CONT,IP1)
              DO 440 MM=1,N-2
              MP=N-MM
              DO 400 I=MP+1,N
 400          CONT(I)=CONT(I)+FJAC(I,MP-1)*CONT(MP)
              I=IPHES(MP)
              IF (I.EQ.MP) GOTO 440
              ZSAFE=CONT(MP)
              CONT(MP)=CONT(I)
              CONT(I)=ZSAFE
 440          CONTINUE
#endif
          END IF
      END IF
      ERR=0.D0
         DO 354 I=1,N
 354     ERR=ERR+(CONT(I)/SCAL(I))**2
      ERR=DMAX1(DSQRT(ERR/N),1.D-10)
      IF ((FIRST.OR.REJECT).AND.ERR.GE.1.D0) THEN
          DO 460 I=1,N
 460      CONT(I)=Y(I)+CONT(I)
          CALL FCN(N,X,CONT,F1)
            if (ierfs.ne.0) goto 78
          NFCN=NFCN+1
          DO 463 I=1,N
 463      CONT(I)=F1(I)+F2(I)
          IF (BANDED) THEN
#if (full)
              CALL SOLB(N,LDE1,E1,MLE,MUE,CONT,IP1)
#endif
          ELSE
              IF (NHESS.EQ.0) THEN
                  CALL SOL(N,LDE1,E1,CONT,IP1)
              ELSE
#if (full)
                  DO 540 MM=N-2,1,-1
                  MP=N-MM
                  I=IPHES(MP)
                  IF (I.EQ.MP) GOTO 510
                  ZSAFE=CONT(MP)
                  CONT(MP)=CONT(I)
                  CONT(I)=ZSAFE
 510              CONTINUE
                  DO 500 I=MP+1,N
 500              CONT(I)=CONT(I)-FJAC(I,MP-1)*CONT(MP)
 540              CONTINUE
                    CALL SOLH(N,LDE1,E1,IONE,CONT,IP1)
                  DO 640 MM=1,N-2
                  MP=N-MM
                  DO 600 I=MP+1,N
 600              CONT(I)=CONT(I)+FJAC(I,MP-1)*CONT(MP)
                  I=IPHES(MP)
                  IF (I.EQ.MP) GOTO 640
                  ZSAFE=CONT(MP)
                  CONT(MP)=CONT(I)
                  CONT(I)=ZSAFE
 640              CONTINUE
#endif
              END IF
          END IF
          ERR=0.D0
             DO 364 I=1,N
 364         ERR=ERR+(CONT(I)/SCAL(I))**2
          ERR=DMAX1(DSQRT(ERR/N),1.D-10)
      END IF
C --- COMPUTATION OF HNEW
C --- WE REQUIRE .2<=HNEW/H<=8.
      FAC=DMIN1(SAFE,CFAC/(NEWT+2*NIT))
      QUOT=DMAX1(.125D0,DMIN1(5.D0,ERR**.25D0/FAC))
      HNEW=H/QUOT
C *** *** *** *** *** *** ***
C  IS THE ERROR SMALL ENOUGH ?
C *** *** *** *** *** *** ***
      IF (ERR.LT.1.D0) THEN
C --- STEP IS ACCEPTED
         FIRST=.FALSE.
         NACCPT=NACCPT+1
         XOLD=X
         HOLD=H
         X=XPH
         DO 75 I=1,N
         Y(I)=Y(I)+Z3(I)
         Z2I=Z2(I)
         Z1I=Z1(I)
         CONT(I+N)=(Z2I-Z3(I))/C2M1
         AK=(Z1I-Z2I)/C1MC2
         ACONT3=Z1I/C1
         ACONT3=(AK-ACONT3)/C2
         CONT(I+N2)=(AK-CONT(I+N))/C1M1
  75     CONT(I+N3)=CONT(I+N2)-ACONT3
         IF (ITOL.EQ.0) THEN
             DO 81 I=1,N
  81         SCAL(I)=ATOL(1)+RTOL(1)*DABS(Y(I))
         ELSE
             DO 82 I=1,N
  82         SCAL(I)=ATOL(I)+RTOL(I)*DABS(Y(I))
         END IF
         IF (IOUT.NE.0) THEN
             NRSOL=NACCPT+1
             XSOL=X
             XOSOL=XOLD
             DO 77 I=1,N
  77         CONT(I)=Y(I)
             NSOLU=N
             HSOL=HOLD
             CALL SOLOUT(NRSOL,XOSOL,XSOL,CONT,NSOLU,IRTRN)
             IF (IRTRN.LT.0) GOTO 79
         END IF
         CALJAC=.FALSE.
C***         IF ((X-XEND)*POSNEG+UROUND.GT.0.D0) THEN
         RR0=(X-XEND)*POSNEG
         IF (RR0+UROUND.GT.0.D0) THEN
            H=HOPT
            IDID=1
            RETURN
         END IF
         CALL FCN(N,X,Y,Y0)
           if (ierfs.ne.0) goto 78
         NFCN=NFCN+1
         HNEW=POSNEG*DMIN1(DABS(HNEW),HMAXN)
         HOPT=HNEW
         IF (REJECT) HNEW=POSNEG*DMIN1(DABS(HNEW),DABS(H))
         REJECT=.FALSE.
c***         IF ((X+HNEW/QUOT1-XEND)*POSNEG.GT.0.D0) THEN
         IF ((X+1.1D0*HNEW/QUOT1-XEND)*POSNEG.GT.0.D0) THEN
            H=XEND-X
         ELSE
            QT=HNEW/H
            HHFAC=H
            IF (THETA.LE.THET.AND.QT.GE.QUOT1.AND.QT.LE.QUOT2) GOTO 30
            H=HNEW
         END IF
         HHFAC=H
         IF (THETA.LE.THET) GOTO 20
         GOTO 10
      ELSE
C --- STEP IS REJECTED
         REJECT=.TRUE.
         IF (FIRST) THEN
             H=H*0.1D0
             HHFAC=0.1D0
         ELSE
             HHFAC=HNEW/H
             H=HNEW
         END IF
         IF (NACCPT.GE.1) NREJCT=NREJCT+1
         IF (CALJAC) GOTO 20
         GOTO 10
      END IF
C --- UNEXPECTED STEP-REJECTION
  78  CONTINUE
C**** RHS FAIL PROCESSING
      IF (ierfs.LT.0 .OR. iabort.NE.0 .OR. NFCN.EQ.0) THEN
C**** COMPUTATION INTERRUPTED   
         GOTO 79
      END IF
C**** STEP IS REJECTED
      IF (IER.NE.0) THEN
C$if defined(hib)
C          WRITE (6,*) ' MATRIX IS SINGULAR, IER=',IER,' X=',X,' H=',H
C$endif
          NSING=NSING+1
          IF (NSING.GE.6) GOTO 89
      END IF
      H=H*0.5D0
      HHFAC=0.5D0
      REJECT=.TRUE.
      IF (CALJAC) GOTO 20
      GOTO 10
C --- FAIL EXIT
  79  IDID=-5
      GOTO 90
  89  IDID=-3
  90  CONTINUE
#if defined(hib)
c      WRITE (6,979) X,H,IER,IDID,IERFS,IRTRN
c 979  FORMAT(' EXIT OF RADAU5 AT X=',D14.7,'   H=',D14.7,'   IER=',I4
c     *       ,'  IDID=',I3,'  IERFS=',I3,'  IRTRN=',I3)
#endif
C      IDID=-1
      RETURN
      END
C
C
      REAL*8 FUNCTION CONTR5(I,X)
C ----------------------------------------------------------
C     THIS FUNCTION CAN BE USED FOR CONINUOUS OUTPUT. IT PROVIDES AN
C     APPROXIMATION TO THE I-TH COMPONENT OF THE SOLUTION AT X.
C     IT GIVES THE VALUE OF THE COLLOCATION POLYNOMIAL, DEFINED FOR
C     THE LAST SUCCESSFULLY COMPUTED STEP (BY RADAU5).
C ----------------------------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER*2(I-N)
c      COMMON /CONT/NN,NN2,NN3,NDUM
c      COMMON /CONT1/XSOL,HSOL,C2M1,C1M1,CONT(1)
      COMMON /CONT/NN,NN2,NN3,NDUM,XSOL,HSOL,C2M1,C1M1,CONT(1)
      S=(X-XSOL)/HSOL
      CONTR5=CONT(I)+S*(CONT(I+NN)+(S-C2M1)*(CONT(I+NN2)
     &     +(S-C1M1)*CONT(I+NN3)))
      RETURN
      END
C

