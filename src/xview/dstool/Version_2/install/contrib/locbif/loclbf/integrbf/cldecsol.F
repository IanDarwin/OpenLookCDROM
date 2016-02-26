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
      SUBROUTINE DEC (N, NDIM, A, IP, IER)
C VERSION REAL DOUBLE PRECISION
      INTEGER*2 N,NDIM,IP,IER,NM1,K,KP1,M,I,J
      DOUBLE PRECISION A,T
      DIMENSION A(NDIM,N), IP(N)
C-----------------------------------------------------------------------
C  MATRIX TRIANGULARIZATION BY GAUSSIAN ELIMINATION.
C  INPUT..
C     N = ORDER OF MATRIX.
C     NDIM = DECLARED DIMENSION OF ARRAY  A .
C     A = MATRIX TO BE TRIANGULARIZED.
C  OUTPUT..
C     A(I,J), I.LE.J = UPPER TRIANGULAR FACTOR, U .
C     A(I,J), I.GT.J = MULTIPLIERS = LOWER TRIANGULAR FACTOR, I - L.
C     IP(K), K.LT.N = INDEX OF K-TH PIVOT ROW.
C     IP(N) = (-1)**(NUMBER OF INTERCHANGES) OR O .
C     IER = 0 IF MATRIX A IS NONSINGULAR, OR K IF FOUND TO BE
C           SINGULAR AT STAGE K.
C  USE  SOL  TO OBTAIN SOLUTION OF LINEAR SYSTEM.
C  DETERM(A) = IP(N)*A(1,1)*A(2,2)*...*A(N,N).
C  IF IP(N)=O, A IS SINGULAR, SOL WILL DIVIDE BY ZERO.
C
C  REFERENCE..
C     C. B. MOLER, ALGORITHM 423, LINEAR EQUATION SOLVER,
C     C.A.C.M. 15 (1972), P. 274.
C-----------------------------------------------------------------------
      IER = 0
      IP(N) = 1
      IF (N .EQ. 1) GO TO 70
      NM1 = N - 1
      DO 60 K = 1,NM1
        KP1 = K + 1
        M = K
        DO 10 I = KP1,N
 10       IF (DABS(A(I,K)) .GT. DABS(A(M,K))) M = I
        IP(K) = M
        T = A(M,K)
        IF (M .EQ. K) GO TO 20
        IP(N) = -IP(N)
        A(M,K) = A(K,K)
        A(K,K) = T
 20     IF (T .EQ. 0.D0) GO TO 80
        T = 1.D0/T
        DO 30 I = KP1,N
 30       A(I,K) = -A(I,K)*T
        DO 50 J = KP1,N
          T = A(M,J)
          A(M,J) = A(K,J)
          A(K,J) = T
          IF (T .EQ. 0.D0) GO TO 50
          DO 40 I = KP1,N
 40         A(I,J) = A(I,J) + A(I,K)*T
 50       CONTINUE
 60     CONTINUE
 70   K = N
      IF (A(N,N) .EQ. 0.D0) GO TO 80
      RETURN
 80   IER = K
      IP(N) = 0
      RETURN
C----------------------- END OF SUBROUTINE DEC -------------------------
      END
C
C
      SUBROUTINE SOL (N, NDIM, A, B, IP)
C VERSION REAL DOUBLE PRECISION
      INTEGER*2 N,NDIM,IP,NM1,K,KP1,M,I,KB,KM1
      DOUBLE PRECISION A,B,T
      DIMENSION A(NDIM,N), B(N), IP(N)
C-----------------------------------------------------------------------
C  SOLUTION OF LINEAR SYSTEM, A*X = B .
C  INPUT..
C    N = ORDER OF MATRIX.
C    NDIM = DECLARED DIMENSION OF ARRAY  A .
C    A = TRIANGULARIZED MATRIX OBTAINED FROM DEC.
C    B = RIGHT HAND SIDE VECTOR.
C    IP = PIVOT VECTOR OBTAINED FROM DEC.
C  DO NOT USE IF DEC HAS SET IER .NE. 0.
C  OUTPUT..
C    B = SOLUTION VECTOR, X .
C-----------------------------------------------------------------------
      IF (N .EQ. 1) GO TO 50
      NM1 = N - 1
      DO 20 K = 1,NM1
        KP1 = K + 1
        M = IP(K)
        T = B(M)
        B(M) = B(K)
        B(K) = T
        DO 10 I = KP1,N
 10       B(I) = B(I) + A(I,K)*T
 20     CONTINUE
      DO 40 KB = 1,NM1
        KM1 = N - KB
        K = KM1 + 1
        B(K) = B(K)/A(K,K)
        T = -B(K)
        DO 30 I = 1,KM1
 30       B(I) = B(I) + A(I,K)*T
 40     CONTINUE
 50   B(1) = B(1)/A(1,1)
      RETURN
C----------------------- END OF SUBROUTINE SOL -------------------------
      END
C
      SUBROUTINE DECC (N, NDIM, AR, AI, IP, IER)
C VERSION COMPLEX DOUBLE PRECISION
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER*2 N,NDIM,IP,IER,NM1,K,KP1,M,I,J
      DIMENSION AR(NDIM,N), AI(NDIM,N), IP(N)
C-----------------------------------------------------------------------
C  MATRIX TRIANGULARIZATION BY GAUSSIAN ELIMINATION
C  ------ MODIFICATION FOR COMPLEX MATRICES --------
C  INPUT..
C     N = ORDER OF MATRIX.
C     NDIM = DECLARED DIMENSION OF ARRAYS  AR AND AI .
C     (AR, AI) = MATRIX TO BE TRIANGULARIZED.
C  OUTPUT..
C     AR(I,J), I.LE.J = UPPER TRIANGULAR FACTOR, U ; REAL PART.
C     AI(I,J), I.LE.J = UPPER TRIANGULAR FACTOR, U ; IMAGINARY PART.
C     AR(I,J), I.GT.J = MULTIPLIERS = LOWER TRIANGULAR FACTOR, I - L.
C                                                    REAL PART.
C     AI(I,J), I.GT.J = MULTIPLIERS = LOWER TRIANGULAR FACTOR, I - L.
C                                                    IMAGINARY PART.
C     IP(K), K.LT.N = INDEX OF K-TH PIVOT ROW.
C     IP(N) = (-1)**(NUMBER OF INTERCHANGES) OR O .
C     IER = 0 IF MATRIX A IS NONSINGULAR, OR K IF FOUND TO BE
C           SINGULAR AT STAGE K.
C  USE  SOL  TO OBTAIN SOLUTION OF LINEAR SYSTEM.
C  IF IP(N)=O, A IS SINGULAR, SOL WILL DIVIDE BY ZERO.
C
C  REFERENCE..
C     C. B. MOLER, ALGORITHM 423, LINEAR EQUATION SOLVER,
C     C.A.C.M. 15 (1972), P. 274.
C-----------------------------------------------------------------------
      IER = 0
      IP(N) = 1
      IF (N .EQ. 1) GO TO 70
      NM1 = N - 1
      DO 60 K = 1,NM1
        KP1 = K + 1
        M = K
        DO 10 I = KP1,N
 10       IF (DABS(AR(I,K))+DABS(AI(I,K)) .GT.
     &          DABS(AR(M,K))+DABS(AI(M,K))) M = I
        IP(K) = M
        TR = AR(M,K)
        TI = AI(M,K)
        IF (M .EQ. K) GO TO 20
        IP(N) = -IP(N)
        AR(M,K) = AR(K,K)
        AI(M,K) = AI(K,K)
        AR(K,K) = TR
        AI(K,K) = TI
 20     IF (DABS(TR)+DABS(TI) .EQ. 0.D0) GO TO 80
        DEN=TR*TR+TI*TI
        TR=TR/DEN
        TI=-TI/DEN
        DO 30 I = KP1,N
          PRODR=AR(I,K)*TR-AI(I,K)*TI
          PRODI=AI(I,K)*TR+AR(I,K)*TI
          AR(I,K)=-PRODR
          AI(I,K)=-PRODI
 30       CONTINUE
        DO 50 J = KP1,N
          TR = AR(M,J)
          TI = AI(M,J)
          AR(M,J) = AR(K,J)
          AI(M,J) = AI(K,J)
          AR(K,J) = TR
          AI(K,J) = TI
          IF (DABS(TR)+DABS(TI) .EQ. 0.D0) GO TO 50
          IF (TI .EQ. 0.D0) THEN
            DO 40 I = KP1,N
            PRODR=AR(I,K)*TR
            PRODI=AI(I,K)*TR
            AR(I,J) = AR(I,J) + PRODR
            AI(I,J) = AI(I,J) + PRODI
 40         CONTINUE
            GO TO 50
          END IF
          IF (TR .EQ. 0.D0) THEN
            DO 45 I = KP1,N
            PRODR=-AI(I,K)*TI
            PRODI=AR(I,K)*TI
            AR(I,J) = AR(I,J) + PRODR
            AI(I,J) = AI(I,J) + PRODI
 45         CONTINUE
            GO TO 50
          END IF
          DO 47 I = KP1,N
            PRODR=AR(I,K)*TR-AI(I,K)*TI
            PRODI=AI(I,K)*TR+AR(I,K)*TI
            AR(I,J) = AR(I,J) + PRODR
            AI(I,J) = AI(I,J) + PRODI
 47         CONTINUE
 50       CONTINUE
 60     CONTINUE
 70   K = N
      IF (DABS(AR(N,N))+DABS(AI(N,N)) .EQ. 0.D0) GO TO 80
      RETURN
 80   IER = K
      IP(N) = 0
      RETURN
C----------------------- END OF SUBROUTINE DECC ------------------------
      END
C
C
      SUBROUTINE SOLC (N, NDIM, AR, AI, BR, BI, IP)
C VERSION COMPLEX DOUBLE PRECISION
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER*2 N,NDIM,IP,NM1,K,KP1,M,I,KB,KM1
      DIMENSION AR(NDIM,N), AI(NDIM,N), BR(N), BI(N), IP(N)
C-----------------------------------------------------------------------
C  SOLUTION OF LINEAR SYSTEM, A*X = B .
C  INPUT..
C    N = ORDER OF MATRIX.
C    NDIM = DECLARED DIMENSION OF ARRAYS  AR AND AI.
C    (AR,AI) = TRIANGULARIZED MATRIX OBTAINED FROM DEC.
C    (BR,BI) = RIGHT HAND SIDE VECTOR.
C    IP = PIVOT VECTOR OBTAINED FROM DEC.
C  DO NOT USE IF DEC HAS SET IER .NE. 0.
C  OUTPUT..
C    (BR,BI) = SOLUTION VECTOR, X .
C-----------------------------------------------------------------------
      IF (N .EQ. 1) GO TO 50
      NM1 = N - 1
      DO 20 K = 1,NM1
        KP1 = K + 1
        M = IP(K)
        TR = BR(M)
        TI = BI(M)
        BR(M) = BR(K)
        BI(M) = BI(K)
        BR(K) = TR
        BI(K) = TI
        DO 10 I = KP1,N
          PRODR=AR(I,K)*TR-AI(I,K)*TI
          PRODI=AI(I,K)*TR+AR(I,K)*TI
          BR(I) = BR(I) + PRODR
          BI(I) = BI(I) + PRODI
 10       CONTINUE
 20     CONTINUE
      DO 40 KB = 1,NM1
        KM1 = N - KB
        K = KM1 + 1
        DEN=AR(K,K)*AR(K,K)+AI(K,K)*AI(K,K)
        PRODR=BR(K)*AR(K,K)+BI(K)*AI(K,K)
        PRODI=BI(K)*AR(K,K)-BR(K)*AI(K,K)
        BR(K)=PRODR/DEN
        BI(K)=PRODI/DEN
        TR = -BR(K)
        TI = -BI(K)
        DO 30 I = 1,KM1
          PRODR=AR(I,K)*TR-AI(I,K)*TI
          PRODI=AI(I,K)*TR+AR(I,K)*TI
          BR(I) = BR(I) + PRODR
          BI(I) = BI(I) + PRODI
 30       CONTINUE
 40     CONTINUE
 50     CONTINUE
        DEN=AR(1,1)*AR(1,1)+AI(1,1)*AI(1,1)
        PRODR=BR(1)*AR(1,1)+BI(1)*AI(1,1)
        PRODI=BI(1)*AR(1,1)-BR(1)*AI(1,1)
        BR(1)=PRODR/DEN
        BI(1)=PRODI/DEN
      RETURN
C----------------------- END OF SUBROUTINE SOLC ------------------------
      END
C
c
#if (full)
      SUBROUTINE DECH (N, NDIM, A, LB, IP, IER)
C VERSION REAL DOUBLE PRECISION
      INTEGER*2 N,NDIM,IP,IER,NM1,K,KP1,M,I,J,LB,NA
      DOUBLE PRECISION A,T
      DIMENSION A(NDIM,N), IP(N)
C-----------------------------------------------------------------------
C  MATRIX TRIANGULARIZATION BY GAUSSIAN ELIMINATION OF A HESSENBERG
C  MATRIX WITH LOWER BANDWIDTH LB
C  INPUT..
C     N = ORDER OF MATRIX A.
C     NDIM = DECLARED DIMENSION OF ARRAY  A .
C     A = MATRIX TO BE TRIANGULARIZED.
C     LB = LOWER BANDWIDTH OF A (DIAGONAL IS NOT COUNTED, LB.GE.1).
C  OUTPUT..
C     A(I,J), I.LE.J = UPPER TRIANGULAR FACTOR, U .
C     A(I,J), I.GT.J = MULTIPLIERS = LOWER TRIANGULAR FACTOR, I - L.
C     IP(K), K.LT.N = INDEX OF K-TH PIVOT ROW.
C     IP(N) = (-1)**(NUMBER OF INTERCHANGES) OR O .
C     IER = 0 IF MATRIX A IS NONSINGULAR, OR K IF FOUND TO BE
C           SINGULAR AT STAGE K.
C  USE  SOLH  TO OBTAIN SOLUTION OF LINEAR SYSTEM.
C  DETERM(A) = IP(N)*A(1,1)*A(2,2)*...*A(N,N).
C  IF IP(N)=O, A IS SINGULAR, SOL WILL DIVIDE BY ZERO.
C
C  REFERENCE..
C     THIS IS A SLIGHT MODIFICATION OF
C     C. B. MOLER, ALGORITHM 423, LINEAR EQUATION SOLVER,
C     C.A.C.M. 15 (1972), P. 274.
C-----------------------------------------------------------------------
      IER = 0
      IP(N) = 1
      IF (N .EQ. 1) GO TO 70
      NM1 = N - 1
      DO 60 K = 1,NM1
        KP1 = K + 1
        M = K
        NA = MIN0(N,LB+K)
        DO 10 I = KP1,NA
 10       IF (DABS(A(I,K)) .GT. DABS(A(M,K))) M = I
        IP(K) = M
        T = A(M,K)
        IF (M .EQ. K) GO TO 20
        IP(N) = -IP(N)
        A(M,K) = A(K,K)
        A(K,K) = T
 20     IF (T .EQ. 0.D0) GO TO 80
        T = 1.D0/T
        DO 30 I = KP1,NA
 30       A(I,K) = -A(I,K)*T
        DO 50 J = KP1,N
          T = A(M,J)
          A(M,J) = A(K,J)
          A(K,J) = T
          IF (T .EQ. 0.D0) GO TO 50
          DO 40 I = KP1,NA
 40         A(I,J) = A(I,J) + A(I,K)*T
 50       CONTINUE
 60     CONTINUE
 70   K = N
      IF (A(N,N) .EQ. 0.D0) GO TO 80
      RETURN
 80   IER = K
      IP(N) = 0
      RETURN
C----------------------- END OF SUBROUTINE DECH ------------------------
      END
C
C
      SUBROUTINE SOLH (N, NDIM, A, LB, B, IP)
C VERSION REAL DOUBLE PRECISION
      INTEGER*2 N,NDIM,IP,NM1,K,KP1,M,I,KB,KM1,LB,NA
      DOUBLE PRECISION A,B,T
      DIMENSION A(NDIM,N), B(N), IP(N)
C-----------------------------------------------------------------------
C  SOLUTION OF LINEAR SYSTEM, A*X = B .
C  INPUT..
C    N = ORDER OF MATRIX A.
C    NDIM = DECLARED DIMENSION OF ARRAY  A .
C    A = TRIANGULARIZED MATRIX OBTAINED FROM DECH.
C    LB = LOWER BANDWIDTH OF A.
C    B = RIGHT HAND SIDE VECTOR.
C    IP = PIVOT VECTOR OBTAINED FROM DEC.
C  DO NOT USE IF DECH HAS SET IER .NE. 0.
C  OUTPUT..
C    B = SOLUTION VECTOR, X .
C-----------------------------------------------------------------------
      IF (N .EQ. 1) GO TO 50
      NM1 = N - 1
      DO 20 K = 1,NM1
        KP1 = K + 1
        M = IP(K)
        T = B(M)
        B(M) = B(K)
        B(K) = T
        NA = MIN0(N,LB+K)
        DO 10 I = KP1,NA
 10       B(I) = B(I) + A(I,K)*T
 20     CONTINUE
      DO 40 KB = 1,NM1
        KM1 = N - KB
        K = KM1 + 1
        B(K) = B(K)/A(K,K)
        T = -B(K)
        DO 30 I = 1,KM1
 30       B(I) = B(I) + A(I,K)*T
 40     CONTINUE
 50   B(1) = B(1)/A(1,1)
      RETURN
C----------------------- END OF SUBROUTINE SOLH ------------------------
      END
C
      SUBROUTINE DECHC (N, NDIM, AR, AI, LB, IP, IER)
C VERSION COMPLEX DOUBLE PRECISION
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER*2 N,NDIM,IP,IER,NM1,K,KP1,M,I,J,LB
      DIMENSION AR(NDIM,N), AI(NDIM,N), IP(N)
C-----------------------------------------------------------------------
C  MATRIX TRIANGULARIZATION BY GAUSSIAN ELIMINATION
C  ------ MODIFICATION FOR COMPLEX MATRICES --------
C  INPUT..
C     N = ORDER OF MATRIX.
C     NDIM = DECLARED DIMENSION OF ARRAYS  AR AND AI .
C     (AR, AI) = MATRIX TO BE TRIANGULARIZED.
C  OUTPUT..
C     AR(I,J), I.LE.J = UPPER TRIANGULAR FACTOR, U ; REAL PART.
C     AI(I,J), I.LE.J = UPPER TRIANGULAR FACTOR, U ; IMAGINARY PART.
C     AR(I,J), I.GT.J = MULTIPLIERS = LOWER TRIANGULAR FACTOR, I - L.
C                                                    REAL PART.
C     AI(I,J), I.GT.J = MULTIPLIERS = LOWER TRIANGULAR FACTOR, I - L.
C                                                    IMAGINARY PART.
C     LB = LOWER BANDWIDTH OF A (DIAGONAL NOT COUNTED), LB.GE.1.
C     IP(K), K.LT.N = INDEX OF K-TH PIVOT ROW.
C     IP(N) = (-1)**(NUMBER OF INTERCHANGES) OR O .
C     IER = 0 IF MATRIX A IS NONSINGULAR, OR K IF FOUND TO BE
C           SINGULAR AT STAGE K.
C  USE  SOL  TO OBTAIN SOLUTION OF LINEAR SYSTEM.
C  IF IP(N)=O, A IS SINGULAR, SOL WILL DIVIDE BY ZERO.
C
C  REFERENCE..
C     C. B. MOLER, ALGORITHM 423, LINEAR EQUATION SOLVER,
C     C.A.C.M. 15 (1972), P. 274.
C-----------------------------------------------------------------------
      IER = 0
      IP(N) = 1
      IF (LB .EQ. 0) GO TO 70
      IF (N .EQ. 1) GO TO 70
      NM1 = N - 1
      DO 60 K = 1,NM1
        KP1 = K + 1
        M = K
        NA = MIN0(N,LB+K)
        DO 10 I = KP1,NA
 10       IF (DABS(AR(I,K))+DABS(AI(I,K)) .GT.
     &          DABS(AR(M,K))+DABS(AI(M,K))) M = I
        IP(K) = M
        TR = AR(M,K)
        TI = AI(M,K)
        IF (M .EQ. K) GO TO 20
        IP(N) = -IP(N)
        AR(M,K) = AR(K,K)
        AI(M,K) = AI(K,K)
        AR(K,K) = TR
        AI(K,K) = TI
 20     IF (DABS(TR)+DABS(TI) .EQ. 0.D0) GO TO 80
        DEN=TR*TR+TI*TI
        TR=TR/DEN
        TI=-TI/DEN
        DO 30 I = KP1,NA
          PRODR=AR(I,K)*TR-AI(I,K)*TI
          PRODI=AI(I,K)*TR+AR(I,K)*TI
          AR(I,K)=-PRODR
          AI(I,K)=-PRODI
 30       CONTINUE
        DO 50 J = KP1,N
          TR = AR(M,J)
          TI = AI(M,J)
          AR(M,J) = AR(K,J)
          AI(M,J) = AI(K,J)
          AR(K,J) = TR
          AI(K,J) = TI
          IF (DABS(TR)+DABS(TI) .EQ. 0.D0) GO TO 50
          IF (TI .EQ. 0.D0) THEN
            DO 40 I = KP1,NA
            PRODR=AR(I,K)*TR
            PRODI=AI(I,K)*TR
            AR(I,J) = AR(I,J) + PRODR
            AI(I,J) = AI(I,J) + PRODI
 40         CONTINUE
            GO TO 50
          END IF
          IF (TR .EQ. 0.D0) THEN
            DO 45 I = KP1,NA
            PRODR=-AI(I,K)*TI
            PRODI=AR(I,K)*TI
            AR(I,J) = AR(I,J) + PRODR
            AI(I,J) = AI(I,J) + PRODI
 45         CONTINUE
            GO TO 50
          END IF
          DO 47 I = KP1,NA
            PRODR=AR(I,K)*TR-AI(I,K)*TI
            PRODI=AI(I,K)*TR+AR(I,K)*TI
            AR(I,J) = AR(I,J) + PRODR
            AI(I,J) = AI(I,J) + PRODI
 47         CONTINUE
 50       CONTINUE
 60     CONTINUE
 70   K = N
      IF (DABS(AR(N,N))+DABS(AI(N,N)) .EQ. 0.D0) GO TO 80
      RETURN
 80   IER = K
      IP(N) = 0
      RETURN
C----------------------- END OF SUBROUTINE DECHC -----------------------
      END
C
C
      SUBROUTINE SOLHC (N, NDIM, AR, AI, LB, BR, BI, IP)
C VERSION COMPLEX DOUBLE PRECISION
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER*2 N,NDIM,IP,NM1,K,KP1,M,I,KB,KM1,LB
      DIMENSION AR(NDIM,N), AI(NDIM,N), BR(N), BI(N), IP(N)
C-----------------------------------------------------------------------
C  SOLUTION OF LINEAR SYSTEM, A*X = B .
C  INPUT..
C    N = ORDER OF MATRIX.
C    NDIM = DECLARED DIMENSION OF ARRAYS  AR AND AI.
C    (AR,AI) = TRIANGULARIZED MATRIX OBTAINED FROM DEC.
C    (BR,BI) = RIGHT HAND SIDE VECTOR.
C    LB = LOWER BANDWIDTH OF A.
C    IP = PIVOT VECTOR OBTAINED FROM DEC.
C  DO NOT USE IF DEC HAS SET IER .NE. 0.
C  OUTPUT..
C    (BR,BI) = SOLUTION VECTOR, X .
C-----------------------------------------------------------------------
      IF (N .EQ. 1) GO TO 50
      NM1 = N - 1
      IF (LB .EQ. 0) GO TO 20
      DO 20 K = 1,NM1
        KP1 = K + 1
        M = IP(K)
        TR = BR(M)
        TI = BI(M)
        BR(M) = BR(K)
        BI(M) = BI(K)
        BR(K) = TR
        BI(K) = TI
        DO 10 I = KP1,MIN0(N,LB+K)
          PRODR=AR(I,K)*TR-AI(I,K)*TI
          PRODI=AI(I,K)*TR+AR(I,K)*TI
          BR(I) = BR(I) + PRODR
          BI(I) = BI(I) + PRODI
 10       CONTINUE
 20     CONTINUE
      DO 40 KB = 1,NM1
        KM1 = N - KB
        K = KM1 + 1
        DEN=AR(K,K)*AR(K,K)+AI(K,K)*AI(K,K)
        PRODR=BR(K)*AR(K,K)+BI(K)*AI(K,K)
        PRODI=BI(K)*AR(K,K)-BR(K)*AI(K,K)
        BR(K)=PRODR/DEN
        BI(K)=PRODI/DEN
        TR = -BR(K)
        TI = -BI(K)
        DO 30 I = 1,KM1
          PRODR=AR(I,K)*TR-AI(I,K)*TI
          PRODI=AI(I,K)*TR+AR(I,K)*TI
          BR(I) = BR(I) + PRODR
          BI(I) = BI(I) + PRODI
 30       CONTINUE
 40     CONTINUE
 50     CONTINUE
        DEN=AR(1,1)*AR(1,1)+AI(1,1)*AI(1,1)
        PRODR=BR(1)*AR(1,1)+BI(1)*AI(1,1)
        PRODI=BI(1)*AR(1,1)-BR(1)*AI(1,1)
        BR(1)=PRODR/DEN
        BI(1)=PRODI/DEN
      RETURN
C----------------------- END OF SUBROUTINE SOLHC -----------------------
      END
C
      SUBROUTINE DECB (N, NDIM, A, ML, MU, IP, IER)
      IMPLICIT INTEGER*2(I-N)
      REAL*8 A,T
      DIMENSION A(NDIM,N), IP(N)
C-----------------------------------------------------------------------
C  MATRIX TRIANGULARIZATION BY GAUSSIAN ELIMINATION OF A BANDED
C  MATRIX WITH LOWER BANDWIDTH ML AND UPPER BANDWIDTH MU
C  INPUT..
C     N       ORDER OF THE ORIGINAL MATRIX A.
C     NDIM    DECLARED DIMENSION OF ARRAY  A.
C     A       CONTAINS THE MATRIX IN BAND STORAGE.   THE COLUMNS
C                OF THE MATRIX ARE STORED IN THE COLUMNS OF  A  AND
C                THE DIAGONALS OF THE MATRIX ARE STORED IN ROWS
C                ML+1 THROUGH 2*ML+MU+1 OF  A.
C     ML      LOWER BANDWIDTH OF A (DIAGONAL IS NOT COUNTED).
C     MU      UPPER BANDWIDTH OF A (DIAGONAL IS NOT COUNTED).
C  OUTPUT..
C     A       AN UPPER TRIANGULAR MATRIX IN BAND STORAGE AND
C                THE MULTIPLIERS WHICH WERE USED TO OBTAIN IT.
C     IP      INDEX VECTOR OF PIVOT INDICES.
C     IP(N)   (-1)**(NUMBER OF INTERCHANGES) OR O .
C     IER     = 0 IF MATRIX A IS NONSINGULAR, OR  = K IF FOUND TO BE
C                SINGULAR AT STAGE K.
C  USE  SOLB  TO OBTAIN SOLUTION OF LINEAR SYSTEM.
C  DETERM(A) = IP(N)*A(MD,1)*A(MD,2)*...*A(MD,N)  WITH MD=ML+MU+1.
C  IF IP(N)=O, A IS SINGULAR, SOLB WILL DIVIDE BY ZERO.
C
C  REFERENCE..
C     THIS IS A MODIFICATION OF
C     C. B. MOLER, ALGORITHM 423, LINEAR EQUATION SOLVER,
C     C.A.C.M. 15 (1972), P. 274.
C-----------------------------------------------------------------------
      IER = 0
      IP(N) = 1
      MD = ML + MU + 1
      MD1 = MD + 1
      JU = 0
      IF (ML .EQ. 0) GO TO 70
      IF (N .EQ. 1) GO TO 70
      IF (N .LT. MU+2) GO TO 7
      DO 5 J = MU+2,N
      DO 5 I = 1,ML
  5   A(I,J) = 0.D0
  7   NM1 = N - 1
      DO 60 K = 1,NM1
        KP1 = K + 1
        M = MD
        MDL = MIN(ML,N-K) + MD
        DO 10 I = MD1,MDL
 10       IF (DABS(A(I,K)) .GT. DABS(A(M,K))) M = I
        IP(K) = M + K - MD
        T = A(M,K)
        IF (M .EQ. MD) GO TO 20
        IP(N) = -IP(N)
        A(M,K) = A(MD,K)
        A(MD,K) = T
 20     IF (T .EQ. 0.D0) GO TO 80
        T = 1.D0/T
        DO 30 I = MD1,MDL
 30       A(I,K) = -A(I,K)*T
        JU = MIN0(MAX0(JU,MU+IP(K)),N)
        MM = MD
        IF (JU .LT. KP1) GO TO 60
        DO 50 J = KP1,JU
          M = M - 1
          MM = MM - 1
          T = A(M,J)
          IF (M .EQ. MM) GO TO 35
          A(M,J) = A(MM,J)
          A(MM,J) = T
 35       IF (T .EQ. 0.D0) GO TO 50
          JK = J - K
          DO 40 I = MD1,MDL
            IJK = I - JK
 40         A(IJK,J) = A(IJK,J) + A(I,K)*T
 50       CONTINUE
 60     CONTINUE
 70   K = N
      IF (A(MD,N) .EQ. 0.D0) GO TO 80
      RETURN
 80   IER = K
      IP(N) = 0
      RETURN
C----------------------- END OF SUBROUTINE DECB ------------------------
      END
C
C
      SUBROUTINE SOLB (N, NDIM, A, ML, MU, B, IP)
      IMPLICIT INTEGER*2(I-N)
      REAL*8 A,B,T
      DIMENSION A(NDIM,N), B(N), IP(N)
C-----------------------------------------------------------------------
C  SOLUTION OF LINEAR SYSTEM, A*X = B .
C  INPUT..
C    N      ORDER OF MATRIX A.
C    NDIM   DECLARED DIMENSION OF ARRAY  A .
C    A      TRIANGULARIZED MATRIX OBTAINED FROM DECB.
C    ML     LOWER BANDWIDTH OF A (DIAGONAL IS NOT COUNTED).
C    MU     UPPER BANDWIDTH OF A (DIAGONAL IS NOT COUNTED).
C    B      RIGHT HAND SIDE VECTOR.
C    IP     PIVOT VECTOR OBTAINED FROM DECB.
C  DO NOT USE IF DECB HAS SET IER .NE. 0.
C  OUTPUT..
C    B      SOLUTION VECTOR, X .
C-----------------------------------------------------------------------
      MD = ML + MU + 1
      MD1 = MD + 1
      MDM = MD - 1
      IF (ML .EQ. 0) GO TO 20
      IF (N .EQ. 1) GO TO 50
      NM1 = N - 1
      DO 20 K = 1,NM1
        M = IP(K)
        T = B(M)
        B(M) = B(K)
        B(K) = T
        MDL = MIN(ML,N-K) + MD
        DO 10 I = MD1,MDL
          IMD = I + K - MD
 10       B(IMD) = B(IMD) + A(I,K)*T
 20     CONTINUE
      DO 40 KB = 1,NM1
        K = N + 1 - KB
        B(K) = B(K)/A(MD,K)
        T = -B(K)
        KMD = MD - K
        LM = MAX0(1,KMD+1)
        DO 30 I = LM,MDM
          IMD = I - KMD
 30       B(IMD) = B(IMD) + A(I,K)*T
 40     CONTINUE
 50   B(1) = B(1)/A(MD,1)
      RETURN
C----------------------- END OF SUBROUTINE SOLB ------------------------
      END
C
      SUBROUTINE DECBC (N, NDIM, AR, AI, ML, MU, IP, IER)
      IMPLICIT INTEGER*2(I-N)
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION AR(NDIM,N), AI(NDIM,N), IP(N)
C-----------------------------------------------------------------------
C  MATRIX TRIANGULARIZATION BY GAUSSIAN ELIMINATION OF A BANDED COMPLEX
C  MATRIX WITH LOWER BANDWIDTH ML AND UPPER BANDWIDTH MU
C  INPUT..
C     N       ORDER OF THE ORIGINAL MATRIX A.
C     NDIM    DECLARED DIMENSION OF ARRAY  A.
C     AR, AI     CONTAINS THE MATRIX IN BAND STORAGE.   THE COLUMNS
C                OF THE MATRIX ARE STORED IN THE COLUMNS OF  AR (REAL
C                PART) AND AI (IMAGINARY PART)  AND
C                THE DIAGONALS OF THE MATRIX ARE STORED IN ROWS
C                ML+1 THROUGH 2*ML+MU+1 OF  AR AND AI.
C     ML      LOWER BANDWIDTH OF A (DIAGONAL IS NOT COUNTED).
C     MU      UPPER BANDWIDTH OF A (DIAGONAL IS NOT COUNTED).
C  OUTPUT..
C     AR, AI  AN UPPER TRIANGULAR MATRIX IN BAND STORAGE AND
C                THE MULTIPLIERS WHICH WERE USED TO OBTAIN IT.
C     IP      INDEX VECTOR OF PIVOT INDICES.
C     IP(N)   (-1)**(NUMBER OF INTERCHANGES) OR O .
C     IER     = 0 IF MATRIX A IS NONSINGULAR, OR  = K IF FOUND TO BE
C                SINGULAR AT STAGE K.
C  USE  SOLBC  TO OBTAIN SOLUTION OF LINEAR SYSTEM.
C  DETERM(A) = IP(N)*A(MD,1)*A(MD,2)*...*A(MD,N)  WITH MD=ML+MU+1.
C  IF IP(N)=O, A IS SINGULAR, SOLBC WILL DIVIDE BY ZERO.
C
C  REFERENCE..
C     THIS IS A MODIFICATION OF
C     C. B. MOLER, ALGORITHM 423, LINEAR EQUATION SOLVER,
C     C.A.C.M. 15 (1972), P. 274.
C-----------------------------------------------------------------------
      IER = 0
      IP(N) = 1
      MD = ML + MU + 1
      MD1 = MD + 1
      JU = 0
      IF (ML .EQ. 0) GO TO 70
      IF (N .EQ. 1) GO TO 70
      IF (N .LT. MU+2) GO TO 7
      DO 5 J = MU+2,N
      DO 5 I = 1,ML
      AR(I,J) = 0.D0
      AI(I,J) = 0.D0
  5   CONTINUE
  7   NM1 = N - 1
      DO 60 K = 1,NM1
        KP1 = K + 1
        M = MD
        MDL = MIN(ML,N-K) + MD
        DO 10 I = MD1,MDL
 10       IF (DABS(AR(I,K))+DABS(AI(I,K)) .GT.
     &          DABS(AR(M,K))+DABS(AI(M,K))) M = I
        IP(K) = M + K - MD
        TR = AR(M,K)
        TI = AI(M,K)
        IF (M .EQ. MD) GO TO 20
        IP(N) = -IP(N)
        AR(M,K) = AR(MD,K)
        AI(M,K) = AI(MD,K)
        AR(MD,K) = TR
        AI(MD,K) = TI
 20     IF (DABS(TR)+DABS(TI) .EQ. 0.D0) GO TO 80
        DEN=TR*TR+TI*TI
        TR=TR/DEN
        TI=-TI/DEN
        DO 30 I = MD1,MDL
          PRODR=AR(I,K)*TR-AI(I,K)*TI
          PRODI=AI(I,K)*TR+AR(I,K)*TI
          AR(I,K)=-PRODR
          AI(I,K)=-PRODI
 30       CONTINUE
        JU = MIN0(MAX0(JU,MU+IP(K)),N)
        MM = MD
        IF (JU .LT. KP1) GO TO 60
        DO 50 J = KP1,JU
          M = M - 1
          MM = MM - 1
          TR = AR(M,J)
          TI = AI(M,J)
          IF (M .EQ. MM) GO TO 35
          AR(M,J) = AR(MM,J)
          AI(M,J) = AI(MM,J)
          AR(MM,J) = TR
          AI(MM,J) = TI
 35       IF (DABS(TR)+DABS(TI) .EQ. 0.D0) GO TO 50
          JK = J - K
          IF (TI .EQ. 0.D0) THEN
            DO 40 I = MD1,MDL
            IJK = I - JK
            PRODR=AR(I,K)*TR
            PRODI=AI(I,K)*TR
            AR(IJK,J) = AR(IJK,J) + PRODR
            AI(IJK,J) = AI(IJK,J) + PRODI
 40         CONTINUE
            GO TO 50
          END IF
          IF (TR .EQ. 0.D0) THEN
            DO 45 I = MD1,MDL
            IJK = I - JK
            PRODR=-AI(I,K)*TI
            PRODI=AR(I,K)*TI
            AR(IJK,J) = AR(IJK,J) + PRODR
            AI(IJK,J) = AI(IJK,J) + PRODI
 45         CONTINUE
            GO TO 50
          END IF
          DO 47 I = MD1,MDL
            IJK = I - JK
            PRODR=AR(I,K)*TR-AI(I,K)*TI
            PRODI=AI(I,K)*TR+AR(I,K)*TI
            AR(IJK,J) = AR(IJK,J) + PRODR
            AI(IJK,J) = AI(IJK,J) + PRODI
 47         CONTINUE
 50       CONTINUE
 60     CONTINUE
 70   K = N
      IF (DABS(AR(MD,N))+DABS(AI(MD,N)) .EQ. 0.D0) GO TO 80
      RETURN
 80   IER = K
      IP(N) = 0
      RETURN
C----------------------- END OF SUBROUTINE DECBC ------------------------
      END
C
C
      SUBROUTINE SOLBC (N, NDIM, AR, AI, ML, MU, BR, BI, IP)
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER*2(I-N)
      DIMENSION AR(NDIM,N), AI(NDIM,N), BR(N), BI(N), IP(N)
C-----------------------------------------------------------------------
C  SOLUTION OF LINEAR SYSTEM, A*X = B ,
C                  VERSION BANDED AND COMPLEX-DOUBLE PRECISION.
C  INPUT..
C    N      ORDER OF MATRIX A.
C    NDIM   DECLARED DIMENSION OF ARRAY  A .
C    AR, AI TRIANGULARIZED MATRIX OBTAINED FROM DECB (REAL AND IMAG. PART).
C    ML     LOWER BANDWIDTH OF A (DIAGONAL IS NOT COUNTED).
C    MU     UPPER BANDWIDTH OF A (DIAGONAL IS NOT COUNTED).
C    BR, BI RIGHT HAND SIDE VECTOR (REAL AND IMAG. PART).
C    IP     PIVOT VECTOR OBTAINED FROM DECBC.
C  DO NOT USE IF DECB HAS SET IER .NE. 0.
C  OUTPUT..
C    BR, BI SOLUTION VECTOR, X (REAL AND IMAG. PART).
C-----------------------------------------------------------------------
      MD = ML + MU + 1
      MD1 = MD + 1
      MDM = MD - 1
      IF (ML .EQ. 0) GO TO 20
      IF (N .EQ. 1) GO TO 50
      NM1 = N - 1
      DO 20 K = 1,NM1
        M = IP(K)
        TR = BR(M)
        TI = BI(M)
        BR(M) = BR(K)
        BI(M) = BI(K)
        BR(K) = TR
        BI(K) = TI
        MDL = MIN(ML,N-K) + MD
        DO 10 I = MD1,MDL
          IMD = I + K - MD
          PRODR=AR(I,K)*TR-AI(I,K)*TI
          PRODI=AI(I,K)*TR+AR(I,K)*TI
          BR(IMD) = BR(IMD) + PRODR
          BI(IMD) = BI(IMD) + PRODI
 10     CONTINUE
 20     CONTINUE
      DO 40 KB = 1,NM1
        K = N + 1 - KB
        DEN=AR(MD,K)*AR(MD,K)+AI(MD,K)*AI(MD,K)
        PRODR=BR(K)*AR(MD,K)+BI(K)*AI(MD,K)
        PRODI=BI(K)*AR(MD,K)-BR(K)*AI(MD,K)
        BR(K)=PRODR/DEN
        BI(K)=PRODI/DEN
        TR = -BR(K)
        TI = -BI(K)
        KMD = MD - K
        LM = MAX0(1,KMD+1)
        DO 30 I = LM,MDM
          IMD = I - KMD
          PRODR=AR(I,K)*TR-AI(I,K)*TI
          PRODI=AI(I,K)*TR+AR(I,K)*TI
          BR(IMD) = BR(IMD) + PRODR
          BI(IMD) = BI(IMD) + PRODI
 30       CONTINUE
 40     CONTINUE
        DEN=AR(MD,1)*AR(MD,1)+AI(MD,1)*AI(MD,1)
        PRODR=BR(1)*AR(MD,1)+BI(1)*AI(MD,1)
        PRODI=BI(1)*AR(MD,1)-BR(1)*AI(MD,1)
        BR(1)=PRODR/DEN
        BI(1)=PRODI/DEN
 50   CONTINUE
      RETURN
C----------------------- END OF SUBROUTINE SOLBC ------------------------
      END
#endif

