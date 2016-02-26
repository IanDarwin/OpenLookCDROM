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

      SUBROUTINE PLO(N,A,AL,BET,X,Y,NA,EPS,IER)
C
C   By~iClEHiE dBuMEPHOj iHBAPiAHTHOj plOCKOCTi
C       ABTOPy - P.M.bOPiC`K, A.i.XibHiK      BEPCiq OT 23.06.81
C---------------------------------------------------------------
C     14.11.92 
C---------------------------------------------------------------
C
C
C   pPOgPAMMA HAXOdiT iHBAPiAHTHu` plOCKOCTx,
C   OTBE~A`}u` CObCTBEHHOMu zHA~EHi`  AL+I*BET .
C   iCKOMyE BEKTOPA X i Y udOBlETBOPq`T uPABHEHiqM:
C     A*X=AL*X+BET*Y
C     A*Y=-BET*X+AL*Y
C     (X,X)*(Y,Y)=1
C     (X,Y)=0
C
      IMPLICIT REAL*8(A-H, O-Z)
      IMPLICIT INTEGER*2(I-N)
      include 'algebrbf.dim'
      parameter (NFMAXd=NFMAX*2, NFMX2d=NFMAX*NFMAX*2,
     A           NFMX2q=NFMAX*NFMAX*4)
      DIMENSION A(NA,1),X(N),IROW(NFMAXd),ICOL(NFMAXd),
     A          Y(N),Z(NFMAXd)
      COMMON/WORK2/ DUMM1(NFMX2d),DUMM2(NFMX2d),B(NFMX2q)
      K=0
      N2=N+N
      N3=N2-2
      DO 3 J=1,N
      DO 1 I=1,N
      K=K+1
      B(K)=A(I,J)
      IF(I.EQ.J) B(K)=B(K)-AL
  1   CONTINUE
      DO 2 I=1,N
      K=K+1
      B(K)=0.D0
       IF(I.EQ.J)B(K)= BET
  2   CONTINUE
  3   CONTINUE
      DO 6 J=1,N
      DO 4 I=1,N
      K=K+1
      B(K)=0.D0
      IF(I.EQ.J) B(K)=-BET
  4   CONTINUE
      DO 5 I=1,N
      K=K+1
      B(K)=A(I,J)
      IF(I.EQ.J) B(K)=B(K)-AL
  5   CONTINUE
  6   CONTINUE
C
      N4=N2*N2
      CALL DMFGR(B,N2,N2,EPS,IRANK,IROW,ICOL)
      IF(IRANK.NE.N3) GO TO 12
      Z(ICOL(N2))=1.D0
      Z(ICOL(N2-1))=1.D0
      DO 7 I=1,N3
      K=N2*N3+I
      K1=K+N2
  7   Z(ICOL(I))=B(K)+B(K1)
      IER=0
C
C   HOPMiPOBKA i OPTOgOHAlizAciq HAjdEHHyX BEKTOPOB
C
      R1=0.
      R2=0.
      R3=0.
      DO 8 I=1,N
        R1=R1+Z(I)**2
        R2=R2+Z(N+I)**2
        R3=R3+Z(N+I)*Z(I)
  8   CONTINUE
      S=0.
      IF (DABS(R3).LT.1.D-10) GO TO 9
        R=(R1-R2)/R3
        S=(-R+DSQRT(R*R+4.))/2.
  9   R4=R1+2.*R3*S+R2*S*S
      R5=R2-2.*R3*S+R1*S*S
      A1=DSQRT(DSQRT(1./(R4*R5)))
      B1=A1*S
      IF (R4.GE.R5) GO TO 10
        R=A1
        A1=-B1
        B1=R
  10  CONTINUE
      DO 11 I=1,N
        X(I)=A1*Z(I)+B1*Z(N+I)
        Y(I)=-B1*Z(I)+A1*Z(N+I)
  11  CONTINUE
      RETURN
  12  IER=1
C     PRINT 101,IRANK
C101  FORMAT(/3X,'iHBAPiAHTHAq plOCKOCTx HE HAjdEHA',/5X,'IRANK=',I3)
      RETURN
      END
 
