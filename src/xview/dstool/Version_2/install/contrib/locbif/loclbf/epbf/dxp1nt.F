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

      SUBROUTINE DXP1NT(NFP,NFP2,DH,X,DXP,FUNS,IER)
C
C   COMPUTING DERIVATIVES FOR NONTRANSVERSALITY CONDITION
C          ON THE HOPF CURVE
C      The number of singular parameter is IPSNG1
C                  20.05.1990
C                  14.02.1991
C                  15.11.92
C
      IMPLICIT REAL*8(A-H,O-Z)
      IMPLICIT INTEGER*2(I-N)
      include 'epbf.dim'
      parameter (NFMAX1=NFMAX+1)
      DIMENSION DXP(NFP,NFP),X(1)
      DIMENSION FP(NFMAX1),F(NFMAX1)
      EXTERNAL FUNS
      COMMON/PPAR/P(NPARMX)
      COMMON/IPPAR/IP(NPARMX)
      COMMON/LINBF/NF,NAP,ILN,NF2,ITP
      COMMON/IPSNG/ IPSNG1,IPSNG2
      COMMON /IERFS/ IERFS, IERDUM
      DATA EPS0/1.D-10/
      IER=0
      NK=NF+NAP
C
      CALL FUNS(X,F)
        IER=IERFS
        IF (IER.NE.0) RETURN
      CALL DETRG(NK,X,F(NF+1),IER)
        IF (IER.NE.0) RETURN
      DO 40 J=1,NFP
        JJ=J
        IF (J.EQ.NF+1) JJ=NF+IPSNG1
C        IF (J.LE.NF) GO TO 15
C          DO 10 I=1,NAP
C            JJ=NF+I
C            IF (IP(I).EQ.IPSNG1) GO TO 15
C 10       CONTINUE
C          IER=-1
C          RETURN
C
 15     DHX=DH*(1.D0+DABS(X(JJ)))
        R=X(JJ)
        X(JJ)=R+DHX
        DO 20 I=1,NAP
          K=IP(I)
          P(K)=X(NF+I)
 20     CONTINUE
        CALL FUNS(X,FP)
        X(JJ)=R
          IER=IERFS
          IF (IER.NE.0) RETURN
        X(JJ)=R+DHX
        CALL DETRG(NK,X,FP(NF+1),IER)
        X(JJ)=R
          IF (IER.NE.0) RETURN
          DO 30 I=1,NFP
            DXP(I,J)=(FP(I)-F(I))/DHX
            IF (DABS(DXP(I,J)).LE.EPS0) DXP(I,J)=0.
 30       CONTINUE
 40   CONTINUE
      RETURN
      END
 
