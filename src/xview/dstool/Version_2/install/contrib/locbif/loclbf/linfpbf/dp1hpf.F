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

      SUBROUTINE DP1HPF(NFP,NFP2,DH,X,DXP,FUNM,IER)
C
C   COMPUTING DERIVATIVES FOR NONTRANSVERSALITY CONDITION
C          ON THE HOPF CURVE
C             ITERATED MAPS            
C      The number of singular parameter is IPSNG1
C                  20.05.1990
C                  14.02.1991
C                  16.11.92			
C
      IMPLICIT REAL*8(A-H,O-Z)
      IMPLICIT INTEGER*2(I-N)
      include 'linfpbf.dim'
      parameter (NFMAX1=NFMAX+1)
      DIMENSION DXP(NFP,NFP),X(1)
      DIMENSION FP(NFMAX1),F(NFMAX1)
      EXTERNAL FUNM
      COMMON/PPAR/P(NPARMX)
      COMMON/IPPAR/IP(NPARMX)
      COMMON/LINBF/NF,NAP,ILN,NF2,ITP
      COMMON/IPSNG/ IPSNG1,IPSNG2
      COMMON /IERFS/ IERFS,IERDUM
      DATA EPS0/1.D-10/
      IER=0
      NK=NF+NAP
C
      CALL FUNM(X,F)
        IER=IERFS
        IF (IER.NE.0) RETURN
      CALL DETHPF(NK,X,F(NF+1),IER)
        IF (IER.NE.0) RETURN
      DO 40 J=1,NFP
        JJ=J
        IF (J.EQ.NF+1) JJ=NF+IPSNG1
        DHX=DH*(1.D0+DABS(X(JJ)))
        R=X(JJ)
        X(JJ)=R+DHX
        DO 10 I=1,NAP
          K=IP(I)
          P(K)=X(NF+I)
 10     CONTINUE
        CALL FUNM(X,FP)
        X(JJ)=R
          IER=IERFS
          IF (IER.NE.0) RETURN
        X(JJ)=R+DHX
        CALL DETHPF(NK,X,FP(NF+1),IER)
        X(JJ)=R
          IF (IER.NE.0) RETURN
          DO 30 I=1,NFP
            DXP(I,J)=(FP(I)-F(I))/DHX
            IF (DABS(DXP(I,J)).LE.EPS0) DXP(I,J)=0.
 30       CONTINUE
 40   CONTINUE
      RETURN
      END
 
