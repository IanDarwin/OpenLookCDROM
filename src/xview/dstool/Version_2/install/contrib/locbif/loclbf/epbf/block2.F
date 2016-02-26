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

      SUBROUTINE BLOCK2(N,A,AL,OM,VEC1,VEC2,P,Q,C,NA,IER)
C
C   Transform matrix to the block-diagonal form with
C      two blocks: 2*2 and (N-2)*(N-2)
C     15.11.92
C
      IMPLICIT REAL*8(A-H,O-Z)
      IMPLICIT INTEGER*2(I-N)
      include 'epbf.dim'
      parameter (NFMX2d=NFMAX2*2)
      DIMENSION A(NA,1),P(NA,1),Q(NA,1),C(NA,1),VEC1(N),VEC2(N)
      DIMENSION VECS1(NFMAX),VECS2(NFMAX),LR(NFMAX),MR(NFMAX)
      COMMON/WORK3/ DUMM1(NFMX2d),AT(NFMAX,NFMAX),ANN(NFMAX2)
      IER=0
      EPSPLO=1.D-4
      IF (OM.LT.1.) EPSPLO=EPSPLO*OM
C   1.By~iClEHiE iHBAP. plOCKOCTi dlq TPAHCpOHiP. MATPicy
C
      DO 110 I=1,N
        DO 100 J=1,N
 100    AT(I,J)=A(J,I)
 110  CONTINUE
      CALL PLO(N,AT,AL,OM,VECS1,VECS2,NA,EPSPLO,IER)
      IF (IER.NE.0) RETURN
C
C   2.OTyCKAHiE OPTOgOHAlxHOgO dOpOlHEHiq K BEKTOPAM VECS1 i VECS2
C
      DO 120 I=1,N
        Q(I,1)=VECS1(I)
        Q(I,2)=VECS2(I)
 120  CONTINUE
      CALL ORTG2(N,NA,Q)
C
C   3.fOPMiPOBAHiE bAziCA Q,B KOTOPOM MATPicA liHEAPizAcii
C     iMEET blO~Hyj Bid,By~iClEHiE MATPicy P,ObPATHOj K Q,
C     i By~iClEHiE MATPicy C=PAQ
C
      DO 130 I=1,N
        Q(I,1)=VEC1(I)
        Q(I,2)=VEC2(I)
 130  CONTINUE
      L=1
      DO 150 J=1,N
        DO 140 I=1,N
          ANN(L)=Q(I,J)
          L=L+1
 140    CONTINUE
 150  CONTINUE
      CALL DMINV(ANN,N,DETQ,LR,MR)
      L=1
      DO 170 J=1,N
        DO 160 I=1,N
          P(I,J)=ANN(L)
          L=L+1
 160    CONTINUE
 170  CONTINUE
      DO 200 J=1,N
        DO 190 I=1,N
          R=0.
          DO 180 K=1,N
 180      R=R+A(I,K)*Q(K,J)
          AT(I,J)=R
 190    CONTINUE
 200  CONTINUE
      DO 230 J=1,N
        DO 220 I=1,N
          R=0.
          DO 210 K=1,N
 210      R=R+P(I,K)*AT(K,J)
          C (I,J)=R
 220    CONTINUE
 230  CONTINUE
      RETURN
      END
