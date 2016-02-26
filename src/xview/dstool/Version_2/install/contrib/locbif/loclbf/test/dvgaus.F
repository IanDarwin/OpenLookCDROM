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

      SUBROUTINE DVGAUS(N,NA,A,X,EPS,IRANG)
C **********************************************************************
C     èPOÉPAMMA DVGAUS
C **********************************************************************
C     14.11.92
C-----------------------------------------------------------------------
C
C     èPOÉPAMMA DVGAUS èPEÑHAáHAóEHA Ñãü PEòEHàü OÑHOPOÑHOâ
C     CàCTEMõ liHEjHyX uPABHEHij   A*X=0 , gdE A- MATPicA N*N
C     PAHg KOTOPOj PABEH N-1.
C     PE{EHiE OCu}ECTBlqETCq METOdOM gAuCCA C BybOPOM glABHOgO
C     |lEMEHTA pO BCEj MATPicE.
C
C       OpiCAHiE pAPAMETPOB
C
C       N - PAzMEPHOCTx MATPicy
C      NA - CTPO~HAq PAzMEPHOCTx dBuMEPHOgO MACCiBA A, COdEPvA}EgO
C           MATPicu N*N, OpiCAHHAq B ByzyBA`}Ej pPOgPAMME.
C       A - dBuMEPHyj MACCiB, COdEPvA}ij MATPicu N*N.
C       X - MACCiB PAzMEPA N, COdEPvA}ij iCKOMOE PE{EHiE OdHOPOdHOj
C           CiCTEMy (OdHA iz KOOPdiHAT BEKTOPA X PABHA 1).
C     EPS - BXOdHOj pAPAMETP dlq pPOBEPKi HA HOlx glABHyX |lEMEHTOB
C           MATPicy.  PEKOMEHduETCq EPS=1.D-3
C   IRANG - ByXOdHOj pAPAMETP, COdEPvA}ij PAHg MATPicy A.
C           ECli IRANG # N-1 , TO |TO OzHA~AET ABAPijHOE zABEP{EHiE
C           PAbOTy pPOgPAMMy. BEKTOP X pPi |TOM HE OpPEdElqETCq.
C
      IMPLICIT REAL*8(A-H,O-Z)
      IMPLICIT INTEGER*2(I-N)
      include 'algebrbf.dim'
      parameter (NA2=NFMAX*NFMAX)
      DIMENSION A(NA,1),X(N),IK(NA2)
      N1=N-1
      N2=N+1
      DO 13 I=1,N
  13  IK(I)=I
      IRANG=0
      L=1
  11  CONTINUE
C
C       i}EM BEdu}ij |lEMEHT pO BCEj MATPicE
C     K-HOMEP CTPOKi, IKL-HOMEP CTOlbcA BEdu}EgO |lEMEHTA
C
      AM=0.D0
      AMAX=0.D0
      DO 1 I=L,N
      DO 1 J=L,N
      AM1=A(I,J)
      IF(AM.GE.DABS(AM1)) GO TO 2
      AM=DABS(AM1)
      AMAX=AM1
      K=I
      IKL=J
  2   CONTINUE
  1   CONTINUE
      IF(AM.GT.EPS) GO TO 4
      RETURN
  4   IF(L.EQ.K) GO TO 5
C
C   MEHqEM MECTAMi L-u` i K-u` CTPOKi
C
      DO 6 J=L,N
      R=A(K,J)
      A(K,J)=A(L,J)
  6     A(L,J)=R
  5   CONTINUE
       IF(IKL.EQ.L) GO TO 7
C
C   MEHqEM MECTAMi L-yj i K-yj CTOlbEc
C
      IK1=IK(L)
      IK(L)=IK(IKL)
      IK(IKL)=IK1
      DO 8 J=1,N
      R=A(J,L)
      A(J,L)=A(J,IKL)
  8   A(J,IKL)=R
  7   CONTINUE
C
C   dEliM L-u` CTPOKu HA BEdu}ij |lEMEHT i By~iTAEM iz dPugiX CTPOK
C
      L1=L+1
      DO 9 J=L1,N
      R=A(J,L)/AMAX
      DO 10 IS=L1,N
  10  A(J,IS)=A(J,IS)-A(L,IS)*R
  9   CONTINUE
      L=L1
      IRANG=IRANG+1
      IF(L.LT.N) GO TO 11
      IF(DABS(A(N,N)).LE.EPS) GO TO 12
      IRANG=IRANG+1
      RETURN
  12  CONTINUE
C
C   HAXOdiM BEKTOP OdHOPOdHyX PE{EHij
C
      X(IK(N))=1.D0
      DO 14 I=1,N1
      M=N-I
      S=0.D0
      DO 15 J=1,I
      J1=N2-J
  15  S=S+A(M,J1)*X(IK(J1))
      X(IK(M))=-S/A(M,N-I)
  14  CONTINUE
      RETURN
      END
 
