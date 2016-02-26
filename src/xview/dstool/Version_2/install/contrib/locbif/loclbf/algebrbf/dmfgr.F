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

      SUBROUTINE DMFGR(A,M,N,EPS,IRANK,IROW,ICOL)
      IMPLICIT INTEGER*2(I-N)
      DIMENSION A(1),IROW(1),ICOL(1)
      DOUBLE PRECISION A,PIV,HOLD,SAVE,EPS
      IF(M)2,2,1
    1 IF(N)2,2,4
    2 IRANK=-1
    3 RETURN
    4 IRANK=0
      PIV=0.D0
      JJ=0
      DO 6 J=1,N
      ICOL(J)=J
      DO 6 I=1,M
      JJ=JJ+1
      HOLD=A(JJ)
      IF(DABS(PIV)-DABS(HOLD))5,6,6
    5 PIV=HOLD
      IR=I
      IC=J
    6 CONTINUE
      DO 7 I=1,M
    7 IROW(I)=I
c      TOL=ABS(EPS*SNGL(PIV))
      TOL=ABS(EPS)
      NM=N*M
      DO 219 NCOL=M,NM,M
    8 IF(ABS(SNGL(PIV))-TOL)20,20,9
    9 IRANK=IRANK+1
      JJ=IR-IRANK
      IF(JJ)12,12,10
   10 DO 11 J=IRANK,NM,M
      I=J+JJ
      SAVE=A(J)
      A(J)=A(I)
   11 A(I)=SAVE
      JJ=IROW(IR)
      IROW(IR)=IROW(IRANK)
      IROW(IRANK)=JJ
   12 JJ=(IC-IRANK)*M
      IF(JJ)15,15,13
   13 KK=NCOL
      DO 14 J=1,M
      I=KK+JJ
      SAVE=A(KK)
      A(KK)=A(I)
      KK=KK-1
   14 A(I)=SAVE
      JJ=ICOL(IC)
      ICOL(IC)=ICOL(IRANK)
      ICOL(IRANK)=JJ
   15 KK=IRANK+1
      MM=IRANK-M
      LL=NCOL+MM
      IF(MM)16,25,25
   16 JJ=LL
      SAVE=PIV
      PIV=0.D0
      DO 119 J=KK,M
      JJ=JJ+1
      HOLD=A(JJ)/SAVE
      A(JJ)=HOLD
      L=J-IRANK
      IF(IRANK-N)17,119,119
   17 II=JJ
      DO 19 I=KK,N
      II=II+M
      MM=II-L
      A(II)=A(II)-HOLD*A(MM)
      IF(DABS(A(II))-DABS(PIV))19,19,18
   18 PIV=A(II)
      IR=J
      IC=I
   19 CONTINUE
  119 CONTINUE   
  219 CONTINUE
   20 IF(IRANK-1)3,25,21
   21 IR=LL
      DO 24 J=2,IRANK
      II=J-1
      IR=IR-M
      JJ=LL
      DO 23 I=KK,M
      HOLD=0.D0
      JJ=JJ+1
      MM=JJ
      IC=IR
      DO 22 L=1,II
      HOLD=HOLD+A(MM)*A(IC)
      IC=IC-1
   22 MM=MM-M
   23 A(MM)=A(MM)-HOLD
   24 CONTINUE
   25 IF(N-IRANK)3,3,26
   26 IR=LL
      KK=LL+M
      DO 30 J=1,IRANK
      DO 29 I=KK,NM,M
      JJ=IR
      LL=I
      HOLD=0.D0
      II=J
   27 II=II-1
      IF(II)29,29,28
   28 HOLD=HOLD-A(JJ)*A(LL)
      JJ=JJ-M
      LL=LL-1
      GOTO 27
   29 A(LL)=(HOLD-A(LL))/A(JJ)
   30 IR=IR-1
      RETURN
      END
 
