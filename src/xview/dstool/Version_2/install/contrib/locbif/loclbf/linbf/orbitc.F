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

      SUBROUTINE ORBITC(N,X,IDIR,IEND)
C
C          16.02.91
C          23.08.91
C          14.11.92
C
      IMPLICIT REAL*8(A-H,O-Z)
      IMPLICIT INTEGER*2(I-N)
      include 'linbf.dim'
      DIMENSION X(N),Y(NFMAX)
      COMMON /ITMAP/ ITMAP
      COMMON /IERFS/ IERFS,IERDUM
      COMMON /LINBF/ NF,NAP,ILN,NF2,ITP
      COMMON /LINBF1/DHF
      COMMON /LINBFQ/ IPT(NPTYP1), IVAL(NVALPT), ISTP
      COMMON /LINBFR/ VALPT(NVALPT)
      COMMON/INTGR/TMAXI,HI,EPSAI,EPSRI,DHI,HMXI,HMNI,SOLVER,RMXSTP
        COMMON /INTOUT/ IOUT,IDIREC
        COMMON /NPNT/ NP
      DATA NPMAX/10000/
      NF=N
      NAP=0
      ILN=-2
      CALL INITBF
      DO 1 I=1,NPTYP1
        IPT(I)=0
 1    CONTINUE
      DO 2 I=1,NVALPT
        IVAL(I)=0
        VALPT(I)=0.
 2    CONTINUE
      IERFS=0
      IEND=0
      ISTOP=0
      IOUT=1
      IDIREC=IDIR
      DO 10 I=1,N
        Y(I)=X(I)
 10   CONTINUE
      NP=0
      T=0.D0
      TMAX=DABS(TMAXI*ITMAP)
      H=HI
      EPSABS=DABS(EPSAI)
      EPSREL=DABS(EPSRI)
      DH=DHI
      HMAX=DABS(HMXI)
      IF (HMAX.GT.0.1D0*TMAX) HMAX=0.1D0*TMAX
      MODE=SOLVER
      NMAX=RMXSTP
C
 20   NP=NP+1
      IPT(2)=3
      IPT(3)=100
      VALPT(1)=T
      CALL OUTPT(N,Y,NP,T,ISTOP)
      IF (ISTOP.NE.0) GO TO 40
      IF (NP.GE.NPMAX) GO TO 50
C        PRINT 102,T,(Y(I),I=1,NF),TMAX
C 102    FORMAT(/' T=',G12.5,' Y=',2G15.7,' H=',G13.5)
      IPT(2)=1
       CALL INTEGR(NF,T,TMAX,Y,H,HMAX,EPSABS,EPSREL,DH,NMAX,MODE,IEND)
C        PRINT 101,IEND,T,(Y(I),I=1,NF),H
C 101    FORMAT(' IEND=',I3,/' T=',G12.5,' Y=',2G15.7,' H=',G13.5)
      H=HI
      IF (IEND.NE.0) GO TO 99
      GO TO 20      
C
 40   CONTINUE
      IEND=-5
      RETURN
C
 50   CONTINUE
      IEND=1
      RETURN
C
 99   CONTINUE
      IF (IEND.NE.-5) IEND=-3 
      RETURN
      END
