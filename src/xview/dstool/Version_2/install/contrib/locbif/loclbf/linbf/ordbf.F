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

      SUBROUTINE ORDBF(ISTEP,NK,X,MODE,ISTOP)
C		21.05.1990
C               07.08.1991
C               14.11.92
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER*2(I-N)
      include 'linbf.dim'
      parameter (NPTYP2=NPTYP1+1)
C      REAL  ARCS,XS,GS,RRS,RIS,VALPTS
      DIMENSION X(NK),XX(NDIM)
      COMMON/OUT1/G(IFMAX)
      COMMON/PPAR/P(NPARMX)
      COMMON/IPPAR/IP(NPARMX)
      COMMON/LINBF/NF,NAP,ILN,NF2,ITP
      COMMON/LINBFE/RR(NFMAX),RI(NFMAX),IPR
      COMMON/LINBEE/RMOD(NFMAX),ARG(NFMAX)
C----
      COMMON /LINBFQ/ IPT(NPTYP1), IVAL(NVALPT), ISTP
      COMMON /LINBFR/ VALPT(NVALPT)
      COMMON /LINBFS/ ARCS(NBUF),XS(NBUF,NDIM),GS(NBUF,IFMAX),
     1                RRS(NBUF,NFMAX),RIS(NBUF,NFMAX),
     2                RMODS(NBUF,NFMAX),ARGS(NBUF,NFMAX),
     3                VALPTS(NBUF,NVALPT)
      COMMON /LINBFT/ IVALS(NBUF,NVALPT),IPTS(NBUF,NPTYP2)
      COMMON /LINBFU/ ARCL,PRP(NDIM),SC(NBUF),SCA(NBUF)
      COMMON /LINBFV/ NPC,NBF,NS(NBUF)
      COMMON /SCALEB/ SCX(NDIM)
C
      GO TO (1,100), MODE
      RETURN
C-------------------------------------------------------------
C----	Part 1. Store current point.
 1    CONTINUE
C
      IF (NBF.LT.NBUF) GO TO 10
#if defined(hib) 
	PRINT 901
 901    FORMAT(//3X,'Buffer is full')
#endif
	RETURN
 10   NBF=NBF+1
      DO 20 I=1,NK
	XS(NBF,I)=X(I)
 20   CONTINUE
      DO 30 I=1,NFMAX
	RRS(NBF,I)=RR(I)
	RIS(NBF,I)=RI(I)
	RMODS(NBF,I)=RMOD(I)
	ARGS(NBF,I)=ARG(I)
 30   CONTINUE
      DO 40 I=1,NVALPT
	VALPTS(NBF,I)=VALPT(I)
	IVALS(NBF,I)=IVAL(I)
 40   CONTINUE
      DO 50 I=1,IFMAX
	GS(NBF,I)=G(I)
 50   CONTINUE
      DO 60 I=1,NPTYP1
	IPTS(NBF,I)=IPT(I)
 60   CONTINUE
      IPTS(NBF,NPTYP2)=ISTEP
      RETURN
C-------------------------------------------------------------
C	Part 2. Order and output stored points.
C
 100  CONTINUE 
C	 To compute secant length.
      R1=0.
      DO 110 I=1,NK
	R1=R1+(PRP(I)-XS(1,I))**2
 110  CONTINUE
C	 To compute the projection coordinate for every point.
      IF (NBF.GT.1) GO TO 120
	NS(1)=1
	GO TO 140
C.................
 120  SC(1)=1.
      DO 122 J=2,NBF
	R2=0.
        DO 121 I=1,NK
	  R2=R2+(PRP(I)-XS(1,I))*(PRP(I)-XS(J,I))
 121    CONTINUE 
        SC(J)=R2/R1
 122  CONTINUE
C	 To order points.
 130  CONTINUE
      NS(NBF)=1
      DO 131 J=1,NBF-1
        NS(J)=J+1
 131  CONTINUE
      DO 135 J=1,NBF-1
        LJ=NS(J)
        R1=SC(LJ)
        DO 134 I=J,NBF-1
	 LI=NS(I)
	 IF (SC(LI).GE.R1) GO TO 134
	   NS(I)=LJ
	   NS(J)=LI
	   R1=SC(LI)
	   LJ=LI
 134    CONTINUE
 135  CONTINUE
C	 To compute arc length for ordered points.
 140  CONTINUE
      DO 142 J=1,NBF
        R1=0.
        L=NS(J)
        DO 141 I=1,NK
          R1=R1+((PRP(I)-XS(L,I))*SCX(I))**2
          PRP(I)=XS(L,I)
 141    CONTINUE
	SCA(J)=DSQRT(R1)
	ARCL=ARCL+SCA(J)
	ARCS(L)=ARCL
 142  CONTINUE 
C        To output the ordered points.
 150  CONTINUE
      NOUT=0
 151  NOUT=NOUT+1
      L=NS(NOUT)
      DO 160 I=1,NK
	XX(I)=XS(L,I)
 160  CONTINUE
      DO 170 I=1,NFMAX
	RR(I)=RRS(L,I)
	RI(I)=RIS(L,I)
	RMOD(I)=RMODS(L,I)
	ARG(I)=ARGS(L,I)
 170  CONTINUE
      DO 180 I=1,NVALPT
	VALPT(I)=VALPTS(L,I)
	IVAL(I)=IVALS(L,I)
 180  CONTINUE
      DO 190 I=1,IFMAX
	G(I)=GS(L,I)
 190  CONTINUE
      DO 200 I=1,NPTYP1
	IPT(I)=IPTS(L,I)
 200  CONTINUE
      ISTP=IPTS(L,NPTYP2)
      DO 210 I=1,NAP
        K=IP(I)
        P(K)=XS(L,NF+I)
 210  CONTINUE
      NPC=NPC+1
      ARC=ARCS(L)
      CALL OUTFUN(NK,XX,NPC,ARC,ISTOP)
        IF (ISTOP.NE.0) RETURN
      CALL OUTPT(NK,XX,NPC,ARC,ISTOP)
      IF (ISTOP.NE.0) RETURN
      IF (NOUT.LT.NBF) GO TO 151
C	 To prepare for the next storing.
      NBF=0
      RETURN
      END
C      SUBROUTINE OUTPT(NK,XX,NP,ARC,ISTOP)
CC		6.06.1989
C      IMPLICIT REAL*8 (A-H,O-Z)
C      IMPLICIT INTEGER*2(I-N)
c      include 'linbf.dim'
c      parameter (NPTYP2=NPTYP1+1)
C      DIMENSION XX(NK)
C      REAL  ARCS,XS,GS,RRS,RIS,VALPTS
c     COMMON/OUT1/G(1)
c     COMMON/PPAR/P(1)
c     COMMON/IPPAR/IP(1)
c     COMMON/LINBF/NF,NAP,ILN,NF2,ITP
c     COMMON/LINBFE/RR(NFMAX),RI(NFMAX),IPR
c     COMMON/LINBEE/RMOD(NFMAX),ARG(NFMAX)
C----
c     COMMON /LINBFQ/ IPT(NPTYP1), IVAL(NVALPT), ISTP
c     COMMON /LINBFR/ VALPT(NVALPT)
c     COMMON /LINBFU/ ARCL,PRP(NDIM),SC(NBUF),SCA(NBUF)
c     COMMON /LINBFV/ NPC,NBF,NS(NBUF)
C
C      PRINT 101, NP, ARC, ISTP
C 101  FORMAT(/1X,'NP=',I3,'  ARCLENGTH=',G13.6, '  ISTEP=',I3)
C      PRINT 102, XX
C 102  FORMAT(/1X,'X=',6(G13.6,2X))
C      PRINT 103, (G(I),I=1,5)
C 103  FORMAT(1X,'G=',6(G13.6,2X))
C      PRINT 111, (RR(I),I=1,NF)
C 111  FORMAT(1X,'RR =',5(G13.6,2X))
C      PRINT 112, (RI(I),I=1,NF)
C 112  FORMAT(1X,'RI =',5(G13.6,2X))
C      PRINT 104,IPT,IVAL(1),VALPT(1),VALPT(2)
C 104  FORMAT(1X,'IPT=',5I4,'  IVAL=',I3,'  VALPT=',2G13.4)
C      PRINT 105,NBF,(NS(I),I=1,NBF)
C 105  FORMAT(1X,'NBF=',I2,'  NS=',10I3)
C      PRINT 107, (SC(I),I=1,NBF)
C 107  FORMAT(1X,'SC =',5(G13.6,2X))
C      PRINT 108, (SCA(I),I=1,NBF)
C 108  FORMAT(1X,'SCA=',5(G13.6,2X))
C      PRINT 109, (PRP(I),I=1,NK)
C 109  FORMAT(1X,'PRP=',6(G13.6,2X))
C      PRINT 110
C 110  FORMAT(/1X,70('='))
C      RETURN
C      END
