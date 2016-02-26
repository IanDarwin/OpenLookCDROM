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

      SUBROUTINE OUTLBF(ISTEP,INDEX,NK,NDIMM,X,VMOVE,DFUN,ISTOP)
C		 6.06.1989
C                14.11.92
C
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER*2(I-N)
      include 'linbf.dim'
      DIMENSION X(1),VMOVE(1),INDEX(NPTYP),DFUN(NDIM,1)
      COMMON/ADD3A/NUF,IPRZER,IFUNS(IFMAX)
      COMMON/OUT1/FUNG(IFMAX)
      COMMON/OUT2/INDZ,IERF(IFMAX)
      COMMON/OUT3/EPSCRZ,EPSCR,DETC,CONDC
      COMMON/OUT4/EPSZ,EPSZR,FZERO
      COMMON/OUT5/EPSEXT,EPSER
      COMMON/OUT6/INDE
      COMMON/OUT7/INDC
      COMMON/OUT8/INDOUT
      COMMON/PPAR/P(NPARMX)
      COMMON/IPPAR/IP(NPARMX)
      COMMON/LINBF/NF,NAP,ILN,NF2,ITP
      COMMON/LINBFC/AL(NPARMX),AR(NPARMX)
      COMMON/LINBFE/RR(NFMAX),RI(NFMAX),IPR
      COMMON/LINBFP/IGRAN
C----
      COMMON /LINBFQ/ IPT(NPTYP), INDER, IVAL(NVALPT), ISTP
      COMMON /LINBFR/ VALPT(NVALPT)
C----
      COMMON /LINBFU/ ARCL,PRP(NDIM),SC(NBUF),SCA(NBUF)
      COMMON /LINBFV/ NPC,NBF,NS(NBUF)
C----
C
      IPR=1
      ISTOP=0
      IF (INDOUT.NE.0) GO TO 500
C----
      IPT(1)=ILN
      IPT(2)=0
      IPT(3)=0
      IPT(4)=0
      INDER =0
      DO 210 I=1,10
	IVAL(I)=0
 210  CONTINUE
C----
      IF (ISTEP.NE.0) GO TO 220
	NPC=0
	NBF=0
	ARCL=0.
	DO 215 I=1,NK
	  PRP(I)=X(I)
 215    CONTINUE
 220  CONTINUE
      IF (INDEX(1).NE.0) GO TO 10
      IF (INDEX(2).NE.0) GO TO 20
      IF (INDEX(3).NE.0) GO TO 30
      IF (INDEX(4).NE.0) GO TO 40
      GO TO 99
 10   CALL REGPBF(NK,X,ILN,NUF,IFUNS,FUNG,IERF,ISTEP)
      GO TO 99
 20   CONTINUE
      CALL CRSBF(NK,X,INDC)
      GO TO 99
 30   CONTINUE
      IFZERO=INDEX(3)
      CALL ZEROBF(NK,X,IFZERO,FZERO,INDZ)
      GO TO 99
 40   CONTINUE
      IEX=IABS(INDEX(4))
      PEX=X(IEX)
      JEX=IP(IEX-NF)
      JSIGN=1
      IF (INDEX(4).LT.0) JSIGN=-1
      CALL EXTRBF(NK,X,JEX,PEX,JSIGN,INDE)
      GO TO 99
 99   MODE=1
      CALL ORDBF(ISTEP,NK,X,MODE,ISTOP)
      CALL WLINE(ISTEP,INDEX,NK,NDIM,X,VMOVE,DFUN,ISTOP)
      GO TO 199
 500  CONTINUE
#if defined(hib) 
      PRINT 901
#endif
      MODE=2
      CALL ORDBF(ISTEP,NK,X,MODE,ISTOP)
      IF (ISTOP.NE.0) GO TO 199
      DO 510 I=1,NAP
        K=IP(I)
        IF (X(NF+I).LE.AL(K).OR.X(NF+I).GE.AR(K)) GO TO 520
 510  CONTINUE
      GO TO 199
 520  ISTOP=100
      IGRAN=1
 199  RETURN
#if defined(hib) 
 901  FORMAT(1X,110('-'))
#endif
      END
