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

      SUBROUTINE LCFUNM(X,F)
C
C   INTEGRATING  ODEs 
C          16.02.91
C          16.11.92
C          10.09.93
C          08.07.94                                    
C
      IMPLICIT REAL*8(A-H,O-Z)
      IMPLICIT INTEGER*2(I-N)
      include 'lcbf.dim'
      include 'locbif.dim'
      DIMENSION X(1),F(1),Y(NFMAX)
      DIMENSION XUSRF(0:NFMAX)
      COMMON /IERFS/ IERFS,IERDUM
      COMMON /LINBF/ NF,NAP,ILN,NF2,ITP
      COMMON /LINBF1/DHF
      COMMON/ITMAP/ITMAP
      COMMON/INTGR/TMAXI,HI,EPSAI,EPSRI,DHI,HMXI,HMNI,SOLVER,RMXSTP
      IERFS=0
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
      DO 10 I=1,NF
        Y(I)=X(I)
 10   CONTINUE
       CALL INTEGR(NF,T,TMAX,Y,H,HMAX,EPSABS,EPSREL,DH,NMAX,MODE,IEND)
      IF (IEND.NE.0) GO TO 90
c
      XUSRF(0)=T
      DO 20 I=1,NF
         XUSRF(I)=Y(I)
 20   CONTINUE
      IEND=0
      CALL USERFUN(NUMTRANS,IEND,XUSRF,GFUN)
      IF (IEND.EQ.-1) GOTO 90
      IF (IEND.EQ.0) THEN
         DO 25 I=1,NF
            Y(I)=XUSRF(I)
 25      CONTINUE
      ENDIF
c
 30   DO 40 I=1,NF
          F(I)=Y(I)
 40   CONTINUE
      RETURN
C
 90   CONTINUE
      IF (IERFS.NE.0) RETURN
      IF (IEND.EQ.-5) IERFS=IEND
      IF (IEND.NE.-5) IERFS=IABS(IEND)
      RETURN
      END
