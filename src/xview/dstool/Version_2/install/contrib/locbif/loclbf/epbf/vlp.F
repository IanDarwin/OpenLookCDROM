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

      SUBROUTINE VLP(NK,X,VL,IER)
C
C   Driver for computing first Lyapunov coefficient
C     04.05.1981
C     15.11.92
C
      IMPLICIT REAL*8(A-H,O-Z)
      IMPLICIT INTEGER*2(I-N)
      include 'epbf.dim'
      DIMENSION X(NK),VEC1(NFMAX),VEC2(NFMAX)
      EXTERNAL FUNS
      COMMON/PPAR/P(NPARMX)
      COMMON/IPPAR/IP(NPARMX)
      COMMON/LINBF3/EKR,H0
      COMMON/LINBF33/DH,NX,NS
      COMMON/LINBF/NF,NAP,ILN,NF2,ITP
      DATA EPS0/1.D-3/
      IER=0
      DO 10 I=1,NAP
        K=IP(I)
        P(K)=X(NF+I)
 10   CONTINUE
      IPRINT=0
      EPS=100.D0*EKR
      IF (EPS.LT.EPS0) EPS=EPS0
      IF (ILN.EQ.3.OR.ILN.EQ.12) EPS=100.D0
      CALL LYAPUN(NF,X,FUNS,IPRINT,EPS,AL,OM,VEC1,VEC2,VL,IEL)
      IER=IEL
      RETURN
      END
