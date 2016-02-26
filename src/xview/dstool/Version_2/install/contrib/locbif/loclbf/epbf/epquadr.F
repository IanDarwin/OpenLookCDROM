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

      SUBROUTINE epQUADR(NK,X,QDR,IER)
C
C   COMPUTING THE COEFFICIENT ON SECOND-ORDER TERM 
C        24.02.1990
C        15.11.92
C        10.07.94
C
      IMPLICIT REAL*8(A-H,O-Z)
      IMPLICIT INTEGER*2(I-N)
      include 'epbf.dim'
      DIMENSION X(NK),DS(NFMAX2)
      COMMON/PPAR/P(NPARMX)
      COMMON/IPPAR/IP(NPARMX)
      COMMON/LINBF/NF,NAP,ILN,NF2,ITP
      COMMON/LINBF1/DHF
      COMMON/LINBF2/D(NFMAX2)
      COMMON/QUAVEC/VEC1(NFMAX),VEC2(NFMAX),VEC3(NFMAX)
      EXTERNAL FUNS,DER1L,DER2Q
      IER=0
      DO 10 I=1,NAP
        K=IP(I)
        P(K)=X(NF+I)
 10   CONTINUE
C
C   COMPUTING JACOBY MATRIX
C
      DH0=DHF
      NDH=0
      CALL DERVAR(NF,NF2,DH0,NDH,X,D,DS,DER1L,FUNS,IER)
      IF (IER.NE.0) RETURN
C   NF=1
      IF (NF.EQ.1) GOTO 60
C   NF>1
      L=1
      DO 30 I=1,NF
        K=I
        DO 20 J=1,NF
          DS(L)=D(K)
          L=L+1
          K=K+NF
 20     CONTINUE
 30   CONTINUE
C
C   COMPUTING RIGHT EIGENVECTOR CORRESPONDIND ZERO EIGENVALUE
C
      EPS=0.01
      CALL DVGAUS(NF,NF,D,VEC1,EPS,IRANG)
      IF (IRANG.NE.(NF-1)) GO TO 100
C
C   COMPUTING LEFT EIGENVECTOR CORRESPONDIND ZERO EIGENVALUE
C
      CALL DVGAUS(NF,NF,DS,VEC2,EPS,IRANG)
C     PRINT 901,IRANG
      IF (IRANG.NE.(NF-1)) GO TO 100
C
C   SCALING EIGENVECTORS
C
      R1=0.D0
      R2=0.D0
      R32=0.D0
      DO 40 I=1,NF
        R1=R1+VEC1(I)**2
        R2=R2+VEC2(I)**2
        R32=R32+VEC3(I)*VEC2(I)
 40   CONTINUE
      R1=1.D0/DSQRT(R1)
      R2=1.D0/DSQRT(R2)
      IF (R32.LT.0.D0) R2=-R2
      DO 50 I=1,NF
        VEC1(I)=R1*VEC1(I)
        VEC2(I)=R2*VEC2(I)
        VEC3(I)=VEC2(I)
 50   CONTINUE
      GOTO 70
C
C   EIGENVECTORS FOR CASE NF=1
C
 60   VEC1(1)=1.
      VEC2(1)=1.
      VEC3(1)=1.
C
C   COMPUTING SECOND DERIVATIVE
C
 70   ND=1
      DH0=1.D-4
      NDH=0
      CALL DERVAR(NF,ND,DH0,NDH,X,D,DS,DER2Q,FUNS,IER)
      IF (IER.NE.0) RETURN
      QDR=D(1)
      RETURN
 100  IER=IRANG+1
      RETURN
      END
