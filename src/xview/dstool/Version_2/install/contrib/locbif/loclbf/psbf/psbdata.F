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

      SUBROUTINE psBDATA
C
C     PARAMETERS' INITIALISATION FOR LINLBF
C                    ITERATED MAPS   
C
C       20.05.1990
C       02.09.1990
C       16.11.92
C       10.07.94
C
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER*2 (I-N)
      include 'psbf.dim'
      COMMON/PPAR/P(NPARMX)
      COMMON/IPPAR/IP(NPARMX)
      COMMON/LINBF/NF,NAP,ILN,NF2,ITP
      COMMON/LINBF1/DHF
      COMMON/LINBF2/D(NFMAX2)
      COMMON/LINBF3/EPS,HV0
      COMMON/LINBFC/AL(NPARMX),AR(NPARMX)
      COMMON/LINBFE/RR(NPARMX),RI(NPARMX),IPDUMM
      COMMON/LINBFN/IOLD
      COMMON/LINBFO/INDFUN
      COMMON/LINBFP/IGRAN
      COMMON/QUAVEC/VEC1(NFMAX),VEC2(NFMAX),VEC3(NFMAX)
      COMMON/SCALEB/SCX(NDIM)
      COMMON /ADD1A/ IPR, NSTEP, NCAN
      COMMON /ADD1B/ DX, HVMN, HVMX
      COMMON /ADD2A/ ICRS, IPRCRS
      COMMON /ADD2B/ EPSCRS
      COMMON /ADD3A/ NUF, IPRZER, IFUNS(IFMAX)
      COMMON /ADD3B/ EPSZER
C!!!  COMMON /ADD4A/ NUE, IPREXT, IVARS(25)
      COMMON /ADD4A/ NUE, IPREXT, IVARS(NDIM)
      COMMON /ADD4B/ EPSEXT
      COMMON /ADD5A/ PXGMN, PXGMX
      COMMON /ADD6A/  XGMN,  XGMX
      COMMON /ADD7A/ EPSNWT
      COMMON /ADD7B/ ITNWT
C
      NK=NF+NAP
      XGMN= AL(NF+1)
      XGMX= AR(NF+1)
      NSTEP=1000
      DX= -DABS(DX)
      DHF=1.D-5
      EPSNWT = EPS
      ITNWT  = 2
      IPOISK = 1
C
      IPR=-1
      NCAN=6
C      HVMN=1.D-10
C      HVMX=10.
      ICRS=1
      IPRCRS=-1
      EPSCRS=EPS
      NUE=NAP
      IPREXT=-1
      EPSEXT=EPS
      DO 10 I=1,NAP
        IVARS(I)=NF+I
 10   CONTINUE
      IPRZER=-1
      EPSZER=EPS
      NUF=0
C
      IF (ILN.NE. 0) GO TO 11 
         NUF=4
         IFUNS(1)=1
         IFUNS(2)=2
         IFUNS(3)=3
         IFUNS(4)=4
         GO TO 100
C
 11   IF (ILN.NE. 1) GO TO 12
         NUF=4
         IFUNS(1)=2
         IFUNS(2)=3
         IFUNS(3)=5
         IFUNS(4)=4
         GO TO 100
C
 12   IF (ILN.NE. 2) GO TO 13
         NUF=3
         IFUNS(1)=1
         IFUNS(2)=3
         IFUNS(3)=4
         GO TO 100
C
 13   IF (ILN.NE. 3) GO TO 14
         NUF=3
         IFUNS(1)=1
         IFUNS(2)=2
         IFUNS(3)=4
         GO TO 100
C
 14   IF (ILN.NE. 4) GO TO 21
         NUF=3
         IFUNS(1)=1
         IFUNS(2)=2
         IFUNS(3)=3
         GO TO 100
C
 21   IF (ILN.NE.11) GO TO 22
         NUF=2
         IFUNS(1)=5
         IFUNS(2)=3
         GO TO 100
C
 22   IF (ILN.NE.12) GO TO 23
         NUF=1
         IFUNS(1)=1
         GO TO 100
C
 23   IF (ILN.NE.13) GO TO 24
         NUF=2
         IFUNS(1)=3
         IFUNS(2)=5
         GO TO 100
C
 24   IF (ILN.NE.14) GO TO 25
         NUF=1
         IFUNS(1)=1
         GO TO 100
C
 25   IF (ILN.NE.15) GO TO 26
         NUF=2
         IFUNS(1)=2
         IFUNS(2)=5
         GO TO 100
C
 26   IF (ILN.NE.16) GO TO 27
         NUF=2
         IFUNS(1)=2
         IFUNS(2)=3
         GO TO 100
C
 27   IF (ILN.NE.17) GO TO 28
         NUF=4
         IFUNS(1)=2
         IFUNS(2)=3
         IFUNS(3)=5
         IFUNS(4)=4
         GO TO 100
C
 28   IF (ILN.NE.18) GO TO 29
         NUF=3
         IFUNS(1)=1
         IFUNS(2)=3
         IFUNS(3)=4
         GO TO 100
C
 29   IF (ILN.NE.19) GO TO 100
         NUF=3
         IFUNS(1)=1
         IFUNS(2)=2
         IFUNS(3)=4
         GO TO 100
 100  CONTINUE
      DO 145 I=1,NK
        SCX(I)=1.D0
 145  CONTINUE
C
      CALL INITBF
C
      RETURN
      END
