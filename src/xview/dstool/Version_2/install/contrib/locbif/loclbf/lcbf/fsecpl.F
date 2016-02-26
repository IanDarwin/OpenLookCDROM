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

      SUBROUTINE FSECPL(NK,X,H,FSEC,IER)
C       26.08.91
C       16.11.92
      IMPLICIT REAL*8(A-H,O-Z)
      IMPLICIT INTEGER*2(I-N)
      include 'lcbf.dim'
      parameter (NFMAX1=NFMAX+1)
      CHARACTER*56 TEXT
      DIMENSION X(1),H(1)
      DIMENSION G(NFMAX)
      DIMENSION PCOORD(0:NFMAX)
      COMMON/PPAR/P(NPARMX)
      COMMON/IPPAR/IP(NPARMX)
      COMMON/LINBF/NF,NAP,ILN,NF2,ITP
      COMMON /IERFS/ IERFS,IERDUM
      COMMON /ISEC/ ISEC
      COMMON /MESSG/ MESSAG
      COMMON /SECPL/ PL(NFMAX1)
      IER=0
      DO 1  I=1,NAP
        K=IP(I)
        P(K)=X(NF+I)
 1    CONTINUE
      IF (ISEC.GT.0) THEN
        CALL FUNS(X,G)
        IF (IERFS.NE.0) IER=1
        FSEC=G(ISEC)
      ENDIF
      IF (ISEC.EQ.0) THEN
c        CALL FUNS(X,G)
c        IF (IERFS.NE.0) IER=1
        RR=PL(NF+1)
        DO I=1,NF
          RR=RR+PL(I)*X(I)
        ENDDO
        FSEC=RR
c        PRINT 201,(X(I),I=1,NF)
c 201    FORMAT(/'  X=',4G15.6)       
c        PRINT 202,(H(I),I=1,NF)
c 202    FORMAT(/'  H=',4G15.6)       
c        PRINT 203,(G(I),I=1,NF)
c 203    FORMAT(/'  G=',4G15.6)       
c        PRINT 204,FSEC
c 204    FORMAT(/'  FSEC=',G15.6)       
      ENDIF
      IF (ISEC.LT.0) THEN
        PCOORD(0)=0.D0
        DO I=1,NF
           PCOORD(I)=X(I)
        ENDDO
        IFN=-ISEC-1
        CALL USERFUN(IFN,IERR,PCOORD,FUNRES)
        FSEC=FUNRES
        IF (IERR.NE.0) IER=-1
      ENDIF
c      IF (MESSAG.EQ.3) THEN
c         WRITE (TEXT,101) FSEC,IER
c 101  FORMAT(
c     *  '   SECANT PLANE:  FSEC=',G11.4,'  IER=',I2,14X)
c         CALL ShowTe(TEXT)
c      ENDIF
      RETURN
      END

