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

      SUBROUTINE OUTFUN(NK,X,NPC,ARC,ISTOP)
C
C         15.02.1991
C         22.08.1991
C         14.11.92
C
      IMPLICIT REAL*8(A-H,O-Z)
      IMPLICIT INTEGER*2(I-N)
      include 'linbf.dim'
      DIMENSION X(NK),DFDX(1)
      COMMON/LINBF3/EPS,HV0
      COMMON/LINBFQ/IPT(NPTYP1), IVAL(NVALPT), ISTP
      COMMON/ADD3A/NUF,IPRZER,IFUNS(IFMAX)
      COMMON/OUT1/G(IFMAX)
      COMMON/OUT2/INDZ,IERF(IFMAX)
C----
      COMMON /LINBFW/ FUNH(IFMAX),ARCLNG,RNPT
      COMMON /CURPNT/ XCUR(NDIM)
C
      DO 1 I=1,NK
	 XCUR(I)=X(I)
 1    CONTINUE
C
      DO 10 I=1,NUF
        INDF        = IFUNS (I)
        IF (IPT(2).EQ.1) GO TO 5
           G(INDF) = FUNALL (INDF,NK,X,FUN,DFDX,IER,ISTOP)
           IF (ISTOP.NE.0) RETURN
           IF (IER.NE.0) G(INDF)=-666.D+33
           IERF(INDF) = IER
 5      FUNH(I) = G(INDF)
 10   CONTINUE
C
      RNPT    = NPC-1
      RNPT    = DSIGN(RNPT,HV0)
C      PRINT 101,RNPT
C 101  FORMAT(' RNPT=',G11.4)
      ARCLNG  = DSIGN(ARC,HV0)

      IF (IPT(2).EQ.1) THEN
         CALL SECANT(NK,X,IER)
      ENDIF
C
      RETURN
      END
 
