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

      FUNCTION epFUNALL (INDF,NX,X,FUN,DFDX,IER,ISTOP)
C     3.05.90É.
C     15.11.92
C     10.07.94
C* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C*                                                                     *
C*    Compute "external" functions defined on a curve                  *
C*                                                                     *
C* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
C     ÇÄêàÄç  variant forí Ñãü LINLBF
C       20.05.1990
C
C* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER*2(I-N)
      DIMENSION X(1)
      COMMON/LINBF/NF,NAP,ILN,NF2,ITP
      EXTERNAL FUN, DFDX
C
      IER=0
      ISTOP=0
      NK=NF+NAP
C
      IF (INDF .LE. 10)
     A          GOTO (100,110,120,130,140,150,160,170,180,190), INDF
C
      IF (INDF .GT. 10)
     A          GOTO (200,210,220,230,240,250,260,270,280,290), INDF
C
      IER=1
      RETURN
 100  CONTINUE
      CALL DETJAC(NK,X,G,IER)
      GO TO 500
C
 110  CONTINUE
      CALL DETRG(NK,X,G,IER)
      GO TO 500
C
 120  CONTINUE
      CALL DETEV(NK,X,G,IER)
      GO TO 500
C
 130  CONTINUE
      CALL QUADR(NK,X,G,IER)
      GO TO 500
C
 140  CONTINUE
      CALL VLP(NK,X,G,IER)
      GO TO 500
C
 150  CONTINUE
      CALL DETKRV(NK,X,G,IER)
      GO TO 500
C
 160  CONTINUE
      CALL DETNTV(NK,X,G,IER)
      GO TO 500
C
 170  CONTINUE
      CALL DETVPR(NK,X,G,IER)
      GO TO 500
C
 180  CONTINUE
      IER=1
      GO TO 500
C
 190  CONTINUE
      IER=1
      GO TO 500
C
 200  CONTINUE
      IER=1
      GO TO 500
C
 210  CONTINUE
      IER=1
      GO TO 500
C
 220  CONTINUE
      IER=1
      GO TO 500
C
 230  CONTINUE
      IER=1
      GO TO 500
C
 240  CONTINUE
      IER=1
      GO TO 500
C
 250  CONTINUE
      IER=1
      GO TO 500
C
 260  CONTINUE
      IER=1
      GO TO 500
C
 270  CONTINUE
      IER=1
      GO TO 500
C
 280  CONTINUE
      IER=1
      GO TO 500
C
 290  CONTINUE
      IER=1
      GO TO 500
C
 500  CONTINUE
      epFUNALL = G
      END
 
