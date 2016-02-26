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

C set demo=0 for working version and set demo=1 for demo version
C                                               Levitin 15.11.90
c$define demo=1
C
C

C Compute RHS values
C  
C
      SUBROUTINE FUNT (T,X,F)
C
C     15.11.92
C
      IMPLICIT REAL*8 (A-H,O-Z)
        IMPLICIT INTEGER*2 (I-N)
	include 'complbf.dim'

c---      PARAMETER (AnyMax=10)
      DIMENSION X(1), F(1)
C
      INTEGER*2 Dummy, NW, NF, TW
      INTEGER*2 RetCD(2), Er(4)
C
C
      COMMON/NMLIT/    N,M,L,IT	
      INTEGER*2    L,M,N,IT
C
      COMMON /PPAR/ P
      DIMENSION P(NPARMX)
C
      DIMENSION XX(0:NFMAX), FF(NFMAX), PP(NPARMX),
     *          SF(NFMAX) , SX(NFMAX)
      INTEGER*2    LL,MM,NN,NN1
      INTEGER*2    I
      INTEGER*2    IERFS,IERDUM,irhs
      INTEGER*2    MESSAG
      INTEGER*4    NFUNS
      COMMON/IERFS/  IERFS,IERDUM
      COMMON /IABORT/ IABORT
      COMMON/NFUNS/  NFUNS
      COMMON/MESSG/ MESSAG
C
      INTEGER*2 IFN,IERR,iifn,iierr
      REAL*8 FUNRES,USRFUN,PCOORD(0:NFMAX)
      REAL*8 LARGVL
C
C
C error handler's data
C
      INTEGER*2 what,where,sw,op
C
      DATA LARGVL/1.D10/
C
C**********************************************
      IERFS=0
      CALL TERMNT
      IF (IERFS.NE.0) RETURN
      er(1)=0
C**********************************************
C
      CALL FUNTDEMO (T,X,F)
C***B
      IF (MESSAG.LE.-11) THEN
         IF (IERFS.NE.0) PRINT 1102,what,irhs,IERFS,NFUNS
      ENDIF
C***E
      RETURN
C***B
 100  CONTINUE
      IERFS=100
      IABORT=1
      IF (MESSAG.LE.-11) THEN
         PRINT 1011,er(1),T,NFUNS,irhs
         PRINT 1012,(X(I),I=1,N)
         PRINT 1013,(P(I),I=1,M)
         IF (IERFS.NE.0) PRINT 1103,what,irhs,IERFS,NFUNS
      ENDIF
 150  CONTINUE
      DO 10 I=1,N
         F(I)=0.0D0
 10   CONTINUE
C
 1011 FORMAT(/'     FUNT:  er(1)=',I3,'  T=',G11.4,'  NFUNS=',I8,
     *       '  irhs=',I3)
 1012 FORMAT( '     FUNT:  X=',/('             ',3G23.15))  
 1013 FORMAT( '     FUNT:  P=',/('             ',3G23.15))  
 1101 FORMAT(/'  1:  what=',I3,'  irhs=',I3,'  IERFS=',I3,'  NFUNS=',I8)
 1102 FORMAT(/'  2:  what=',I3,'  irhs=',I3,'  IERFS=',I3,'  NFUNS=',I8)
 1103 FORMAT(/'  3:  what=',I3,'  irhs=',I3,'  IERFS=',I3,'  NFUNS=',I8)
C
      RETURN
C***E
C
C Compute user functions
C
      ENTRY USERFUN(IFN,IERR,PCOORD,FUNRES)
C**********************************************
      IERFS=0
c      CALL TERMNT
c      IF (IERFS.NE.0) RETURN
      er(1)=0
C**********************************************
C
      TT=PCOORD(0)
      DO 20 I=1,N
         SX(I)=PCOORD(I)
 20   CONTINUE
      CALL USRFDEMO (IFN,TT,SX,FUNRES)
      IF (IFN .EQ. 10) THEN
        PCOORD(0)=TT
        DO 30 I=1,N
           PCOORD(I)=SX(I)
 30     CONTINUE
      ENDIF

C***B
      IF (MESSAG.LE.-11) THEN
      IF (IERFS.NE.0) PRINT 1112,what,irhs,IERFS,NFUNS
      ENDIF
C***E
      RETURN

C***B
 200  CONTINUE
      IERFS=100
      IABORT=1
      IF (MESSAG.LE.-11) THEN
         PRINT 1021,er(1),iierr,IFN,FUNRES,TT
         PRINT 1022,(PCOORD(I),I=1,N)
         PRINT 1023,(P(I),I=1,M)
         IF (IERFS.NE.0) PRINT 1113,what,irhs,IERFS,NFUNS
      ENDIF
C
 1021 FORMAT(/'     USRF:  er(1)=',I3,'  iierr=',I3,'  IFN=',I3,
     *       /'            FUNRES=',G11.4,'  T=',G11.4)
 1022 FORMAT( '     USRF:  PCOORD=',/('             ',3G23.15))  
 1023 FORMAT( '     USRF:  P=',/('             ',3G23.15))  
 1111 FORMAT(/' USRF 1:  what=',I3,
     *       '  irhs=',I3,'  IERFS=',I3,'  NFUNS=',I8)
 1112 FORMAT(/' USRF 2:  what=',I3,
     *       '  irhs=',I3,'  IERFS=',I3,'  NFUNS=',I8)
 1113 FORMAT(/' USRF 3:  what=',I3,
     *       '  irhs=',I3,'  IERFS=',I3,'  NFUNS=',I8)
C
      RETURN
C***E
      END
