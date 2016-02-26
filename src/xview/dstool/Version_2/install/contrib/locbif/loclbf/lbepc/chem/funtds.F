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

      SUBROUTINE FUNTDEMO (T,X,F)
      IMPLICIT INTEGER*2 (I-N)
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION X(1),F(1)
      COMMON/PPAR/   P(1)
      COMMON/IERFS/  IERFS,IERDUM
C
         CALL RCHEMKIN(T,X,F)
         RETURN
C
      ENTRY USRFDEMO (IFN,T,X,FUNRES)
         CALL FCHEMKIN(IFN,T,X,FUNRES)
         RETURN
      END
C
      SUBROUTINE RCHEMKIN (T,X,F)
      IMPLICIT INTEGER*2 (I-N)
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION X(1),F(1)
      COMMON/PPAR/   P(1)
      COMMON/IERFS/  IERFS,IERDUM
        IERFS=0
C
c chemkin
c      z=1-x-y-s
c      x'=2*k1*z**2-2*km1*x**2-k3*x*y
c      y'=k2*z-km2*y-k3*x*y
c      z'=k4*(z-q*s)
        Z=1.D0-X(1)-X(2)-X(3)
        F(1)=2*P(1)*Z**2-2*P(5)*X(1)**2
     *       -P(3)*X(1)*X(2)
        F(2)=P(2)*Z-P(6)*X(2)-P(3)*X(1)*X(2)
        F(3)=P(4)*(Z-P(7)*X(3))
	MAXDIM=P(8)
	DO 10 I=4,MAXDIM
	   F(I)=-(1.0+0.01*I)*X(I)
 10     CONTINUE
      RETURN
C
      ENTRY FCHEMKIN (IFN,T,X,FRES)
        IERFS=0
        IF (IFN.EQ.0) THEN
           Z=1.D0-X(1)-X(2)-X(3)
           FRES=Z
        ENDIF
      RETURN
      END

