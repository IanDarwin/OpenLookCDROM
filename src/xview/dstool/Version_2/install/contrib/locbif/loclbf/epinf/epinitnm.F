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

	SUBROUTINE epInitNM
C
C	15.11.92
C       10.07.94
C
        IMPLICIT INTEGER*2 (I-N)
	include 'epinf.dim'
        INTEGER*2      LineNum
C
        PARAMETER (LineNum=17)
	parameter (NSTNM=1+2*NFMAX)
C
	INTEGER*2      N,M,L,IT
C
	CHARACTER*6    NC(NRVC)
	CHARACTER*13   VC(NRVC),PC(NRPC),LIM(4)
        CHARACTER*6    STNAME(NSTNM)
        CHARACTER*20   LineName(LineNum)
	CHARACTER*20   LNTP(20)
C
	COMMON/NMLIT/  N,M,L,IT	
	COMMON/CHAR/   NC,VC,PC,LIM
	COMMON/LNTP/   LNTP
	COMMON/LNNM/   LNNM
C
	DATA LineName 
     *  /' Orbit             0',
     *   ' Curve             1',
     *   ' Equilibrium       1',
     *   ' Fold              2',
     *   ' Hopf              2',
     *   ' Double Eigenvalue 2',
     *   ' Double Zero       3',
     *   ' Fold + Hopf       3',
     *   ' Double Hopf       3',
     *   ' Cusp              3',
     *   ' Hopf+Lyapunov Zero3',
     *   ' Fold + Extr       3',
     *   ' Hopf + Extr       3',
     *   ' Double Zero+Cusp  4',
     *   ' Hopf + Cusp       4',
     *   ' Double Zero+Extr  4',
     *   ' Fold+Hopf+Extr    4'/
c	DATA STNAME/'time  ',
c     *   'Re(1) ','Re(2) ','Re(3) ','Re(4) ','Re(5) ','Re(6) ',
c     *   'Re(7) ','Re(8) ','Re(9) ','Re(10)',
c     *   'Im(1) ','Im(2) ','Im(3) ','Im(4) ','Im(5) ','Im(6) ',
c     *   'Im(7) ','Im(8) ','Im(9) ','Im(10)'/
C
	STNAME(1)='time  '
	DO 1 I=1,NFMAX
	WRITE (STNAME(I+1),101) I
 101    FORMAT ('Re(',I2,')')
 1      CONTINUE
	DO 2 I=1,NFMAX
	WRITE (STNAME(I+1+NFMAX),102) I
 102    FORMAT ('Im(',I2,')')
 2      CONTINUE
C
      LNNM=LineNum
C
C   Write names to the COMMON blocks
C
	DO 10 I=1,LineNum
           LNTP(I)=LineName(I)
 10     CONTINUE
	IF (IT.EQ.0) NC(N+M+L+1)=STNAME(1)
C
	DO 20 I=1,N
	   NC(1+I+N+M+L)=STNAME(I+1)
           NC(1+I+2*N+M+L)=STNAME(I+NFMAX+1)
 20     CONTINUE
	RETURN
C
	END	

