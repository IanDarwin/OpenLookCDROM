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

	SUBROUTINE psInitNM
C
C       16.11.92
C       10.07.94
C
        IMPLICIT INTEGER*2 (I-N)
	include 'psinf.dim'
        INTEGER*2      LineNum
C
        PARAMETER (LineNum=16)
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
     *   ' Periodic solution 1',
     *   ' Fold              2',
     *   ' Hopf              2',
     *   ' Flip              2',
     *   ' Double Multiplier 2',
     *   ' Double Fold       3',
     *   ' Double Flip       3',
     *   ' Fold + Hopf       3',
     *   ' Flip + Hopf       3',
     *   ' Fold + Flip       3',
     *   ' Cusp              3',
     *   ' Fold + Extr       3',
     *   ' Hopf + Extr       3',
     *   ' Flip + Extr       3'/
C	DATA STNAME/' time ',
C     *   'Mod(1)','Mod(2)','Mod(3)','Mod(4)','Mod(5)','Mod(6)',
C     *   'Mod(7)','Mod(8)','Mod(9)','Mod(0)',
C     *   'Arg(1)','Arg(2)','Arg(3)','Arg(4)','Arg(5)','Arg(6)',
C     *   'Arg(7)','Arg(8)','Arg(9)','Arg(0)'/
C
       STNAME(1)='time  '
       DO 1 I=1,NFMAX
       WRITE (STNAME(I+1),101) I
 101   FORMAT ('Md(',I2,')')
 1     CONTINUE
       DO 2 I=1,NFMAX
       WRITE (STNAME(I+1+NFMAX),102) I
 102    FORMAT ('Ar(',I2,')')
 2      CONTINUE

      LNNM=LineNum
C
C   Write names to the COMMON blocks
C
	DO I=1,LineNum
           LNTP(I)=LineName(I)
        END DO
	IF (IT.EQ.0) NC(N+M+L+1)=STNAME(1)
C
	DO I=1,N
	   NC(1+I+N+M+L)=STNAME(I+1)
           NC(1+I+2*N+M+L)=STNAME(I+NFMAX+1)
        END DO
	RETURN
C
	END	

