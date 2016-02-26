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


      SUBROUTINE Locbif
C
C     19.10.94
C
      IMPLICIT INTEGER*2 (I-N)
	include 'complbf.dim'
C
c      INCLUDE 'FGRAPH.FD'		
c      INCLUDE 'KEYDEF.DAT'	
c      INCLUDE 'LETDEF.DAT'

      INTEGER*2    NCANF,NCANB
      CHARACTER*7  NAMPAR(NRPC),NAMPAR1(NRPC)
      REAL*8       W(NRVC),DUMBFL(NBFVL)
      INTEGER*2    RDraw1(4),RDraw(4),RMsg(4),RLine(4)
      INTEGER*2    LINE0
      CHARACTER*56 TEXT 
      CHARACTER*80 FSTOR,FSTOR1
      INTEGER*2    IERFS,IERDUM,NCPAR,NP1,NP2,NP3
      INTEGER*2    EVENT,MAXLINE,PointType,Npt
      INTEGER*2    I,L,M,N,ColorType,FillColor,XYColor(16),IER
      INTEGER*2    IERR,IDIR
      LOGICAL*2    KEYEVE,DONE,WAIT,COMP,INST,STOP,AUTO
        INTEGER*2      Select,Data(5),SOS,SOLDOT
C
	INTEGER*2      J,POS,J0,POS0,K,ILine,NVar,ID(NRVC),LINE,IX,IY
	CHARACTER*20   LNTP(20)
	CHARACTER*6    NC(NRVC)
	CHARACTER*13   VC(NRVC),PC(NRPC),LIM(4)
	REAL*8         RVC(NRVC),RPC(NRPC),RLIM(4)
	LOGICAL*2      ACTIVE(NRVC),STATE,WRT
        INTEGER*2      KEYFLG,KEYCODE,NUMFLG
c---        INTEGER*2      hsim,wsim,maxx,maxy,maxcolor

	COMMON/NMLIT/  N,M,L,IT	
	COMMON/JPOS/   J,POS
	COMMON/INTEG/  K,ILine,NVar,ID,LINE,IX,IY
	COMMON/CHAR/   NC,VC,PC,LIM
	COMMON/REAL/   RVC,RPC,RLIM
	COMMON/LOGIC/  ACTIVE,STATE,INST
	COMMON/COLOR/  ColorType,FillColor
        COMMON/XYCOLO/ XYColor
        COMMON/DRAWL/  RDraw1,RDraw,RMsg,RLine
	COMMON/LNTP/   LNTP
        COMMON/LINE0/  LINE0
	COMMON/ERROR/  IER
	COMMON/COMP/   COMP,STOP,AUTO
	COMMON/WRT/    WRT
c---        COMMON/VIDEO/  hsim,wsim,maxx,maxy,maxcolor
	COMMON/IERFS/  IERFS,IERDUM
	COMMON/IX0IY0/ IX0,IY0
        COMMON/NUMFLG/ NUMFLG
        COMMON/KEYFLG/ KEYFLG,KEYCODE
        COMMON/PARNUM/ NCPAR
        COMMON/NAMPAR/ NAMPAR1
        COMMON/NP123/   NP1,NP2,NP3
        COMMON/STNAME/ FSTOR,FSTOR1
        COMMON/MAXLINE/MAXLINE
        COMMON/SOS/    SOS
        COMMON/SOLDOT/ SOLDOT
C 
 	DATA NAMPAR /
     *      'H0crv =', 'Hmxcrv=', 'Angcrv=', 'Dhcrv =', 'Dhjac =',     
     *      'Maxit =', 'Modit =', 'Epscrv=', 'Epscrs=', 'Epszer=',     
     *      'Epsext=', 'Iprsng=', 'Algcrv=',     
     *      'Itmap =', 'Tint  =', 'H0int =', 'Hmxint=', 'Dhint =',     
     *      'Epsint=', 'Epsrel=', 'Solver=', 'Isec  =', 'Irhs  =',     
     *      'Iorbit=',     

     *      'Join  =', 'Sound =', 'Flash =', 'Messag=', 'Maxnpt=',    
     *      'Init  =',    

     *      'Ivar  =', 'File  =', 8*'       '/
 
      DATA NCANF/7/, NCANB/8/


C   Open STORE files
 
        CALL StoreO(NCANF)
        CALL StoreO(NCANB)

C   Fill in point id array with standard order

        DO 11 I=1,NRVC
11         ID(I) = I

	CALL InitNM

        call lbdump

C   NP1, NP2 and NP3 must be initialized by now (mrm)

        NCPAR=NP1+NP2+NP3

        DO 10 I=1,NCPAR
 10        NAMPAR1(I)=NAMPAR(I)

        DO 20 I=1,NBFVL
 20        DUMBFL(I)=0.D0

        NUMFLG=0
        KEYFLG=0
 
C  Write Initial Point to STORE Files
 
        PointType=-1
        NCAN=NCANF
        Npt=1
        CALL Store (NCAN,PointType,Npt,N,M,L,
     *              RVC(1),RVC(N+1),RVC(N+M+1),RVC(N+M+L+1),
     *              RVC(N+M+L+2),RVC(2*N+M+L+2),DUMBFL,IERR)
        NCAN=NCANB
        Npt=-1
        CALL Store (NCAN,PointType,Npt,N,M,L,
     *              RVC(1),RVC(N+1),RVC(N+M+1),RVC(N+M+L+1),
     *              RVC(N+M+L+2),RVC(2*N+M+L+2),DUMBFL,IERR)


	NVar=3*N+M+L+1
 
        LINE0=LINE

C           CALL LineLo
        DO 30 I=1,NVar
 30        W(I)=RVC(I)


        WRT = .TRUE.           

 104    CONTINUE
	      IF (NUMFLG.EQ.1) THEN
                 PointType=-1
		 NCAN=NCANF
                 Npt=1
		 CALL StoreR (NCAN)
                 CALL Store (NCAN,PointType,Npt,N,M,L,
     *                       RVC(1),RVC(N+1),RVC(N+M+1),RVC(N+M+L+1),
     *                       RVC(N+M+L+2),RVC(2*N+M+L+2),DUMBFL,IERR)
		 NCAN=NCANB
                 Npt=-1
		 CALL StoreR (NCAN)
                 CALL Store (NCAN,PointType,Npt,N,M,L,
     *                       RVC(1),RVC(N+1),RVC(N+M+1),RVC(N+M+L+1),
     *                       RVC(N+M+L+2),RVC(2*N+M+L+2),DUMBFL,IERR)
		 NUMFLG=0
	      ENDIF
              CALL WrtCom

	      DO 40 I=1,NVar
 40 		 W(I)=RVC(I)

	      DO 50 I=1,L+2*N+1
 50              W(N+M+I)=0.0D0

	      IERDUM=1
              IF (WRT) CALL ProtO (N,M,L,LNTP(LINE),IERR)
              NCAN=NCANF
              IDIR=1
              CALL StoreC(NCAN)
              CALL StoreO(NCAN)
              CALL Comput (IDIR)

      CALL ProtC

      RETURN 
      END
 
