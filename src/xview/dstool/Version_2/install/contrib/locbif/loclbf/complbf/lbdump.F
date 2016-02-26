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


      SUBROUTINE lbdump
C
C       19.10.94
C
      IMPLICIT INTEGER*2 (I-N)
	include 'complbf.dim'

      INTEGER*2    NCANF,NCANB
      CHARACTER*7  NAMPAR(NRPC),NAMPAR1(NRPC)
      REAL*8       W(NRVC),DUMBFL(NBFVL)
      INTEGER*2    RDraw1(4),RDraw(4),RMsg(4),RLine(4)
      INTEGER*2    LINE0
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

      open(unit=15,file='DEBUG',status='unknown',
     *access='sequential',form='formatted')

      write(15,*) 'RVC :'
      write(15,*) rvc
      write(15,*) 'RPC :'
      write(15,*) rpc
      write(15,*) 'RLIM :'
      write(15,*) rlim
      write(15,*) 'IORBIT :'
      write(15,*) iorbit
      write(15,*) 'SOLDOT :'
      write(15,*) soldot
      write(15,*) 'ISOUND :'
      write(15,*) isound
      write(15,*) 'MAXNPT: '
      write(15,*) maxnpt
      write(15,*) 'IFLASH: '
      write(15,*) iflash
      write(15,*) 'MESSAG'
      write(15,*) messag
      write(15,*) 'INIT: '
      write(15,*) init
      write(15,*) 'EPSCRV: '
      write(15,*) epscrv
      write(15,*) 'H0CRV: '
      write(15,*) h0crv
      write(15,*) 'NP1, NP2, NP3: '
      write(15,*) np1,np2,np3
      write(15,*) 'N, M, L, IT: '
      write(15,*) n,m,l,it
      write(15,*) 'ACTIVE: '
      write(15,*) active
      write(15,*) 'LINE: '
      write(15,*) line
      write(15,*) 'IX, IY, K, ID: '
      write(15,*) ix,iy,k,id
      write(15,*) 'STATE: '
      write(15,*) state

      close(15)

      RETURN 
      END
 
