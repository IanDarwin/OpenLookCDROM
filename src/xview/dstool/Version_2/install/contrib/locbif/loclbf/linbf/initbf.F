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

      SUBROUTINE INITBF
C
C       28.11.1991
C       14.11.92
C
      IMPLICIT REAL*8(A-H,O-Z)
      IMPLICIT INTEGER*2(I-N)
      include 'linbf.dim'
      COMMON /LINBF1/ DHJAC
      COMMON /LINBF3/ EPSCRV,H0CRV
      COMMON /ITMAP/  ITMAP
      COMMON /ADD1B/  DHCRV,HMNCRV,HMXCRV
      COMMON /ADD2A/  ICRS,IPRCRS
      COMMON /ADD2B/  EPSCRS
      COMMON /ADD3A/  NUF,IPRZER,IFUNS(IFMAX)
      COMMON /ADD3B/  EPSZER
      COMMON /ADD4A/  NUE,IPREXT,IVARS(NDIM)
      COMMON /ADD4B/  EPSEXT
      COMMON /ADD7A/  EPSNWT
      COMMON /ADD7B/  ITNWT
      COMMON /NMLIT/  N,M,L,IT
      COMMON /REAL/   RVC(NRVC),RPC(NRPC),RLIM(4)
      COMMON /INTGR/  TINT,H0INT,EPSAI,EPSRI,DHINT,HMXINT,HMNINT,
     *                SOLVER,RMXSTP
      COMMON /IORBIT/ IORBIT
      COMMON /ADRHS/  IADRHS,IFNINT
      COMMON /CDOP5/  RELTOL,RMETH,ALG
      COMMON /CONTRH/ CDEMP,RMODE,ANGMAX,ANGMIN,ANGCRV
      COMMON /WRTO6/  ITCMAX,IT2
      COMMON /ISEC/   ISEC
      COMMON /INIFVL/ IFVL
      COMMON /NP123/  NP1,NP2,NP3
      COMMON /IPSNG/  IPSNG1,IPSNG2
      COMMON /IHOMP/  IHOMP
      CHARACTER*6     NC(NRVC)
      CHARACTER*13   VC(NRVC),PC(NRPC),LIM(4)
      COMMON /CHAR  / NC,VC,PC,LIM
C
c      SAVE   /LINBF1/, /LINBF3/, /ITMAP/, /ADD1B/, /ADD2A/
c      SAVE   /ADD2B/, /ADD3A/, /ADD3B/, /ADD4A/, /ADD4B/
c      SAVE   /ADD7A/, /ADD7B/, /CONTRH/, /WRTO6/
c      SAVE   /INTGR/, /IORBIT/, /CDOP5/, /ISEC/
C
C
c 	DATA NAMPAR /
c     *                  'H0crv =',      ! 1
c     *                  'Hmxcrv=',      ! 2
c     *                  'Angcrv=',      ! 3
c     *                  'Dhcrv =',      ! 4
c     *                  'Dhjac =',      ! 5
c     *                  'Maxit =',      ! 6
c     *                  'Modit =',      ! 7
c     *                  'Epscrv=',      ! 8
c     *                  'Epscrs=',      ! 9
c     *                  'Epszer=',      ! 10
c     *                  'Epsext=',      ! 11
c     *                  'Iprsng=',      ! 12
c     *                  'Algcrv=',      ! 13            NP1=13
c
c     *                  'Itmap =',      ! 1     14
c     *                  'Tint  =',      ! 2     15
c     *                  'H0int =',      ! 3     16
c     *                  'Hmxint=',      ! 4     17
c     *                  'Dhint =',      ! 5     18
c     *                  'Epsint=',      ! 6     19
c     *                  'Epsrel=',      ! 7     20
c     *                  'Solver=',      ! 8     21      
c     *                  'Isec  =',      ! 9     22
c     *                  'Irhs  =',      ! 10    23     
c     *                  'Iorbit=',      ! 11    24	NP2=11
c
c     *                  'Join  =',      ! 1     25
c     *                  'Sound =',      ! 2     26
c     *                  'Flash =',      ! 3     27
c     *                  'Messag=',      ! 4     28
c     *                  'Maxnpt=',      ! 5     29      
c     *                  'Init  =',      ! 6     30      NP3=6
c
c     *                  'Ivar  =',      !       31
c     *                  'File  =',      !       32
c     *                  '       ',      !       33
c     *                  '       ',      !       34
c     *                  '       ',      !       35
c     *                  '       ',      !       36
c     *                  '       ',      !       37
c     *                  '       ',      !       38
c     *                  '       ',      !       39
c     *                  '       '       !       40
c     *             /

c      H0CRV  = RPC(1)
      HMXCRV = RPC(2)
      ANGCRV = RPC(3)
      DHCRV  = RPC(4)
      DHJAC  = RPC(5)
      ITCMAX = RPC(6)
        ITCMAX=MAX(ITCMAX,5)
      ITNWT  = RPC(7)
      EPSCRV = RPC(8)
      EPSCRS = DMAX1(RPC(9),1.D-10)
        IF (RPC(9).LE.0.D0) ICRS=0
      EPSZER = DMAX1(RPC(10),1.D-10)
        IF (RPC(10).LE.0.D0) NUF=0
      EPSEXT = DMAX1(RPC(11),1.D-10)
        IF (RPC(11).LE.0.D0) NUE=0
      K1     = RPC(12)/10.
      K2     = RPC(12)-K1*10
        IF (K1.LE.0) THEN
           IF (1.LE.K2 .AND.K2.LE.10) IPSNG1=K2
        ELSE
           IF (1.LE.K1 .AND.K1.LE.10) IPSNG1=K1
           IF (1.LE.K2 .AND.K2.LE.10) IPSNG2=K2
        ENDIF
c      IPSNG1 = RPC(12)
      RMODE  = RPC(13)
C
      ITMAP  = RPC(NP1+1)
      TINT   = RPC(NP1+2)
        IF (DABS(TINT-6.28).LE.0.01) TINT=8.D0*DATAN(1.D0)
      H0INT  = RPC(NP1+3)
      HMXINT = RPC(NP1+4)
      DHINT  = RPC(NP1+5)
      EPSAI  = RPC(NP1+6)
      EPSRI  = RPC(NP1+7)
        IF (EPSRI.EQ.0.D0) EPSRI = 1.D-3*EPSAI
      SOLVER = RPC(NP1+8)
      ISEC   = RPC(NP1+9)
        IF (ISEC.GT.N) ISEC=0
      ALG    = RPC(NP1+10)
      IORBIT = RPC(NP1+11)
      IFVLTM   = RPC(NP1+NP2+6)
	IF (IFVLTM .EQ. -1) IFVL=1
C
      HMNCRV = 0.01D0*EPSCRV
      ANGMAX = 2.D0*ANGCRV
      ANGMIN = 0.6D0*ANGCRV
      EPSNWT = 2.D0*EPSCRV
      CDEMP  = 0.1
      RMXSTP = 10000
      IADRHS = 0
      IHOMP  = 0
      DO 10 I=1,NPARMX
	 IF (NC(N+I).EQ.'HOMPAR') IHOMP=I
 10   CONTINUE
      RETURN
      END
