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

        SUBROUTINE WrtCom
C
C       19.10.94
C
        IMPLICIT INTEGER*2 (I-N)
        IMPLICIT REAL*8 (A-H,O-Z)
          include 'complbf.dim'

        INTEGER*2 SOLDOT
        COMMON/REAL/ RVC(NRVC),RPC(NRPC),RLIM(4)
        COMMON/IORBIT/ IORBIT
        COMMON/SOLDOT/ SOLDOT
        COMMON/SOS/ ISOUND
        COMMON/MAXNPT/ MAXNPT
        COMMON/IFLASH/ IFLASH
        COMMON/MESSG/  MESSAG
        COMMON/INITV/  INIT
	COMMON/EPSH0/  EPSCRV,H0CRV
	COMMON/NP123/  NP1,NP2,NP3

	H0CRV =RPC(1)
	EPSCRV=RPC(8)
        IORBIT=RPC(NP1+11)
        SOLDOT=RPC(NP1+NP2+1)
        ISOUND=RPC(NP1+NP2+2)
        IFLASH=RPC(NP1+NP2+3)
        MESSAG=RPC(NP1+NP2+4)
        MAXNPT=RPC(NP1+NP2+5)
        INIT  =RPC(NP1+NP2+6)

        RETURN
        END
