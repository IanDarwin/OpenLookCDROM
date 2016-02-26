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

c	INCLUDE 'FGRAPH.FI'
C
c---      INTERFACE TO INTEGER*2 FUNCTION system [C] (STRING[REFERENCE])
c---       IMPLICIT INTEGER*2 (I-N)
c---      CHARACTER*1 STRING
c---      END        

        SUBROUTINE Store (NCAN,PointType,Npt,N,M,L,
     *                    X,P,G,T,RR,RI,BifVal,IERR)
C
C       19.10.94
C
        IMPLICIT INTEGER*2(I-N)
          include 'complbf.dim'
 
c	INCLUDE 'FGRAPH.FD'
c---	INCLUDE 'KEYDEF.DAT'

        CHARACTER*56 TEXT
        CHARACTER*80 FSTOR,FSTOR1
c	INTEGER*2 hsim,wsim,maxx,maxy,maxcolor
        INTEGER*2 PointType,EVNT,SYSTEM
        LOGICAL*2 WAIT,KEYEVE
        REAL*8 X(1),P(1),G(1),T,RR(1),RI(1),BifVal(1)
c	COMMON/IX0IY0/ IX0,IY0
c	COMMON/VIDEO/  hsim,wsim,maxx,maxy,maxcolor
        COMMON/STNAME/ FSTOR,FSTOR1
        COMMON/NUMFLG/ NUMFLG

	external lbmem !$pragma C( lbmem )

c	RECORD /xycoord/ xy

        IERR=0
            WRITE (NCAN,*,ERR=10) PointType
	    WRITE (NCAN,*,ERR=10) Npt
	    WRITE (NCAN,*,ERR=10) (X(I),I=1,N)
            WRITE (NCAN,*,ERR=10) (P(I),I=1,M)
	    IF (L.GT.0) THEN
	       WRITE (NCAN,*,ERR=10) (G(I),I=1,L)
            ENDIF
	    WRITE (NCAN,*,ERR=10) T
            WRITE (NCAN,*,ERR=10) (RR(I),I=1,N)
            WRITE (NCAN,*,ERR=10) (RI(I),I=1,N)
	    WRITE (NCAN,*,ERR=10) (BifVal(I),I=1,NBFVL)

c       report point to the memory object

	call lbmem( X, N, P, M, T, PointType, Npt )

        RETURN

10      CONTINUE
c---              TEXT=
c---     *	' Store writing error                                    '
c---              CALL ShowT2(TEXT)
c---              WAIT=.TRUE.
c---              IF (KEYEVE(WAIT,EVNT)) CONTINUE
c---              IF (NCAN.EQ.7) THEN
c---                 FSTOR(76:76)=CHAR(0)
c---                 I=system ('del '//FSTOR(1:76))
c---                 OPEN (7,FILE=FSTOR ,ACCESS='APPEND',FORM='BINARY')
c---              ELSE
c---                 FSTOR1(76:76)=CHAR(0)
c---                 I=system ('del '//FSTOR1(1:76))
c---                 OPEN (8,FILE=FSTOR1,ACCESS='APPEND',FORM='BINARY')
c---              ENDIF
c---        NUMFLG=1
        IERR=1
        RETURN

        END

