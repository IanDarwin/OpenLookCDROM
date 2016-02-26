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

C
c---      INCLUDE 'FGRAPH.FI'
      SUBROUTINE StoreI
      IMPLICIT INTEGER*2 (I-N)
C
c---      INCLUDE 'FGRAPH.FD'		
C
      INTEGER*2    NCAN
      CHARACTER*80 FSTOR,FSTOR1,cmdbuf
      INTEGER*4    narg
      INTEGER*2    status		

        COMMON/STNAME/ FSTOR,FSTOR1
C	
C   Read command line defining location of STORE files
C
c---      narg=NARGS()
c---      IF (narg.EQ.2) THEN	
c---	 CALL GETARG (narg-1,cmdbuf,status)
c---	 FSTOR=cmdbuf(1:status)//'store.dat'//cmdbuf(status+10:80)
c---	 FSTOR1=cmdbuf(1:status)//'store1.dat'//cmdbuf(status+11:80)
c---      ELSE
c---	 DO I=1,80
c---	    cmdbuf(I:I)=' '
c---	 ENDDO
c---	 FSTOR='store.dat'//cmdbuf(10:80)
c---	 FSTOR1='store1.dat'//cmdbuf(11:80)
c---      ENDIF
      RETURN
C
C   Open STORE files
C
      ENTRY StoreO(NCAN)
C
      IF (NCAN.EQ.7) THEN
c---         OPEN (NCAN,FILE=FSTOR ,ACCESS='APPEND',FORM='BINARY')
      ELSE
c---         OPEN (NCAN,FILE=FSTOR1,ACCESS='APPEND',FORM='BINARY')
      ENDIF
c***
      IF (NCAN.EQ.7) THEN
         OPEN (NCAN,FILE='storef.dat' ,FORM='FORMATTED')
      ELSE
         OPEN (NCAN,FILE='storef1.dat',FORM='FORMATTED')
      ENDIF
c***
      RETURN
C
C   Rewind STORE files
C
      ENTRY StoreR(NCAN)
C
      REWIND NCAN
      RETURN
C
C   Close STORE files
C
      ENTRY StoreC(NCAN)
C
      CLOSE (NCAN,STATUS='DELETE')
      RETURN
      END
