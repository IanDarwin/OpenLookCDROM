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

      SUBROUTINE RZLTNT(N,M,A,B,C,REZ)
C
C   By~iClEHiE PEzulxTAHTA  MHOgO~lEHOB A(X) CTEpEHi N
C                                     i B(X) CTEpEHi M
C     ABTOP - A.i.XibHiK    BEPCiq OT 07.02.1983
C
      IMPLICIT REAL*8(A-H,O-Z)
      IMPLICIT INTEGER*2(I-N)
      DIMENSION A(1),B(1),C(1)
      NM=N+M
      NM2=NM*NM
      N1=N+1
      M1=M+1
      DO 1 I=1,NM2
 1    C(I)=0.
      L=0
      DO 20 I=1,M
        DO 10 J=1,N1
          L=L+1
          C(L)=A(J)
 10     CONTINUE
        L=L+M
 20   CONTINUE
      L=M*(N+M)
      DO 40 I=1,N
        DO 30 J=1,M1
          L=L+1
          C(L)=B(J)
 30     CONTINUE
        L=L+N
 40   CONTINUE
      CALL DETBF(C,NM,REZ)
      RETURN
      END
 
