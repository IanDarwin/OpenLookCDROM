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

      SUBROUTINE fpMODJAC(NK,X,D,IER)
C
C   MODIFICATION OF THE JACOBY MATRIX TO STUDY LIMIT CYCLE BIFURCATIONS
C        20.02.91
C        10.07.94
C
C     DUMMY SUBROUTINE
C
C   FORMAL PARAMETERS:
C    NK     - DIMENSION OF THE CONTINUATION PROBLEM
C    X      - VECTOR OF THE STATE VARIABLES (LENGTH NK)
C    D      - REAL ARRAY OF THE LENGTH NF*NF 
C    IER    - ERROR CONDITION
C                 0 - NORMAL VALUE
C
      IMPLICIT REAL*8(A-H,O-Z)
      IMPLICIT INTEGER*2(I-N)
      DIMENSION X(NK),D(1)
      IER=0
      RETURN
      END
 
