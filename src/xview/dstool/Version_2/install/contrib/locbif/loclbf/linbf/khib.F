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

      REAL*8 FUNCTION KHIB0(X)
      IMPLICIT REAL*8(A-H,O-Z)
      IMPLICIT INTEGER*2(I-N)
      COMMON /IABORT/ IABORT
        IABORT=X
        KHIB0=0.D0
      RETURN
      END
      REAL*8 FUNCTION KHIB1(X)
      IMPLICIT REAL*8(A-H,O-Z)
      IMPLICIT INTEGER*2(I-N)
      COMMON /ADRHS/ IADRHS,IFNINT
	IFNINT=X
        KHIB1=0.D0
      RETURN
      END
      REAL*8 FUNCTION KHIB2(X)
      IMPLICIT REAL*8(A-H,O-Z)
      IMPLICIT INTEGER*2(I-N)
        KHIB2=0.D0
      RETURN
      END
      REAL*8 FUNCTION KHIB3(X)
      IMPLICIT REAL*8(A-H,O-Z)
      IMPLICIT INTEGER*2(I-N)
        KHIB3=0.D0
      RETURN
      END
      REAL*8 FUNCTION KHIB4(X)
      IMPLICIT REAL*8(A-H,O-Z)
      IMPLICIT INTEGER*2(I-N)
        KHIB4=0.D0
      RETURN
      END
      REAL*8 FUNCTION KHIB5(X)
      IMPLICIT REAL*8(A-H,O-Z)
      IMPLICIT INTEGER*2(I-N)
        KHIB5=0.D0
      RETURN
      END
      REAL*8 FUNCTION KHIB6(X)
      IMPLICIT REAL*8(A-H,O-Z)
      IMPLICIT INTEGER*2(I-N)
        KHIB6=0.D0
      RETURN
      END
      REAL*8 FUNCTION KHIB7(X)
      IMPLICIT REAL*8(A-H,O-Z)
      IMPLICIT INTEGER*2(I-N)
        KHIB7=0.D0
      RETURN
      END
      REAL*8 FUNCTION KHIB8(X)
      IMPLICIT REAL*8(A-H,O-Z)
      IMPLICIT INTEGER*2(I-N)
        KHIB8=0.D0
      RETURN
      END
      REAL*8 FUNCTION KHIB9(X)
      IMPLICIT REAL*8(A-H,O-Z)
      IMPLICIT INTEGER*2(I-N)
        KHIB9=0.D0
      RETURN
      END
