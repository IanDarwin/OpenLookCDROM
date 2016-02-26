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

      SUBROUTINE PRNTBF(NK,X)
C
C   pE~ATx TO~EK HA KPiByX lOKAlxHyX bifuPKAcij
C     ABTOP - A.i.XibHiK     BEPCiq OT 11.02.1983
C
      IMPLICIT REAL*8(A-H,O-Z)
      IMPLICIT INTEGER*2(I-N)
      DIMENSION X(NK)
      include 'linbf.dim'
      COMMON/PPAR/P(NPARMX)
      COMMON/IPPAR/IP(NPARMX)
      COMMON/LINBF/NF,NAP,ILN,NF2,ITP
C
C   pE~ATx O~EPEdHOj TO~Ki HA KPiBOj
C
      DO  10 I=1,NAP
        K=IP(I)
        P(K)=X(NF+I)
 10   CONTINUE
#if defined(hib)
      PRINT 901
      IF (NAP.EQ.1) PRINT 902,IP(1),X(NF+1),(X(I),I=1,NF)
      IF (NAP.EQ.2) PRINT 903,(IP(I),X(NF+I),I=1,2),(X(I),I=1,NF)
      IF (NAP.EQ.3) PRINT 904,(IP(I),X(NF+I),I=1,3),(X(I),I=1,NF)
      IF (NAP.EQ.4) PRINT 905,(IP(I),X(NF+I),I=1,4),(X(I),I=1,NF)
#endif
      RETURN
#if defined(hib)
 901  FORMAT(1X)
 902  FORMAT( 5X,'P(',I2,')=',G13.6,2X,'X =',6G14.6 /(29X,6G14.6))
 903  FORMAT( 5X,'P(',I2,')=',G13.6,2X,'P(',I2,')=',G13.6,2X,
     A  'X =',5G14.6 /(50X,5G14.6))
 904  FORMAT( 5X,'P(',I2,')=',G13.6,2X,'P(',I2,')=',G13.6,2X,
     A  'P(',I2,')=',G13.6,2X,'X =',4G14.6 /(71X,4G14.6))
 905  FORMAT( 5X,'P(',I2,')=',G13.6,2X,'P(',I2,')=',G13.6,2X,
     A  'P(',I2,')=',G13.6,2X,'P(',I2,')=',G13.6
     B  /82X,'X =',3G14.6/(85X,3G14.6))
#endif
      END
 
