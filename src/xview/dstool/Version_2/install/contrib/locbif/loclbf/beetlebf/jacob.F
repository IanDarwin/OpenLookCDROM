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

      SUBROUTINE JACOB (NX,NDIMM,X0,FUN,DFDX,DFUN,BMAS,IER,ISTOP)
C     3.05.90�.
C     13.11.92
C     15.06.94
C* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C*                                                                     *
C*       �PO�PAMMA JACOB �PE�HA�HA�EHA ��� B���C�EH�� MATP��� �ACTH�X  *
C*       �PO��BO�H�X. �PO��BO�H�E B���C���TC� ��C�EHHO ���             *
C*       AHA��T��ECK�, B �AB�C�MOCT� OT �E�AH�� �O���OBATE��.          *
C*                                                                     *
C*       �APAMETP�:                                                    *
C*       =========                                                     *
C*                                                                     *
C*  NX    -PA�MEPHOCT� �POCTPAHCTBA �EPEMEHH�X (X(1),...,X(NX)).       *
C*  NDIM  -�A�B�EHHA� CTPO�HA� PA�MEPHOCT� MATP��� DFUN.               *
C*  X0    -KOOP��HAT� TO�K�, B KOTOPO� B���C��ETC� �KO��AH.            *
C*  FUN   -�M� �O�PO�PAMM� ��HK���.                                    *
C*  DFDX  -�M� �PO�PAMM� B���C�EH�� MATP��� �ACTH�X �PO��BO�H�X.       *
C*  DFUN  -MACC�B B���C�EHH�X �ACTH�X �PO��BO�H�X, DFUN=DFUN(NFUN,NX). *
C*  BMAS  -HA BXO�E -MACC�B �HA�EH�� ��HK��� B TO�KE X0: BMAS =F(X0).  *
C*  IER   -�APAMETP O���K�:  IER = 0- B���C�EH� BCE �PO��BO�H�E;       *
C*                           IER < 0- �KO��AH HE B���C�EH.             *
C* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C*  �P� ��MEHEH�� MAKC�MA��HO�O ��C�A �EPEMEHH�X (NDIM) HEO�XO��MO     *
C*  ��MEH�T� PA�MEPHOCT� MACC�BA:  DMAS(NDIM-1).                       *
C* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER*2 (I-N)
      include 'beetlebf.dim'
      EXTERNAL FUN, DFDX
      DIMENSION X0(1), DFUN(NDIM,NX), BMAS(1), DMAS(NDIM1)
      COMMON /DERN1/ VARX
      NFUN  = NX - 1
      IER   = 0
      ISTOP = 0
C
C.... �OPM�P�EM MATP��� �EPB�X �PO��BO�H�X - DFUN.
C
      IF (VARX .LT. 0) GOTO 130
          DO 120 J = 1,NX
             IER = 0
             CALL DERN (NFUN,NX,J,X0,BMAS,DMAS,FUN,IER,ISTOP)
             IF (ISTOP .NE. 0) RETURN
             IF (IER   .EQ. 0) GOTO 104
                 IER = -4
                 RETURN
 104         CONTINUE
             DO 118 I = 1,NFUN
                DFUN(I,J) = DMAS(I)
 118         CONTINUE
 120      CONTINUE
          RETURN
 130  CONTINUE
      IER = 0
      CALL DFDX (X0,DFUN,NDIM,NX,IER,ISTOP)
      IF (ISTOP .NE. 0) RETURN
      IF (IER   .NE. 0) IER = -4
      RETURN
      END
