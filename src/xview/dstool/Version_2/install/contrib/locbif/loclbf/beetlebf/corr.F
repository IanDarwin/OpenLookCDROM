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

      SUBROUTINE CORR (NX,NDIMM,NFIX1,NFIX,VMOVE,DMOVE,DFUN)
C     3.05.90ƒ.
C     13.11.92
C     15.06.94
C* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C*                                                                     *
C*    Subroutine CORR serves for:
C*       1) correction of 
C*       1) KOPPEK–ˆˆ KACATE‹œHOƒO BEKTOPA;                            *
C*       2) CMEH› MATPˆ–›, HEOXO„ˆMO‰ „‹Ÿ KOHTPO‹Ÿ TO—EK              *
C*          CAMOEPECE—EHˆŸ;                                           *
C*    POƒPAMMA CORR B›‡›BAETCŸ TO‹œKO TOƒ„A, KOƒ„A POˆCXO„ˆT ‡AMEHA  *
C*    ”ˆKCˆPOBAHHO‰ EPEMEHHO‰.                                        *
C*                                                                     *
C*    CˆCOK ”OPMA‹œH›X  A P A M E T P O B:                           *
C*                                                                     *
C*    NX   -PA‡MEPHOCTœ POCTPAHCTBA EPEMEHH›X;                       *
C*    NDIM -‡AŸB‹EHHAŸ CTPO—HAŸ PA‡MEPHOCTœ MATPˆ–› DFUN;              *
C*    NFIX1-CTAPOE ‡HA—EHˆE ”ˆKCˆPOBAHHO‰ EPEMEHHO‰;                  *
C*    NFIX -HOBOE ‡HA—EHˆE ”ˆKCˆPOBAHHO‰ EPEMEHHO‰;                   *
C*    VMOVE-HA BXO„E: KOOP„ˆHAT› KACATE‹œHOƒO BEKTOPA C VMOVE(NFIX1)>0;*
C*          HA B›XO„E:KOOP„ˆHAT› KACATE‹œHOƒO BEKTOPA C VMOVE(NFIX)>0; *
C*    DMOVE-˜Aƒ B„O‹œ KPˆBO‰: HA BXO„E ˆ HA B›XO„E ˆ‡ POƒPAMM›,       *
C*          BE‹ˆ—ˆHA  DMOVE*VMOVE(NFIX1) COXPAHŸET ‡HAK;               *
C*    DFUN -MATPˆ–A —ACTH›X POˆ‡BO„H›X.                               *
C*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*
C*                                                                     *
C*    B›‡›BAEM›E POƒPAMM›: DECOMP.                                    *
C*                                                                     *
C* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C*   Pˆ ˆ‡MEHEHˆˆ MAKCˆMA‹œHOƒO —ˆC‹A EPEMEHH›X  NDIM  HEOXO„ˆMO    *
C*   ˆ‡MEHˆTœ PA‡MEPHOCTˆ MACCˆBOB: A(*,*) IPVT(*) WORK(*)             *
C* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER*2 (I-N)
      include 'beetlebf.dim'
      DIMENSION VMOVE(NX), DFUN(NDIM,NX), IPVT(NDIM), WORK(NDIM)
      COMMON /CORR1/ DET, COND,SGNDET
      COMMON /LEQV1/ DETL, CONDL
C
      COMMON /WORKBE/ ARR1(NDIM,NDIM), A(NDIM,NDIM)
C
C.... KOPPEK–ˆŸ OPˆEHTA–ˆˆ KACATE‹œHOƒO BEKTOPA.
C
c      WRITE (6,*) ' CORR:  DET=',DET,'  DETL=',DETL
      IF (VMOVE(NFIX) .GT. 0.D0) GOTO 210
          DO 200 I=1,NX
             VMOVE(I) = -1.D0*VMOVE(I)
 200      CONTINUE
          DMOVE = -1.D0*DMOVE
 210  CONTINUE
C
C.... KOPPEK–ˆŸ MATPˆ–› —ACTH›X POˆ‡BO„H›X.
C
      DFUN(NX,NFIX1) = 0.0D0
      DFUN(NX,NFIX)  = 1.0D0
C
C.... O„ƒOTOBKA PAO—E‰ MATPˆ–› A „‹Ÿ B›—ˆC‹EHˆŸ OPE„E‹ˆTE‹Ÿ.
C
      DO 230 I=1,NX
         DO 220 J=1,NX
            A(I,J) = DFUN(I,J)
 220     CONTINUE
 230  CONTINUE
C
C.... B›—ˆC‹EHˆE OPE„E‹ˆTE‹Ÿ (DET), COOTBETCTB“ž™EƒO HOBO‰
C     ”ˆKCˆPOBAHHO‰ EPEMEHHO‰  NFIX.
C
      CALL DECOMP (NDIM,NX,A,COND,IPVT,WORK)
      DET = IPVT(NX)
      DO 310 I=1,NX
         DET = DET*A(I,I)
 310  CONTINUE
c      WRITE (6,*) ' CORR:  DET=',DET,'  DETL=',DETL
      RETURN
      END
