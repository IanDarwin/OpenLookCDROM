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

      SUBROUTINE psLINLBF(IKR,N,X0,NPAR,P0,PMIN,PMAX,NAP1,IAP,
     *                  H0,E,IEND)
C
C-------------------------------------------------------------------
C   A PACKAGE FOR CONTINUATION AND BIFURCATION ANALYSIS
C   OF FIXED POINTS AND PERIODIC ORBITS OF ITERATED MAPS
C                X'=F(X,P)
C
C   AUTHORS:  A.I.KHIBNIK AND E.V.NIKOLAEV
C  
C             INSTITUTE OF MATHEMATICAL PROBLEMS IN BIOLOGY
C               (former RESEARCH COMPUTING CENTRE)
C             RUSSIAN ACADEMY OF SCIENCES
C             PUSHCHINO MOSCOW REGION
C             142292 RUSSIA
C
C   VERSION OF  19 DECEMBER 1990
C
C     (LAST REVISION 08.08.1991, 16.11.92, 10.07.94)
C-------------------------------------------------------------------
C
C   SHORT DESCRIPTION.
C
C   A. INPUT PARAMETERS.
C      ----------------
C 
C   IKR    - THE CODE OF THE CURVE (SEE BELOW)
C   N      - DIMENSION OF THE SYSTEM (N.LE.10)
C   X0     - INITIAL VALUES OF STATE VARIABLES
C            (VECTOR OF THE LENGTH N)                
C   NPAR   - NUMBER OF CONTROL PARAMETERS (NPAR.LE.10)
C   P0     - INITIAL VALUES OF THE CONTROL PARAMETERS
C            (VECTOR OF THE LENGTH NPAR)  
C   PMIN   - LOWER LIMITS OF THE CONTROL PARAMETERS
C            (VECTOR OF THE LENGTH NPAR)
C   PMAX   - UPPER LIMITS OF THE CONTROL PARAMETERS
C            (VECTOR OF THE LENGTH NPAR)
C   NAP1   - NUMBER OF ACTIVE (FREE) PARAMETERS (NAP1.LE.4)
C   IAP    - INDICES OF THE ACTIVE PARAMETERS
C            (INTEGER VECTOR OF THE LENGTH NAP1)
C   H0     - INITIAL STEP SIZE GUESS
C   E      - ACCURACY OF CURVE COMPUTATION
C
C   B. OUTPUT PARAMETERS.
C      -----------------
C
C   IEND   - REPORTS ON SUCCESSFULNESS AND FINAL STATE UPON RETURN
C
C            IEND>0 - COMPUTATION SUCCESSFUL:
C             1  -  GIVEN NUMBER OF STEPS DONE,
C             2  -  BOUNDARY VALUE OF A CONTROL PARAMETER REACHED,
C             3  -  CLOSED CURVE FOUND,
C             4  -  THE SAME AS IEND=2, BUT WITHOUT PRECISE LOCATION
C                   OF THE BOUNDARY POINT.              
C
C            IEND<0 -  COMPUTATION UNSUCCESSFUL: 
C            -1  -  ERROR IN INPUT DATA,
C            -2  -  CAN'T FIND FIRST POINT ON A CURVE
C            -3  -  CURRENT STEP SIZE LESS THEN MINIMAL
C            -4  -  CAN'T FIND TANGENT VECTOR AT THE FIRST POINT
C            -5  -  COMPUTATIONS INTERRUPTED IN USER'S SUBROUTINE
C            -6  -  NOT USED IN THIS VERSION
C            -7  -  CAN'T COMPUTE RHS VALUES AT INITIAL POINT
C            -8  -  CAN'T FIND JACOBY MATRIX AT INITIAL POINT 
C            -9  -  CAN'T FIND TANGENT VECTOR AT INITIAL POINT
C
C   C. USER-SUPPLIED SUBROUTINES.
C      -------------------------
C
C   1. SUBROUTINE TO COMPUTE RIGHT HAND SIDES OF THE SYSTEM
C
C                SUBROUTINE FUNM(X,F)
C                REAL*8 X(1),F(1),P(1)
C                INTEGER*2 IERFS
C                COMMON /PPAR/  P
C                COMMON /IERFS/ IERFS
C                F(1)=...
C                 
C                F(N)=...
C                IERFS=...
C                RETURN
C                END
C
C    HERE:
C           X - VECTOR OF STATE VARIABLES,
C           P - VECTOR OF CONTROL PARAMETERS,
C           F - VECTOR OF COMPUTED RHS VALUES.
C    IERFS=0 INDICATE THAT RHS COMPUTED SUCCESSFULLY, 
C            OTHERWISE IERFS SHOULD BE ASSIGNED TO NONZERO VALUE.
C                 -----------------
C
C   2. SUBROUTINE TO SET NEW VALUES OF COMPUTATIONAL PARAMETERS
C       INSTEAD OF DEFAULT ONES (OPTIONAL)
C 
C                SUBROUTINE INITBF
C                IMPLICIT REAL*8 (A-H,O-Z)
C                IMPLICIT INTEGER*2 (I-N)
C                COMMON ...
C
C                RETURN
C                END
C
C       SEE COMMENTS IN THE TEXT OF INITBF.
C                 -----------------
C
C   3. SUBROUTINES TO ORGANIZE USER'S OUTPUT 
C-----------------------------------------------------------------
C       20.09.1990
C
      IMPLICIT REAL*8(A-H,O-Z)
      IMPLICIT INTEGER*2(I-N)
      include 'psbf.dim'
      parameter (NDFUN=NDIM*(NDIM-1), NLP=8*(NFMAX-2)+13)
      parameter (NFMAXd=2*NFMAX)
      INTEGER*4 NFUNS
      DIMENSION X0(N),P0(NPAR),PMIN(NPAR),PMAX(NPAR),IAP(1)
      DIMENSION IADD(7)
      DIMENSION X(NDIM),F(NDIM),DFUN(NDFUN),IROW(NDIM),ICOL(NDIM)
      EXTERNAL FUNBF,DFUNBF,OUTLBF
      COMMON/WORK6/ WORK(NFMAXd,NLP)
      COMMON/PPAR/P(NPARMX)
      COMMON/IPPAR/IP(NPARMX)
      COMMON/LINBF/NF,NAP,ILN,NF2,ITP
      COMMON/LINBF1/DHF
      COMMON/LINBF2/D(NFMAX2)
      COMMON/LINBF3/EPS,HV0
      COMMON/LINBFC/AL(NPARMX),AR(NPARMX)
      COMMON/LINBFE/RR(NFMAX),RI(NFMAX),IPR
      COMMON/LINBEE/RMOD(NFMAX),ARG(NFMAX)
      COMMON/LINBFN/IOLD
      COMMON/LINBFO/INDFUN
      COMMON/LINBFP/IGRAN
      COMMON/QUAVEC/VEC1(NFMAX),VEC2(NFMAX),VEC3(NFMAX)
      COMMON/ITMAP/ITMAP
      COMMON/ADD1A/IPRB,NSTEP,NCAN
      COMMON/ADD1B/DX,HVMN,HVMX
      COMMON/ADD2A/ICRS,IPRCRS
      COMMON/ADD2B/EPSCRS
      COMMON/ADD3A/NUF,IPRZER,IFUNS(IFMAX)
      COMMON/ADD3B/EPSZER
      COMMON/ADD4A/NUE,IPREXT,IVARS(NDIM)
      COMMON/ADD4B/EPSEXT
      COMMON/SCALEB/SCX(NDIM)
      COMMON/NFUNS/NFUNS
      COMMON/NJAC/NJAC,NJACN,NJACB
      COMMON/IPSNG/ IPSNG1,IPSNG2
      COMMON/IERFS/IERFS,IERDUM
      COMMON/INTOUT/ IOUT,IDIREC
      COMMON/FUNVAL/FVAL(NDIM)
      COMMON/INIFVL/IFVL
      COMMON/INTGR/TINT,H0INT,EPSAI,EPSRI,DHINT,HMXINT,HMNINT,
     *             SOLVER,RMXSTP
      DATA IADD/1,1,1,1,1,0,1/
C      IKR:       -1 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 
C                   18 19 20 21 22 23 24
c      DATA ITYPE / 1,2,3,4,0,5,0,0,0,0,0, 0, 6, 7, 8, 9,10,11,12,
c     *               0, 0, 0,13,14,15,16/
C     
c      DATA ICFN /
c     *            1,5,
C
C   SPECIFYING INTERNAL PARAMETERS
C
      NFUNS=0
      NJAC=0
      NJACN=0
      NJACB=0
      IER=0
      ISTOP=0
      IERFS=0
      IPSNG1=1
      IPSNG2=2
      IOUT=0
      IDIREC=1
      NF=N
      NF2=NF*NF
      NAP=NAP1
      ILN=IKR
c      ITP=ITYPE(IKR+2)
      DHF=1.D-7
      NK=NF+NAP
      NK1=NK-1
      NG=N+1
c      XGMN=PMIN(IAP(1))
c      XGMX=PMAX(IAP(1))
      EPS=E
      HV0=H0
C      NSTEP=500
      DX= -1.D-7
      IOLD=0
      INDFUN=0
      IGRAN=0
      IFVL=0
      ITMAP=1
C
C----------------------------------------------
C      IPRB=-2
C      HVMN=1.D-10
C      HVMX=100.*DABS(H0)
C      ICRS=0
C      IPRCRS=-1
C      EPSCRS=EPS
C      NUE=NAP
C      IPREXT=-1
C      EPSEXT=EPS
C      DO 10 I=1,NAP
C        IVARS(I)=NF+I
C 10   CONTINUE
C      IPRZER=-1
C      EPSZER=EPS
C----------------------------------------------
C
      IEND=-6
c      SELECT CASE (ILN)
c         CASE (-1,0)
	  IF (ILN.EQ.-1 .OR. ILN.EQ.0) THEN
            IF (NAP.NE.1) RETURN
          ENDIF
c         CASE (1,2,3,4)
	  IF (     ILN.EQ.1 
     *        .OR. ILN.EQ.2
     *	      .OR. ILN.EQ.3 
     *        .OR. ILN.EQ.4) THEN
            IF (NAP.NE.2) RETURN
	  ENDIF
c         CASE (11,12,13,14,15,16,17,18,19)
	  IF (     ILN.EQ.11 
     *        .OR. ILN.EQ.12
     *	      .OR. ILN.EQ.13 
     *        .OR. ILN.EQ.14
     *        .OR. ILN.EQ.15
     *	      .OR. ILN.EQ.16 
     *	      .OR. ILN.EQ.17 
     *	      .OR. ILN.EQ.18 
     *        .OR. ILN.EQ.19) THEN
            IF (NAP.NE.3) RETURN
          ENDIF
c         CASE DEFAULT
c           RETURN
c      END SELECT   
      IEND=0
C
      NUF=0
C
      IF (ILN.NE. 0) GO TO 11 
         NUF=4
         IFUNS(1)=1
         IFUNS(2)=2
         IFUNS(3)=3
         IFUNS(4)=4
         GO TO 100
C
 11   IF (ILN.NE. 1) GO TO 12
         NUF=4
         IFUNS(1)=2
         IFUNS(2)=3
         IFUNS(3)=5
         IFUNS(4)=4
         GO TO 100
C
 12   IF (ILN.NE. 2) GO TO 13
         NUF=3
         IFUNS(1)=1
         IFUNS(2)=3
         IFUNS(3)=4
         GO TO 100
C
 13   IF (ILN.NE. 3) GO TO 14
         NUF=3
         IFUNS(1)=1
         IFUNS(2)=2
         IFUNS(3)=4
         GO TO 100
C
 14   IF (ILN.NE. 4) GO TO 21
         NUF=3
         IFUNS(1)=1
         IFUNS(2)=2
         IFUNS(3)=3
         GO TO 100
C
 21   IF (ILN.NE.11) GO TO 22
         NUF=2
         IFUNS(1)=5
         IFUNS(2)=3
         GO TO 100
C
 22   IF (ILN.NE.12) GO TO 23
         NUF=1
         IFUNS(1)=1
         GO TO 100
C
 23   IF (ILN.NE.13) GO TO 24
         NUF=2
         IFUNS(1)=3
         IFUNS(2)=5
         GO TO 100
C
 24   IF (ILN.NE.14) GO TO 25
         NUF=1
         IFUNS(1)=1
         GO TO 100
C
 25   IF (ILN.NE.15) GO TO 26
         NUF=2
         IFUNS(1)=2
         IFUNS(2)=5
         GO TO 100
C
 26   IF (ILN.NE.16) GO TO 27
         NUF=2
         IFUNS(1)=2
         IFUNS(2)=3
         GO TO 100
C
 27   IF (ILN.NE.17) GO TO 28
         NUF=4
         IFUNS(1)=2
         IFUNS(2)=3
         IFUNS(3)=5
         IFUNS(4)=4
         GO TO 100
C
 28   IF (ILN.NE.18) GO TO 29
         NUF=3
         IFUNS(1)=1
         IFUNS(2)=3
         IFUNS(3)=4
         GO TO 100
C
 29   IF (ILN.NE.19) GO TO 100
         NUF=3
         IFUNS(1)=1
         IFUNS(2)=2
         IFUNS(3)=4
         GO TO 100
C
 100  CONTINUE
      DO 110 I=1,NPAR
        P(I)=P0(I)
        AL(I)=PMIN(I)
        AR(I)=PMAX(I)
 110  CONTINUE
      DO 120 I=1,NF
        X(I)=X0(I)
        RR(I)=0.D0
        RI(I)=0.D0
        RMOD(I)=0.D0
        ARG(I)=0.D0
 120  CONTINUE
      DO 130 I=1,NAP
        K=IAP(I)
        IP(I)=K
        X(NF+I)=P0(K)
 130  CONTINUE
      VEC3(1)=1.D0
      DO 140 I=2,NF
        VEC3(I)=0.D0
 140  CONTINUE
      DO 145 I=1,NK
        SCX(I)=1.D0
	FVAL(I)=0.D0
 145  CONTINUE
C
C   PRINTING INPUT DATA
C
#if defined(hib)
      PRINT 951
      IF (IKR.EQ.-1) PRINT 899
      IF (IKR.EQ.0) PRINT 900
      IF (IKR.EQ.1) PRINT 901
      IF (IKR.EQ.2) PRINT 902
      IF (IKR.EQ.3) PRINT 903
      IF (IKR.EQ.4) PRINT 904
      IF (IKR.EQ.11) PRINT 911
      IF (IKR.EQ.12) PRINT 912
      IF (IKR.EQ.13) PRINT 913
      IF (IKR.EQ.14) PRINT 914
      IF (IKR.EQ.15) PRINT 915
      IF (IKR.EQ.16) PRINT 916
      IF (IKR.EQ.17) PRINT 917,IP(1)
      IF (IKR.EQ.18) PRINT 918,IP(1)
      IF (IKR.EQ.19) PRINT 919,IP(1)
      PRINT 952
      PRINT 931,N
      PRINT 932,X0
      PRINT 933,NPAR
      PRINT 934,P0
      PRINT 935,PMIN
      PRINT 936,PMAX
      PRINT 937,NAP
      PRINT 938,(IAP(I),I=1,NAP)
      PRINT 941,H0
      PRINT 942,E
      PRINT 981,ITMAP
#endif
      CALL INITBF
      CALL FUNBF(X,F,IER,ISTOP)
        IF (IER.EQ.0.AND.ISTOP.EQ.0) GO TO 150
        IEND=-7
        GO TO 170
 150  CONTINUE
      R=0.
      DO 152 I=1,NK-1
         R=R+F(I)**2
         IF (IFVL.NE.0) FVAL(I)=F(I)
 152  CONTINUE
      FVAL(NK)=DSQRT(R)    
#if defined(hib)
      PRINT 943,(F(I),I=1,NK1)
      IF (NUF.EQ.0) GO TO 160
      PRINT 970
      DO 155 I=1,NUF
        IF (IFUNS(I).EQ.1) PRINT 971
        IF (IFUNS(I).EQ.2) PRINT 972
        IF (IFUNS(I).EQ.3) PRINT 973
        IF (IFUNS(I).EQ.4) PRINT 974
        IF (IFUNS(I).EQ.5) PRINT 975
 155  CONTINUE
 160  PRINT 952
#endif
      CALL DFUNBF(X,DFUN,NK1,NK,IER,ISTOP)
      IF (IER.EQ.0.AND.ISTOP.EQ.0) GO TO 161
        IEND=-8
        GO TO 170
 161  CONTINUE
      EPSTAN=1.D-10
      CALL DMFGR(DFUN,NK1,NK,EPSTAN,IRANK,IROW,ICOL)
      IF(IRANK.EQ.NK1) GO TO 162
        IEND=-9
        GOTO 170
 162  NG=ICOL(NK)
C      PRINT 1101,IRANK,NG,(icol(i),i=1,nk),(dfun(nk1*nk1+i),i=1,nk1)
C 1101 FORMAT(/1x,'irank=',i2,'  ng=',i2,' icol=',6i3,/' dfun=',6g13.4)
      R=1.D0
      DO 163 I=1,NK1
        IF(ICOL(I).EQ.NF+1) R=DFUN(NK1*NK1+I)
 163  CONTINUE
      IF (DABS(R).LT.1.D-3) R=0.D0
      IF (R.LT.0) HV0=-HV0
C
      CALL BEETLE(NK,NG,IADD,X,HV0,EPS,FUNBF,DFUNBF,OUTLBF,IEND)
 170  CONTINUE
      IF (IERFS.NE.0) IEND=-5
#if defined(hib)
      IF (IEND.LT.0.AND.IEND.NE.-5) GO TO 200
        IF (IEND.EQ.-5.AND.IGRAN.EQ.1) PRINT 955
        IF (IEND.EQ.2.OR.IEND.EQ.4) PRINT 955
        PRINT 960,IEND
        GO TO 300
 200  PRINT 961,IEND
 300  PRINT 952
#endif
      RETURN
#if defined(hib)
 899  FORMAT( /1X,'CONTINUATION:  IKR=-1 ')
 900  FORMAT( /1X,'EQUILIBRIUM CURVE:  IKR=0 ')
 901  FORMAT( /1X,'FOLD CURVE:  IKR=1 ')
 902  FORMAT( /1X,'HOPF CURVE:  IKR=2 ')
 903  FORMAT( /1X,'FLIP CURVE:  IKR=3 ')
 904  FORMAT( /1X,'DOUBLE MULTIPLIER CURVE:  IKR=4 ')
 911  FORMAT( /1X,'DOUBLE +1 CURVE:  IKR=11 ')
 912  FORMAT( /1X,'DOUBLE -1 CURVE:  IKR=12 ')
 913  FORMAT( /1X,'FOLD + HOPF CURVE:  IKR=13 ')
 914  FORMAT( /1X,'FLIP + HOPF CURVE:  IKR=14 ')
 915  FORMAT( /1X,'FOLD + FLIP CURVE:  IKR=15 ')
 916  FORMAT( /1X,'CUSP CURVE:  IKR=16 ')
 917  FORMAT( /1X,'FOLD + NONTRANSVERSALITY (P(',I2,')) :  IKR=17 ')
 918  FORMAT( /1X,'HOPF + NONTRANSVERSALITY (P(',I2,')) :  IKR=18 ')
 919  FORMAT( /1X,'FLIP + NONTRANSVERSALITY (P(',I2,')) :  IKR=19 ')
 931  FORMAT(//3X,'€‡Œ…Ž‘’œ ”€‡Ž‚ŽƒŽ Ž‘’€‘’‚€: N=',I2)
 932  FORMAT(/3X,'€—€‹œ€Ÿ ’Ž—Š€ ‚ ”€‡Ž‚ŽŒ Ž‘’€‘’‚…:'
     A  /5X,'X0=',8G14.6 /(8X,8G14.6))
 933  FORMAT(/3X,'—ˆ‘‹Ž €€Œ…’Ž‚ ‘ˆ‘’…Œ›: NPAR=',I2)
 934  FORMAT(/3X,'€—€‹œ›… ‡€—…ˆŸ €€Œ…’Ž‚:'
     A  /5X,'P0=',8G14.6 /(8X,8G14.6))
 935  FORMAT(/3X,'ƒ€ˆ–› ˆ‡Œ……ˆŸ €€Œ…’Ž‚:'
     A  /3X,'PMIN=',8G14.6 /(8X,8G14.6))
 936  FORMAT(3X,'PMAX=',8G14.6 /(8X,8G14.6))
 937  FORMAT(/3X,'—ˆ‘‹Ž €Š’ˆ‚›• €€Œ…’Ž‚ ‘ˆ‘’…Œ›: NAP=',I2)
 938  FORMAT(/3X,'ˆ„…Š‘› €Š’ˆ‚›• €€Œ…’Ž‚: IAP=',10I3)
 941  FORMAT(/3X,'€—€‹œ›‰ ˜€ƒ „‚ˆ†…ˆŸ Ž Šˆ‚Ž‰: H0=',
     A  G11.4)
 942  FORMAT(/3X,'’Ž—Ž‘’œ €•Ž†„…ˆŸ ’Ž—…Š € Šˆ‚Ž‰: E=',G11.4)
 943  FORMAT(/3X,'‡€—…ˆŸ ‘ˆ‘’…Œ› ”“Š–ˆ‰, ‡€„€ž™ˆ• Šˆ‚“ž,',
     A  ' ‚ €—€‹œŽ‰ ’Ž—Š…:' /5X,'F0=',8G14.6 /(8X,8G14.6))
 951  FORMAT(///1X,50('*'))
 952  FORMAT(/1X,50('*'))
 955  FORMAT(/3X,'„Ž‘’ˆƒ“’€ ƒ€ˆ–€ ‡€„€Ž‰ €€Œ…’ˆ—…‘ŠŽ‰ ',
     A   'Ž‹€‘’ˆ')
 960  FORMAT(//3X,'Žƒ€ŒŒ€ LINLBF ‡€ŠŽ—ˆ‹€ €Ž’“  ',
     A   '(IEND=',I3,')')
 961  FORMAT(//3X,'€‚€ˆ‰Ž… ‡€‚…˜…ˆ… €Ž’› Žƒ€ŒŒ› LINLBF  ',
     A   '(IEND=',I3,')')
 970  FORMAT(/3X,'”“Š–ˆˆ € Šˆ‚Ž‰:')
 971  FORMAT(6X,'G(1)=0 - “‘‹Ž‚ˆ… Š€’Ž‘’ˆ')
 972  FORMAT(6X,'G(2)=0 - “‘‹Ž‚ˆ… …‰’€‹œŽ‘’ˆ')
 973  FORMAT(6X,'G(3)=0 - “‘‹Ž‚ˆ… Š€’›• ‘Ž‘’‚…›• —ˆ‘…‹')
 974  FORMAT(6X,'G(4)=0 - “‘‹Ž‚ˆ… ’…•Š€’Ž‘’ˆ')
 975  FORMAT(6X,'G(5)=0 - Ž‹œ …‚Ž‰ ‹Ÿ“Ž‚‘ŠŽ‰ ‚…‹ˆ—ˆ›')
 981  FORMAT(/6X,'POWER OF MAP =',I3)
#endif
      END
 
