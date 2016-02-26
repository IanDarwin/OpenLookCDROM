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

      SUBROUTINE LYAPUN(N,X,FUNS,IPRINT,EPS,AL,OM,VEC1,VEC2,VL,IEL)
C
C   MANAGING PROGRAM FOR COMPUTING FIRST LYAPUNOV VALUE
C            23.06.81
C            28.02.90
C            15.11.92
C
      IMPLICIT REAL*8(A-H,O-Z)
      IMPLICIT INTEGER*2(I-N)
      include 'epbf.dim'
      parameter (NDMAX=7*NFMAX-4, NFMAX1=NFMAX-2)
      DIMENSION X(N),VEC1(N),VEC2(N)
      DIMENSION INT(NFMAX),SCALE(NFMAX),DER23(NDMAX),DERH23(NDMAX),
     A          DERH1(NFMAX2),E(NFMAX1,NFMAX1)
      COMMON/WORK7/ ANN(NFMAX2),E1(NFMAX,NFMAX)
      COMMON/MDEREV/A(NFMAX,NFMAX),RR(NFMAX),RI(NFMAX)
      COMMON/PQ/P(NFMAX,NFMAX),Q(NFMAX,NFMAX)
      COMMON/DRVR/DH1,DH23,NDH1,NDH23
      COMMON /IERFS/ IERFS,IERDUM
      EXTERNAL FUNS,DER1L,DER23L
      M1=NFMAX
      M1E=M1-2
      NN=N*N
      NM2=N-2
      EPS1=1.D-6
      EPSPLO=1.D-4
      IEL=0
      ND=7*N-4
      DH1=1.D-7
      NDH1=0
      DH23=5.D-3
      NDH23=0
C
C   pE~ATx HA~AlxHyX dAHHyX
C
      CALL FUNS(X,ANN)
      IF (IPRINT.LE.1) GO TO 5
#if defined(hib)
      PRINT 901,N,IPRINT,EPS,X
 901  FORMAT(/1X,'N=',I2,2X,'IPRINT=',I2,2X,'EPS=', G13.6,
     A  /1X,'X=',8G14.6,/(3X,8G14.6))
      PRINT 902,(ANN(I),I=1,N)
 902  FORMAT(1X,'F=',8G14.6,/(3X,8G14.6))
#endif
C
C   KOHTPOlx zHA~EHij pPAByX ~ACTEj
C
 5    DO 6 I=1,N
        IF (DABS(ANN(I)).LE.EPS) GO TO 6
C   OTKAz: zAdAHHAq TO~KA X HE qBlqETCq CTAciOHAPHOj
        IEL=1
        GO TO 260
 6    CONTINUE
      IF (IERFS.EQ.0) GO TO 7
        IEL=6
        GO TO 260
7     CONTINUE  
C
C   By~iClEHiE MATPicy liHEAPizAcii - A
C
      CALL DERVAR(N,NN,DH1,NDH1,X,ANN,DERH1,DER1L,FUNS,IER)
      IF (IER.EQ.0) GO TO 8
        IEL=7
        GO TO 260
8     CONTINUE
      L=1
      DO 20 J=1,N
        DO 10 I=1,N
          A(I,J)=ANN(L)
          L=L+1
 10     CONTINUE
 20   CONTINUE
C
C   By~iClEHiE CObCTBEHHyX zHA~EHij
C
      CALL DBALAN(N,N,ANN,LOW,IGH,SCALE)
      CALL DELMHE(N,N,LOW,IGH,ANN,INT)
      CALL DHQR(N,N,LOW,IGH,ANN,RR,RI,IER)
      IF (IER.EQ.0) GO TO 25
C   OTKAz: CObCTBEHHyE zHA~EHiq HE HAjdEHy
        IEL=2
        GO TO 260
 25   CONTINUE
      IF (IPRINT.LE.1) GO TO 28
#if defined(hib)
      PRINT 913,(RR(I),RI(I),I=1,N)
913   FORMAT(/1X,'ëéÅëíÇÖççõÖ áçÄóÖçàü:',/8X,'RE',13X,'IM',
     A  /(3X,2G14.6))
#endif
C
C   BydElEHiE blivAj{Ej K MHiMOj OCi
C   pAPy CObCTBEHHyX zHA~EHij
C
 28   AL=RR(1)
      OM=DABS(RI(1))
      DO 40 I=2,N
        IF (DABS(AL).LT.DABS(RR(I))) GO TO 40
        IF (DABS(AL).EQ.DABS(RR(I))) GO TO 30
          AL=RR(I)
          OM=DABS(RI(I))
          GO TO 40
 30     IF (OM.LT.DABS(RI(I))) OM=DABS(RI(I))
 40   CONTINUE
C
C   pPOBEPKA uClOBiq HEjTPAlxHOCTi
C
      IF (DABS(AL).LE.EPS.AND.OM.NE.0) GO TO 45
C   OTKAz: HE BypOlHEHO uClOBiE HEjTPAlxHOCTi
        IEL=3
        GO TO 260
 45   IF (IPRINT.LE.2) GO TO 50
#if defined(hib)
      PRINT 914,AL,OM
 914  FORMAT(/1X,'''çÖâíêÄãúçÄü'' èÄêÄ ëéÅëíÇÖççõï áçÄóÖçàâ:',
     A  /1X,'RE=',G13.6,2X,'IM=',G13.6)
#endif
C
C   AHAliz BzAiMHOgO PACpOlOvEHiq CObCTBEHHyX zHA~EHij
C
 50   CONTINUE
      L=0
      DO 60 I=1,N
        IF (DABS(RR(I)-AL).GT.EPS1) GO TO 60
          L=L+1
 60   CONTINUE
      IF (L.EQ.2) GO TO 70
C   OTKAz: dEjCTBiTElxHAq ~ACTx COBpAdAET bOlEE
C          ~EM u dBuX CObCTBEHHyX zHA~EHij
        IEL=4
        GO TO 260
 70   CONTINUE
C     pEPEXOd K AHAlizu Clu~Aq N=2
      IF (N.EQ.2) GO TO 240
C
C   OpPEdElEHiE iHBAPiAHTHOj plOCKOCTi
C
      IF (OM.LT.1.) EPSPLO=EPSPLO*OM
      CALL PLO(N,A,AL,OM,VEC1,VEC2,M1,EPSPLO,IER)
      IF (IER.EQ.0) GO TO 75
        IEL=5
        GO TO 260
 75   IF (IPRINT.LE.2) GO TO 80
#if defined(hib)
      PRINT 915, VEC1
 915  FORMAT(/1X,'ÇÖäíéêõ, áÄÑÄûôàÖ àçÇÄêàÄçíçìû èãéëäéëíú:',
     A  /2X,'VEC1=',8G14.6,/(7X,8G14.6))
      PRINT 916,VEC2
 916  FORMAT( 2X,'VEC2=',8G14.6,(/7X,8G14.6))
#endif
C
C   pPiBEdEHiE MATPicy liHEAPizAcii K blO~HO-diAgOHAlxHOMu Bidu
C
 80   CALL BLOCK2(N,A,AL,OM,VEC1,VEC2,P,Q,E1,M1,IER)
      IF (IER.EQ.0.) GO TO 210
        IEL=5
        GO TO 260
 210  DO 230 J=3,N                               
        DO 220 I=3,N
 220    E(I-2,J-2)=E1(I,J)
 230  CONTINUE
C
C   pEPEXOd K By~iClEHi` lqpuHOBCKOj BEli~iHy
      GO TO 250
C
C   AHAliz Clu~Aq N=2
C
 240  R=-OM/A(1,2)
      S=1.
      IF (R.LT.0.) S=-1.
      R=DSQRT(S*R)
      VEC1(1)=1./R
      VEC1(2)=(AL-A(1,1))/(A(1,2)*R)
      VEC2(1)=0.
      VEC2(2)=-OM/(A(1,2)*R)
      Q(1,1)=VEC1(1)
      Q(2,1)=VEC1(2)
      Q(1,2)=VEC2(1)
      Q(2,2)=VEC2(2)
      P(1,1)=S*Q(2,2)
      P(2,1)=-S*Q(2,1)
      P(1,2)=-S*Q(1,2)
      P(2,2)=S*Q(1,1)
      IF (IPRINT.LE.2) GO TO 250
#if defined(hib)
      PRINT 915,VEC1
      PRINT 916,VEC2
#endif
C
C   By~iClEHiE pPOizBOdHyX dlq lqpuHOBCKOj BEli~iHy
C
 250  CALL DERVAR(N,ND,DH23,NDH23,X,DER23,DERH23,DER23L,FUNS,IER)
      IF (IER.EQ.0) GO TO 251
        IEL=8
        GO TO 260
C
C   By~iClEHiE lqpuHOBCKOj BEli~iHy
C
 251  VL=VLIAP(NM2,AL,OM,DER23,E,M1E)
      IF (IPRINT.LE.1) GO TO 1000
#if defined(hib)
      PRINT 924,VL
 924  FORMAT(/1X,'ãüèìçéÇëäÄü ÇÖãàóàçÄ=',G24.16)
#endif
      RETURN
C
C   pE~ATx COOb}EHij Ob OTKAzAX
C
 260  CONTINUE
#if defined(hib)
      IF(IPRINT.EQ.0) GO TO 1000
      GO TO (265,270,280,290,295,300,305,310),IEL
 265  PRINT 930,IEL
 930  FORMAT(//1X,110('*'),
     A  //3X,'IEL=',I2,3X,'áÄÑÄççÄü íéóäÄ x çÖ üÇãüÖíëü ëíÄñàéçÄêçéâ')
      IF (IPRINT.GT.1) GO TO 1000
      PRINT 901,N,IPRINT,EPS,X
      PRINT 902,(ANN(I),I=1,N)
      GO TO 1000
 270  PRINT 925,IEL,IER
 925  FORMAT(//1X,110('*'),
     A  /3X,'IEL=',I2,3X,'ëéÅëíÇÖççõÖ áçÄóÖçàü çÖ çÄâÑÖçõ  IER=',I3)
      IF (IPRINT.GT.1) GO TO 1000
        PRINT 901,N,IPRINT,EPS,X
        GO TO 1000
 280  PRINT 926,IEL
 926  FORMAT(//1X,110('*'),
     A  //3X,'IEL=',I2,3X,'çÖ ÇõèéãçÖçé ìëãéÇàÖ ''çÖâíêÄãúçéëíà''')
      IF (IPRINT.GT.1) GO TO 1000
        PRINT 901,N,IPRINT,EPS,X
        GO TO 400
 290  PRINT 927,IEL
 927  FORMAT(//1X,110('*'),
     A  //3X,'IEL=',I2,3X,'çÖí ÇõÑÖãÖççéâ èÄêõ äéåèãÖäëçé-ëéèêüÜÖççõï',
     B  ' ëéÅëíÇÖççõï áçÄóÖçàâ')
      IF (IPRINT.GT.1) GO TO 1000
        PRINT 901,N,IPRINT,EPS,X
        GO TO 400
 295  PRINT 931,IEL
 931  FORMAT(//1X,110('*'),
     A  //3X,'IEL=',I2,3X,'àçÇÄêàÄçíçÄü èãéëäéëíú çÖ çÄâÑÖçÄ')
      IF (IPRINT.GT.1) GO TO 1000
        PRINT 901,N,IPRINT,EPS,X
        GO TO 400
 300  PRINT 941,IEL
 941  FORMAT(//1X,110('*'),
     A  //3X,'IEL=',I2,3X,'ERROR IN RIGHT-HAND-SIDES')
      IF (IPRINT.GT.1) GO TO 1000
        PRINT 901,N,IPRINT,EPS,X
        GO TO 1000
 305  PRINT 951,IEL
 951  FORMAT(//1X,110('*'),
     A  //3X,'IEL=',I2,3X,'ERROR IN COMPUTING JACOBY MATRIX')
      IF (IPRINT.GT.1) GO TO 1000
        PRINT 901,N,IPRINT,EPS,X
        GO TO 1000
 310  PRINT 961,IEL
 961  FORMAT(//1X,110('*'),
     A  //3X,'IEL=',I2,3X,'ERROR IN COMPUTING HIGH-ORDER DERIVATIVES')
      IF (IPRINT.GT.1) GO TO 1000
        PRINT 901,N,IPRINT,EPS,X
 400  PRINT 928,(RR(I),RI(I),I=1,N)
 928  FORMAT(/3X,'ëéÅëíÇÖççõÖ áçÄóÖçàü:',/(3X,2G24.16))
#endif
 1000 RETURN
      END


