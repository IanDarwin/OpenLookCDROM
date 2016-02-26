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

      FUNCTION VLIAP(NM2,AL,OM,D,E,M1E)
C
C   Compute first Lyapunov coefficient using
C      coefficients of Teylor expansion
C      23.06.81
C
      IMPLICIT REAL*8(A-H,O-Z)
      IMPLICIT INTEGER*2(I-N)
      include 'epbf.dim'
      parameter (NFMAX1=NFMAX-2, NT=NFMAX1*NFMAX1)
      DIMENSION E(M1E,NM2),D(1)
      COMMON/WORK8/ D11(NFMAX1),D12(NFMAX1),D21(NFMAX1),D22(NFMAX1),
     *             E20(NFMAX1),E02(NFMAX1), E11(NFMAX1),
     *             T1(NT),T2(NT),T3(NT),U1(NFMAX1),U2(NFMAX1),
     *             U3(NFMAX1),V1(NFMAX1),V2(NFMAX1)
      N1=NM2+2
      N2=2*N1
      N3=3*N1
      B120=D(1)/2.
      B220=D(2)/2.
      B102=D(N1+1)/2.
      B202=D(N1+2)/2.
      B111=D(N2+1)
      B211=D(N2+2)
      B130=D(N3+1)/6.
      B112=D(N3+2)/2.
      B221=D(N3+3)/2.
      B203=D(N3+4)/6.
      R1=B120+B102
      R2=B220+B202
      S1=B120+B211-B102
      S2=B220-B111-B202
      R3=0.375*(S1*R1-S2*R2)+0.25*(R1*R1+R2*R2)
      R4=S1*S1+S2*S2
      R5=S1*R2+S2*R1
      VLIAP=AL*(R3/(AL**2+OM**2)+R4/(AL**2+9.*OM**2)/8.)-
     -OM*R5/(8.*(AL**2+OM**2))+
     +0.125*(3.*B130+B221+B112+3.D0*B203)
      IF (NM2.EQ.0) RETURN
      N3=3*N1+4
      N4=N3+NM2
      N5=N4+NM2
      N6=N5+NM2
      N1=N1+2
      N2=N2+2
      DO 10 I=1,NM2
        E20(I)=D(2+I)/2.
        E02(I)=D(N1+I)/2.
        E11(I)=D(N2+I)
        D11(I)=D(N3+I)
        D12(I)=D(N4+I)
        D21(I)=D(N5+I)
        D22(I)=D(N6+I)
 10   CONTINUE
      DO 20 I=1,NM2
        U1(I)=D11(I)-D22(I)
        U2(I)=D12(I)+D21(I)
        U3(I)=D11(I)+D22(I)
 20   CONTINUE
      OM2=2.*OM
      OM24=4.*OM**2
      DO 40 I=1,NM2
        R1=0.
        R2=0.
        DO 30 J=1,NM2
          R1=R1+E(J,I)*U1(J)
          R2=R2+E(J,I)*U2(J)
 30     CONTINUE
        V1(I)=R1-OM2*U2(I)
        V2(I)=R2+OM2*U1(I)
 40   CONTINUE
      L=1
      DO 70 J=1,NM2
        DO 60 I=1,NM2
          R=0.
          DO 50 K=1,NM2
 50       R=R+E(K,I)*E(J,K)
          IF (I.EQ.J) R=R+OM24
          T1(L)=R
          T2(L)=R
          T3(L)=E(J,I)
 60     L=L+1
 70   CONTINUE
      IF (NM2.EQ.1) GO TO 80
      CALL DSIMQ(T1,V1,NM2,IER)
      CALL DSIMQ(T2,V2,NM2,IER)
      CALL DSIMQ(T3,U3,NM2,IER)
      GO TO 90
 80   V1(1)=V1(1)/T1(1)
      V2(1)=V2(1)/T2(1)
      U3(1)=U3(1)/T3(1)
 90   R1=0.
      R2=0.
      R3=0.
      DO 100 I=1,NM2
        R1=R1+V1(I)*(E20(I)-E02(I))
        R2=R2+V2(I)*E11(I)
        R3=R3+U3(I)*(E20(I)+E02(I))
 100  CONTINUE
      VLIAP=VLIAP-0.125*(R1+R2+2.D0*R3)
      RETURN
      END
 
