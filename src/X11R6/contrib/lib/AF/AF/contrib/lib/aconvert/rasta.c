/*
 * Modified for inclusion in aconvert by Dave Wecker (8/23/93)
 */

/*
 * Modified to accept binary input from stdin and uses a different fft routine
 * which is a little faster than the old fft routine. 3/6/92 - Chuck Wooters 
 */

/* rasta.c - Hynek Hermansky 6/25/91	 */
/*
 * does RASTA processing in log spectral domain 
 */

#include "aconvP.h"

#define two_to_the(N) (int)(pow(2.0,(N))+0.5)

/* Table of constant values */
static float   VerySmall  =	  .000001;
static double  c_b17 	  =	  .33;
static double  c_b28 	  = 	10.;

/* Subroutine */
static void hwind_(float *weight, long *npoint)
{
  /* System generated locals */
  long            i_1;

  /* Builtin functions */
  double          atan(), cos();

  /* Local variables */
  static long     ii;
  static float   pai;

  /* Parameter adjustments */
  --weight;

  /* Function Body */
  pai = atan((float) 1.) * (float) 4.;
  i_1 = *npoint;
  for (ii = 1; ii <= i_1; ++ii) {
    weight[ii] = (float).54 - cos(pai * (float) 2.* (ii - 1) / (*npoint -
							    1)) * (float).46;
  }
}				/* hwind_ */

/* subroutine setfilt	 */

static int setfilt_(
  float num[][5],
  float denom[][4],
  long *numord,
  long *denord,
  long *nfilt)
{
  /* System generated locals */
  long            i_1;
  long            j_1;
  float           pole;

  /* Local variables */
  static long     ii;
  static long     jj;

  i_1 = *nfilt - 1;
  j_1 = 4;

  pole = -0.94;

  for (ii = 1; ii < i_1; ii++) {
    numord[ii] = 4;
    denord[ii] = 1;

    denom[ii][0] = 1.0;
    denom[ii][1] = pole;

    for (jj = 0; jj <= j_1; jj++) {
      num[ii][jj] = -0.1 * (float) (jj - 2);
    }
  }
}				/* setfilt_ */

/* Subroutine */

static int bndpss_(
  float *inasp,
  float *outasp,
  float num[][5],
  float denom[][4],
  float inbuf[][5],
  float outbuf[][4],
  long *numord,
  long *denord,
  long *nfilt,
  long *icall)
{

  /* System generated locals */
  long            i_1;
  long            i_2;

  /* Local variables */
  static long     ii;
  static long     jj;
  static float    sum;

  i_1 = *nfilt - 1;
  i_2 = *icall;


  for (ii = 1; ii < i_1; ii++) {

    inbuf[ii][0] = inasp[ii];

    sum = 0.0;

    if (i_2 >= 5) {

      for (jj = 0; jj <= numord[ii]; jj++) {
	sum = sum + num[ii][jj] * inbuf[ii][jj];
      }

      for (jj = 1; jj <= denord[ii]; jj++) {
	sum = sum - denom[ii][jj] * outbuf[ii][jj - 1];
      }

      for (jj = denord[ii]; jj > 0; jj--) {
	outbuf[ii][jj] = outbuf[ii][jj - 1];
      }
    }
    for (jj = numord[ii]; jj > 0; jj--) {
      inbuf[ii][jj] = inbuf[ii][jj - 1];
    }

    outbuf[ii][0] = sum;

    outasp[ii] = sum;
  }

}				/* bndpss_ */

/* Subroutine */

static void adw_(
  long *npoint,
  long *nfilt,
  float *cb,
  float *eql,
  long *ibegen,
  float *sf)
{
  /* System generated locals */
  long            i_1, i_2;
  float           r_1, r_2;
  double          d_1;


  /* Local variables */
  static float    freq, zdel, rsss;
  static long     i, j;
  static float    x, z, f0, z0, f2samp, fh, fl, fnqbar;
  static long     icount;
  static float    fsq;

  /* Parameter adjustments */
  --cb;
  ibegen -= 24;

  /* Function Body */
  /* subroutine computes auditory weighting functions */
  /* inputs: */
  /* npoint - number of points in the fft spectrum */
  /* nfilt - number of samples of auditory spectrum */
  /* equally spaced on the bark scale; */
  /* 1st filter at dc and last at the Nyquist frequency */
  /* outputs: */
  /* cb - array of weighting coefficients to simulate */
  /* critical band spectral resolution */
  /* eql - array of weighting coefficients to simulate */
  /* equal loudness sensitivity of hearing */
  /* on npoint speech power spectrum */
  /* ibegen - three-dimensional array which indicates where */
  /* to begin and where to end integration */
  /* of speech spectrum and where the given */
  /* weighting function starts in array cb */
  /* get Nyquist frequency in Bark */

  r_1 = *sf / (float) 1200.;
  fnqbar = log((double) *sf / (double) 1200.+ sqrt(r_1 * r_1 + (double) 1.))
    * (double) 6.;
  /* compute number of filters for less than 1 Bark spacing */
  *nfilt = (long) fnqbar + 2;
  /* frequency -> fft spectral sample conversion */
  f2samp = (float) (*npoint - 1) / (*sf / (float) 2.);
  /* compute filter step in Bark */
  zdel = fnqbar / (float) (*nfilt - 1);

  /* loop over all weighting functions */

  icount = 1;
  i_1 = *nfilt - 1;
  for (j = 2; j <= i_1; ++j) {
    ibegen[j + 69] = icount;
    /* get center frequency of the j-th filter in Bark */
    z0 = zdel * (float) (j - 1);
    /* get center frequency of the j-th filter in Hz */
    f0 = (exp((double) z0 / 6.) - exp(-(double) z0 / 6))
      * (float) 600./ (float) 2.;
    /* get low-cut frequency of j-th filter in Hz */
    fl = (exp((z0 - (double) 2.5) / 6) - exp(-(double) (z0 - (float) 2.5)
					     / 6)) * (float) 600./ (float) 2.;
    r_1 = fl * f2samp;
    ibegen[j + 23] = (((double) r_1) + 0.5) + 1;
    if (ibegen[j + 23] < 1) {
      ibegen[j + 23] = 1;
    }
    /* get high-cut frequency of j-th filter in Hz */
    fh = (exp((z0 + (double) 1.3) / 6) - exp(-(double) (z0 + (float) 1.3)
					     / 6)) * (float) 600./ (float) 2.;
    r_1 = fh * f2samp;
    ibegen[j + 46] = (((double) r_1) + 0.5) + 1;
    if (ibegen[j + 46] > *npoint) {
      ibegen[j + 46] = *npoint;
    }
    /* do-loop over the power spectrum */
    i_2 = ibegen[j + 46];
    for (i = ibegen[j + 23]; i <= i_2; ++i) {
      /* get frequency of j-th spectral point in Hz */
      freq = (float) (i - 1) / f2samp;
      /* get frequency of j-th spectral point in Bark */
      x = freq / (float) 600.;
      r_1 = x;
      z = log(x + sqrt(r_1 * r_1 + (double) 1.)) * (float) 6.;
      /* normalize by center frequency in barks: */
      z -= z0;
      /* compute weighting */
      if (z <= (float) -.5) {
	d_1 = (double) (z + (float).5);
	cb[icount] = pow(c_b28, d_1);
      } else if (z >= (float).5) {
	d_1 = (double) ((z - (float).5) * (float) -2.5);
	cb[icount] = pow(c_b28, d_1);
      } else {
	cb[icount] = (float) 1.;
      }
      /* calculate the LOG 40 db equal-loudness curve */
      /* at center frequency */
      r_1 = f0;
      fsq = r_1 * r_1;
      r_1 = fsq;
      r_2 = fsq + (float) 1.6e5;
      rsss = r_1*r_1*(fsq+(float)1.44e6) / (r_2*r_2*(fsq+(float)9.61e6));
      /* take log and put the equal-loundness curve into eql array */
      eql[j] = log((double) rsss);
      ++icount;
    }
  }
}				/* adw_ */

/* Subroutine */
static void cosf_(long *m, long *nfilt, float *wcos)
{
  /* System generated locals */
  long            i_1, i_2;

  /* Builtin functions */
  double          atan(), cos();

  /* Local variables */
  static long     ii, jj;
  static float    pai;

  /* Parameter adjustments */
  wcos -= 24;

  /* Function Body */
  /* subroutine for computing cosine weightings for IDFT */
  pai = atan((float) 1.) * (float) 4.;
  i_1 = *m + 1;
  for (ii = 1; ii <= i_1; ++ii) {
    i_2 = *nfilt - 1;
    for (jj = 2; jj <= i_2; ++jj) {
      wcos[jj + ii * 23] = cos(pai * (float) 2.* (ii - 1) * (jj - 1) / (
					      *nfilt - 1 << 1)) * (float) 2.;
    }
    wcos[*nfilt + ii * 23] = cos(pai * (float) 2.* (ii - 1) * (jj - 1) / (
							   *nfilt - 1 << 1));
  }
}				/* cosf_ */

/* Subroutine */
static void a2gexp_(float *a, float *gexp, long *i, long *nc, float *expon)
{
  /* System generated locals */
  long            i_1, i_2;
  double          d_1, d_2;

  /* Builtin functions */
  double         log(), pow();

  /* Local variables */
  static float    c[257];
  static long     j, l, jb, ii, lp;
  static float    sum;

  /* Parameter adjustments */
  --a;
  --gexp;

  /* Function Body */
  /* i = number of elements in a() - 1 */
  /* nc = number of elements in output array c() */

  if (a[1] == 0.0)
    c[0] = log(VerySmall);
  else
    c[0] = log(a[1]);
  c[1] = -(double) a[2] / a[1];
  i_1 = *nc;
  for (l = 2; l <= i_1; ++l) {
    lp = l + 1;
    if (l < *i + 1) {
      sum = l * a[lp] / a[1];
    } else {
      sum = (float) 0.;
    }
    i_2 = l;
    for (j = 2; j <= i_2; ++j) {
      jb = l - j + 2;
      if (j <= *i + 1) {
	sum += a[j] * c[jb - 1] * (jb - 1) / a[1];
      }
    }
    c[lp - 1] = -(double) sum / l;
  }
  i_1 = *nc;
  gexp[1] = c[0];
  for (ii = 2; ii <= i_1; ++ii) {
    if (*expon != (float) 0.) {
      d_1 = (double) ((float) (ii - 1));
      d_2 = (double) (*expon);
      gexp[ii] = pow(d_1, d_2) * c[ii - 1];
    } else {
      gexp[ii] = c[ii - 1];
    }
  }
}				/* a2gexp_ */

/* Subroutine rastaplp */
/* Bandpass filter version (log domain) */
static void rastaplp_(
  Ctxt* tpC,
  long *nwind,
  long *m,
  float *a,
  float *rc,
  float *gain,
  float *sf)
{
  float*	  speech = tpC->fpBuf1;

  /* Initialized data */

  static long     icall = 0;

  /* System generated locals */
  long            i_1, i_2;
  double          d_1;

  /* Builtin functions */
  double          atan(), log(), exp();

  /* Local variables */
  static float    hwei[512];
  static long     nfft;
  static float    wcos[368];
  static long     nspt;
  static float    r[16], s;
  static long     jfilt;
  static float    rcmct;
  static long     nfilt;
  static float    cb[900];
  static float    eql[23];
  static long     ib, ii, kk, mh, ll, ibegen[69], ip;
  static float    alpmin, audspe[23], spectr[512];
  static long     npoint;
  static long     nsize;
  static float    aib;
  static long     icb;
  static float    pai, aip, alp[17];
  static long     mct, idx, mct2;
  int             jj;
  static float    inasp[23], outasp[23];
  static float    num[23][5], denom[23][4];
  static long     numord[23], denord[23];
  static float    inbuf[23][5], outbuf[23][4];
  static float    inbuf_2[23][5], outbuf_2[23][4];

  /* Parameter adjustments */
  --speech;
  --a;
  --rc;

  /* Function Body */
  /* subroutine which computes m-th order (max 15) PLP model */
  /* (described by m+1 autoregressive coefficients a */
  /* and gain gain or by m reflection coefficients rc) */
  /* from nwind-points (max 512) of speech signal with */
  /* sampling frequency sf (max 20 000 Hz) */

  pai = atan((float) 1.) * (float) 4.;

  /* if called for the first time, */

  if (icall == 0) {

    /* compute window array */
    hwind_(hwei, nwind);
    /* get fft size from the window length */
    nfft = (long) (log((float) (*nwind)) / (float).693148) + 1;
    /* get number of spectral points */
    npoint = two_to_the((double) nfft) / 2 + 1;
    nsize = two_to_the((double) nfft);
    /* compute spectral weights */
    adw_(&npoint, &nfilt, cb, eql, ibegen, sf);
    /* compute IDFT coefficient table */
    cosf_(m, &nfilt, wcos);
    /* set coefficients of bandpass filters */
    setfilt_(num, denom, numord, denord, &nfilt);
  }
  icall++;

  i_1 = *nwind;
  /* window input speech */
  for (ii = 1; ii <= i_1; ++ii) {
    speech[ii] = hwei[ii - 1] * speech[ii];
  }

  /* compute speech power spectrum */
#if DEBUG
  printf("nfft= %d, nwind= %d\n", nfft, *nwind);
#endif
  AConvertFFT(tpC,&speech[1],spectr,*nwind,nfft);

  /* compute auditory spectrum */

  i_1 = nfilt - 1;
  for (jfilt = 2; jfilt <= i_1; ++jfilt) {
    audspe[jfilt - 1] = (float) 0.;
    i_2 = ibegen[jfilt + 22];
    for (kk = ibegen[jfilt - 1]; kk <= i_2; ++kk) {
      icb = ibegen[jfilt + 45] - ibegen[jfilt - 1] + kk;
      audspe[jfilt - 1] += spectr[kk - 1] * cb[icb - 1];
    }
  }

  i_2 = nfilt - 1;

  /* auditory spectrum processing	 */


  /* take logarithm	 */

  for (ii = 2; ii <= i_2; ++ii) {
    if ((double) audspe[ii - 1] == 0.0)
      audspe[ii - 1] = log((double) VerySmall);
    else
      audspe[ii - 1] = log((double) audspe[ii - 1]);
  }

  /* bandpass filter log auditory spectrum. for rasta only */
  if (tpC->tOut.eType == RASTA)
    bndpss_(audspe,audspe,num,denom,inbuf,outbuf,numord,denord,&nfilt,&icall);

  /* equal-loudness curve and cubic root intensity-loudness compression */

  for (ii = 2; ii <= i_2; ++ii) {
    audspe[ii - 1] = c_b17 * (audspe[ii - 1] + eql[ii]);
  }

  /* take inverse logarithm 		 */

  for (ii = 2; ii <= i_2; ++ii) {
    audspe[ii - 1] = exp((double) audspe[ii - 1]);
  }

  /* the first and last point of auditory spectrum */

  audspe[0] = audspe[1];
  audspe[nfilt - 1] = audspe[nfilt - 2];


  /* inverse discrete fourier transform */

  nspt = nfilt - 1 << 1;
  i_2 = *m + 1;
  for (kk = 1; kk <= i_2; ++kk) {
    r[kk - 1] = audspe[0];
    i_1 = nfilt;
    for (ll = 2; ll <= i_1; ++ll) {
      r[kk - 1] += audspe[ll - 1] * wcos[ll + kk * 23 - 24];
    }
    r[kk - 1] /= nspt;
  }
  /* solution for autoregressive model */
  a[1] = (float) 1.;
  alp[0] = r[0];
  rc[1] = -(double) r[1] / r[0];
  a[2] = rc[1];
  alp[1] = r[0] + r[1] * rc[1];
  i_2 = *m;
  for (mct = 2; mct <= i_2; ++mct) {
    s = (float) 0.;
    mct2 = mct + 2;
    alpmin = alp[mct - 1];
    i_1 = mct;
    for (ip = 1; ip <= i_1; ++ip) {
      idx = mct2 - ip;
      s += r[idx - 1] * a[ip];
    }
    rcmct = -(double) s / alpmin;
    mh = mct / 2 + 1;
    i_1 = mh;
    for (ip = 2; ip <= i_1; ++ip) {
      ib = mct2 - ip;
      aip = a[ip];
      aib = a[ib];
      a[ip] = aip + rcmct * aib;
      a[ib] = aib + rcmct * aip;
    }
    a[mct + 1] = rcmct;
    alp[mct] = alpmin - alpmin * rcmct * rcmct;
    rc[mct] = rcmct;
  }
  *gain = alp[*m];
}				/* plp_ */

static float zcptp(Ctxt* tpC,float* fpPTP)
{
# define	QUIET_THRESH 	150.0
  static Bool	bFirst 		= bTRUE;
  static float	fMax 		= 3000.0;
  static float	fMin 		= 50.0;
  static float	fTotLowValue 	= 0.0;
  static float	fLowCount 	= 0.0;
  static float	fBand 		= 100.0;
  static int	iLastSign 	= -1;
  float*	fpBuf 		= tpC->fpBuf1;
  float		fZC;
  int		i;
  float		fVal;
  float		fValue;
  int		iZC;
  int		iSign;

  if (bFirst) {
    bFirst = bFALSE;
    /*
     * instead of messing with 150 lookahead for hysteresis fBand, I'm getting
     * it off to a good start and then not using a lookahead.  It's a bit
     * different at first (better!), but then settles down.  I think it's fine
     * for training. 
     *
     * mf 8-12-93 
     */
    for (i = 0; i < tpC->tOut.iWindow; i++) {
      fVal = *fpBuf++;
      if (fVal < fMin) 		fMin = fVal;
      else if (fVal > fMax) 	fMax = fVal;
      fValue = ((float) fMax - fMin) / 2.;

      /* update zc band */
      if (fValue < QUIET_THRESH) {
	fTotLowValue += fValue;
	fLowCount    += 1.0;
	fBand 	      = fTotLowValue / fLowCount;
      }
    }
    fpBuf = tpC->fpBuf1;
  }

  fMin = fMax = *fpBuf;
  iZC  = 0;
  for (i = 0; i < tpC->tOut.iWindow; i++) {
    fVal = *fpBuf++;
    if (fVal < fMin) 	  	fMin = fVal;
    else if (fVal > fMax) 	fMax = fVal;

    if (fVal > fBand)		iSign = 1;
    else if (fVal < -fBand)	iSign = -1;
    else 			continue;

    if ((iSign * -1) == iLastSign) {
      iZC++;
      iLastSign = iSign;
    }
  }

  fZC 	 = (float)iZC / (float)tpC->tOut.iWindow;
  fValue = ((float) fMax - fMin) / 2.;

  /* update zc fBand */
  if (fValue < QUIET_THRESH) {
    fTotLowValue	+= fValue;
    fLowCount		+= 1.0;
    fBand 		 = fTotLowValue / fLowCount;
  }
  *fpPTP = fValue / 32768.0;

  return fZC;
}

/* Main program */
int AConvertRasta(
  Ctxt*   tpC,
  int	  iCnt,
  short*  spBuf,
  float*  fpBuf)
{
  int      i;
  float   gain, gexp[129], a[16];
  float   rc[15];
  int	   iCount = 0;			/* output vector count */
  long	   Np 		= tpC->tOut.iCoeffs - 2;
  long	   M  		= Np - 1;
  float   Sf 		= (float)tpC->tOut.iFreq;
  long	   lWindow	= tpC->tOut.iWindow;
  int	   iIncr	= tpC->tOut.iIncr;
  int	   iKeep	= lWindow - iIncr;
  float*  fpTmp1;
  float*  fpTmp2;

  /* Take care of first call */
  if (!tpC->fpBuf1) {
    tpC->fpBuf1 = (float*)malloc(lWindow * sizeof(float));
    if (!tpC->fpBuf1) AConvertUsage("Can't allocate Rasta Buffer");
    tpC->fpBuf2 = (float*)malloc(lWindow * sizeof(float));
    if (!tpC->fpBuf2) AConvertUsage("Can't allocate Rasta Buffer");
    tpC->iBufSaved = 0;
  }

  /* start of analysis loop */
  while (1) {

    /* Make sure the bottom section of the buffer is loaded */
    fpTmp1 = &tpC->fpBuf1[tpC->iBufSaved];
    while (tpC->iBufSaved < iIncr) {
      if (!iCnt--) return iCount;
      *fpTmp1++ = (float)*spBuf++;
      tpC->iBufSaved++;
    }

    /* Now do the top section of the buffer(s) */
    fpTmp2 = &tpC->fpBuf2[tpC->iBufSaved - iIncr];
    while (tpC->iBufSaved < lWindow) {
      float	fpVal;

      if (!iCnt--) return iCount;
      fpVal	= (float)*spBuf++;
      *fpTmp1++	= fpVal;
      *fpTmp2++ = fpVal;
      tpC->iBufSaved++;
    }

    /* here is the analysis */
    rastaplp_(tpC,&lWindow,&M,a,rc,&gain,&Sf);

    /* Do the gain scaling */
    for (i = 0; i < M+1; i++) a[i] = a[i] / gain;

    a2gexp_(a, gexp, &M, &Np, &tpC->tOut.fExpon);
    gexp[0] = -gexp[0];

    /* Output the rasta features */
    *fpBuf++	= gexp[0];
    *fpBuf++	= zcptp(tpC,fpBuf++);
    for (i = 1; i < Np; i++) *fpBuf++ = gexp[i];
    iCount++;

    /* shift the data down to make room for next frame */
    fpTmp1      	= tpC->fpBuf1;
    tpC->fpBuf1		= tpC->fpBuf2;
    tpC->fpBuf2 	= fpTmp1;
    tpC->iBufSaved 	= iKeep;
  }
}
