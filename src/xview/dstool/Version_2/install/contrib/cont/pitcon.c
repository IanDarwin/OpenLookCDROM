/*  -------------------------------------------------------------------

This program is the property of:

                             Cornell University 
                        Center for Applied Mathematics 
                              Ithaca, NY 14853

and may be used, modified and distributed freely, subject to the 
following restrictions:

       Any product which incorporates source code from the dstool
       program or utilities, in whole or in part, is distributed
       with a copy of that source code, including this notice. You
       must give the recipients all the rights that you have with
       respect to the use of this software. Modifications of the
       software must carry prominent notices stating who changed
       the files and the date of any change.

DsTool is distributed in the hope that it will be useful, but 
WITHOUT ANY WARRANTY; without even the implied warranty of FITNESS 
FOR A PARTICULAR PURPOSE.  The software is provided as is without 
any obligation on the part of Cornell faculty, staff or students to 
assist in its use, correction, modification or enhancement.

  -----------------------------------------------------------------  */
#include <stdio.h>
#include <math.h>

#define	TRUE	1
#define	FALSE	0

int
pitcon(df,fpar,fx,ipar,iwork,liw,nvar,rwork,lrw,slv,xr)
int   	(*df)(), (*fx)(), (*slv)();
int	*ipar, *iwork,nvar,liw,lrw;
double	*fpar, *rwork, *xr; 
{
	int	itemp, iwrite, kerror,ipoin,ipoint,istate,nmitl,nmitt,nmitx,nmred;
	int	nmstp,neqn,i,maxcor,imitl,iflag,nred,ipcx;
	int	lxc,lxc1,lxf,lxf1,ltc,ltc1,lwk,lwk1,libeg,lrbeg,ipc,ipram,modnew,it,lim,jac,ihold;
	int	ncor,kci,mod,ipl,jpc,kti,maxtmp,itmp,ktl,lpc,mlstep,imtl,kfi,lwkn;
	int	zwtch,zwtch1,ierror,tangnt();
	double	detsgn,tmax,tmax2,temp,scipl,tcipl,atcipc,atcjpc,tlipc,tcipc,tllim,tclim;
	double	abserr,relerr,hmin,hmax,htan,dir,stepx,cordis,tmp1,tmp2;
	double	arclxc,arclxf,arclxr,one,c,u,tmp,epmach,epsate,epsqrt,tcos,tsin,alfmin,hfact;
	double	qual,xit,del,xav,skale,xfi,xci,xlow,xup;
	double	detsnl,alphlc,atllim,atclim,xcl,xfl,xabs,fa,fb,axrlpc,dirlpc,sn,tsn;
	double	a,b,snl,salphlc,absnlc,hsec,hsecl,curvn,curv,curvx,qcorx,adjus;
/*	double	sqrt(), atan2(), fmod(), coqual(), copysign(), fabs(), sdist2(); */
	double	 coqual(), sdist2();
	void	err_print();

/*    lounit = iwork(8) */
      itemp = iwork[7];			/* 1.  preparations.						*/
      if(itemp > 3) itemp = 3;		/* set machine dependent constants if this is the first call	*/
      if(itemp <= 0) itemp = 0;		/* check user information and set defaults,			*/
      iwrite = itemp;			/* on first call, initialize counters and check norm of f(xr)	*/
      iwork[7] = iwrite;		/* and correct starting point xr if necessary.			*/
      kerror = 0;
      if(nvar <= 1) 
        {kerror = 1;
        if(iwrite >= 1)
          printf("pitcon - number of variables must exceed one \n");
	err_print( kerror, &ipoin, &ierror, ipoint, arclxc, arclxf, arclxr, istate,
		   nmitl, nmitt, nmitx, nmred, nmstp, iwork, rwork);
        return(ierror);}
      neqn = nvar - 1;
      one = 1.0;
/*
   on first call set machine dependent constants
   and initialize basic program constants
*/
      ipoint = iwork[1];		/* on first call set machine dependent constants	*/
      if(ipoint == 0)			/* and initialize basic program constantsa		*/
          {for(i=10; i<=28; i++)
              {rwork[i] = 0.0;
               iwork[i] = 0;}
 
           u = one;
	   while( tmp != one)
	     {u = 0.5*u;
              tmp = one + u;}
           epmach = 2.0*u;
           epsate=8.0*epmach;
           rwork[18] = epmach;
           epsqrt = sqrt(epmach);
           rwork[19] = epsqrt;
           tcos = sqrt(one - epmach);
           tsin = epsqrt;
           alfmin = 2.0*atan2(tsin,tcos);
           rwork[10] = alfmin;
           hfact = 3.0e0;
           rwork[20] = hfact;
/*           maxcor = 10; */
           maxcor = 20;
           iwork[17] = maxcor;}
 
      lxc = 29;				/* for any call set pointers into work arrays */
      lxc1 = lxc + 1;
      lxf = lxc + nvar;
      lxf1 = lxf + 1;
      ltc = lxf + nvar;
      ltc1 = ltc + 1;
      lwk = ltc + nvar;
      lwk1 = lwk + 1;
      libeg=30;
      iwork[13]=libeg;
      iwork[14] = liw;
      lrbeg=lwk + nvar + 1;
      iwork[15] = lrbeg;
      iwork[16] = lrw;
 
      ipc = nvar;						/* check user information and set */
      if((iwork[2] >= 1) && (iwork[2] <= nvar))			/* defaults for integer data      */
	 ipc = iwork[2];
      iwork[2] = ipc;
      ipram = 0;
      if(iwork[3] == 1) ipram = 1;
      iwork[3] = ipram;
      modnew = 0;
      if(iwork[4] != 0) modnew = 1;
      iwork[4] = modnew;
      it = 0;
      if((iwork[5] >= 1) && (iwork[5] <= nvar))
	 it = iwork[5];
      iwork[5] = it;
      lim = 0;
      if((iwork[6] >= 1) && (iwork[6] <= nvar))
	 lim = iwork[6];
      iwork[6] = lim;
      jac=0;
      if((iwork[9] >= 0) && (iwork[9] <= 2))
	 jac=iwork[9];
      iwork[9]=jac;
 
      epsqrt = rwork[19];					/* check user information and set   */
      abserr = epsqrt;						/* defaults for real data           */
      relerr = epsqrt;
      if(rwork[1] > 0.0) abserr = rwork[1];
      if(rwork[2] > 0.0) relerr = rwork[2];
      rwork[1] = abserr;
      rwork[2] = relerr;
      hmin = epsqrt;
      if(rwork[3] > hmin) hmin = rwork[3];
      rwork[3] = hmin;
      tmp=nvar;
      tmp=sqrt(tmp);
      hmax = (hmin > tmp) ? hmin : tmp;
      if(rwork[4] >= hmin) hmax = rwork[4];
      rwork[4] = hmax;
      htan = 0.5*(hmax+hmin);
      tmp = rwork[5];
      if(tmp < 0.0) tmp=-tmp; 
      if((tmp >= hmin) && (tmp <= hmax)) htan = tmp;
      rwork[5] = htan;
      dir = one;
      if(rwork[6] < 0.0) dir = -dir;
 
      arclxc = rwork[12];					/* restore data from work array  */
      arclxf = rwork[13];					/* for use during this step      */
      arclxr = rwork[14];
      epmach = rwork[18];
      epsate=8.0*epmach;
      hfact = rwork[20];
      istate = iwork[10];
      maxcor = iwork[17];
 
      nmitl = iwork[23];
      nmitt = iwork[24];
      nmitx = iwork[25];
      nmred = iwork[26];
      nmstp = iwork[27];
 
/*    at first call check norm of f[xr]; at starting point,
      and if not acceptable, apply newton's method, holding the value
      of the ipc-th component fixed. */
 
      if(ipoint == 0) 
         {
          if(iwrite >= 2)								/* print out note that starting */
              printf("pitcon - correct initial point fixing index %d \n", ipc);		/* point will be corrected      */
 
/*    record starting point as xc in rwork(lxc1)-rwork(lxc+nvar)
      set tangent vector tc in rwork(ltc1)-rwork(ltc+nvar) equal
      to the ipc-th basis vector */
 
          for(i=1; i<=nvar; i++) 
             {rwork[ltc+i] = 0.0;
              rwork[lxc+i] = xr[i];}
          rwork[ltc+ipc] = 1.0;
 
          ihold = ipc;								/* correct the starting point  */
          ierror = 0;
          kerror = corect(df,fpar,fx,ihold,ipar,iwork,&maxcor,&ncor,
                          nvar,rwork,slv,&stepx,&rwork[lwk1-1],xr);
          nmitx = nmitx + ncor;
          if(kerror != 0)
             {if(iwrite >= 1)
                 printf("pitcon - starting point correction failed \n");
	      err_print( kerror, &ipoin, &ierror, ipoint, arclxc, arclxf, arclxr, istate,
	       	     nmitl, nmitt, nmitx, nmred, nmstp, iwork, rwork);
              return(ierror);}
     
          cordis = 0.0;					/* compute and record corrector distance and set xc=xr, xf=xr.  */
          for(i=1; i<=nvar; i++)			/* here xc is rwork(lxc1)-rwork(lxc+nvar) and xf is             */
            {tmp1 = xr[i];				/* rwork(lxf1)-rwork(lxf+nvar)				        */
            kci = lxc + i;
            tmp2 = fabs(tmp1 - rwork[kci]);
            if(tmp2 > cordis) cordis = tmp2;
            rwork[kci] = tmp1;
            rwork[lxf+i] = tmp1;}
          rwork[15] = cordis;
 
/*        qual = coqual(stepx,cordis,epsate,maxcor,mod,ncor);	 error in modnew  argumemt? */

          qual = coqual(stepx,cordis,epsate,maxcor,modnew,ncor);   /* compute and record quality of convergence   */
          rwork[23] = qual;
     
          arclxr = arclxf;				/* record arclength, increase step counter, */
          nmstp = nmstp + 1;				/* and record new values of istate          */
          istate = 1;					/* and ipoint				    */
          ipoint = 1;
          err_print( kerror, &ipoin, &ierror, ipoint, arclxc, arclxf, arclxr, istate,
	       	 nmitl, nmitt, nmitx, nmred, nmstp, iwork, rwork);
          return(ierror);
         }

/*   2.  target point check.  if (it.ne.0) target points are sought.
         check to see if target component it has value xit lying
         between xc(it) and xf(it).  if so, get linearly interpolated
         starting point, and use newton's method to get target point   */

      if((it == 0) || (istate <= 1))
          goto label_300;
      xit = rwork[7];
      if(ipoin == 3 && xit == rwork[28] && it == iwork[11])
          goto label_300;
      zwtch = FALSE; 
      xlow = rwork[lxc+it];
      xup = rwork[lxf+it];
      if(xlow > xup)
         {tmp = xlow;
          xlow = xup;
          xup = tmp;
          zwtch = TRUE;} 
      if(xit < xlow || xit > xup)
          goto label_300;
 
      if(iwrite >= 2)						/* print out forthcoming correction */ 
 	  printf("pitcon - correction of target point \n");	/* of target point		    */
 
      zwtch1=TRUE; 					/* determine target predictor */
      if(xlow != xup)
        {del = xup - xlow;
         xav = 0.5*(xlow + xup);
         if(xit >= xav)
            {skale = (xit - xlow)/del;
             zwtch1= TRUE; }
         else
            {skale = (xit - xup)/del;
	     zwtch = (zwtch == TRUE) ? FALSE : TRUE;
             zwtch1= FALSE; }
         
         if( !zwtch && zwtch1)
            for(i=1; i<=nvar; i++)
               {xci=rwork[lxc+i];
                xr[i]=xci+skale*(rwork[lxf+i]-xci);}
         if(zwtch && !zwtch1)
            for(i=1; i<=nvar; i++)
               {xfi = rwork[lxf+i];
                xr[i] = xfi + skale*(xfi-rwork[lxc+i]);}
         if(zwtch && zwtch1)
            for(i=1; i<=nvar; i++)
               {xfi=rwork[lxf+i];
                xr[i]=xfi+skale*(rwork[lxc+i]-xfi);}
         if(!zwtch && !zwtch1)
            for(i=1; i<=nvar; i++)
               {xci=rwork[lxc+i];
                xr[i]=xci+skale*(xci-rwork[lxf+i]);}
        }
      else
         for(i=1; i<=nvar; i++)
            xr[i] = rwork[lxf+i];
      xr[it]=xit;
 
      ihold=it;							/* correct for target point */
      ierror = 0;
      kerror = corect(df,fpar,fx,ihold,ipar,iwork,&maxcor,&ncor,
                      nvar,rwork,slv,&tmp,&rwork[lwk1-1],xr);
      nmitt = nmitt + ncor;
      if(kerror != 0)
         {if(iwrite >= 1)
             printf("pitcon - target calculation failed \n");
	  err_print( kerror, &ipoin, &ierror, ipoint, arclxc, arclxf, arclxr, istate,
	  	     nmitl, nmitt, nmitx, nmred, nmstp, iwork, rwork);
          return(ierror);}
 
      iwork[11] = it;						/* record values of it and xit, compute */
      rwork[28] = xit;						/* arclength of xr, increase step       */
      arclxr = arclxc + sdist2(nvar,xr,rwork[lxc1],2);		/* counter, and record new value for    */
      nmstp = nmstp + 1;					/* ipoint.				*/
      ipoint = 3;
      err_print( kerror, &ipoin, &ierror, ipoint, arclxc, arclxf, arclxr, istate,
	  	 nmitl, nmitt, nmitx, nmred, nmstp, iwork, rwork);
      return(ierror);
 
/*     3. tangent and local continuation parameter calculation.  unless
          the tangent and limit point calculations were already performed
          (because the loop was interrupted for a limit point), set up and
          solve the equation for the tangent vector.  force the tangent
          to be of unit length, and force the ipl-component to have the same
          sign as the ipl-th component of the previous secant vector, or (on
          first step) the same sign as the input direction dir.  set the
          local parameter ipc to the location of the largest component
          of the tangent vector, unless a limit point in that direction
          appears to be approaching and another choice is available.         */
 
label_300: 
      if (istate == 3)
          goto label_600;
 
/*    store old tangent in work array rwork(lwk1)-rwork(lwk+nvar), compute
      new tangent for current point xf in rwork(lxf1)-rwork(lxf+nvar).
      the tangent is called tc and stored in rwork(ltc1)-rwork(ltc+nvar).
      note that for a normal return the current point xf will be stored
      as xc in rwork(lxc1)-rwork(lxc+nvar) and hence that tc will then
      correspond to the point xc					     */
 
      ipl = ipc;
      for(i=1; i<=nvar; i++)
         rwork[lwk+i] = rwork[ltc+i];
      detsnl = rwork[17];
      kerror = tangnt(&detsgn,fx,df,fpar,ipc,ipar,iwork,nvar,
                           rwork,slv,&rwork[ltc1-1],&rwork[lxf1-1]);
      if(kerror != 0)
        {if(iwrite >= 1)
             printf("tangnt - tangent calculation failed \n");
         err_print( kerror, &ipoin, &ierror, ipoint, arclxc, arclxf, arclxr, istate,
	  	    nmitl, nmitt, nmitx, nmred, nmstp, iwork, rwork);
         return(ierror);}
 
      tmax=0.0;				/* compute new continuation parameter */
      tmax2=0.0;
      jpc=0;
      ipc=0;
      for(i=1; i <= nvar; i++)
         {temp=fabs(rwork[ltc+i]);
          if(temp > tmax)
             {jpc=ipc;
              ipc=i;
              tmax2=tmax;
              tmax=temp;}
          else if(temp > tmax2)
             {jpc=i;
              tmax2=temp;}
         }
      if(jpc == 0) jpc=ipc;
      if(ipram == 1) jpc=ipc;
 
/*    adjust sign of the tangent vector.
      compare the sign of the component tc(ipl) with the
      sign of xf(ipl)-xc(ipl). if a target or limit point
      has been computed between xc and xf then use xf(ipl)-xr(ipl)
      instead. at the first step compare with the user input dir.
      if these signs differ, change the sign of the tangent vector
      and the sign of the determinant.					*/
 
      dir = rwork[6];
      scipl = dir;
      kti = ltc + ipl;
      tcipl = rwork[kti];
      if(istate > 1)
         {scipl = rwork[lxf+ipl]; 
          tmp = rwork[lxc+ipl];
          if((ipoint == 3) || (ipoint == 4)) tmp = xr[ipl];
          scipl = scipl - tmp;}
      if(copysign(one,tcipl) != copysign(one,scipl))
         {for(i=ltc1; i<=lwk; i++)
            rwork[i] = -rwork[i];
          detsgn = -detsgn;}
 
/*      record new state if we are not at a starting point
        record the new sign of the determinant                     */
 
      if(istate > 1) istate = 3;
      rwork[17] = detsgn;
 
/*    the location of the largest component of the tangent vector
      will be used for the local parameterization of the curve unless
      a limit point in ipc appears to be coming.
      to check this, we compare tcipc =tc(ipc) and the
      second largest component tcjpc =tc(jpc).  if tcjpc is no less
      than .1 of tcipc, and tc(jpc) is larger than tl(jpc),
      whereas tc(ipc) is less than tl(ipc), we will reset the
      local parameter ipc to jpc.
      but if ipram.ne.0, ignore the check				*/
 
      if(ipram != 1)
         {atcipc = fabs(rwork[ltc+ipc]);
          atcjpc = fabs(rwork[ltc+jpc]);
          if(jpc != ipc && istate > 1)
             {tlipc = rwork[lwk+ipc];
              tcipc = rwork[ltc+ipc];
              tmp = fabs(rwork[lwk+jpc]);
	      maxtmp = (0.1*atcipc > tmp) ? 0.1*atcipc : tmp;
              if((copysign(one,tcipc) == copysign(one,tlipc)) && 	/* detect limit points */
                         (atcipc < fabs(tlipc)) && (atcjpc >= maxtmp))
                 {if(iwrite >= 3)
		  printf("pitcon - expect limit point in variable %d \n",ipc);
                  itmp = ipc;
                  ipc = jpc;
                  jpc = itmp;
                 }
             }
         }
 
/* print out choices of continuation index */
 
      itemp = 0;							/* print out choices of */
      if(iwrite >= 3 && ipram == 0) 					/* continuation index   */
         {printf("pitcon - continuation index: first choice %d \n",ipc);
          printf("pitcon -                    second choice %d \n",jpc); }
 
/*    record values of the component tcipc = tc(ipc) of
      new tangent vector, as well as of the corresponding component
      tlipc = wk(ipc) of the old tangent vector.
      set dir = sign of the determinant.
      for use in the limit point computation record the
      component tclim = tc(lim) of the new tangent vector		*/
 
      iwork[2] = ipc;
      iwork[12] = jpc;
      tcipc = rwork[ltc+ipc];
      rwork[24] = tcipc;
      tlipc = rwork[lwk+ipc];
      rwork[25] = tlipc;
      dir = detsgn;
      rwork[6] = dir;
      if(lim > 0)
         {tllim = rwork[26];
          rwork[27] = tllim;
          ktl = ltc + lim;
          tclim = rwork[ktl];
          rwork[26] = tclim;}
 
      if(istate <= 1)		/* compute alphlc, the angle between old tangent and tangent at xc */
	  goto label_600;
      tcos = 0.0; 
      for( i=1; i<=nvar; i++)
         tcos = tcos + rwork[lwk+i]*rwork[ltc+i];
      if(tcos > 1.0) tcos=1.0;
      if(tcos < -1.0) tcos=-1.0;
      tsin=sqrt(1.0-tcos*tcos);
      alphlc=atan2(tsin,tcos);
      rwork[11] = alphlc;

/* ----------------------------------------------------------------------
 
   bifurcation routine can be inserted here
 
   ---------------------------------------------------------------------- */

      if((istate > 1) && (detsgn != detsnl))
         if(iwrite >= 2)						/* print out note of anticipated  */
             printf("pitcon - bifurcation point detected \n");		/* bifurcation                    */
 
/*    4.  limit point check.  if (lim.ne.0) check to see if
      old and new tangents differ in sign of lim-th component.
      if so, attempt to compute a point xr between xc and xf
      for which tangent component vanishes			*/
    
      if((lim <= 0) || (istate <= 1) || (copysign(one,tclim) == copysign(one,tllim)))
         goto label_600;
 
      if(iwrite >= 3)						/* print out note of detection of limit point */
         printf("pitcon - correction of limit point \n");
 
      atllim=fabs(tllim);			/* see if either endpoint can be accepted as limit point. */
      if (atllim <= 0.5*abserr) 		/* if xc is limit point, work already contains tangent at xc. */
         {for(i=1; i<=nvar; i++)
            xr[i] = rwork[lxc+i];
          goto label_490;}
      atclim=fabs(tclim);
      if (atclim <= 0.5*abserr) 
         {for( i=1; i<=nvar; i++)
            {xr[i] = rwork[lxf+i]; 
            rwork[lwk+i] = rwork[ltc+i];}
          goto label_490;}
 
      xcl=rwork[lxc+lim];		/* if interval is too small, accept one endpoint anyway. */
      xfl=rwork[lxf+lim];
      del=fabs(xfl-xcl);
      xabs= (fabs(xcl) > fabs(xfl)) ? fabs(xcl) : fabs(xfl);
      if (del <= epsate*(1.0+xabs)) 
         {if (atllim > atclim)
            {for(i=1; i<=nvar; i++)
                {xr[i] = rwork[lxf+i];
                 rwork[lwk+i] = rwork[ltc+i];}
             goto label_490;}
         }
      else
         for(i=1; i<=nvar; i++) 
            xr[i] = rwork[lxc+i];
 
      a=0.0;		/* begin root-finding iteration with interval (0,1) and */
      fa=tllim;		/* function values tllim, tclim.		        */
      b=1.0;
      fb=tclim;
 
/*    set lpc to the index of maximum entry of secant
      (except that lpc must not equal lim)
      and save the sign of the maximum component in dirlpc
      so that new tangents may be properly signed.		*/
    
      axrlpc = 0.0;
      for(i=1; i<=nvar; i++)
         {tmp = 0.0;
          if(i != lim) tmp = fabs(rwork[lxf+i]-rwork[lxc+i]);
          if(tmp > axrlpc) 
             {axrlpc = tmp;
              lpc = i;}
         }                 
      dirlpc=copysign(one,rwork[lxf+lpc]-rwork[lxc+lpc]);
 
      if(atclim >= atllim)		/* set first approximation to root to whichever endpoint */
         {sn=0.0;			/* has smallest lim-th component of tangent		 */
          tsn=tllim;
          for(i=1; i<=nvar; i++)
             xr[i] = rwork[lxc+i];
         }
      else
         {sn=1.0;
          tsn=tclim;
          for(i=1; i<=nvar; i++)
             xr[i] = rwork[lxf+i];
         }
 
/*    call rootfinder for approximate root sn.  use linear combination
      of previous root snl, and one of 0.0 and 1.0 to get a starting
      point for correction.  return to curve on line x(lpc)=constant,
      compute tangent there, and set function value to tangent(lim)    */
 
      mlstep=25;
      imitl=0;
label_441:        
      snl=sn;
      kerror = root(a,fa,b,fb,sn,tsn,&imitl,&iflag,epmach);
      nmitl=nmitl+1;
      if(kerror != 0)
        {if(iwrite >= 1)
            printf("pitcon - wrong interval for rootfinder \n");
	 err_print( kerror, &ipoin, &ierror, ipoint, arclxc, arclxf, arclxr, istate,
		   nmitl, nmitt, nmitx, nmred, nmstp, iwork, rwork);
         return(ierror);}
      if(iflag == -1 || iflag == 0)
         goto label_490;
 
/*    find whether sn lies in (0.0,snl) or (snl,1.0).
      use appropriate linear combination to get starting point.  */
 
      if (sn <= snl) 
        {if(snl <= 0.0) skale=0.0;
         if(snl > 0.0) skale=sn/snl;
         if(skale <= 0.0) skale = 0.0;
         if(skale >= 1.0) skale = 1.0;
         for(i=1; i<=nvar; i++)
            {kci = lxc + i;
        }    xr[i] = rwork[kci] + skale*(xr[i]-rwork[kci]);}
      else
        {if(snl >= 1.0) skale=0.0;
         if(snl < 1.0) skale=(1.0-sn)/(1.0-snl);
         if(skale <= 0.0) skale = 0.0;
         if(skale >= 1.0) skale = 1.0;
         for(i=1; i<=nvar; i++)
            {kfi = lxf + i;
             xr[i] = rwork[kfi] - skale*(rwork[kfi]-xr[i]);}
        }
      ierror=0;
      ihold=lpc;

      kerror = corect(df,fpar,fx,ihold,ipar,iwork,&maxcor,&ncor,nvar,rwork,slv,&tmp,&rwork[lwk1-1],xr);
      if(kerror != 0)
        {if(iwrite >= 1)
            printf("pitcon - corrector failed in limit calculation \n");
	 err_print( kerror, &ipoin, &ierror, ipoint, arclxc, arclxf, arclxr, istate,
	  	    nmitl, nmitt, nmitx, nmred, nmstp, iwork, rwork);
         return(ierror);}

      kerror = tangnt(&tmp,fx,df,fpar,lpc,ipar,iwork,nvar,rwork,slv,&rwork[lwk1-1],xr);
      if(kerror != 0) 
        {if(iwrite >= 1)
            printf("pitcon - tangent error in limit calculation \n");
	 err_print( kerror, &ipoin, &ierror, ipoint, arclxc, arclxf, arclxr, istate,
	  	    nmitl, nmitt, nmitx, nmred, nmstp, iwork, rwork);
         return(ierror);}
 
      if (dirlpc != copysign(one,rwork[lwk+lpc])) 	/* adjust the sign of the tangent so that     */
         {lwkn = lwk + nvar;				/* the lpc-th component has the same          */
          for(i=lwk1; i<=lwkn; i++)			/* sign as the lpc-th component of the secant */
             rwork[i] = -rwork[i];
         }     
 
/*    see if we can accept the new point because tangent(lim) is small
      or must stop because 25 iterations taken,
      or if we must go on.						*/
 
      tsn=rwork[lwk+lim];
      if(fabs(tsn) <= abserr)
          goto label_490;
      if(imitl < mlstep)
          goto label_441;
 
      kerror = 8;		/* limit point iteration has failed */
      if(iwrite >= 1)
         printf("pitcon - too many steps in limit calculation \n");
 
/*    limit point iteration successful
      compute arclength, increase step counter, and record ipoint	*/
 
label_490:      
      arclxr = arclxc + sdist2(nvar,xr,rwork[lxc1],2);
      nmstp = nmstp + 1;
      ipoint = 4;
      err_print( kerror, &ipoin, &ierror, ipoint, arclxc, arclxf, arclxr, istate,
	  	 nmitl, nmitt, nmitx, nmred, nmstp, iwork, rwork);
      return(ierror);
 
/* -----------------------------------------------------------------------------
    
      6.  step length computation.  compute steplength htan
    
      the formulas underlying the algorithm are
    
      let
    
      alphlc = maximum of arccos(tl,tc) and alfmin = 2*arccos(1-epmach)
      hsec   = norm(xc-xf)
      hsecl  = norm(xl-xc)
      absnlc = abs(sin(.5*alphlc))
      curv   = last value of the curvature
      curvn  = 2*absnlc/hsec
      cordis = corrector distance to current continuation point.
               force cordis to lie between .01*hsec and hsec.
               unless (cordis.eq.0.0), because the predicted point was
               accepted.  in such a case, set htan=hfact*hsec
               instead of using first estimate for htan.
    
      then
    
      curvx = curvn + hsec*(curvn-curv)/(hsec+hsecl)
      a simpler formula is used if we do not have data at two previous
      points.
    
      if (istate.ge.2) curvx must be at least as large as
      the maximum of 0.001 and .01/hsec
    
      first estimate   (unless (cordis.eq.0.0) )
    
      htan  = sqrt(2*qual*cordis/curvx)
    
      adjusted value
    
      htan = htan*(1.0 + htan*(tc(ipc)-tl(ipc))/(2*hsec*tc(ipc)))
    
      readjustment and truncations
    
      if stepsize reduction occurred during last corrector process,
      htan is forced to be less than (hfact-1)*hsec/2.
    
      htan must lie between (hsec/hfact) and (hsec*hfact).
    
      htan is always forced to lie between hmin and hmax.
    
    ----------------------------------------------------------------------------- */
    
label_600:         
      if (istate > 1)
         {salphlc = rwork[11];
          if(alphlc < rwork[10]) alphlc = rwork[10];
          absnlc=fabs(sin(0.5*alphlc)); 
     
          hsec = rwork[21];				/* compute new curvature data */
          hsecl = rwork[22];
          curvn = 2.0*absnlc/hsec;
          curv = rwork[16];
          if(curv == 0.0) curv = curvn;
          curvx = curvn;
          if(hsecl != 0.0)
            curvx = curvn + hsec*(curvn-curv)/(hsec+hsecl);
          tmp = (0.001 > (0.01/hsec)) ? 0.001 : (0.01/hsec);
          curvx = (curvx > tmp) ? curvx : tmp;
          rwork[16] = curvn;
     
          htan = hfact*hsec;			/* if convergence distance is zero, set estimate to hfact*hsec. */
          cordis = rwork[15];			/* else truncate qual*cordis to lie between .01*hsec and hsec.  */
          qual = rwork[23];
          if(cordis != 0.0) 
             {tmp = 0.01*hsec;
              qcorx = qual*cordis;
              qcorx = (qcorx > tmp) ? qcorx:tmp;
             qcorx = (qcorx < hsec) ? qcorx:hsec;
             htan = sqrt(2.0*qcorx/curvx);}
     
          if(iwork[18] > 0) 
	       htan=(htan < 0.5*(hfact-1.0)*hsec)? htan:0.5*(hfact-1.0)*hsec;
          tcipc= rwork[24];
          tlipc = rwork[25];
          adjus=1.0+(1.0-tlipc/tcipc)*(0.5*htan)/hsec;	/* adjust for curvature in continuation */
          if(ipram != 1)htan = htan*adjus;			/* parameter direction, then truncate   */
          htan = (htan > hsec/hfact)? htan:hsec/hfact;
          htan = (htan < hsec*hfact)? htan:hsec*hfact;
          htan = (htan > hmin)? htan:hmin;
          htan = (htan < hmax)? htan:hmax;
         }
/*
    
      7.  prediction and correction steps.  using xr=xc+htan*tc
      as starting point, correct xr with a full or modified newton
      iteration.  if corrector fails, reduce stepsize used for predictor
      point, and try again.  correction will be abandoned if stepsize
      falls below hmin.
    
*/
      nred = 0;
      ipc = iwork[2];
      jpc = iwork[12];
      ipcx=ipc;
 
label_710:      
 
label_720:        
      if(iwrite >= 2)
          printf("pitcon - use predictor stepsize %lg \n",htan);
      for(i=1; i<=nvar; i++)
         xr[i] = rwork[lxf+i] + htan*rwork[ltc+i];
 
label_740: 
      if(iwrite >= 2 && ipram == 0)
         printf("pitcon - corrector will fix index %d \n",ipcx);
      ihold=ipcx;
      kerror = corect(df,fpar,fx,ihold,ipar,iwork,&maxcor,&ncor,nvar,rwork,slv,&stepx,&rwork[lwk1-1],xr);
      nmitx = nmitx + ncor;
      if(kerror != 0)
/*    only very fatal errors should abort the continuation
      point correction process					*/
    
         {if(kerror <= 2)
             {printf("pitcon - fatal error during correction \n");
	      err_print( kerror, &ipoin, &ierror, ipoint, arclxc, arclxf, arclxr, istate,
		       nmitl, nmitt, nmitx, nmred, nmstp, iwork, rwork);
              return(ierror);}
/*
      for cases where corrector failed to converge,
      and the value of ipcx was not jpc,
      and the user has not fixed the continuation parameter
      keep the same stepsize but use jpc instead of ipcx.
*/  
          if((ipram != 1) && (ipcx != jpc)) 
             {ipcx=jpc;
              if(kerror == 5) goto label_740;
              nmred = nmred + 1;
              goto label_720;}		
     
          ipcx=ipc;		/* if jpc fails as ipcx value, return to ipc.  */
     
          htan = htan/hfact;	/* no convergence, try a smaller stepsize */
          if (htan < hmin) 
            kerror = 4;
            if(iwrite >= 1)
               {printf("pitcon - stepsize fell below minimum \n");
	        err_print( kerror, &ipoin, &ierror, ipoint, arclxc, arclxf, arclxr, istate,
	       	      nmitl, nmitt, nmitx, nmred, nmstp, iwork, rwork);
                return(ierror);}
          nred = nred+1;
          nmred = nmred + 1;
     
          if(iwrite >= 2)				/* print out note of step-reduction */
             printf("pitcon - step reduced \n"); 
     
          if (kerror == 5)
             {skale = 1.0/hfact;
              for(i=1; i<=nvar; i++)
                 {kfi = lxf + i;
                  xr[i] = rwork[kfi] + skale*(xr[i]-rwork[kfi]);}
              goto label_740;}
          else
              goto label_710;
         }
/*    8.  successful step.  store information before return.
           update step counter and record new values of istate and ipoint.
           update old and current continuation points.
           update size of secant step.
           update the size of the corrector step.
           update the convergence quality factor.				*/
      
      nmstp = nmstp + 1;
      iwork[2] = ipcx;
      istate = 2;
      ipoint = 2;
 
      iwork[18] = nred; 			/* update hsec, secant step */
      rwork[5] = htan;
      rwork[22] = rwork[21];
      if(ipram != 1) ipc = ipcx;
      hsec = sdist2(nvar,xr,&rwork[lxf1],2);
      rwork[21] = hsec;
      arclxc=arclxf;
      arclxf=arclxc+hsec;
      arclxr = arclxf;
 
      cordis = 0.0;
      for(i=1; i<=nvar; i++)
         {kfi = lxf + i;			/* update values of computed points.  store old point in        */
         tmp1 = rwork[kfi];			/* rwork(lxc1)-rwork(lxc+nvar) and new point in rwork(lxf1)-    */
         rwork[lxc+i] = tmp1;			/* rwork(lxf+nvar). compute and store cordis, the maximum norm  */
         tmp2 = xr[i];				/* of the corrector distance.				        */
         rwork[kfi] = tmp2;
         tmp1 = tmp2 - (tmp1+htan*rwork[ltc+i]);
         if(fabs(tmp1) > cordis) cordis=fabs(tmp1);}
      if(ncor == 0) cordis=0.0;
      rwork[15] = cordis;
 
/*    update qual, a convergence quality factor which
      multiplies cordis to give an effective radius of convergence
      at xf.	*/

      qual = coqual(stepx,cordis,epsate,maxcor,modnew,ncor);
      rwork[23] = qual;
      err_print( kerror, &ipoin, &ierror, ipoint, arclxc, arclxf, arclxr, istate,
		 nmitl, nmitt, nmitx, nmred, nmstp, iwork, rwork);
      return(ierror);
}

void  
err_print( kerror, ipoin, ierror, ipoint, arclxc, arclxf, arclxr, istate,
		 nmitl, nmitt, nmitx, nmred, nmstp, iwork, rwork) 
int	kerror, *ipoin, *ierror, ipoint,istate, nmitl, nmitt, nmitx, nmred, nmstp, *iwork;
double	arclxc, arclxf, arclxr, *rwork;
{
 
      if(kerror != 0)
	  switch(kerror){
	     case 1:
		printf("pitcon - storage or data error \n");
		break;
	     case 2:
		printf("pitcon - function or derivative error \n");
		break;
	     case 3:
		printf("pitcon - solver failed \n");
		break;
	     case 4:
		printf("pitcon - iteration failed \n");
		break;
	     case 5:
		printf("pitcon - too many corrector steps \n");
		break;
	     case 6:
		printf("pitcon - tangent vector zero \n");
		break;
	     case 7:
		printf("pitcon - root finder failed \n");
		break;
	     case 8:
		printf("pitcon - too many limit point steps \n");
		break;
             default:
		printf("pitcon - unidentified error = %d \n", kerror );
		kerror = 9;
		break;
            }
      *ipoin=ipoint;
      *ierror = kerror;
      rwork[12] = arclxc;
      rwork[13] = arclxf;
      rwork[14] = arclxr;
      iwork[1]  = ipoint;
      iwork[10] = istate;
      iwork[23] = nmitl;
      iwork[24] = nmitt;
      iwork[25] = nmitx;
      iwork[26] = nmred;
      iwork[27] = nmstp;
}  
