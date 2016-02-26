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
#include <constants.h>
#include "continue.h"

extern struct  Cont_Cntl_Ds            cont_ds;

 
/* ---------------------------------------------------------------------
    
      subroutine corect performs the correction of an approximate
      solution of the equation f(xr)=0.0. for the correction  either
      newton's method or the chord newton method is used. in the latter
      case the jacobian is  evaluated only at the starting point.
      if b is the value of xr(ihold) for the input starting point,
      then the augmenting equation is xr(ihold)=b, that is, the ihold-th
      component of xr is to be held fixed.  the augmented system to be
      solved is then dfa(xr,ihold)*(-delx)=fa(xr)
    
      acceptance and rejection criteria-
    
      let ncor denote the current iteration index; that is,  ncor=0 for
      the predicted point which serves as starting point.
      after each iteration, the current point is accepted
      if any one of the following conditions is valid
    
      strong acceptance criterion
    
      1.  fnrm.le.abserr and stepx.le.(abserr+relerr*xnrm))
    
      weak acceptance criteria
    
      2.  (ncor.eq.0) and
          fnrm.le.(.5*abserr)
      3.  fnrm.le.epsate or stepx.le.epsate
      4.  (ncor.ge.2) and
          (fnrm+fnrml).le.abserr and
          stepx.le.8*(abserr+relerr*xnrm)
      5.  (ncor.ge.2) and
          fnrm.le.8.0*abserr and
          (stepx+stepxl).le.(abserr+relerr*xnrm)
    
      after each step of the iteration, the process is to be aborted if
      any of the following conditions holds
    
      1.  fnrm.gt.(fmp*fnrml+abserr)
      2.  (ncor.ge.2) and
          (stepx.gt.(fmp*stepxl+abserr))
      3.  ncor.ge.maxcor (number of iterations exceeded).
    
      here fmp=2.0 for ncor.eq.1, fmp=1.05 for ncor.gt.1
    
      error conditions: procedure returns
             1  data or storage error
             2  error in function or derivative call
             3  solver failed
             4  unsuccessful iteration
             5  too many corrector steps

      subprograms called

             conmsg, df, fx, slv


  ---------------------------------------------------------------------- */

int
corect(df,fpar,fx,ihold,ipar,iwork,maxcor, ncor,nvar,rwork,slv,stepx,wk,xr)
int	ihold, *ipar, *iwork, *maxcor, *ncor, nvar;
double	*fpar, *rwork, *stepx, *wk, *xr; 
int   	(*df)(), (*fx)(), (*slv)(); 
{
      static int	first=1, debug=FALSE;    
      static FILE       *filename;
      int           	n_par = *((int *) pm( GET, "Model.Param_Dim", NULL ));

      int	i,j,kk,icall,ie,ifmax,iwrite,ixmax,job,k,ksmax,
		lidim,liw,lounit,lrdim,lrw,maxnew,modnew;
      double	abserr,conmsg,dets,epmach,epsate,fmp,fnrm,fnrml,
		relerr,stepxl,tlstep,tmp1,tmp2,xnrm,xvalue, fabs(), temp;
 
      abserr = rwork[1];				/*  initialize */
      relerr = rwork[2];
      epmach = rwork[18];
      epsate=8.0*epmach;
      modnew = iwork[4];
      iwrite = iwork[7];
      lounit = iwork[8];
      liw = iwork[14];
      lrw = iwork[16];
      *maxcor = iwork[17];
      *ncor=0;
      maxnew=*maxcor;
      if(modnew == 1) maxnew=2*(*maxcor);
      fmp=2.0;
      icall=1;
      *stepx=0.0;
      xvalue=xr[ihold];
 
      ie=0;						/* store initial function value */
      ie=fx(nvar,fpar,ipar,xr,wk);			/* in the work vector wk        */
      iwork[22] = iwork[22]+1;

      if(ie != 0) 
	  {fprintf(stderr,"corect - error flag received from user function \n");
           return(2);}
      else if (ie==0 && iwrite >= 2)
	  {
           fprintf(stderr,"\n %d Starting x() = ");
	   for(j=1; j<=nvar; j++) fprintf(stderr," %lg ",xr[j]); 
           fprintf(stderr,"\n %d Starting F() = ");
	   for(j=1; j<=nvar; j++) fprintf(stderr," %lg ",wk[j]);
           fprintf(stderr,"\n");
	  }

      wk[nvar]=xr[ihold]-xvalue;
      fnrm = 0.0;
      xnrm = 0.0;
      for(i=1; i<=nvar; i++)
         {if(fabs(wk[i]) >= fnrm) 
            {fnrm = fabs(wk[i]);
             ifmax = i;}
          if(fabs(xr[i]) >= xnrm)
            {xnrm = fabs(xr[i]);
             ixmax = i;}
	 }
 
      if(iwrite >= 2) 
	 fprintf( stderr,"corect - residual %d = %lg  i=%d \n",*ncor, fnrm, ifmax);
 
/* check weak acceptance of input point */
 
      if (fnrm <= 0.5*abserr)     
        {if(iwrite >= 2) 
	    fprintf(stderr,"corect - weak acceptance criterion used \n");
         return(0);}
 
      for(i=1; i<=maxnew; i++)					/* newton iteration loop begins */
          {*ncor=i;
           if(modnew == 1 && *ncor == (*maxcor)) icall=1;
 
           job = 0;						/* solve system fprime*(-delx)=fx */
           if(icall == 0 && modnew == 1) job=1;			/* for newton direction delx      */
           ie=0;

           ie = slv(&dets,fx,df,fpar,ihold,ipar,iwork,liw,job,nvar,rwork,lrw,xr,wk);

           if(ie != 0)
	      {fprintf(stderr,"corect - error flag= %d returned from solver \n",ie);
               return(ie);}
    
           xnrm = 0.0;						/* compute length of last step, update */
           stepxl = *stepx;						/* the iterate xr and get its norm.    */
           *stepx = 0.0;
           for(k=1; k<=nvar; k++)
              {tmp1 = wk[k];
               if(fabs(tmp1) >= *stepx) 
                  {ksmax = k;
                   *stepx = fabs(tmp1);}
               tmp2 = xr[k]-tmp1;
               xr[k] = tmp2;
               if(fabs(tmp2) >= xnrm)
                 {xnrm = fabs(tmp2);
                  ixmax = k;}
	      }

           if(modnew == 1) icall=0;				/* compute new function value and its norm */
           ie=0;
           ie = fx(nvar,fpar,ipar,xr,wk);

           iwork[22] = iwork[22]+1;
           if(ie != 0) 
              {fprintf(stderr,"corect - error flag %d  received from user function. \n", ie);
               return(2);}
           else if (ie==0 && iwrite >= 2)
	      {
               fprintf(stderr,"\n %d   F() = ",i);
	       for(j=1; j<=nvar; j++) fprintf(stderr," %lg ",wk[j]);
               fprintf(stderr,"\n");
	      }

           wk[nvar]=xr[ihold]-xvalue;
           fnrml=fnrm;
           fnrm = 0.0;
           for(k=1; k<=nvar; k++)
              {if(fabs(wk[k]) >= fnrm)
                 {fnrm = fabs(wk[k]);
                  ifmax = k;}
              }          
   
	   /* print out new function norm and step length */
           if(iwrite >= 2) 
               {fprintf(stderr,"corect - step  %d  =  %lg, i= %d \n", *ncor,*stepx,ksmax);
                fprintf(stderr,"corect - residual  %d         %lg, i= %d \n", *ncor,*stepx,ksmax);}
    
           tlstep=abserr+relerr*xnrm;				/* check for strong acceptance criterion */
           if (fnrm <= abserr && *stepx <= tlstep)
               return(0);
    
           if (fnrm <= epsate || *stepx <= epsate)      	/* check for weak acceptance criteria or whether the */
             if(iwrite >= 2)				/* the newton iteration should be aborted */
	        {fprintf(stderr,"corect - weak acceptance criterion used \n");
                 return(0); }
           if (*ncor != 1) 
	      {if ((fnrm+fnrml) <= abserr && *stepx <= 8.0*tlstep) 
                  {if(iwrite >= 2)
		      fprintf(stderr,"corect - weak acceptance criterion used \n");
                   return(0); } 
               if (fnrm <= 8.0*abserr && (*stepx+stepxl) <= tlstep) 
                  {if(iwrite >= 2)
		      fprintf(stderr,"corect - weak acceptance criterion used \n");
                   return(0); } 
               if (*stepx > (fmp*stepxl+abserr))         
	          {if(iwrite >= 2)
		      fprintf(stderr,"corect - correction not decreasing \n");
                      return(4); 
		  }
	      }
	   if (fnrm > (fmp*fnrml+abserr)) 
              {if(iwrite >= 2)
	         fprintf(stderr,"corect - residual not decreasing \n");
                 return(4); 
	       }
           fmp=1.05;
	}
 
      if(iwrite >= 2)							/* fall through loop if maximum */
	  fprintf(stderr,"corect - convergence too slow \n");			/* number of steps taken */
      return (5);
}


