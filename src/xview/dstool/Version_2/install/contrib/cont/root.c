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


 
/* ---------------------------------------------------------------------
 
   subroutine root seeks a root of the equation f(x)=0.0,
   given a starting interval (a,b) on which f changes sign.
      on first call to root, the interval and function values fa and fb
      are input and an approximation u for the root is returned.
      before each subsequent call, the user evaluates fu=f(u), and the
      program tries to return a better approximation u.
    
      see reference 7 of pitcon for details.
    
      variables
    
      a      = is one endpoint of an interval in which f changes sign.
      fa     = the value of f(a).  the user must evaluate f(a) before
               first call only.  thereafter the program sets fa.
      b      = is the other endpoint of the interval in which
               f changes sign.  note that the program will return
               immediately with an error flag if fb*fa.gt.0.0.
      fb     = the value of f(b).  the user must evaluate f(b) before
               first call only.  therafter the program sets fb.
      u      = on first call, u should not be set by the user.
               on subsequent calls, u should not be changed
               from its output value, the current approximant
               to the root.
      fu     = on first call, fu should not be set by the user.
               thereafter, the user should evaluate the function
               at the output value u, and return fu=f(u).
      kount  = a counter for the number of calls to root.  kount
               should be set to zero on the first call for a given
               root problem.
      iflag  = program return flag
               iflag=-1  the current bracketing interval with
                         endpoints  stored in a and b is less than
                               4*epmach*abs(u)+epmach
                         hence u should be accepted as the root.
                         the function value f(u) is stored in fu.
               iflag= 0  the input value fu is exactly zero, and
                         u should be accepted as the root.
    
               iflag.gt.0 the current approximation to the root is
                         contained in u.  if a better approximation is
                         desired, set fu=f(u) and call root again.
                         the value of iflag indicates
                         the method that was used to produce u.
    
               iflag= 1  bisection was used.
               iflag= 2  linear interpolation (secant method).
               iflag= 3  inverse quadratic interpolation.
    
      kerror = global error flag.
               kerror=0 no error found
               kerror=7  on first call, fa*fb.gt.0.0. and hence
                         the given interval is unacceptable.
      epmach = the relative machine precision
    
      local variables include
    
      halfub = signed halfwidth of interval.  during segment 3, the
               change of sign interval is (u,b) or (b,u).  the midpoint
               is xmid=u+halfub, regardless of orientation.
      sdel1  = size of change in sign interval.
      sdel2  = previous value of sdel1.
      sdel3  = previous value of sdel2.
      sdel4  = previous value of sdel3.
      step   = the new root is computed as a correction to u of the
               form u(new)=u(old)+step.
      toler  = a number we accept as small when examining interval
               size or step size.  toler=2.0*epmach*abs(u) + epmach is
               a minimum below which we do not allow such values to fall.
    
      the program uses the following fortran functions
    
         abs, sign
    
   ------------------------------------------------------------------------ */
int 
root(a,fa,b,fb,u,fu,kount,iflag,epmach)
int	*kount, *iflag;
double	*a, *fa, *b, *fb, *u, *fu, epmach;
{
	double	halfub,one,p,q,r,s,sdel1,sdel2,sdel3,sdel4,step,toler;
/*	double	copysign(), fabs(); */
 
      if (*kount <= 0) 							/* segment 1   		          */
	 {if((*fa > 0.0 && *fb > 0.0) || (*fa < 0.0 && *fb < 0.0)) 		/* first call handled specially.  */
            {*kount = 0;							/* set certain values only for    */
             return(7);}						/* initial call with kount=0      */
          one = 1.0;
          *kount=1;
          sdel1=2.0*fabs(*b-*a);
          sdel2=2.0*sdel1;
          sdel3=2.0*sdel2;
          *u=*b;
	  *b=*a;
          *fu=*fb;
          *fb=*fa; 
	 }
      else
         {*kount = *kount + 1;				/* increment counter and check whether f(u) = 0. */
          if(*fu == 0.0)
             {*iflag = 0;
              return(0);}
          if(copysign(one,*fu) == copysign(one,*fb))	/* if fu and fb have the same sign overwrite b with a */
             {*b=*a;
              *fb=*fa;}
         }      
 
      if(fabs(*fb) < fabs(*fu)) 			/* segment 2   rearrange points and function values if */
         {*a=*u;					/* necessary so that  abs(fu) < abs(fb)                */
          *u=*b;
          *b=*a;
          *fa=*fu;
          *fu=*fb;
          *fb=*fa; }
 
      toler=2.0*epmach*fabs(*u)+epmach;			/* segment 3   check for acceptance because of small interval */
      halfub=0.5*(*b-*u);				/* current change in sign interval is (b,u) or (u,b).         */
      sdel4=sdel3;
      sdel3=sdel2;
      sdel2=sdel1;
      sdel1=fabs(*b-*u);
      if(fabs(halfub) < toler) 
         {*iflag=-1;
          *a=*u;
          *fa=*fu;
          return(0); }
 
      if(fabs(*fu) >= fabs(*fa))				/* segment 4   compute new approximant        */ 
        {*iflag=1;						/* to root of the form                        */
         step=halfub;						/* u(new)=u(old)+step.                        */
         *a=*u;							/* methods available are linear interpolation */
         *fa=*fu;						/* inverse quadratic interpolation            */ 
         if(fabs(step) <= toler) step=copysign(toler,halfub);	/* and bisection.                             */
         *u=*u+step;
         return(0);}
 
      if(*a == *b) 						/* attempt linear interpolation if only       */
         {*iflag=2;						/* two points available compute p and         */
          s=(*fu)/(*fa);					/* q for approximation u(new)=u(old)+p/q      */
          p=2.0*halfub*s;
          q=1.0-s;}
      else
         {*iflag=3;						/* attempt inverse quadratic interpolation    */
          s=(*fu)/(*fa);					/* if three points available compute p and    */
          q=(*fa)/(*fb);					/* q for approximation u(new)=u(old)+p/q      */
          r=(*fu)/(*fb);
          p=s*(2.0*halfub*q*(q-r)-(*u-*a)*(r-1.0));
          q=(q-1.0)*(r-1.0)*(s-1.0); }
 
      if(p > 0.0) q=-q;						/* correct the signs of p and q */
      p=fabs(p);
 
      if((8.0*sdel1 > sdel4) || (p >= 1.5*fabs(halfub*q)-fabs(toler*q))) 
        {*iflag=1;
         step=halfub;						/* if p/q is too large, go back to bisection */
         *a=*u;
         *fa=*fu;
         if(fabs(step) <= toler) step=copysign(toler,halfub);
         *u=*u+step;
         return(0); }

      step=p/q;
 
      *a=*u;							/* segment 5   value of step has been computed.   */
      *fa=*fu;							/* update information  a =u, fa =fu, u =u+step.   */
      if(fabs(step) <= toler) step=copysign(toler,halfub);	/* change in sign interval is now (a,b) or (b,a). */
      *u=*u+step;
      return(0);
}



    
/* -------------------------------------------------------------------------
    
         job = 1  euclidean norm of the n-vector stored in sx
         job = 2  euclidean distance between the vectors sx and sy.
    
         if    n .le. 0 return with result = 0.
    
         this routine represents a rewritten version of the linpack
         routine snrm2 written by c. l. lawson.  see reference 6
         of pitcon for details.
    
         four phase method     using two built-in constants that are
         hopefully applicable to all machines.
             cutlo = maximum of  sqrt(u/eps)  over all known machines.
             cuthi = minimum of  sqrt(v)      over all known machines.
         where
             eps = smallest no. such that eps + 1. .gt. 1.
             u   = smallest positive no.   (underflow limit)
             v   = largest  no.            (overflow  limit)
    
         brief outline of algorithm..
    
         phase 1    scans zero components.
         move to phase 2 when a component is nonzero and .le. cutlo
         move to phase 3 when a component is .gt. cutlo
         move to phase 4 when a component is .ge. cuthi/m
         where m = n for x() real and m = 2*n for complex.
    
         values for cutlo and cuthi..
         from the environmental parameters listed in the imsl converter
         document the limiting values are as follows..
         cutlo, s.p.   u/eps = 2**(-102) for  honeywell.  close seconds are
                       univac and dec at 2**(-103)
                       thus cutlo = 2**(-51) = 4.44089e-16
         cuthi, s.p.   v = 2**127 for univac, honeywell, and dec.
                       thus cuthi = 2**(63.5) = 1.30438e19
         cutlo, d.p.   u/eps = 2**(-67) for honeywell and dec.
                       thus cutlo = 2**(-33.5) = 8.23181d-11
         cuthi, d.p.   same as s.p.  cuthi = 1.30438d19
    
    
 ----------------------------------------------------------------------- */
 

double
sdist2(n,sx,sy,job)
double	*sx, *sy;
int	n, job;
{
      int	i,j,jjob,next,nn, op_flag;
      double	hitest;
      double	sxi,sxj,sum,tmp,xmax;
 
      double	zero=0.0, one=1.0, fabs(), sqrt();
      double	cutlo=8.232e-11, cuthi=1.304e19; 
 
      jjob = 2;
      if(job != 2) jjob=1;
 
      if(n <= 0)     				/* return zero if n is not positive */
         return(0.0);
 
      sum = zero;
      xmax = zero;
      nn = n;
 
      hitest = cuthi/( (double) nn);		/* for real or d.p. set hitest = cuthi/n      */
      op_flag = 20;				/* for complex      set hitest = cuthi/(2*n)  */
      i = 0;
 
main_loop:
      i = i + 1;
      if(i > nn) 
          return( xmax*sqrt(sum) );
      else
         {sxi = sx[i];
         if(jjob == 2)sxi = sxi - sy[i];}
 
      switch(op_flag) 
	{case 20:
            if(fabs(sxi) > cutlo) goto done;  
            op_flag = 40;			/* phase 3. sum in mid-range. no scaling */
            xmax = zero;
	 case 40:
            if(sxi == zero) goto main_loop;
            if(fabs(sxi) > cutlo) goto done;	/* phase 1. sum is zero */
            op_flag = 60;      			/* prepare for phase 2. */
            xmax = fabs(sxi);			
            tmp = sxi/xmax;
            sum = sum + tmp*tmp;
            goto main_loop;
	 case 60:
            if(fabs(sxi) > cutlo) 		/* phase 2. sum is small --  */
               {sum = (sum*xmax)*xmax;		/* scale to avoid destructive underflow */
                goto done;}
            else
               {if(fabs(sxi) > xmax) 
                   {tmp = xmax/sxi;
                    sum = one + tmp*tmp;
                    xmax = fabs(sxi);}
	        else
		   {tmp = sxi/xmax;		/* sum is large. proceed as in phase 4. */
		    sum = sum + tmp*tmp;}
                goto main_loop;}
	 case 80:
            if(fabs(sxi) > xmax)
               {tmp = xmax/sxi;			/* in phase 4 sum is large.             */ 
                sum = one + sum*(tmp*tmp);	/* scale to avoid overflow              */
                xmax = fabs(sxi);}
            else
               {tmp = sxi/xmax;
                sum = sum + tmp*tmp;}
            goto main_loop;
         }					/* end switch */


done: for(j=i; j<=nn; j++) 
          {sxj = sx[j];				/* loop for adding mid-range components */
           if(jjob == 2) sxj=sxj-sy[j];
           if(fabs(sxj) >= hitest) 
	      {op_flag = 80;                             /* prepare for phase 4.                 */
		i = j;
		sxi = sxj;
		sum = (sum/sxi)/sxi;
		xmax = fabs(sxi);    
		tmp = sxi/xmax;  
		sum = sum + tmp*tmp;
		goto main_loop;}
           sum = sum + sxj*sxj;}
      return( sqrt(sum) );
}
