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



/* ---------------------------------------------------------------------
    
      subroutine coqual computes the factor qual which measures
      the convergence quality of the corrector iteration. this
      factor is used in the steplength algorithm.
      for the basic idea behind this subroutine see reference 2
      of pitcon.
    
      clast  = last step of the iteration
      ctotal = distance between initial point and terminal point
      epsate = 8.0*epmach
      maxcor = maximal number of iteration steps
      modnew = process indicator (see pitcon)
      nstep  = actual number of iteration steps taken
      qual   = the output quality factor
    
  ---------------------------------------------------------------------- */
 
double
coqual(clast,ctotal,epsate,maxcor,modnew,nstep)
int	maxcor, modnew, nstep;
double	clast, ctotal, epsate;
{
      int	nave,nmax; 
      double	base,bot,esab,expo,term,test,top;
      double	fabs(), pow(),qual, itop, ibot;

      if((nstep <= 1) || (ctotal <= epsate)) 
          return(8.0);
 
      if(modnew == 0) nave=((double) (maxcor-1))/2.0;
      if(modnew == 1) nave=maxcor;		/* see if average number of steps taken */
      if(nstep == nave)
          return(1.0);
 
      if(modnew == 0) nmax=maxcor;
      if(modnew == 1) nmax=2*maxcor;		/* see if maximum number of steps taken */
      if(nstep >= nmax) 
          return(0.125);
 
      if( fabs(clast/ctotal)<1.e-20 )
	      return(0.125);

      if(modnew != 1) 				
         {
	  itop=pow( 2.0,(double) (nstep-nave) );				   
          ibot=pow(2.0,(double)(nstep-1)) - 1.0;	/* for modnew=0, let w=(clast/ctotal),				*/
          expo=1.0/ibot;				/*        iexp=1/(2**(nstep-1)-1)				*/
          base=pow( (clast/ctotal),expo );		/*        u=w**iexp						*/
          top=base+1.0+(1.0/base);			/*        jexp=2**(nstep-nave)					*/
          term=pow( base,itop) ;		        /*        then  qual=(u+1+(1/u)) / (u**jexp+1+(1/u**jexp))	*/
          bot=term+1.0+(1.0/term);			/* otherwise,    exp=(nstep-nave)/(nstep-1)			*/
          qual=top/bot;				        /*               w=(clast/ctotal)				*/
          if(qual > 8.0) qual=8.0;			/*               qual=w**exp                                    */
          if(qual < 0.125) qual=0.125;
          return(qual);}
 
      itop = (double)(nstep - nave); 				/* try to anticipate underflow */
      ibot = (double)(nstep - 1.0); 				/* and overflow.               */
      expo=ibot/itop;
      test=pow( 8.0,expo );
      base=clast/ctotal;
      esab=ctotal/clast;
      if((nstep < nave && test > base) || (nstep > nave && test < base))
          return(8.0);
      if((nstep < nave && test > esab) || (nstep > nave && test < esab))
          return(0.125);
 
      expo=itop/ibot;		/* compute *qual */
      qual=pow( base,expo );
      if(qual > 8.0) qual=8.0;
      if(qual < 0.125) qual=0.125;
      return(qual);
}
