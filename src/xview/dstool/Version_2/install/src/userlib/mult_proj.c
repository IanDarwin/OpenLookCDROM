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
/*
 * This is the file which allows the user to add
 * projection functions to the multiple orbit window.
 *
 */
#include <stdio.h>
#include <math.h>
#include <mult_proj.h>

/* ********************************************* *
 *                                               *
 * DECLARE your mult projection functions here.  *
 *                                               *
 * ********************************************* */
extern int xyrot();
extern int ellipse_proj();
/* ********************************************* *
 *                                               *
 * ADD an entry for your mult projection here.   *
 *                                               *
 *********************************************** */

struct Mult_Proj  MULT_PROJS[] = {   
  { "ellipse",ellipse_proj},
  {"x-y rotation",xyrot},
};



/* ******************************************* *
 *                                             *
 * DO NOT MODIFY THE FOLLOWING LINE.           *
 *                                             *
 ********************************************* */
int NUM_MULT_PROJS = sizeof(MULT_PROJS) / sizeof(struct Mult_Proj);


/* ******************************************* *
 *                                             *
 * PUT YOUR PROJECTION FUNCTIONS BELOW HERE    *
 *                                             *
 ********************************************* */

/*
 * project onto an ellipsoid
 */
int
  ellipse_proj(x, fx, x_dim, params, fparams, params_dim, center, radius, alpha)
double *x, *fx, *params, *fparams, *center, *radius, alpha;
int x_dim, params_dim;
{
  double d=0.0;
  int i;
  
  for (i=0; i<x_dim-1; i++) /* ignore time */
    d += (x[i]-center[i])*(x[i]-center[i])/(radius[i] * radius[i]);
  if (d > 1.0)
    {
      d = sqrt(d);
      for (i=0; i<x_dim-1; i++) 
	fx[i] = (x[i]-center[i])/d + center[i];
      fx[x_dim-1] = x[x_dim-1];
    }
  else
    {
      for (i=0; i<x_dim; i++)
	fx[i] = x[i];
    }
  for (i=0; i<params_dim; i++)
    fparams[i] = params[i];
}



/*
 * rotate  in x-y plane by theta about center
 */
xyrot(x, fx, x_dim, params, fparams, params_dim, center, radius, alpha)
double *x, *fx, *params, *fparams, *center, *radius, alpha;
int x_dim, params_dim;
{
  double c, s;
  int i;

  if (x_dim<2) 
    {
      for (i=0; i<x_dim; i++)
	fx[i] = x[i];
    }
  else
    {
      c = cos(alpha);
      s = sin(alpha);
      fx[0] = c*(x[0]-center[0])-s*(x[1]-center[1]);
      fx[1] = s*(x[0]-center[0])+c*(x[1]-center[1]);
      fx[0] += center[0];
      fx[1] += center[1];
      for (i=2; i<x_dim; i++)
	fx[i] = x[i];
    }
  for (i=0; i<params_dim; i++)
    fparams[i] = params[i];
}



/*
 * This is the stub of a multiple projection procedure.
 * This implements the identity transformation!
 */
mult_stub(x, fx, x_dim, params, fparams, params_dim, center, radius, alpha)
double *x, *fx, *params, *fparams, *center, *radius, alpha;
int x_dim, params_dim;
{
  int i;

  /* FILL IN AND REPLACE CODE HERE ! */
  for (i=0; i<x_dim; i++)
    fx[i] = x[i];
  for (i=0; i<params_dim; i++)
    fparams[i] = params[i];

}
