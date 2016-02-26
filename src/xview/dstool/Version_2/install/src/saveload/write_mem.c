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
#include <defaults.h>
#include <memory.h>
#include <saveload.h>

/*
 * write_mem() writes to file the contents of a memory data object.
 * The objects handled are trajectories, fixed points, continuation data, 
 * parameter data, and selected points.
 */
int
  write_mem(fp, ptr, label)
FILE		*fp;
memory		ptr;
char		*label;
{
  int	  	n_doubles, n_ints, n_dparams, n_iparams, n_trajs, n_points;
  int           *p_ints, *p_iparams;
  double       	*p_doubles, *p_dparams;
  int	        format;
  int           first_obj;

  format = SAVE_PRECISION;		 /* for now, constant precision */
  
  /* note if no memory obj exists of requested form, 
     then control falls through this loop */
  if (memory_reset_read(ptr) == 0) 
    while ( memory_vanilla_read_next_flow(ptr, &n_trajs, &p_dparams, 
				&n_dparams, &p_iparams, &n_iparams) == 0)
      {
	fprintf(fp,"# %s\n# Objects %d\n# Double_Params %d : ",
		label,n_trajs,n_dparams);
	if (n_dparams > 0) 
	  write_double_point_1p1(fp, n_dparams, format, p_dparams);
	else
	  fprintf(fp,"\n");
	fprintf(fp,"# Integer_Params %d : ",n_iparams);
	if (n_iparams > 0)
	  write_integer_point_1p1(fp, n_iparams, p_iparams);
	else
	  fprintf(fp,"\n");
	first_obj = TRUE;
	while ( memory_vanilla_read_next_traj(ptr, &n_points, &n_doubles, 
					      &n_ints) == 0 )
	  {
	    if (first_obj) 
	      { 
		fprintf(fp,"# Doubles= %d Integers= %d\n",n_doubles,n_ints);
		first_obj = FALSE;
	      }
	    fprintf(fp,"# New_Obj:  %d Points\n",n_points);
	    while ( memory_vanilla_read_next_point(ptr, &p_doubles, &p_ints) 
		   == 0)
	      {
		write_double_point_1p1(fp, n_doubles, format, p_doubles);
		write_integer_point_1p1(fp, n_ints, p_ints);
	      }
	  }
      }		  
}

/*
write_double_point()   writes data to file.  If dimensions are too large, break up output so 
that only MAX_DOUBLE_PER_LINE points are written to each record.
*/
int
  write_double_point(fp,dim,format,pt)
FILE          *fp;
int           dim, format;
double        *pt;
{
  int           j,k,rmndr,div;
  double        fmod();

  if(dim == 0) return;
  div = dim / MAX_DOUBLE_PER_LINE;
  rmndr = dim - div * MAX_DOUBLE_PER_LINE;

  fprintf(fp,"{ ");
  for (j=0; j<div; j++)
    {
      for (k=0;k<MAX_DOUBLE_PER_LINE;k++)
	fprintf(fp,"%.*lg ", format, pt[ j*MAX_DOUBLE_PER_LINE + k ]);
      fprintf(fp,"\n");
    }
  
  for (j=0;j<rmndr;j++)
    fprintf(fp,"%.*lg ", format, pt[ div*MAX_DOUBLE_PER_LINE + j ]);

  fprintf(fp," }\n");

}


/*
write_double_point_1p1()   writes data to file.  If dimensions are too large, break up output so 
that only MAX_DOUBLE_PER_LINE points are written to each record.
*/
int
  write_double_point_1p1(fp,dim,format,pt)
FILE          *fp;
int           dim, format;
double        *pt;
{
  int           j,k,rmndr,div;
  double        fmod();

  if(dim == 0) return;
  div = dim / MAX_DOUBLE_PER_LINE;
  rmndr = dim - div * MAX_DOUBLE_PER_LINE;

  for (j=0; j<div; j++)
    {
      for (k=0;k<MAX_DOUBLE_PER_LINE;k++)
	fprintf(fp,"%.*lg ", format, pt[ j*MAX_DOUBLE_PER_LINE + k ]);
      fprintf(fp,"\n");
    }
  
  for (j=0;j<rmndr;j++)
    fprintf(fp,"%.*lg ", format, pt[ div*MAX_DOUBLE_PER_LINE + j ]);

  fprintf(fp,"\n");

}


/*
write_integer_point_1p1()   writes data to file.  If dimensions are too large, break up output so 
that only MAX_DOUBLE_PER_LINE points are written to each record.
*/
int
  write_integer_point(fp,dim,pt)
FILE          *fp;
int           dim, *pt;
{
  int           j,k,rmndr,div;
  double        fmod();

  if(dim == 0) return;
  div = dim / MAX_INTEGER_PER_LINE;
  rmndr = dim - div * MAX_INTEGER_PER_LINE;

  fprintf(fp,"{ ");
  for (j=0; j<div; j++)
    {
      for (k=0;k<MAX_INTEGER_PER_LINE;k++)
	fprintf(fp,"%ld ", pt[ j*MAX_INTEGER_PER_LINE + k ]);
      fprintf(fp,"\n");
    }

  for (j=0;j<rmndr;j++)
    fprintf(fp,"%ld ", pt[ div*MAX_INTEGER_PER_LINE + j ]);

  fprintf(fp," }\n");

}

/*
write_integer_point_1p1()   writes data to file.  If dimensions are too large, break up output so 
that only MAX_DOUBLE_PER_LINE points are written to each record.
*/
int
  write_integer_point_1p1(fp,dim,pt)
FILE          *fp;
int           dim, *pt;
{
  int           j,k,rmndr,div;
  double        fmod();

  if(dim == 0) return;
  div = dim / MAX_INTEGER_PER_LINE;
  rmndr = dim - div * MAX_INTEGER_PER_LINE;

  for (j=0; j<div; j++)
    {
      for (k=0;k<MAX_INTEGER_PER_LINE;k++)
	fprintf(fp,"%ld ", pt[ j*MAX_INTEGER_PER_LINE + k ]);
      fprintf(fp,"\n");
    }

  for (j=0;j<rmndr;j++)
    fprintf(fp,"%ld ", pt[ div*MAX_INTEGER_PER_LINE + j ]);

  fprintf(fp,"\n");

}
