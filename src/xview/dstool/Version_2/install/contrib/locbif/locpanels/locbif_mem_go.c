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
#include <complib.h>
#include <pm.h>
#include <memory.h>

#include "../locpanels/lbmain.h"
#include "../locpanels/lbsolver/lbsolver.h"
#include "../locpanels/lbdisplay/lbdisplay.h"
#include "../locpanels/lbcont/lbcont.h"
#include "../locpanels/complbf.h"

extern struct Lbmain_Ds   	        lbmain_ds;
extern struct Lbdisplay_Ds              lbdisplay_ds;
extern struct Lbsolver_Ds               lbsolver_ds;
extern struct Lbcont_Ds                 lbcont_ds;

/* ---------------------------------------------------------------------------------------
   Level 1 driver for the locbif main entry point

   last change:  5/12/92  (mrm)
  
   --------------------------------------------------------------------------------------- */

locbif_mem_go()
{
      int		iterations = 200;
      static int	last_dir = LB_FORWARD;
      memory            cont_mem=NULL;
      double		fabs();

      cont_mem = (memory)  pm( GET, "Memory.Cont", NULL);

      switch(lbmain_ds.Locbif_Dir)
	{
	 case LB_FORWARD:
	 case LB_BACKWARD:
	   lbcont_ds.H0crv = (lbmain_ds.Locbif_Dir == LB_FORWARD)?
			      fabs(lbcont_ds.H0crv) : -fabs(lbcont_ds.H0crv);
           last_dir = (lbmain_ds.Locbif_Dir == LB_FORWARD)? LB_FORWARD : LB_BACKWARD;
	   break;
         case LB_CONTINUE:
	   lbcont_ds.H0crv = (last_dir == LB_FORWARD)?
	                      fabs(lbcont_ds.H0crv) : -fabs(lbcont_ds.H0crv);
	   last_dir = (last_dir == LB_FORWARD)? LB_FORWARD : LB_BACKWARD;
           break;
	}

      pack_statecmn_locbif(lbmain_ds.Locbif_Dir);  
      pack_parmcmn_locbif(); 

      memory_start_new_flow( cont_mem, 1, 0, 0, iterations, 0, 0);

      locbif_proc(); 

      update_locbif();
      lbstate_panel_refresh();
  
}





/* ---------------------------------------------------------------------------------------
   Level 2 driver for locbif

   last change:  5/12/92  (mrm)
   --------------------------------------------------------------------------------------- */


locbif_proc() 
{
      extern    locbif_ ();

      reset_interrupt();

      locbif_ ();
}







/* ---------------------------------------------------------------------------------------

   procs used to transfer data between the locbif/dstool interface data structure and the
   common areas used by locbif internals 

   last change:  5/11/92  (mrm)

   --------------------------------------------------------------------------------------- */


extern struct cmn_real{ double rvc[NRVC]; 		/* these common blocks MUST be aligned with */
			double rpc[NRPC];			/* their locbif counterpart common blocks!  */
			double rlim[4]; };

extern struct cmn_epsh0{double epscrv;
			double h0crv;  };

extern struct cmn_nmlit{short  n;
			short  m;
			short  l;
			short  it;     };

extern struct cmn_logic{short active[NRVC];
			short state;   };

extern struct cmn_np123{short  np1;
			short  np2;
			short  np3;    };

extern struct cmn_integ{short  k;
			short  iline;
			short  nvar;
			short  id[NRVC];
			short  line;
			short  ix;
			short  iy;     };

extern struct cmn_mswtch{short   mflag;  };

extern struct cmn_iorbit{short iorbit;};
extern struct cmn_soldot{short soldot;};
extern struct cmn_sos{short isound;};
extern struct cmn_iflash{short iflash;};
extern struct cmn_messg{short messag;};
extern struct cmn_maxnpt{short maxnpt;};
extern struct cmn_initv{short init;};
extern struct cmn_np123 np123_ ;

extern struct cmn_real real_ ;
extern struct cmn_epsh0 epsh0_ ;
extern struct cmn_nmlit nmlit_ ;
extern struct cmn_iorbit iorbit_ ;
extern struct cmn_soldot soldot_ ;
extern struct cmn_sos sos_ ;
extern struct cmn_iflash iflash_ ;
extern struct cmn_messg messg_ ;
extern struct cmn_maxnpt maxnpt_ ;
extern struct cmn_initv initv_ ;
extern struct cmn_logic logic_ ;
extern struct cmn_integ integ_ ;
extern struct cmn_mswtch mswtch_ ;

pack_parmcmn_locbif()
{
  int	i;
/*
  int	n_varb = *((int *) pm(GET, Traj_Ds_Object, Varb_Dim, NULL)) - 1;
  int	n_param = *((int *) pm(GET, Traj_Ds_Object, Param_Dim, NULL));
*/

  int n_varb = *((int *) pm(GET, "Model.Varb_Dim", NULL)) - 1;
  int n_param = *((int *) pm(GET, "Model.Param_Dim", NULL));

  np123_.np1=13;               /* Number of computational parameters */
  np123_.np2=11;
  np123_.np3=6;

  real_.rpc[0]  = epsh0_.h0crv 		= lbcont_ds.H0crv;
  real_.rpc[1]  = lbcont_ds.Hmxcrv;
  real_.rpc[2]  = lbcont_ds.Angcrv;
  real_.rpc[3]  = lbcont_ds.Dhcrv;
  real_.rpc[4]  = lbcont_ds.Dhjac;
  real_.rpc[5]  = (double) lbcont_ds.Maxit;
  real_.rpc[6]  = (double) lbcont_ds.Modit;
  real_.rpc[7]  = epsh0_.epscrv 	= lbcont_ds.Epscrv;
  real_.rpc[8]  = lbcont_ds.Epscrs;
  real_.rpc[9]  = lbcont_ds.Epszer;
  real_.rpc[10] = lbcont_ds.Epsext;
  real_.rpc[11] = (double) lbcont_ds.Iprsng;
  real_.rpc[12] = lbcont_ds.Algcrv;

  real_.rpc[13] = (double) lbsolver_ds.Itmap;
  real_.rpc[14] = lbsolver_ds.Tint;
  real_.rpc[15] = lbsolver_ds.H0int;
  real_.rpc[16] = lbsolver_ds.Hmxint;
  real_.rpc[17] = lbsolver_ds.Dhint;
  real_.rpc[18] = lbsolver_ds.Epsint;
  real_.rpc[19] = lbsolver_ds.Epsrel;
  real_.rpc[20] = lbsolver_ds.Solver;
  real_.rpc[21] = (double) lbsolver_ds.Isec;
  real_.rpc[22] = (double) lbsolver_ds.Irhs;

  iorbit_.iorbit = lbsolver_ds.Iorbit;
  real_.rpc[23]  = iorbit_.iorbit = (double) lbsolver_ds.Iorbit;

  soldot_.soldot = lbdisplay_ds.Soldot;
  real_.rpc[24]  = (double) lbdisplay_ds.Soldot;

  sos_.isound    = lbdisplay_ds.Isound;
  real_.rpc[25]  = (double) lbdisplay_ds.Isound;

  iflash_.iflash = lbdisplay_ds.Iflash;
  real_.rpc[26]  = (double) lbdisplay_ds.Iflash;

  messg_.messag  = (double) lbdisplay_ds.Messag;
  real_.rpc[27]  = (double) lbdisplay_ds.Messag;

  maxnpt_.maxnpt = lbdisplay_ds.Maxnpt;
  real_.rpc[28]  = (double) lbdisplay_ds.Maxnpt;

  initv_.init    = (double) lbdisplay_ds.Init;
  real_.rpc[29]  = (double) lbdisplay_ds.Init;

  for(i=0; i<n_varb+n_param; i++)
     logic_.active[i] = lbmain_ds.active[i];

  if(lbmain_ds.Locbif_Mode == LB_EQUIL)
    mswtch_.mflag = 1;
  else if (lbmain_ds.Locbif_Mode == LB_FPTS)
    mswtch_.mflag = 2;
  else if (lbmain_ds.Locbif_Mode == LB_PERIODIC_AUTO)
    mswtch_.mflag = 3;
  else if (lbmain_ds.Locbif_Mode == LB_PERIODIC_NAUTO)
    mswtch_.mflag = 4;
    
  integ_.line = lbmain_ds.Line + 1;

}




pack_statecmn_locbif( dir )
int	dir;
{
  static int	index, zero = 0;			/* must pass by reference to FORTRAN */
  int		i;
  char          *name[MAX_LEN_VARB_NAME];
  extern        setch_ ();

/*
  nmlit_.n  = (short)  *((int *) pm( GET, Traj_Ds_Object, Varb_Dim, NULL )) - 1;
  nmlit_.m  = (short)  *((int *) pm( GET, Traj_Ds_Object, Param_Dim, NULL ));
  nmlit_.l  = (short)  *((int *) pm( GET, Traj_Ds_Object, Function_Dim, NULL ));
*/
  nmlit_.n  = (short) *((int *) pm(GET, "Model.Varb_Dim", NULL)) - 1; 
  nmlit_.m  = (short) *((int *) pm(GET, "Model.Param_Dim", NULL)); 
  nmlit_.l  = (short) *((int *) pm(GET, "Model.Funct_Dim", NULL));          
  nmlit_.it = 1;

  if( dir == LB_FORWARD | dir == LB_BACKWARD )
    {
/*
     pm( GET_LIST, Traj_Ds_Object, Varb_Ic, 0, (int) (nmlit_.n-1), &(real_.rvc[0]), NULL);
     pm( GET_LIST, Traj_Ds_Object, Param, 0, (int) (nmlit_.m-1), &(real_.rvc[nmlit_.n]), NULL);
*/
     pm( GET_LIST, "Selected.Varb_Ic", 0, (int) (nmlit_.n-1), &(real_.rvc[0]), NULL);
     pm( GET_LIST, "Selected.Param_Ic", 0, (int) (nmlit_.m-1), &(real_.rvc[nmlit_.n]), NULL);
    }
  else
    {
/*
     pm( GET_LIST, Cont_Control, Cont_Fc, 0, (int) (nmlit_.n-1), &(real_.rvc[0]), NULL);
     pm( GET_LIST, Cont_Control, Cont_Param_Fc, 0, (int) (nmlit_.m-1), &(real_.rvc[nmlit_.n]), NULL);
*/
     pm( GET_LIST, "Lb_Control.Lb_Fc", 0, (int) (nmlit_.n-1), &(real_.rvc[0]), NULL);
     pm( GET_LIST, "Lb_Control.Lb_Param_Fc", 0, (int) (nmlit_.m-1), &(real_.rvc[nmlit_.n]), NULL);
    }

  for (i=0; i<nmlit_.n; i++)
    {
     index = i + 1;
/*   pm(GET, Traj_Ds_Object, Varb_Names, i, name, NULL);  */
     pm(GET, "Model.Varb_Names", i, name, NULL);
     strncat(name,"         ",6);
     setch_( name, &zero, &index );
    }
  for (i=0; i<nmlit_.m; i++)
    {
     index = i + nmlit_.n + 1;
/*     pm(GET, Traj_Ds_Object, Param_Names, i, name, NULL);  */
     pm(GET, "Model.Param_Names", i, name, NULL);
     strncat(name,"         ",6);
     setch_( name, &zero, &index );
    }
  for (i=0; i<nmlit_.l; i++)
    {
     index = i + nmlit_.n + nmlit_.m + 1;
/*   pm(GET, Traj_Ds_Object, Function_Names, i, name, NULL);  */
     pm(GET, "Model.Funct_Names", i, name, NULL);
     strncat(name,"         ",6);
     setch_( name, &zero, &index );
    }
/*  pm(GET, Traj_Ds_Object, Varb_Names, nmlit_.n, name, NULL);  */
  pm(GET, "Model.Varb_Names", i, name, NULL);
  index = nmlit_.n + nmlit_.m + nmlit_.l + 1;
  strncat(name,"         ",6);
  setch_( name, &zero, &index );
}


update_state_locbif()
{
/*
  pm( PUT_LIST, Traj_Ds_Object, Varb_Ic, 0, (int) (nmlit_.n-1), &(real_.rvc[0]), NULL);
  pm( PUT_LIST, Traj_Ds_Object, Param, 0, (int) (nmlit_.m-1), &(real_.rvc[nmlit_.n]), NULL); 
*/
  pm( PUT_LIST, "Selected.Varb_Ic", 0, (int) (nmlit_.n-1), &(real_.rvc[0]), NULL); 
  pm( PUT_LIST, "Selected.Param_Ic", 0, (int) (nmlit_.m-1), &(real_.rvc[nmlit_.n]), NULL);
  sel_data_refresh();
}


update_locbif()
{
/*
  pm( PUT_LIST, Cont_Control, Cont_Fc, 0, (int) (nmlit_.n-1), &(real_.rvc[0]), NULL);
  pm( PUT_LIST, Cont_Control, Cont_Param_Fc, 0, (int) (nmlit_.m-1), &(real_.rvc[nmlit_.n]), NULL);
*/
  pm( PUT_LIST, "Lb_Control.Lb_Fc", 0, (int) (nmlit_.n-1), &(real_.rvc[0]), NULL);
  pm( PUT_LIST, "Lb_Control.Lb_Param_Fc", 0, (int) (nmlit_.m-1), &(real_.rvc[nmlit_.n]), NULL);
}



pack_struct_locbif()
{
  int	i;
/*
  int	n_varb = *((int *) pm(GET, Traj_Ds_Object, Varb_Dim, NULL)) - 1;
  int	n_param = *((int *) pm(GET, Traj_Ds_Object, Param_Dim, NULL));
*/
  int   n_varb = *((int *) pm(GET, "Model.Varb_Dim", NULL)) - 1;
  int   n_param = *((int *) pm(GET, "Model.Param_Dim", NULL));


  epsh0_.h0crv       = lbcont_ds.H0crv        = real_.rpc[0]; 
  lbcont_ds.Hmxcrv			      = real_.rpc[1];
  lbcont_ds.Angcrv			      = real_.rpc[2];
  lbcont_ds.Dhcrv			      = real_.rpc[3];
  lbcont_ds.Dhjac			      = real_.rpc[4];
  lbcont_ds.Maxit                     = (int)   real_.rpc[5];
  lbcont_ds.Modit	              = (int)   real_.rpc[6];
  epsh0_.epscrv      = lbcont_ds.Epscrv       = real_.rpc[7];
  lbcont_ds.Epscrs			      = real_.rpc[8];
  lbcont_ds.Epszer		   	      = real_.rpc[9];
  lbcont_ds.Epsext			      = real_.rpc[10];
  lbcont_ds.Iprsng		      = (int)   real_.rpc[11];
  lbcont_ds.Algcrv			      = real_.rpc[12];

  lbsolver_ds.Itmap		      = (int)   real_.rpc[13]; 
  lbsolver_ds.Tint		              = real_.rpc[14];
  lbsolver_ds.H0int			      = real_.rpc[15];
  lbsolver_ds.Hmxint			      = real_.rpc[16];
  lbsolver_ds.Dhint			      = real_.rpc[17];
  lbsolver_ds.Epsint			      = real_.rpc[18];
  lbsolver_ds.Epsrel			      = real_.rpc[19];
  lbsolver_ds.Solver			      = real_.rpc[20];
  lbsolver_ds.Isec		      = (int)   real_.rpc[21];
  lbsolver_ds.Irhs		      = (int)   real_.rpc[22];
  iorbit_.iorbit = lbsolver_ds.Iorbit =  (int)  real_.rpc[23];

  soldot_.soldot = lbdisplay_ds.Soldot = (int)  real_.rpc[24]; 
  sos_.isound    = lbdisplay_ds.Isound = (int)  real_.rpc[25];
  iflash_.iflash = lbdisplay_ds.Iflash = (int)  real_.rpc[26];
  messg_.messag  = lbdisplay_ds.Messag = (int)  real_.rpc[27];
  maxnpt_.maxnpt = lbdisplay_ds.Maxnpt = (int)  real_.rpc[28];
  initv_.init    = lbdisplay_ds.Init   = (int)  real_.rpc[29];

  lbmain_ds.Line = integ_.line - 1;

  for(i=0; i<n_varb+n_param; i++) lbmain_ds.active[i] = (int) logic_.active[i];
}



/* 
   proc to initialize np1 (number of continuation control parameters), np2 (number of 
   solver parameters) and np3 (number of display parameters) and then call the locblf
   FORTRAN routine to do a formatted read on the initalization file

   last change:   05/12/92  (mrm)

*/


lb_rd_file()
{ 
   char                    fname[SIZE_OF_DIR_PLUS_FNAME];

   np123_.np1=13;		/* Number of computational parameters */
   np123_.np2=11;
   np123_.np3=6;

   lbmain_panel_refresh();                      /* and refresh the main panel    */
   strcpy(fname, lbmain_ds.Dirname);		/* get directory                 */
   strcat(fname, "/");
   strcat(fname, lbmain_ds.Fname);

   /* check if file exists */
   if (!check_file_to_read(fname))						
     {
/*      error_notice( address, "Invalid file or path or locbif file cannot be read"); */
      fprintf(stderr,"Invalid file or path or locbif file cannot be read \n");
     }
   else
     {
      rdinit_ (fname);			/* Read Initialization File        */
      pack_struct_locbif();	        /* update level 1-2 data structure */
      lbmain_panel_refresh();	        /* and refresh the main panel      */
     }
}



/* ---------------------------------------------------------------------------------------
   
   this little routine provides the interface between the FORTRAN locbif() subroutine
   which fetches a function value, and the dstool C-language procedures for peforming
   the same task

   last change:  5/17/92  (mrm)
  
   --------------------------------------------------------------------------------------- */

extern struct cmn_ppar {double  p[NPARMX];};
extern struct cmn_ppar ppar_ ;

getfv(x, t, f)
double	*f, *x, *t;
{
  int       	i, (*function)();          
  double	*state, *params;

  state = (double *) calloc(nmlit_.n+1, sizeof(double));
  params = (double *) calloc(nmlit_.m+1, sizeof(double));

  for(i=0; i<nmlit_.n; i++) state[i] = x[i];
  state[nmlit_.n] = *t;
  for(i=0; i<nmlit_.m; i++) params[i] = ppar_.p[i];

/*
  function = (void *) pm( GET, Model_Cntl, DS_Def, NULL );
*/
  function = (void *) pm( GET, "Model.DS_Def", NULL );

  (int) function(f,state,params);

  free(state);
  free(params);

  return(0);
}



/* ---------------------------------------------------------------------------------------
   
   this little routine provides the interface between the FORTRAN locbif() subroutine
   which fetches a user-defined auxilary function value, and the dstool C-language 
   procedures for peforming the same task

   last change:  5/17/92  (mrm)
  
   --------------------------------------------------------------------------------------- */

getufv(ifn, x, t, f)
short	*ifn;
double	*f, *x, *t;
{
  int       	i, index = *ifn;          
  double	*state, *params, *values;

  if( nmlit_.l == 0 ) return(-1);

  state = (double *) calloc(nmlit_.n+1, sizeof(double));
  params = (double *) calloc(nmlit_.m+1, sizeof(double));
  values = (double *) calloc(nmlit_.l+1, sizeof(double));

  for(i=0; i<nmlit_.n; i++) state[i] = x[i];
  state[nmlit_.n] = *t;
  for(i=0; i<nmlit_.m; i++) params[i] = ppar_.p[i];

  get_ds_func(values,state,params);
  *f = values[index];

  free(state);
  free(params);
  free(values);

  return(0);
}


/* ---------------------------------------------------------------------------------------
   routine used to stuff computed points into the continuation memory object
   this routine is called by the locbif FORTRAN subroutine STORE.F

   last change:  5/17/92
   --------------------------------------------------------------------------------------- */


lbmem( x, n, p, m, t, pointtype, npt )
double	*x, *p, *t;
short   *pointtype, *npt, *n, *m;
{
  int		i;
  double	*state;
  memory        cont_mem=NULL;
  static int	typ_color[3] = {-1,SYS_RED,MED_POINT};
  static int	sp_color[3] = {-1,SYS_BLUE,LARGE_POINT};

/*
  cont_mem = (memory)  pm( GET, Mem_Obj_Cntl , Cont_Mem_Ptr , NULL);
*/
  cont_mem = (memory)  pm(GET, "Memory.Cont", NULL); 
  state = (double *) calloc(*n+1, sizeof(double));

  for(i=0; i<*n; i++) state[i] = x[i];
  state[*n] = *t;

  if(*pointtype != 0)
     memory_add_point(cont_mem, state, p, sp_color, NULL, NULL, NULL, NULL);
  else
     memory_add_point(cont_mem, state, p, typ_color, NULL, NULL, NULL, NULL);

  free(state);

/*  update_mess_on_panel(); */
  lbstate_panel_refresh();

  mem_all_win(cont_mem);
}



/* ---------------------------------------------------------------------------------------

   routines to do allocation and de-allocation of temporary space required by the
   level-1 driver

   last change:  4/26/92  (mrm)
  
   --------------------------------------------------------------------------------------- */


allocate_locbif()
{
	int	i, n_varb, n_param, status = 0, *ivector();
	double	*dvector();

/*
  	n_varb  = *((int *) pm( GET, Traj_Ds_Object, Varb_Dim, NULL )) - 1;
  	n_param = *((int *) pm( GET, Traj_Ds_Object, Param_Dim, NULL ));
*/
	n_varb = *((int *) pm(GET, "Model.Varb_Dim", NULL)) - 1;
	n_param = *((int *) pm(GET, "Model.Param_Dim", NULL));

	if ( ! (lbmain_ds.active = ivector(0,n_varb+n_param-1)) ) status = -1;
	for(i=0; i<n_varb+n_param; i++) lbmain_ds.active[i] = 0;
	
	if (status == -1) release_integ();

	return(status);
}
	



release_locbif()
{
	int	n_varb, n_param;

/*
  	n_varb  = *((int *) pm( GET, Traj_Ds_Object, Varb_Dim, NULL )) - 1;
  	n_param = *((int *) pm( GET, Traj_Ds_Object, Param_Dim, NULL ));
*/
	n_varb = *((int *) pm(GET, "Model.Varb_Dim", NULL)) - 1;
	n_param = *((int *) pm(GET, "Model.Param_Dim", NULL));

	free_ivector(lbmain_ds.active,0,n_varb+n_param-1);   
}




/*
 * routine to check if software interrupt request has occurred
*/

int
loclbf_halt( halt )
int	*halt;
{
/*
   *halt = *((int *) pm( GET, Flow_Control, Interrupt, NULL ));
*/
   *halt =  interrupt();
}
