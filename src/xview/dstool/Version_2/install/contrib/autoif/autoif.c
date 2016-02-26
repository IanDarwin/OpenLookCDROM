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
#include <stdlib.h>
#include <sys/param.h>
#include <sys/types.h>

#include <portability.h>

#ifdef HAS_STRINGS_H
#include <strings.h>
#endif

#ifdef HAS_STRING_H
#include <string.h>
#endif

#include <xview/xview.h>
#include <xview/panel.h>
#include <xview/textsw.h>
#include <xview/xv_xrect.h>
#include <xview/cms.h>
#include <ui_init.h>
#include <pm.h>
#include <xview/cms.h>
#include <memory.h>
#include <constants.h>
#include <user_panels.h>

#include "autoif_cui.h"
#include "autoif.h"

void	coaraatm_notify();

static autoif_coautord_objects		*Autoif_coautord = NULL;
struct Autoif_Ds		autoif_ds;

autoif_open(use_default,left,top,width,height)
int   use_default;    /* either SET_WIN_CONFIG  or  DEFAULT_WIN_CONFIG */
int   left;           /* position (on screen) of left edge of window.  */
int   top;            /* position (on screen) of top edge of window.   */
int   width;          /* width of window;  width <= 0 means use default width */
int   height;         /* height of window; height <= 0 means use default width */
{
  Rect
    *rect;
  if (Autoif_coautord == NULL)
    {
      Autoif_coautord = autoif_coautord_objects_initialize(NULL, cmd_ip->win);
        register_win_and_type("AUTO",Autoif_coautord->coautord,POPUP_WINDOW);
/*      autoif_init(); */
      autoif_field_manager();
    }

  if(use_default == SET_WIN_CONFIG)
    {
      rect = (Rect *)calloc(1,sizeof(Rect));
      frame_get_rect(Autoif_coautord->coautord,rect);
      rect->r_left = (short) left;
      rect->r_top = (short) top;
      if(width>0) rect->r_width = (short) width;
      if(height>0) rect->r_height = (short) height;
      frame_set_rect(Autoif_coautord->coautord,rect);
      free(rect);
    }
  mark_window_open("AUTO");
  xv_set(Autoif_coautord->coautord, FRAME_CMD_PUSHPIN_IN, TRUE, NULL);
  xv_set(Autoif_coautord->coautord, WIN_SHOW, TRUE, WIN_FRONT, NULL);
  autoif_panel_refresh();
}

int
autoif_close()
{
    mark_window_closed("AUTO");
    if(Autoif_coautord) {
	xv_set(Autoif_coautord->coautord, FRAME_CMD_PUSHPIN_IN, FALSE, NULL);
	xv_set(Autoif_coautord->coautord, XV_SHOW, FALSE, NULL);
    }
}


autoif_field_manager()
{

  int status=0;
  
  /* initialize data structure for the porbit window for the new dynamical system */
  status = autoif_init();

  /* exit now if the window has not been created */
  if (Autoif_coautord == NULL || status != 0) return(status);
return(status);
}


autoif_init()
{
  strcpy(autoif_ds.Dirname,"./");
  strcpy(autoif_ds.Fname,"fort.8");

  autoif_ds.Show_Flag = FALSE;
  autoif_ds.Bv_Flag = TRUE;
  autoif_ds.Save_Type = TRAJ_and_CONT;
  autoif_ds.Read_Mode = SEARCH;
  autoif_ds.Aatm = FALSE;

  autoif_ds.Ibr = 0;
  autoif_ds.Ntot = 0;
  autoif_ds.Itp = 0;
  autoif_ds.Lab = 0;
  autoif_ds.Nfpar = 0;
  autoif_ds.Isw = 0;
  autoif_ds.Ntpl = 0;
  autoif_ds.Nar = 0;
  autoif_ds.Nrowpr = 0;
  autoif_ds.Ntst = 0;
  autoif_ds.Ncol = 0;
  autoif_ds.Npar = 20;

  autoif_ds.Total_Blocks = 0;
  autoif_ds.Cur_Block = 0;
  autoif_ds.Block_Index = 0;
  return(0);
} 



int
  autoif_panel_refresh()
{
  char    	strng[26];
  int		index;
  double	floor(), a1, a2;
  static char  	*err_mes[] =  { " ", 
				"Bifurcation Point.",
				"Limit Point.",
				"Hopf Bifurcation Point.",
				" ",
				"Zero of User Function.",
				"Limit Point.",
				"Bifurcation Point.",
				"Period Doubling Bif.",
				"Bif to Invarient Torus",  
				"Branch Endpt. Norm Term.",
				"Branch Endp. Ab Term."};

  if( autoif_ds.Itp < 0 )
     strcpy( strng, "Convergence Error. Terminated");
  else if (autoif_ds.Itp == 0 )
     strcpy( strng, " ");
  else
     {
      index = autoif_ds.Itp;
      if (index > 9)
          while( index > 9 ) index -= 10;
      strcpy( strng, err_mes[index] ); 
     }
    
  xv_set( Autoif_coautord->coaritp ,PANEL_VALUE, strng, NULL);

  sprintf(strng, "  %d", autoif_ds.Ibr );
  xv_set( Autoif_coautord->coaribr ,PANEL_VALUE, strng, NULL);
  sprintf(strng, "  %d", autoif_ds.Ntot );
  xv_set( Autoif_coautord->coarntot ,PANEL_VALUE, strng, NULL);
  sprintf(strng, "  %d", autoif_ds.Lab );
  xv_set( Autoif_coautord->coarlab ,PANEL_VALUE, strng, NULL);
  sprintf(strng, "  %d", autoif_ds.Nfpar );
  xv_set( Autoif_coautord->coarnfpar ,PANEL_VALUE, strng, NULL);
  sprintf(strng, "  %d", autoif_ds.Isw );
  xv_set( Autoif_coautord->coarisw ,PANEL_VALUE, strng, NULL);
  sprintf(strng, "  %d", autoif_ds.Ntpl );
  xv_set( Autoif_coautord->coarntpl ,PANEL_VALUE, strng, NULL);
  sprintf(strng, "  %d", autoif_ds.Nar );
  xv_set( Autoif_coautord->coarnar ,PANEL_VALUE, strng, NULL);
  sprintf(strng, "  %d", autoif_ds.Npar );
  xv_set( Autoif_coautord->coarnpar ,PANEL_VALUE, strng, NULL);
  sprintf(strng, "  %d", autoif_ds.Total_Blocks );
  xv_set( Autoif_coautord->coartblks ,PANEL_VALUE, strng, NULL);
  sprintf(strng, "  %d", autoif_ds.Cur_Block );
  xv_set( Autoif_coautord->coarcurblk ,PANEL_VALUE, strng, NULL);

  if( autoif_ds.Read_Mode == SEARCH )
       xv_set( Autoif_coautord->coarmode, PANEL_VALUE, 0 , NULL);
  else xv_set( Autoif_coautord->coarmode, PANEL_VALUE, 1 , NULL);

  if( autoif_ds.Bv_Flag == FALSE )
      {
       xv_set( Autoif_coautord->coarntst, PANEL_INACTIVE, TRUE, NULL);
       xv_set( Autoif_coautord->coarncol, PANEL_INACTIVE, TRUE, NULL);
       xv_set( Autoif_coautord->coarftype, PANEL_VALUE, 1 , NULL);
      }
  else
      {
       xv_set( Autoif_coautord->coarntst, PANEL_INACTIVE, FALSE, NULL);
       xv_set( Autoif_coautord->coarncol, PANEL_INACTIVE, FALSE, NULL);

       sprintf(strng, "  %d", autoif_ds.Ntst );
       xv_set( Autoif_coautord->coarntst ,PANEL_VALUE, strng, NULL);
       sprintf(strng, "  %d", autoif_ds.Ncol );
       xv_set( Autoif_coautord->coarncol ,PANEL_VALUE, strng, NULL);

       xv_set( Autoif_coautord->coarftype, PANEL_VALUE, 0 , NULL);
      }

  if( autoif_ds.Aatm == FALSE )
       xv_set( Autoif_coautord->coaraatm, PANEL_VALUE, 0 , NULL);
  else xv_set( Autoif_coautord->coaraatm, PANEL_VALUE, 1 , NULL);

  switch( autoif_ds.Save_Type )
      {
       case NULL:
	 xv_set( Autoif_coautord->coartype, PANEL_VALUE, 0 , NULL);
	 break;
       case TRAJ:
	 xv_set( Autoif_coautord->coartype, PANEL_VALUE, 1 , NULL);
	 break;
       case CONT:
	 xv_set( Autoif_coautord->coartype, PANEL_VALUE, 2 , NULL);
	 break;
       case TRAJ_and_CONT:
	 xv_set( Autoif_coautord->coartype, PANEL_VALUE, 3 , NULL);
	 break;
      }

  if(autoif_ds.Show_Flag == TRUE)
     xv_set(Autoif_coautord->coautord, XV_HEIGHT, 524, NULL);
  else
     xv_set(Autoif_coautord->coautord, XV_HEIGHT, 210, NULL);

  xv_set( Autoif_coautord->coarfile, PANEL_VALUE, autoif_ds.Fname, NULL);
  xv_set( Autoif_coautord->coardir, PANEL_VALUE, autoif_ds.Dirname, NULL);
}



int
autoif_data_refresh()
{ 
  int	value; /* atoi(); */

  value = xv_get( Autoif_coautord->coartype, PANEL_VALUE, NULL);
  if (value == 1)
    autoif_ds.Save_Type = TRAJ;
  else if (value == 2)
    autoif_ds.Save_Type = CONT;
  else if (value == 3)
    autoif_ds.Save_Type = TRAJ_and_CONT;
  else
    autoif_ds.Save_Type = NULL;

  value = xv_get( Autoif_coautord->coarmode, PANEL_VALUE, NULL);
  autoif_ds.Read_Mode = SEARCH;
  if (value == 1) autoif_ds.Read_Mode = READ_ALL;

  value = xv_get( Autoif_coautord->coarftype, PANEL_VALUE, NULL);
  autoif_ds.Bv_Flag = TRUE;
  if (value == 1) autoif_ds.Bv_Flag = FALSE;

  value = xv_get( Autoif_coautord->coaraatm, PANEL_VALUE, NULL);
  autoif_ds.Aatm = FALSE;
  if (value == 1) autoif_ds.Aatm = TRUE;

  autoif_ds.Npar = atoi((char *) xv_get(Autoif_coautord->coarnpar, PANEL_VALUE, NULL));

  strcpy( autoif_ds.Dirname, (char *) xv_get(Autoif_coautord->coardir, PANEL_VALUE, NULL) );
  strcpy( autoif_ds.Fname, (char *) xv_get(Autoif_coautord->coarfile, PANEL_VALUE, NULL) );

  autoif_panel_refresh();
}




autoif_rd_curblk()
{
  autoif_ds.Cur_Block = atoi((char *) xv_get(Autoif_coautord->coarcurblk, PANEL_VALUE, NULL)); 
}



/*
* Menu handler for `AUTO Interface Reader...'.
*/
Menu_item
autoif_handler(item, op)
Menu_item       item;
Menu_generate   op;
{
    int
	locn[4];
    if (op == MENU_NOTIFY)
	if (item != (Menu_item) NULL)
	    autoif_open(DEFAULT_WIN_CONFIG,0,0,0,0);
	else {
	    pm(GET_LIST,"Win.Locn.Auto",0,3,locn,NULL);
	    if ((locn[0] != NO_LOCATION))
		autoif_open(SET_WIN_CONFIG,locn[0],locn[1],locn[2],locn[3]);
	    else
		autoif_open(DEFAULT_WIN_CONFIG,0,0,0,0);
	}

    /* if (op == MENU_NOTIFY)
       autoif_open(DEFAULT_WIN_CONFIG,0,0,0,0); */
    
   return item;
}





/* ------------------------------------------------------------------------------------- */
/* -------------------------------------------------------------------------------------
   Utilities for reading the AUTO Interface file

   last change:        6/25/92  (mrm)

   ------------------------------------------------------------------------------------- */
/* ------------------------------------------------------------------------------------- */

int
open_auto_file()
{
  int		status=NULL;
  static char   fname[SIZE_OF_DIR_PLUS_FNAME];

  if( autoif_ds.Auto_Fp != NULL ) status = close_auto_file();
  if( status != NULL ) return( status );

  strcpy( fname, autoif_ds.Dirname );
  strcat( fname, "/" );
  strcat( fname, autoif_ds.Fname);
  autoif_ds.Auto_Fp = fopen( fname, "r" );
  if (autoif_ds.Auto_Fp==NULL) status = AUTOIF_ERROR;
  
  return(status);
}


int
close_auto_file()
{
  int	status=NULL;

  fclose(autoif_ds.Auto_Fp);
  autoif_ds.Auto_Fp = NULL;

  return(status);
}



int
auto_rdblock_id( block_number )
int	block_number;
{
  int	i, start, status=NULL;

  if( block_number < 1 ) return( AUTOIF_ERROR );
  if( block_number == autoif_ds.Block_Index) return( status );

  if( block_number < autoif_ds.Block_Index)
    {
     status = open_auto_file();
     if( status != NULL ) return( status);
     start = 1;
    }
  else
    {
     if( autoif_ds.Auto_Fp == NULL )
	{
	 status = open_auto_file();
         if( status != NULL ) return( status);
        }
     start = autoif_ds.Block_Index + 1;
    }

  for( i=start; i<=block_number; i++)
     {
      status = auto_rdblock();
      if( status != NULL ) return( status );
     }
  autoif_ds.Cur_Block = autoif_ds.Block_Index = block_number;

  return( status );
}




int
auto_rdblock()
{
  double	dummy;
  int		status = NULL;
  int		i, j, total_points;

  if( fscanf( autoif_ds.Auto_Fp,"%d",&(autoif_ds.Ibr)) == EOF) return(AUTO_EOF);
  if( fscanf( autoif_ds.Auto_Fp,"%d",&(autoif_ds.Ntot)) == EOF) return(AUTO_EOF);
  if( fscanf( autoif_ds.Auto_Fp,"%d",&(autoif_ds.Itp)) == EOF) return(AUTO_EOF);
  if( fscanf( autoif_ds.Auto_Fp,"%d",&(autoif_ds.Lab)) == EOF) return(AUTO_EOF);
  if( fscanf( autoif_ds.Auto_Fp,"%d",&(autoif_ds.Nfpar)) == EOF) return(AUTO_EOF);
  if( fscanf( autoif_ds.Auto_Fp,"%d",&(autoif_ds.Isw)) == EOF) return(AUTO_EOF);
  if( fscanf( autoif_ds.Auto_Fp,"%d",&(autoif_ds.Ntpl)) == EOF) return(AUTO_EOF);
  if( fscanf( autoif_ds.Auto_Fp,"%d",&(autoif_ds.Nar)) == EOF) return(AUTO_EOF);
  if( fscanf( autoif_ds.Auto_Fp,"%d",&(autoif_ds.Nrowpr)) == EOF) return(AUTO_EOF);
  if( autoif_ds.Bv_Flag == TRUE )
     {
      if( fscanf( autoif_ds.Auto_Fp,"%d",&(autoif_ds.Ntst)) == EOF) return(AUTO_EOF);
      if( fscanf( autoif_ds.Auto_Fp,"%d",&(autoif_ds.Ncol)) == EOF) return(AUTO_EOF);
     }

  autoif_mem_manage( AUTOIF_MEM_INIT );

  if( autoif_ds.Bv_Flag == TRUE )
     {
      for(i=0; i<autoif_ds.Nfpar; i++)
         if( fscanf( autoif_ds.Auto_Fp,"%d",&(autoif_ds.Icp[i])) == EOF) return(AUTO_EOF);

      total_points = autoif_ds.Ntst*autoif_ds.Ncol+1;
      for(i=0; i<total_points; i++)
        {
         if( fscanf( autoif_ds.Auto_Fp,"%lg",&(autoif_ds.T[i])) == EOF) return(AUTO_EOF);
         for(j=0; j<autoif_ds.Nar-1; j++) 
	    if( fscanf( autoif_ds.Auto_Fp,"%lg",&(autoif_ds.U[i][j])) == EOF) return(AUTO_EOF);
        }

      for(i=0; i<autoif_ds.Nfpar; i++)
         if( fscanf( autoif_ds.Auto_Fp,"%lg",&dummy ) == EOF) return(AUTO_EOF);
 
      for(i=0; i<total_points; i++)
         for(j=0; j<autoif_ds.Nar-1; j++) 
	    if( fscanf( autoif_ds.Auto_Fp,"%lg",&dummy ) == EOF) return(AUTO_EOF);
     }
  else
     {
      if( fscanf( autoif_ds.Auto_Fp,"%lg",&(autoif_ds.T[0])) == EOF) return(AUTO_EOF);
	 for(j=0; j<autoif_ds.Nar-1; j++)
	    if( fscanf( autoif_ds.Auto_Fp,"%lg",&(autoif_ds.U[0][j])) == EOF) return(AUTO_EOF);
     }
 
  for(i=0; i<autoif_ds.Npar; i++)
      if( fscanf( autoif_ds.Auto_Fp,"%lg",&(autoif_ds.Par[i])) == EOF) return(AUTO_EOF);
   
  return( status );

}


/* -------------------------------------------------------------------------------------------
   This routine is tasked with ensuring the arrays ICP, T, U, PAR are always large enough to 
   accomodate the data to be read in from the AUTO interface file.   It can be called to
   allocate space or clear and free space, depending upon flag `operation', which may 
   take the values AUTOIF_MEM_INIT and AUTOIF_MEM_CLEAR (global constants).  This routine
   returns a status flag set to TRUE if action requested is successful and FALSE otherwise.

   last change:  6/26/92  (mrm)
   ------------------------------------------------------------------------------------------- */
int
autoif_mem_manage( operation )
int	operation;
{
  int		status = TRUE, total_pts=0, *ivector();

  double	*dvector(), **dmatrix();

  static int	nar_save=0,	/* store allocation dimensions for */
		npar_save=0,	/* current instances of Par, T, U  */
		nfpar_save=0,
		ncol_save=0,
		ntst_save=0,
		bv_flag_save=TRUE; 

  switch( operation )
      {
       case AUTOIF_MEM_INIT:

	   if( nfpar_save<autoif_ds.Nfpar )
	     {
	      if( nfpar_save > 0 )
		  free_ivector( autoif_ds.Icp, 0, nfpar_save );
	      nfpar_save = autoif_ds.Nfpar;
	      autoif_ds.Icp = ivector(0,autoif_ds.Nfpar);
             }

	   if( npar_save<autoif_ds.Npar )
	     {
	      if( npar_save > 0 )
		  free_dvector( autoif_ds.Par, 0, npar_save );
	      npar_save = autoif_ds.Npar;
	      autoif_ds.Par = dvector(0,autoif_ds.Npar);
             }

	   if( nar_save < autoif_ds.Nar | 
	       ncol_save<autoif_ds.Ncol | 
	       ntst_save<autoif_ds.Ntst )
		 {
		  total_pts = (bv_flag_save == TRUE)? ncol_save*ntst_save + 1:1;
                 
                  if( total_pts >= 1 )
		    free_dvector( autoif_ds.T, 0, total_pts );

		  if( total_pts >= 1 & nar_save > 0 )
		     free_dmatrix( autoif_ds.U, 0, total_pts, 0, nar_save);

	          nar_save  = autoif_ds.Nar;
		  if( autoif_ds.Bv_Flag == TRUE )
		    {
	             ncol_save = autoif_ds.Ncol;
	             ntst_save = autoif_ds.Ntst;
                    }
                  else ncol_save = ntst_save = 0;

		  bv_flag_save = autoif_ds.Bv_Flag;
		  total_pts = (autoif_ds.Bv_Flag == TRUE)? autoif_ds.Ncol*autoif_ds.Ntst + 1 : 1;

	          autoif_ds.T   = dvector(0,total_pts);
	          autoif_ds.U   = dmatrix(0,total_pts,0,autoif_ds.Nar);
                 }

	   break;

       case AUTOIF_MEM_CLEAR:

	   if( npar_save<autoif_ds.Npar )
	     {
	      if( npar_save > 0 ) free_dvector( autoif_ds.Par, 0, npar_save );
	      npar_save = 0;
             }

	    total_pts = ncol_save*ntst_save + 1;
	    if( total_pts > 1 & nar_save > 0 )
	     {
	      ncol_save = ntst_save = nar_save = 0;
	      free_dvector( autoif_ds.T, 0, total_pts );
	      free_dmatrix( autoif_ds.U, 0, total_pts, 0, nar_save);
             }
	   break;

       default:
	   return( FALSE );

      }

   return( status );
}


autoif_dump_blk()
{
  int		i, j, total_pts;

  fprintf( stderr,"Ibr   = %d           Ntpl = %d   \n", autoif_ds.Ibr, autoif_ds.Ntpl);
  fprintf( stderr,"Ntot  = %d           Nar  = %d   \n", autoif_ds.Ntot, autoif_ds.Nar);
  fprintf( stderr,"Lab   = %d           Ntst = %d   \n", autoif_ds.Lab, autoif_ds.Ntst);
  fprintf( stderr,"Nfpar = %d           Ncol = %d   \n", autoif_ds.Nfpar, autoif_ds.Ncol);
  fprintf( stderr,"Isw   = %d           Npar = %d   \n", autoif_ds.Isw, autoif_ds.Npar);

  fprintf( stderr,"\n\n Time (T)      Solution Components: \n");
  total_pts = (autoif_ds.Bv_Flag == TRUE)? autoif_ds.Ncol*autoif_ds.Ntst + 1 : 1;
  for(i=0; i<total_pts; i++)
    {
     fprintf(stderr,"\n%18.10lf   ",autoif_ds.T[i]);
     for(j=0; j<autoif_ds.Nar-1; j++)
	fprintf(stderr,"%18.10lg  ",autoif_ds.U[i][j]);
    }

  fprintf(stderr,"\n\n Parameters = ");
  for(i=0; i<autoif_ds.Npar; i++)
     fprintf(stderr,"%18.10lg ",autoif_ds.Par[i]);

  if (autoif_ds.Bv_Flag == TRUE)
     {
      fprintf(stderr,"\n\n Free Parameters = ");
      for(i=0; i<autoif_ds.Nfpar; i++)
        fprintf(stderr,"%d   ",autoif_ds.Icp[i]);
     }
}





autoif_init_mem()
{
  autoif_ds.Traj_Mem = (memory)  pm( GET, "Memory.Traj_Mem_Ptr" , NULL);
  autoif_ds.Cont_Mem = (memory)  pm( GET, "Memory.Cont_Mem_Ptr" , NULL);

  autoif_ds.Color[0] = *((int *) pm( GET, "Color.Plotting_Symbol", NULL));
  autoif_ds.Color[1] = (int) get_alt_color();
  autoif_ds.Color[2] = *((int *) pm( GET, "Color.Pick_Color_Choice", NULL));
  memory_start_new_flow( autoif_ds.Traj_Mem, 1, 0, 0, 100, 0, 0);
  memory_start_new_flow( autoif_ds.Cont_Mem, 1, 0, 0, 20, 0, 0);
}


autoif_add_pt()
{  
  int		i, j, dim, total_pts;
  double	*state, *parameters;

  static int    sp_color[3] = {-1,SYS_BLUE,LARGE_POINT};

  int   ph_space_dim  = *((int *) pm( GET, "Selected.Varb_Dim", NULL ));
  int   parameter_dim = *((int *) pm( GET, "Selected.Param_Dim", NULL ));

  state = (double *) calloc(ph_space_dim, sizeof(double));
  parameters = (double *) calloc(parameter_dim, sizeof(double));

  if( autoif_ds.Block_Index == 0 ) return( AUTOIF_ERROR );

  dim = (parameter_dim >= autoif_ds.Npar)? autoif_ds.Npar:parameter_dim;
  for(j=0; j<dim; j++) parameters[j] = autoif_ds.Par[j];

  total_pts = autoif_ds.Ncol*autoif_ds.Ntst + 1;
  for(i=0; i<total_pts; i++)
     {
      dim = (ph_space_dim >= autoif_ds.Nar)? autoif_ds.Nar:ph_space_dim;
      for(j=0; j<dim-1; j++) state[j] = autoif_ds.U[i][j];
      state[dim-1] = autoif_ds.T[i];

      memory_add_point(autoif_ds.Traj_Mem, state, parameters, sp_color, NULL, NULL, NULL, NULL);  
      if( i==0 )
	 memory_add_point(autoif_ds.Cont_Mem, state, parameters, sp_color, NULL, NULL, NULL, NULL);
     }

  free(state);  free(parameters);
}

