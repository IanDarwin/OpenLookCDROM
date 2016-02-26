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
 * print_procs.c contains code for printing a postscript file
 */
#include <stdio.h>
#include <stdlib.h>

#include <portability.h>

#ifdef HAS_STRINGS_H
#include <strings.h>
#endif

#ifdef HAS_STRING_H
#include <string.h>
#endif

#include <sys/param.h>
#include <sys/types.h>
#include <math.h>
#include <xview/xview.h>
#include <gcm.h>
#include <signal.h>
#include <setjmp.h>

#include <filters.h>
#include <constants.h>
#include <defaults.h>
#include <memory.h>
#include "twoD.h"
#include <pm.h>
#include <plot.h>
#include <symbols.h>
#include <print.h>

jmp_buf         env;				/* saves stack info in case of broken pipe */

/* 
 * print_go() prints postscript to printer or file. In the case of a broken pipe error
 *	(caused, for example, by an incorrect printer name) the program is interrupted,
 *	an error message is displayed, and control returns to the window manager.
 * Arguments: TRUE means overwrite file even if it exists; FALSE means ask user whether
 * 	to overwrite or cancel print.
 * Returns 0 = successful printing; -1 = cannot open file/pipe; -2 = broken pipe
 *         1 = file exists; ask for overwrite permission.
 */
print_go(force)
int	force;
{
  FILE          *fp;
  void		broken_pipe_error(),(*old_pipe_errfunc)();
  int		status=0;
  char          *fname =  (char *) calloc(SIZE_OF_FNAME+6,sizeof(char)); /* 6 = strlen("lpr -P") */
  char          *dirname =  (char *) calloc(SIZE_OF_DIR_PLUS_FNAME+6,sizeof(char));
  
  if( *((int *)pm(GET, "Print.File_Flag", NULL)) ) /* open PS file */
    {
      dirname = strcat(strcat((char *)pm(GET, "Print.Directory", dirname, NULL), "/"), 
		       (char *)pm(GET, "Print.Filename", fname, NULL) );
      if (check_file_to_read(dirname) && !force )	/* file exists; ask user for permission */
	status = 1;
      else if (check_file_to_write(dirname))
	{
	  fp = fopen(dirname,"w");
	  status = print_to_file(fileno(fp));
	  fclose(fp);
	}
      else status = -1;
    }
  else							/* pipe data to printer */
    {
      /* set up interrupt handler for broken pipe; save previous handler if not set to ignore */
      if( (old_pipe_errfunc=signal(SIGPIPE,SIG_IGN)) != SIG_IGN) 
	signal(SIGPIPE, broken_pipe_error);
      if(setjmp(env) == 0)				/* save stack (return here on error) */
	{		
	  strcpy(fname, "lpr -P");
	  pm(GET, "Print.Printer_Name", &(fname[6]), NULL);
	  if( !(fp=popen( fname, "w")))			/* open pipe to printer */
	    status = -1;
	  else
	    {
	      status = print_to_file(fileno(fp));	/* fileno returns file descriptor */
	      pclose(fp);
	    }
	}
      else						/* there was a broken pipe! */
	status = -2;
      signal(SIGPIPE,old_pipe_errfunc);			/* restore old value for error handler */
    }
  free(fname);
  free(dirname);
  return(status);
}

/*
 * print_to_file() calls routines to produce postscript file.  The main driver.
 * Returns 0 upon successful printing
 */
int
print_to_file(fd)
int		fd;
{
  char		buf[BUFFERSIZE];
  int		num_pts;

  write_header(fd, buf);
  if( write_macros(fd, buf) != 0 )			/* Prolog not properly read */
    return -1;
  write_prolog(fd, buf);
  write_ps_prelim(fd, buf);
  num_pts = write_ps_data(fd, buf);
  if( *((int *)pm(GET, "Print.Show_BBox", NULL)) )
    write_bbox(fd, buf);
  if( *((int *)pm(GET, "Print.Show_Info", NULL)) )
    write_sys_info(fd, buf, num_pts);
  write_trailer(fd, buf);
  return 0;
}

/*
 * write_ps_data() takes data from dstool memory and writes it to a PS file
 */
int
write_ps_data(fd, buf)
int		fd;
char		buf[];
{
  int		number_sys_colors, number_traj_colors;
  memory	mem_ptr;
  double	red[MAX_COLORS],green[MAX_COLORS],blue[MAX_COLORS];
  struct RGB_Color	rgb;
  struct Filter_DataS   *filter;
  struct Plot_Pt 	*plot;
  int		num_pts=0;
  int		color = *((int *)pm(GET, "Print.Color_Flag", NULL));
  int 		connect_flag = *((int *)pm(GET, "Print.Connect", NULL));

  filter = (struct Filter_DataS *)calloc(1,sizeof(struct Filter_DataS));
  plot   = (struct Plot_Pt *)calloc(1,sizeof(struct Plot_Pt));
  filter->window_id = plot->win_num = *((int *)pm(GET, "Print.Owner", NULL));
  
  if(color)
    {
      rgb.r = red;  rgb.g = green; rgb.b = blue;
      index_to_ps_rgb(&rgb,&number_sys_colors,&number_traj_colors);
    }

  sprintf(buf,"%% translate to user coords\ngsave\nxscale  yscale  scale x0 neg y0 neg translate\n");
  write(fd,buf,strlen(buf));

  if ( (mem_ptr = (memory) pm(GET, "Memory.Traj", NULL)) &&
      valid_mem_to_win(memory_get_type(mem_ptr), plot->win_num) )
    num_pts += plot_data(fd, buf, filter, plot, color, &rgb, mem_ptr, number_sys_colors, connect_flag); 

  if( (mem_ptr = (memory) pm(GET, "Memory.Mult", NULL)) &&
     valid_mem_to_win(memory_get_type(mem_ptr),plot->win_num) )
    num_pts += plot_data(fd, buf, filter, plot, color, &rgb, mem_ptr, number_sys_colors, connect_flag);

  if ( (mem_ptr = (memory) pm(GET, "Memory.Fixed", NULL)) &&
      valid_mem_to_win(memory_get_type(mem_ptr), plot->win_num) )
    num_pts += plot_data(fd, buf, filter, plot, color, &rgb, mem_ptr, number_sys_colors, connect_flag);

  if ( (mem_ptr = (memory) pm(GET, "Memory.Cont", NULL)) &&
      valid_mem_to_win(memory_get_type(mem_ptr), plot->win_num))
    num_pts += plot_data(fd, buf, filter, plot, color, &rgb, mem_ptr, number_sys_colors, connect_flag); 

  if ( (mem_ptr = (memory) pm(GET, "Memory.Sel_Pt", NULL)) &&
      valid_mem_to_win(memory_get_type(mem_ptr), plot->win_num) )
    num_pts += plot_data(fd, buf, filter, plot, color, &rgb, mem_ptr, number_sys_colors, FALSE); 

  if ( (mem_ptr = (memory) pm(GET, "Memory.Param", NULL)) &&
      valid_mem_to_win(memory_get_type(mem_ptr), plot->win_num) )
    num_pts += plot_data(fd, buf, filter, plot, color, &rgb, mem_ptr, number_sys_colors, FALSE); 

  sprintf(buf,"grestore\n");
  write(fd,buf,strlen(buf));
  free(filter);
  free(plot);
  return(num_pts);
}

/*
 * plot_data() writes data from memory to PS file
 * returns number of points written 6/10/92 fjw
 */
int
  plot_data(fd,buf,filter,plot,color,rgb,mem_ptr,number_sys_colors,connect_flag)
int           		fd;
char			buf[];
struct  Filter_DataS    *filter;
struct	Plot_Pt		*plot;
int             	color;
struct	RGB_Color	*rgb;
memory			mem_ptr;
int			number_sys_colors,connect_flag;
{
  int	        *colors, num_pts = 0, last_plot_color=-1, format = PRINT_PRECISION;
  char		*symbol,*get_ps_symbol(), *line_str;
  double	hor_min = *((double *)pm(GET, "Print.HorMin", NULL)),
  		hor_max = *((double *)pm(GET, "Print.HorMax", NULL)),
  		ver_min = *((double *)pm(GET, "Print.VerMin", NULL)),
  		ver_max = *((double *)pm(GET, "Print.VerMax", NULL));   
  int		f_dim = *((int *)pm(GET, "Model.Funct_Dim",NULL));
  double	*dwkspace, *dvector();

  dwkspace = dvector(0,f_dim-1);
  if (memory_reset_read(mem_ptr) == 0)
    while (memory_read_next_flow(mem_ptr, NULL, NULL, NULL, NULL, NULL) == 0)
      {
	while (memory_read_next_traj(mem_ptr, NULL, NULL, NULL) == 0)
	  {
	    line_str = "\0";
	    while (memory_read_next_point(mem_ptr, &(filter->state), &(filter->parameters), &colors,
					  NULL,  NULL) == 0)
	      {
		filter->alt_color_index = colors[0];
		filter->pick_color_index = colors[1];
		plot->symbol = colors[2];
		symbol = get_ps_symbol(plot->symbol);
		pt_to_xy(filter->window_id, filter->state, filter->parameters,&(plot->x), &(plot->y), dwkspace);
		if(pt_within_region(plot,hor_min,hor_max,ver_min,ver_max))  /* checks and SCALES */
		  {
		    if(color)
		      {
			if( filter->alt_color_index < 0 )
			  plot->color = filter->pick_color_index; /* use system colormap */
			else
			  plot->color = number_sys_colors + get_plot_color(filter);
			if (last_plot_color != plot->color)
			  {
			    sprintf(buf,"%.6f\t%.6f\t%.6f\tsetrgbcolor ",
				    rgb->r[plot->color],rgb->g[plot->color],rgb->b[plot->color]);
			    write(fd,buf,strlen(buf));
			    last_plot_color = plot->color;
			  }
		      }
		    sprintf(buf,"%.6f\t%.6f\t%s%s\n",plot->x,plot->y,line_str,symbol);  
		    write(fd,buf,strlen(buf));
		    num_pts++;
		    if (connect_flag) line_str = "Line\t";
		  }
		else line_str = "\0";
	      }
	  }
      }
  free_dvector(dwkspace,0,f_dim-1);
  return(num_pts);
}

/*
 * write_header() writes PostScript header comments in compliance with PostScript
 * 	structuring conventions
 */
int
write_header(fd,buf)
int		fd;
char		buf[];
{
  char		*get_the_time(),*get_user_info(),*get_ds_name(),*get_plot_font();

  sprintf(buf,"%%!PS-Adobe-2.0\n%%%%Title: %s\n%%%%Creator: dstool\n%%%%CreationDate: %s\n%%%%For: %s\n%%%%Pages: 1\n%%%%DocumentFonts: %s\n%%%%BoundingBox: 0 0 612 792\n%%%%EndComments\n%% begin the prolog...\n", 
	  get_ds_name(),get_the_time(),get_user_info(),get_plot_font());
  write(fd,buf,strlen(buf));
}

/*
 * write_prolog() defines constants, macros, and procedures to be used by PostScript
 */
int
write_prolog(fd,buf)
int		fd;
char		buf[];
{
  char 		*get_plot_font();

  sprintf(buf,"\n%s %d %s %d %s\n%s %s %s\n%s %s %s\n",	    /* fonts */
	  "/SetFonts { /LabelPtSize", *((int *)pm(GET, "Print.Label_Pt", NULL)),
	  "def /TitlePtSize", *((int *)pm(GET, "Print.Title_Pt", NULL)), "def",
	  "   /LabelFont   ",get_plot_font(),"findfont LabelPtSize scalefont def",
	  "   /TitleFont   ",get_plot_font(),"findfont TitlePtSize scalefont def } def");
  write(fd,buf,strlen(buf));
  sprintf(buf,"%s %d %s\t\t\t\t%s\n%s %d %s\t\t\t\t%s\n",   /* BBox sides */
	  "/xBBoxSide",*((int *)pm(GET, "Print.BBox_Hor_Length", NULL)),"def",
	  "% length (in pts) of bounding box",
	  "/yBBoxSide",*((int *)pm(GET, "Print.BBox_Ver_Length", NULL)),"def",
	  "% height (in pts) of bounding box");
  write(fd,buf,strlen(buf));
  sprintf(buf,"%s %d %s\t\t\t\t\t%s\n%s %d %s\t\t\t\t%s\n",	/* BBox origin*/
	  "/xorigin",*((int *)pm(GET, "Print.BBox_Hor_Offset", NULL)),"def",
	  "% displacement (in pts) from left side of page",
	  "/yorigin",*((int *)pm(GET, "Print.BBox_Ver_Offset", NULL)),"def",
	  "% displacement (in pts) from bottom of page");
  write(fd,buf,strlen(buf));

  /* The following are definitions which are based on the above definitions.
     They do not vary from system to system. */
  sprintf(buf, "%s\n%s\t%s\n%s\t\t%s\n%s\t\t%s\n%s\t\t\t%s\n%s\t\t\t%s\n%s\n\n",
	  "% The below are not model specific but depend on the above definitions",
	  "/Yp yBBoxSide 60 add 6 HeadPtSize mul add def",
	  "% print 6 header lines at this height",
	  "/xscale xBBoxSide xf x0 sub div def",
	  "% horizontal scaling factor",
	  "/yscale yBBoxSide yf y0 sub div def",
	  "% vertical scaling factor",
	  "/xscalei 1 xscale div def",
	  "% horizontal scaling factor inverse",
	  "/yscalei 1 yscale div def",
	  "% vertical scaling factor inverse",
	  "%%%%EndProlog");
  write(fd,buf,strlen(buf));
}

/*
 * write_sys_info() writes information (to the PS file) about the dynamical system being studied
 */
int
write_sys_info(fd,buf,num_pts)
int		fd;
char		buf[];
int		num_pts;
{
  char          *get_the_time(),*get_ds_name();
  char		label[MAX_LEN_VARB_NAME];
  int		i,dim;
  int 		format = *((int *) pm(GET, "Defaults.Precision", NULL));

  sprintf(buf,"%s\n(Title: %s) Display\n(Date: %s) Display\n",
	  "gsave /Courier findfont HeadPtSize scalefont setfont",
	  get_ds_name(),get_the_time());
  write(fd,buf,strlen(buf));
  sprintf(buf,"(%s Range = [ %.*lg, %.*lg ];   ",
	  (char *) pm(GET, "Print.Hor_Label", label, NULL),
	  format,  *((double *) pm(GET, "Print.HorMin", NULL)),
	  format,  *((double *) pm(GET, "Print.HorMax", NULL)) );
  write(fd,buf,strlen(buf));
  sprintf(buf,"%s Range = [ %.*lg, %.*lg ]) Display\n",
	  (char *) pm(GET, "Print.Ver_Label", label, NULL),
	  format,  *((double *) pm(GET, "Print.VerMin", NULL)),
	  format,  *((double *) pm(GET, "Print.VerMax", NULL)) );
  write(fd,buf,strlen(buf));

  pm(GET, "Model.Varb_Names", 0, label, NULL);
  sprintf(buf,"(Initial Conditions: ( %s",label);
  write(fd,buf,strlen(buf));
  dim = *((int *)pm( GET, "Model.Varb_Dim", NULL ));
  for(i=1;i<dim;i++){
    pm(GET, "Model.Varb_Names", i, label, NULL);
    sprintf(buf,", %s",label);
    write(fd,buf,strlen(buf));
  }
  sprintf(buf," )=( %.*lg",format, *((double *) pm( GET, "Selected.Varb_Ic", 0, NULL))); 
  write(fd,buf,strlen(buf));
  for(i=1;i<dim;i++){
    sprintf(buf,", %.*lg",format, *((double *) pm( GET, "Selected.Varb_Ic", i, NULL))); 
    write(fd,buf,strlen(buf));
  }
  sprintf(buf," )) Display\n");
  write(fd,buf,strlen(buf));

  dim = *((int *)pm( GET, "Model.Param_Dim", NULL ));
  if(dim>0)
    {
      pm(GET, "Model.Param_Names", 0, label, NULL);
      sprintf(buf,"(Parameters: ( %s",label);
      write(fd,buf,strlen(buf));
      for(i=1;i<dim;i++){
	pm(GET, "Model.Param_Names", i, label, NULL);
	sprintf(buf,", %s",label);
	write(fd,buf,strlen(buf));
      }
      sprintf(buf," )=( %.*lg",format, *((double *) pm( GET, "Selected.Param_Ic", 0, NULL)));
      write(fd,buf,strlen(buf));
      for(i=1;i<dim;i++){
	sprintf(buf,", %.*lg",format, *((double *) pm( GET, "Selected.Param_Ic", i, NULL))); 
	write(fd,buf,strlen(buf));
      }
    }
  else
    {
      sprintf(buf,"(Parameters: ( none");
      write(fd,buf,strlen(buf));
    }
  sprintf(buf," )) Display\n");
  write(fd,buf,strlen(buf));

  if( *((int *)pm(GET, "Model.Mapping_Flag", NULL)) )
    sprintf(buf,"(Num Pts = %d) Display   grestore\n",num_pts);
  else
    sprintf(buf,"(Num Pts = %d;  Time Step = %.*lg) Display   grestore\n",
	    num_pts, format, *((double *) pm( GET, "Flow.Stepsize", NULL))); 
  write(fd,buf,strlen(buf));
}

/*
 * write_bbox() encodes the information for a bounding box and tick marks
 */
int
write_bbox(fd,buf)
int		fd;
char		buf[];
{
  sprintf(buf,"gsave 1 setlinewidth  BoundingBox grestore\n%d %d Ticks\n",
	  *((int *)pm(GET, "Print.Num_Hor_Ticks", NULL)), 
	  *((int *)pm(GET, "Print.Num_Ver_Ticks", NULL)) );
  write(fd,buf,strlen(buf));
}

/*
 * write_ps_prelim() sets up fonts and draws labels
 */
int
write_ps_prelim(fd,buf)
int		fd;
char		buf[];
{
  char		s[MAX_LEN_DS_TITLE]; /* should really be max(MAX_LEN_DS_TITLE, MAX_LEN_VARB_NAME) */

  sprintf(buf,"gsave xorigin yorigin translate linewidth setlinewidth SetFonts\n");
  write(fd,buf,strlen(buf));

  if( *((int *)pm(GET, "Print.Landscape", NULL)) )	    /* landscape mode? */
    {
      sprintf(buf, "90 rotate\t\t\t\t%% Landscape mode\n");
      write(fd,buf,strlen(buf));
    }

  sprintf(buf, "xBBoxSide 2 div yBBoxSide TitlePtSize 2 mul add\n"); /* Title */
  write(fd,buf,strlen(buf));
  pm(GET, "Print.Title", s, NULL);			   
  sprintf(buf,"(%s) () FigureTitle\n", s );
  write(fd,buf,strlen(buf));

  if ( *((int *)pm(GET, "Print.Label_Range", NULL))	)   /* label axes range? */
    {
      sprintf(buf,"(%.6lg) (%.6lg) 2 PlaceXLabels\n(%.6lg) (%.6lg) 2 PlaceYLabels\n",
	      *((double *) pm(GET, "Print.HorMin", NULL)),
	      *((double *) pm(GET, "Print.HorMax", NULL)),
	      *((double *) pm(GET, "Print.VerMin", NULL)),
	      *((double *) pm(GET, "Print.VerMax", NULL)) );
      write(fd,buf,strlen(buf));
    }

  pm(GET, "Print.Hor_Label", s, NULL);		    /* label axes */
  sprintf(buf,"(%s) XLabel\n", s );
  write(fd,buf,strlen(buf));
  pm(GET, "Print.Ver_Label", s, NULL);
  sprintf(buf,"(%s) HorYLabel\n%s\n", s,"% Place all Labels and Annotations BEFORE this line");
  write(fd,buf,strlen(buf));
}

/*
 * write_trailer() ends the PS document
 */
int
write_trailer(fd, buf)
int		fd;
char		buf[];
{
  sprintf(buf,"grestore  showpage\n%%%%Trailer\n");
  write(fd,buf,strlen(buf));
}

/*
 * get_plot_font() returns a PostScript font name
 */
char *
get_plot_font()
{
  return(PS_font[*((int *)pm(GET, "Print.Font", NULL))]); /* PS_font[] is defined in print.h */
}

/*
 * pt_within_region() returns TRUE if pt is visible on the view window; FALSE otherwise
 * The pt is scaled to fit within [0,1] x [0,1]
 */
int
pt_within_region(plot,hor_min,hor_max,ver_min,ver_max)
struct Plot_Pt  *plot;
double          hor_min,hor_max,ver_min,ver_max;
{
  int in_region;
  if (in_region = ( (plot->x > hor_min ) &&  (plot->x < hor_max ) &&
	            (plot->y > ver_min ) && (plot->y < ver_max ) ))
    {
      plot->x = (plot->x - hor_min ) / (hor_max - hor_min);
      plot->y = (plot->y - ver_min ) / (ver_max - ver_min);
    }
  return(in_region);
}

/*
 * get_ps_symbol() returns encoded PostScript macro which will draw an appropriate symbol
 */
char *
get_ps_symbol(symbol_index)
int		symbol_index;
{
  int		i;
  for(i=0; i<Num_Symbols; i++)
    if(Symbol_Codes[i] == symbol_index)
      return(Symbol_Labels[i]); /* make change to src/include/symbols_def.h */
	  
  system_mess_proc(1,"print: Symbol not found! Using default symbol");
  return(Symbol_Labels[0]);
}

/*
 * pt_to_xy() takes a window number, state, and parameters, and returns the x,y
 * coords of the projection of that pt onto the given window
 */
pt_to_xy(win_num, state, param, x, y, f)
int	win_num;
double	*state, *param, *f,	/* allocated dvectors; f is wkspace of lenght function_dim */
  	*x, *y;			/* pointers to doubles */
{
  switch(TwoD_Ds[win_num]->TwoD_Win_Ds->Active_Hor_Type) 
    {
    case PHASE_SPACE_VARB:
      *x = state[TwoD_Ds[win_num]->TwoD_Win_Ds->Active_Hor_Index];
      break;
    case PARAMETER_VARB:
      *x = param[TwoD_Ds[win_num]->TwoD_Win_Ds->Active_Hor_Index];
      break;
    case FUNCTION_VARB:
      get_ds_func(f, state, param);
      *x = f[TwoD_Ds[win_num]->TwoD_Win_Ds->Active_Hor_Index];
      break;
    }
  switch(TwoD_Ds[win_num]->TwoD_Win_Ds->Active_Ver_Type) 
    {
    case PHASE_SPACE_VARB:
      *y = state[TwoD_Ds[win_num]->TwoD_Win_Ds->Active_Ver_Index];
      break;
    case PARAMETER_VARB:
      *y = param[TwoD_Ds[win_num]->TwoD_Win_Ds->Active_Ver_Index];
      break;
    case FUNCTION_VARB:
      get_ds_func(f, state, param);
      *y = f[TwoD_Ds[win_num]->TwoD_Win_Ds->Active_Ver_Index];
      break;
    }
}

/*
 * returns index into cms of colortable of "current" twoD window
 */
get_plot_color(filter)
struct  Filter_DataS    *filter;
{
  return(TwoD_Ds[filter->window_id]->TwoD_Win_Ds->ColorTable[get_color(filter)]);  
}

/*
 * index_to_ps_rgb() obtains the current colormap and associates to each index
 *	the corresponding postscript (RGB) color
 */
int
index_to_ps_rgb(rgb,number_sys_colors, number_traj_colors)
struct RGB_Color        *rgb;
int			*number_sys_colors, *number_traj_colors;
{
  int		i, *red, *green, *blue, *ivector(), n_total;

  red = ivector(0, MAX_COLORS);
  green = ivector(0, MAX_COLORS);
  blue = ivector(0, MAX_COLORS);
  *number_sys_colors = *( (int *)  pm( GET, "Color.Sys_Colormap_Size", NULL));
  *number_traj_colors = *( (int *) pm( GET, "Color.Traj_Colormap_Size", NULL));
  n_total = *number_sys_colors + *number_traj_colors;
  pm( GET_LIST, "Color.Red_Table", 0, n_total-1, red, NULL); 
  pm( GET_LIST, "Color.Green_Table", 0, n_total-1, green, NULL); 
  pm( GET_LIST, "Color.Blue_Table", 0, n_total-1, blue, NULL); 


  for(i=0;i<n_total;i++)				/* PostScript RGB is between 0 and 1 */
    {
      rgb->r[i] = (double) red[i] / (double)(MAX_COLORS - 1.);
      rgb->g[i] = (double) green[i] / (double)(MAX_COLORS - 1.);
      rgb->b[i] = (double) blue[i] / (double)(MAX_COLORS - 1.); 
    }  
  free_ivector(red, 0, MAX_COLORS);
  free_ivector(green, 0, MAX_COLORS);
  free_ivector(blue, 0, MAX_COLORS);
  return(0);
}

/*
 * write_macros() reads in the PostScript prolog as defined by the environmental variable 
 * DSTOOL_PS_PROLOG if defined, or, if not, as defined in $DSTOOL/site_specific/PROLOG_FILE
 * where PROLOG_FILE is as defined in defaults.h.  Upon success, 0 is returned.
 */
int
write_macros(fd,buf)
int		fd;
char		buf[];
{
  FILE    *pp;
  char    *prologname;
  int	  pd,n;

  prologname = (char *) calloc(SIZE_OF_DIR_PLUS_FNAME,sizeof(char));

  if( !get_dstool_path( prologname, DSTOOL_PS_PROLOG ) )		/* choose DSTOOL_PS_PROLOG file if defined */
    {  
      if ( !get_dstool_path( prologname, DSTOOL_DIR ) )	/* otherwise choose default file from defaults.h */
	{
	  free(prologname);
	  system_mess_proc(1,"print: DSTOOL environmental variable not set! Prolog not written!");
	  return -1;
	}
      strcat(prologname,"/site_specific/");
      strcat(prologname,PROLOG_FILE);  
    }
  if( !(pp = fopen(prologname, "r")) )/* replace this filename with prologname  */
    {
      system_mess_proc(1,"print: PostScript prolog not found or permission denied");
      system_mess_proc(1,prologname);
      return -1;
    }
  else
    {
      pd = fileno(pp);
      while( (n = read(pd, buf, BUFFERSIZE)) > 0)	/* sucks up the Prolog and prints it out again */
	write(fd, buf, n);
    } 
  fclose(pp);
  free(prologname);
  return 0;
}


/*
 * broken_pipe_error() is called upon the error signal SIGPIPE (attempt to write on pipe
 *   or other socket with no one to read it).  The function returns the stack (and flow) to the
 *   point just prior to opening the pipe.
 */
void
broken_pipe_error()
{
   longjmp(env,-1);
}

