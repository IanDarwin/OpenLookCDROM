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
 * parser.c
 *
 * standard procs for parser window
 *
 */
#include <stdlib.h>
#include <stdio.h>
#include <xview/xview.h>
#include <xview/textsw.h>

#include <ui_init.h>
#include <constants.h>
#include <modellib.h>
#include "parserwin_ui.h"

#include "parser.h"

static parserwin_win_objects *parserwin_ip = NULL;
static int parserwin_type;
static char *parserwin_name;
static parserwin_editCwin_objects *parsereditCwin_ip = NULL;

/* 
 * parserwin_open()
 *
 * displays the parser window, creating it if necessary.
 */
parserwin_open()
{
  int status;
  char *buffer;
  extern char *parserwin_read_textpane();
  int error_notice();

  if (parserwin_ip == NULL)
    {
      /* create the window, make the owner the dstool cmd panel */
      parserwin_ip = parserwin_win_objects_initialize(NULL, cmd_ip->win);
      parserwin_init();
      parserwin_field_manager();
    }
  /* show the window */
  xv_set(parserwin_ip->win, WIN_SHOW, TRUE, WIN_FRONT, NULL);
  
  buffer = parserwin_read_textpane(parserwin_ip->textpane);
  status = parserwin_go(parserwin_type, buffer);
  free(buffer);

  if (status != PARSER_OK)       /* the parsed model failed, so notify user */
      error_notice(parserwin_ip->pan, "Error in parsing new dynamical system.");

}

/*
 * parserwin_field_manager()
 *
 * manager for the custom panel items
 */
parserwin_field_manager()
{
  
  /* exit now if the window has not been created */
  if (parserwin_ip == NULL) return(0);
  
  /* write data into the fields */
  parserwin_data_refresh();

  /* insert default file into textpane */
  xv_set(parserwin_ip->textpane, 
	 TEXTSW_LINE_BREAK_ACTION, TEXTSW_WRAP_AT_WORD, 
	 TEXTSW_IGNORE_LIMIT, TEXTSW_INFINITY,
	 NULL);
  xv_set(parserwin_ip->textpane, 
	 TEXTSW_CONTENTS, "# The Lorenz system\nx' = sigma ( y - x )\ny' = rho x - y - x z \nz' = -beta z + x y\n\nINITIAL sigma 10.0 rho 28.0 beta 2.66666666\nRANGE x -30 30 y -30 30 z -20 50\n", NULL);

  return(0);
}


int
  parserwin_init()
{
  /* initialize the values for the text fields */
  parserwin_type = 0;

  return(0);
}

/*
 * parserwin_read_window()
 *
 * routine to read data from all items in the parser window
 */
int
  parserwin_read_window()
{
  int i;
  char *strng;
/*  double atof(); */ /* declared in stdlib.h */

  /* if no window, then nothing to read */
  if (parserwin_ip == NULL) return;

  /* read setting item */
  parserwin_type = xv_get(parserwin_ip->type, PANEL_VALUE);
  parserwin_name = (char *) xv_get(parserwin_ip->name, PANEL_VALUE);
}


/*
 * parserwin_data_refresh()
 *
 * routine to refresh the data in all items in the parser window
 */
int
  parserwin_data_refresh()
{
  int i;

  /* if no window, then nothing to refresh */
  if (parserwin_ip == NULL) return;

  /* refresh setting items */
  xv_set(parserwin_ip->type, PANEL_VALUE, parserwin_type, NULL);

}



/*
 * parserwin_build_go
 *
 * routine called when the user wants to load the parser ds
 *
 */
parserwin_build_go()
{
  int i, n = -1;
  extern int parserwin_init();

  /* find out which model we are and load it! */
  for (i=0; i<N_DS; i++)
    {
      if (DS_Sel[i].ds_init == parserwin_open) /* found it! */
      {
	n = i;
	i = N_DS;
      }
    }
  if (n == -1)
    {
      fprintf(stdout," The PARSER dynamical system is not loaded in the models!\n");
      return;
    }
  load_model(n);
}

char *
parserwin_read_textpane(tp)
Textsw tp;
{
  char *buffer;
  int comment = FALSE;
  int i, len = xv_get(tp, TEXTSW_LENGTH);
  

  buffer = (char *) malloc((len+1)*sizeof(char));
  if (!buffer) return NULL;

  xv_get(tp, TEXTSW_CONTENTS, 0, buffer, len);
  buffer[len] = '\0';

  /* now remove carriage returns, and comment lines! */
  for (i=0; i<len; i++)
    {

      if (buffer[i]=='#') comment = TRUE;
      else if (buffer[i] == '\n')
	{
	  buffer[i]=' ';
	  comment = FALSE;
	}
      if (comment) buffer[i]=' ';
    }
  return buffer;
}



/*
 * parserwin_writec_go
 *
 * routine called when the user wants to write a C-code file
 *
 */
parserwin_writec_go()
{
  char *buffer;
  extern char *parserwin_read_textpane();
  FILE *fp;
  Textsw_status tstatus;
  char filename[L_tmpnam], *tmpnam();
  int status;

  tmpnam(filename);
  /* fprintf(stdout, "Using tempfile: %s\n",filename); */
  buffer = parserwin_read_textpane(parserwin_ip->textpane);

  /* open up editor window */
  if (parsereditCwin_ip == NULL)
    {
      parsereditCwin_ip = parserwin_editCwin_objects_initialize(NULL, cmd_ip->win);
    }
  /* show the window */
  xv_set(parsereditCwin_ip->editCwin, WIN_SHOW, TRUE, WIN_FRONT, NULL);

  /* write C-code to temporary file */
  fp = fopen(filename, "w");
  if (fp)
    {
      status = parserwin_writec(parserwin_type, parserwin_name, buffer, fp);
      fclose(fp);
      if (status == PARSER_OK)
	{
	  /* empty window and load in temp file */
	  xv_set(parsereditCwin_ip->editCtextpane, TEXTSW_STATUS, &tstatus,
		 TEXTSW_FILE_CONTENTS, filename, NULL);
	  if (tstatus == TEXTSW_STATUS_OKAY)
	    {
	      /* put cursor at top of buffer */
	      xv_set(parsereditCwin_ip->editCtextpane,
		     TEXTSW_INSERTION_POINT, 0, NULL);
	      textsw_possibly_normalize(parsereditCwin_ip->editCtextpane, 0);
	    }
	  else
	    system_mess_proc(0,
		 "parserwin_writec_go: Cannot load C code into window.");
	}
      else /* failure reading model */
	error_notice(parserwin_ip->pan,"Cannot parse dynamical system into C.");
      
      /* remove temp file */
      unlink(filename);
    }
  else
    {
      system_mess_proc(0,
	"parserwin_writec_go: Cannot open temp file for writing C-code.");
    }
  free(buffer);
}
