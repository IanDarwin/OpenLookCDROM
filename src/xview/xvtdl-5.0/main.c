/* $Id: main.c,v 3.3 1992/10/06 11:32:10 jipping Exp $
 *
 * **********************************************************************
 *
 *  Main program for Xvtdl.
 *
 *  This routine sets up parameters from the command line, parses the
 *  specified todo database, and creates the windows, and sits back 
 *  to gather events.
 *
 *  Copyright (c) 1993 by Mike Jipping and Hope College
 * 
 *  Permission is granted to copy and distribute this file in modified or
 *  unmodified form, for noncommercial use, provided (a) this copyright notice
 *  is preserved, (b) no attempt is made to restrict redistribution of this
 *  file, and (c) this file is not distributed as part of any collection whose
 *  redistribution is restricted by a compilation copyright.
 *
 *  Revision History:
 *
 *  $Log: main.c,v $
 * Revision 3.3  1992/10/06  11:32:10  jipping
 * fixes after beta test:  adding HELPDIR to HELPPATH
 *
 * Revision 3.2  1992/07/30  19:46:10  jipping
 * Got rid of access calls (!?) for a simpler scheme of setting up the
 * filename THEN opening after gathering parameters.
 *
 * Revision 3.1  1992/07/29  17:37:13  jipping
 * Added calls to access() to properly handle missing database files.
 *
 * Revision 3.0  1992/07/27  18:40:26  jipping
 * Release 3.0 includes:
 * * changed from "version.h" to "patchlevel.h" for comp.windows.x
 * * check X resources through a call to "load_xdefaults"
 * * fixed timer initialization
 * * interpose a destroy function
 *
 * Revision 2.5  1992/07/16  16:16:35  jipping
 * Fixed a bug with the Printer resource when none is present.
 *
 * Revision 2.4  1992/07/16  13:31:18  jipping
 * Implemented two resource values: default_printer and postscriptmode.
 *
 * Revision 2.3  1992/07/14  12:31:38  jipping
 * Added more flexible version notation (using version.h).
 *
 * Revision 2.2  1992/07/13  15:52:57  jipping
 * Changed version number.
 * Hardcoded initial main window creation size.
 *
 * Revision 2.1  1992/07/13  15:15:19  jipping
 * Cleaned up code to avoid compilation warnings.
 *
 * Revision 2.0  1992/07/10  17:31:08  jipping
 * Initial release
 *
 *
 *
 * **********************************************************************
 */

#include "globaldefs.h"
#include "patchlevel.h"

extern FILE *yyin;
extern Notify_value midnight(), hourly();
extern Notify_value my_destroy_func();
extern Notify_value my_signal_handler();
extern Notify_value my_event_interposer();

int debug=FALSE, verbose=FALSE, changed=FALSE;
int has_color=FALSE;
int mono=FALSE;

Frame tdlist;
Canvas calendar;

struct tm current, *tm, today, *localtime();
int curr_month, curr_day, curr_year;
time_t access_time, modify_time;
char fname[FILENAMESIZ];

/*
 * This routine parses the command line, looking for options.  The only
 * options possible are
 *
 *   -d        -> turns on debugging output
 *   -f file   -> use "file" as the todo database instead of
 *                $HOME/.tododb
 *   -m        -> use monochrome display settings
 */

void parse_command_line (argc, argv)
int argc;
char **argv;
{
   int argind;
   char *tcp;

   argind = 1;
   while (argind < argc)
   {   
      tcp = argv[argind];
      if (*tcp == '-') {
         tcp++;
         switch(*tcp){
            case 'd':   /* d selects Debug output */
                debug = TRUE;
                verbose = TRUE;
                break;
            
            case 'f':
                argind++;
                tcp = argv[argind];
                strcpy(fname, tcp);
                break;

				case 'm':
					 mono = TRUE;
                break;

            default:
                printf("%c is not a legal flag\n", *tcp);
         }
      } else {
         printf("Bad argument: %s\n", tcp);
      }
      argind++;
    }
}


/*
 * The main program.
 */

void main (argc, argv)
int argc;
char **argv;
{
   struct timeval tv;
   struct itimerval timer;
	struct stat *st;
	int sterror;
   char timstr[16], title[20];
	char *home, *helppath, help[LINESIZ];
   long till_midnight;
	Display *dpy;

#ifdef HELPDIR
	/* Set up the HELP var */
	helppath = (char *)getenv("HELPPATH");
	if (helppath == NULL) {
		sprintf(help, "HELPPATH=/usr/lib/help:%s", HELPDIR);
	} else {
		sprintf(help, "HELPPATH=%s:%s", helppath, HELPDIR);
	}
	if (putenv(help)) fprintf(stderr, "Could not put environment\n");
#endif

   /* init XView */
   xv_init(XV_INIT_ARGC_PTR_ARGV, &argc, argv, 0);

   /* set up initial parameters, the parse the command line */
	home = (char *)getenv("HOME");
	if (home == NULL) {
		strcpy(fname, ".tododb");
	} else {
		strcpy(fname, getenv("HOME"));
		strcat(fname, "/.tododb");
	}
   parse_command_line(argc, argv);
	st = NEW(struct stat);
	sterror = stat(fname, st);
	access_time = st->st_atime;
	modify_time = st->st_mtime;
	yyin = fopen(fname, "r");

	/* Process any X resources that we have */
	load_xdefaults();

   /* set up date-oriented variables */
   gettimeofday(&tv, 0);
   tm = localtime(&tv.tv_sec);
   today = *tm;
   curr_month = today.tm_mon+1;
   curr_day = today.tm_mday;
   curr_year = today.tm_year+1900;

   /* create the windows */
	sprintf(title, "To Do List %d.%s", VERSION, PATCHLEVEL);
   tdlist = xv_create(NULL, FRAME,
                      FRAME_LABEL, title,
							 XV_WIDTH, 435,
							 XV_HEIGHT, 480,
                      0);

	dpy = (Display *)xv_get(tdlist, XV_DISPLAY);
   if (!mono) {
		has_color = (DefaultDepth(dpy, DefaultScreen(dpy)) > 1);
	}

   /* parse the todo database and set up the "current" list */
   if (yyin != NULL) {
		yyparse();
		propagate();
		fclose(yyin);
	}
	if (category_head == NULL) {
		category_head = (struct category_rec *)new_category("Every Day", NULL, FALSE);
		changed = TRUE;
	}

   create_windows();
	
   xv_set(categories,
          PANEL_CLIENT_DATA, category_head,
          0);

	entry_head = category_head->entry_head;
	entry_tail = category_head->entry_tail;
	rl_head = category_head->rl_head;
	rl_tail = category_head->rl_tail;
	display_list(curr_month, curr_day, curr_year);

   /* set up the timer alarms to go off every hour */
   timer.it_value.tv_sec = 3600;
   timer.it_interval.tv_sec = 3600;
   timer.it_value.tv_usec = 0;
   timer.it_interval.tv_usec = 0;
   notify_set_itimer_func(tdlist, hourly, ITIMER_REAL, &timer, NULL);

	/* catch window quits... */
	notify_interpose_destroy_func(tdlist, my_destroy_func);
	notify_interpose_event_func(tdlist, my_event_interposer, NOTIFY_SAFE);

   /* sit back and watch the show */
   xv_main_loop(tdlist);
}
