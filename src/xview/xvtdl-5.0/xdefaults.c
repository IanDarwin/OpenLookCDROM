/*
 * $Id: xdefaults.c,v 2.0.1.1 1995/06/29 20:48:04 ian Exp $
 * **********************************************************************
 *
 *  Xdefaults.c ==> Routines to control the setting of X defaults.
 *
 *   ** Please note:  These routines were shamelessly ripped off from
 *   ** the Ftptool 4.3 distribution.  This tool was written by Mike 
 *   ** Sullivan (Mike.Sullivan@Sun.COM) and are governed by the
 *   ** copyright below.  Thanks, Mike!
 *
 * ----------------------------------------------------------------------
 * 	NOTICE TO USER: The source code, including the glyphs or icons 
 * 	forming a par of the OPEN LOOK TM Graphic User Interface, on this 
 * 	tape and in these files is copyrighted under U.S. and international
 * 	laws. Sun Microsystems, Inc. of Mountain View, California owns
 * 	the copyright and has design patents pending on many of the icons. 
 * 	AT&T is the owner of the OPEN LOOK trademark associated with the
 * 	materials on this tape. Users and possessors of this source code 
 * 	are hereby granted a nonexclusive, royalty-free copyright and 
 * 	design patent license to use this code in individual and 
 * 	commercial software. A royalty-free, nonexclusive trademark
 * 	license to refer to the code and output as "OPEN LOOK" compatible 
 * 	is available from AT&T if, and only if, the appearance of the 
 * 	icons or glyphs is not changed in any manner except as absolutely
 * 	necessary to accommodate the standard resolution of the screen or
 * 	other output device, the code and output is not changed except as 
 * 	authorized herein, and the code and output is validated by AT&T. 
 * 	Bigelow & Holmes is the owner of the Lucida (R) trademark for the
 * 	fonts and bit-mapped images associated with the materials on this 
 * 	tape. Users are granted a royalty-free, nonexclusive license to use
 * 	the trademark only to identify the fonts and bit-mapped images if, 
 * 	and only if, the fonts and bit-mapped images are not modified in any
 * 	way by the user. 
 *
 *
 * 	Any use of this source code must include, in the user documentation 
 * 	and internal comments to the code, notices to the end user as  
 * 	follows:
 *
 *
 * 	(c) Copyright 1989 Sun Microsystems, Inc. Sun design patents
 * 	pending in the U.S. and foreign countries. OPEN LOOK is a 
 * 	trademark of AT&T. Used by written permission of the owners.
 *
 *
 *  	(c) Copyright Bigelow & Holmes 1986, 1985. Lucida is a registered 
 * 	trademark of Bigelow & Holmes. Permission to use the Lucida 
 * 	trademark is hereby granted only in association with the images 
 * 	and fonts described in this file.
 *
 *
 *
 * 	SUN MICROSYSTEMS, INC., AT&T, AND BIGELOW & HOLMES 
 * 	MAKE NO REPRESENTATIONS ABOUT THE SUITABILITY OF
 *  	THIS SOURCE CODE FOR ANY PURPOSE. IT IS PROVIDED "AS IS" 
 * 	WITHOUT EXPRESS OR IMPLIED WARRANTY OF ANY KIND. 
 * 	SUN  MICROSYSTEMS, INC., AT&T AND BIGELOW  & HOLMES, 
 * 	SEVERALLY AND INDIVIDUALLY, DISCLAIM ALL WARRANTIES 
 * 	WITH REGARD TO THIS SOURCE CODE, INCLUDING ALL IMPLIED
 * 	WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * 	PARTICULAR PURPOSE. IN NO EVENT SHALL SUN MICROSYSTEMS,
 * 	INC., AT&T OR BIGELOW & HOLMES BE LIABLE FOR ANY
 * 	SPECIAL, INDIRECT, INCIDENTAL, OR CONSEQUENTIAL DAMAGES,
 * 	OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA 	
 * 	OR PROFITS, WHETHER IN AN ACTION OF  CONTRACT, NEGLIGENCE
 * 	OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION
 * 	WITH THE USE OR PERFORMANCE OF THIS SOURCE CODE.
 * ----------------------------------------------------------------------
 *
 * Revision History:
 * $Log: xdefaults.c,v $
 * Revision 2.0.1.1  1995/06/29  20:48:04  ian
 * Ifdef to include unistd made little more general.
 *
 * Revision 2.0  1992/09/15  11:47:49  jipping
 * Release 4.0 beta:  Added new props for deadlines and default priorities.
 *
 * Revision 1.2  1992/07/30  14:10:31  jipping
 * Changed "lp" to "" for default printer to accomodate using PRINTER
 * env var.
 *
 * Revision 1.1  1992/07/27  18:45:10  jipping
 * Initial revision
 *
 *
 *
 */

#include "globaldefs.h"
#if	defined(SVR4) || defined(linux)
#include <unistd.h>
#endif

char default_printer[25], log_preference[10], log_file[LINESIZ];
char priority_listing[13], log_info_pref[25];
char sort_order[5], chron_listing[13], print_file[LINESIZ];
char default_print_dest[10];
char on_propagation[8], fgcolor[50], bgcolor[50];
int postscriptmode,logging,default_priority;
struct deadline_rec default_deadline;
int multiclick_timeout;

char *find_dotfile(dotfile)
char	*dotfile;
{
	char	*home;
	char	*filename=NULL;

	home = (char *)getenv("HOME");
	if (home != NULL && home[0] != '\0') {
		/* try $HOME/dotfile */
		filename = malloc((unsigned int)(strlen(home)+1+strlen(dotfile)+1));
		if (filename == NULL)
			return NULL;
		sprintf(filename, "%s/%s", home, dotfile);
		if (access(filename, F_OK) == -1) {
			free(filename);
			return NULL;
		}
		/* found it */
		return filename;
	}
	filename = strdup(dotfile);
	if (filename == NULL)
		return NULL;
	if (access(filename, F_OK) == -1) {
		free(filename);
		return NULL;
	}

	return filename;
}

char *create_dotfile(dotfile, mode)
char    *dotfile;
int             mode;
{
	char    *home;
	char    *filename=NULL;
	int             fd;
	
	home = (char *)getenv("HOME");
	if (home != NULL && home[0] != '\0') {
		/* try $HOME/dotfile */
		filename = malloc((unsigned int)(strlen(home)+1+strlen(dotfile)+1));
		if (filename == NULL)
			return NULL;
		sprintf(filename, "%s/%s", home, dotfile);
		if ((fd = creat(filename, mode)) == -1) {
			free(filename);
			return NULL;
		}
		close(fd);
		/* found it */
		return filename;
	}
	filename = strdup(dotfile);
	if (filename == NULL)
		return NULL;
	if ((fd = creat(filename, mode)) == -1) {
		free(filename);
		return NULL;
	}
	close(fd);
	
	return filename;
}

void load_xdefaults()
{
	char	*str;
	int		ct;
	char	*xapplresdir, res_file[LINESIZ];
	char	*xvtdl_defaults;

	if ((xapplresdir = (char *)getenv("XAPPLRESDIR")) != NULL) {
		sprintf(res_file, "%s/Xvtdl", xapplresdir);
		if (access(res_file, R_OK) == 0) {
			defaults_load_db(res_file);
		}
	}
	xvtdl_defaults = find_dotfile(".xvtdlrc");
	if (xvtdl_defaults) {
		defaults_load_db(xvtdl_defaults);
		free(xvtdl_defaults);
	}

	strcpy(sort_order, 
			 (char *)defaults_get_string("xvtdl.sortorder",
												  "Xvtdl.SortOrder",
												  "033"));
	strcpy(priority_listing,
	       (char *)defaults_get_string("xvtdl.prioritylisting",
												  "Xvtdl.PriorityListing",
												  "ascending"));
	strcpy(chron_listing,
	       (char *)defaults_get_string("xvtdl.chronlisting",
												  "Xvtdl.ChronListing",
												  "newest"));

	strcpy(default_print_dest, 
		    (char *)defaults_get_string("xvtdl.printdestination",
												  "Xvtdl.PrintDestination",
												  "printer"));

	strcpy(default_printer,
			 (char *)defaults_get_string("xvtdl.printer",
												  "Xvtdl.Printer",
												  ""));

	postscriptmode =
		(int)defaults_get_boolean("xvtdl.postscript",
										  "Xvtdl.PostScript",
										  FALSE);

	strcpy(print_file,
			 (char *)defaults_get_string("xvtdl.printfile",
												  "Xvtdl.PrintFile",
												  ""));

	logging = 
		(int)defaults_get_boolean("xvtdl.logging",
										  "Xvtdl.Logging",
										  FALSE);
	if (! logging) log_level = LOG_NEVER;

	strcpy(log_preference, 
			 (char *)defaults_get_string("xvtdl.logpreference",
												  "Xvtdl.LogPreference",
												  "atquit"));
	if (logging) {
		if (EQUAL(log_preference, "atquit")) {
			log_level = LOG_AT_QUIT;
		} else {
			log_level = LOG_AT_CHECKED;
		}
	}

	strcpy(log_info_pref, 
			 (char *)defaults_get_string("xvtdl.loginfopreference",
												  "Xvtdl.LogInfoPreference",
												  "timestamp"));
	if (EQUAL(log_info_pref, "timestamp")) {
		log_info_level = LOG_TIMESTAMP;
	} else {
		log_info_level = LOG_USER_SPEC;
	}

	strcpy(log_file, 
	       (char *)defaults_get_string("xvtdl.logfilename",
												  "Xvtdl.LogFileName",
												  ""));

	default_priority = 
		(int)defaults_get_integer("xvtdl.defaultpriority",
										  "Xvtdl.DefaultPriority",
										  5);

	strcpy(on_propagation,
			 (char *)defaults_get_string("xvtdl.onPropagation",
												  "Xvtdl.OnPropagation",
												  "delete"));

	strcpy(fgcolor,
	       (char *)defaults_get_string("xvtdl.foregroundColor",
												  "Xvtdl.ForegroundColor",
												  "ivory"));
	strcpy(bgcolor,
	       (char *)defaults_get_string("xvtdl.backgroundColor",
												  "Xvtdl.BackgroundColor",
												  "blue"));

	default_deadline.actions = 0;

	default_deadline.actions =
		(int) defaults_get_boolean("xvtdl.deadlineDelete",
											"Xvtdl.DeadlineDelete",
											FALSE);
	default_deadline.delete_time =
		(int) defaults_get_integer("xvtdl.deadlineDeleteTime",
											"Xvtdl.DeadlineDeleteTime",
											0);
	default_deadline.delete_units =
		(int) defaults_get_integer("xvtdl.deadlineDeleteUnits",
											"Xvtdl.DeadlineDeleteUnits",
											0);

	default_deadline.actions +=
		(int) defaults_get_boolean("xvtdl.deadlineUpPriority",
											"Xvtdl.DeadlineUpPriority",
											FALSE) * 2;
	default_deadline.priority_up_units =
		(int) defaults_get_integer("xvtdl.deadlineUpIncrement",
											"Xvtdl.DeadlineUpIncrement",
											0);

	default_deadline.actions +=
		(int) defaults_get_boolean("xvtdl.deadlineDownPriority",
											"Xvtdl.DeadlineDownPriority",
											FALSE) * 4;
	default_deadline.priority_down_units =
		(int) defaults_get_integer("xvtdl.deadlineDownIncrement",
											"Xvtdl.DeadlineDownIncrement",
											0);

	default_deadline.actions +=
		(int) defaults_get_boolean("xvtdl.deadlineMailOn",
											"Xvtdl.DeadlineMailOn",
											FALSE) * 8;
	strcpy(default_deadline.mail_on,
			 (char *)defaults_get_string("xvtdl.deadlineMailOnAddress",
												  "Xvtdl.DeadlineMailOnAddress",
												  ""));
	
	default_deadline.actions +=
		(int) defaults_get_boolean("xvtdl.deadlineMailAfter",
											"Xvtdl.DeadlineMailAfter",
											FALSE) * 16;
	strcpy(default_deadline.mail_after,
			 (char *)defaults_get_string("xvtdl.deadlineMailAfterAddress",
												  "Xvtdl.DeadlineMailAfterAddress",
												  ""));

	default_deadline.actions +=
		(int) defaults_get_boolean("xvtdl.deadlineMoveAfter",
											"Xvtdl.DeadlineMoveAfter",
											FALSE) * 32;
	default_deadline.move_time =
		(int) defaults_get_integer("xvtdl.deadlineMoveTime",
											"Xvtdl.DeadlineMoveTime",
											0);
	default_deadline.move_units =
		(int) defaults_get_integer("xvtdl.deadlineMoveUnits",
											"Xvtdl.DeadlineMoveUnits",
											0);

	multiclick_timeout =
		defaults_get_integer("openwindows.multiclicktimeout",
				     "OpenWindows.MultiClickTimeout", 4);
}

void set_xdefaults()
{
	defaults_set_string("Xvtdl.SortOrder", sort_order);
	defaults_set_string("Xvtdl.ChronListing", chron_listing);
	defaults_set_string("Xvtdl.Printer", default_printer);
	defaults_set_boolean("Xvtdl.PostScript", postscriptmode);
	defaults_set_string("Xvtdl.PrintDestination", default_print_dest);
	defaults_set_string("Xvtdl.PrintFile", print_file);
	defaults_set_boolean("Xvtdl.Logging", logging);
	defaults_set_string("Xvtdl.LogPreference", log_preference);
	defaults_set_string("Xvtdl.LogInfoPreference", log_info_pref);
	defaults_set_string("Xvtdl.LogFileName", log_file);
	defaults_set_string("Xvtdl.PriorityListing", priority_listing);
	defaults_set_integer("Xvtdl.DefaultPriority", default_priority);
	defaults_set_string("Xvtdl.OnPropagation", on_propagation);
	defaults_set_string("Xvtdl.ForegroundColor", fgcolor);
	defaults_set_string("Xvtdl.BackgroundColor", bgcolor);
	defaults_set_boolean("Xvtdl.DeadlineDelete",
			     BIT_IS_SET(default_deadline.actions, 0)?TRUE:FALSE);
	defaults_set_boolean("Xvtdl.DeadlineUpPriority",
			     BIT_IS_SET(default_deadline.actions, 1)?TRUE:FALSE);
	defaults_set_boolean("Xvtdl.DeadlineDownPriority",
			     BIT_IS_SET(default_deadline.actions, 2)?TRUE:FALSE);
	defaults_set_boolean("Xvtdl.DeadlineMailOn",
			     BIT_IS_SET(default_deadline.actions, 3)?TRUE:FALSE);
	defaults_set_boolean("Xvtdl.DeadlineMailAfter",
			     BIT_IS_SET(default_deadline.actions, 4)?TRUE:FALSE);
	defaults_set_boolean("Xvtdl.DeadlineMoveAfter",
			     BIT_IS_SET(default_deadline.actions, 5)?TRUE:FALSE);
	defaults_set_integer("Xvtdl.DeadlineDeleteTime",
								default_deadline.delete_time);
	defaults_set_integer("Xvtdl.DeadlineDeleteUnits",
								default_deadline.delete_units);
	defaults_set_integer("Xvtdl.DeadlineUpIncrement",
								default_deadline.priority_up_units);
	defaults_set_integer("Xvtdl.DeadlineDownIncrement",
								default_deadline.priority_down_units);
	defaults_set_string("Xvtdl.DeadlineMailOnAddress",
							  default_deadline.mail_on);
	defaults_set_string("Xvtdl.DeadlineMailAfterAddress",
							  default_deadline.mail_after);
	defaults_set_integer("Xvtdl.DeadlineMoveTime",
								default_deadline.move_time);
	defaults_set_integer("Xvtdl.DeadlineMoveUnits",
								default_deadline.move_units);
}

void save_xdefaults()
{
   char  *filename=NULL;
	FILE	*fp;
	char	*str;
	char	*true = "True";
	char	*false = "False";

	filename = find_dotfile(".xvtdlrc");
	if (filename == NULL)
		if ((filename = (char *)create_dotfile(".xvtdlrc", 0644)) == NULL)
			return;
	if ((fp = fopen(filename, "w")) == NULL) {
		fprintf(stderr, "Could not write defaults file...\n");
		return;
	}

	fprintf(fp, "Xvtdl.SortOrder:\t%s\n", sort_order);
	fprintf(fp, "Xvtdl.ChronListing:\t%s\n", chron_listing);
	fprintf(fp, "Xvtdl.Printer:\t%s\n", default_printer);
	fprintf(fp, "Xvtdl.PostScript:\t%s\n", 
		(postscriptmode == 0) ? false : true);
	fprintf(fp, "Xvtdl.PrintDestination:\t%s\n", default_print_dest);
	fprintf(fp, "Xvtdl.PrintFile:\t%s\n", print_file);
	fprintf(fp, "Xvtdl.Logging:\t%s\n", 
		(logging == 0) ? false : true);
	fprintf(fp, "Xvtdl.LogPreference:\t%s\n", log_preference);
	fprintf(fp, "Xvtdl.LogInfoPreference:\t%s\n", log_info_pref);
	fprintf(fp, "Xvtdl.LogFileName:\t%s\n", log_file);
	fprintf(fp, "Xvtdl.PriorityListing:\t%s\n", priority_listing);
	fprintf(fp, "Xvtdl.DefaultPriority:\t%d\n", default_priority);
	fprintf(fp, "Xvtdl.OnPropagation:\t%s\n", on_propagation);
	fprintf(fp, "Xvtdl.ForegroundColor:\t%s\n", fgcolor);
	fprintf(fp, "Xvtdl.BackgroundColor:\t%s\n", bgcolor);
	fprintf(fp, "Xvtdl.DeadlineDelete:\t%s\n",
		     BIT_IS_SET(default_deadline.actions, 0)?true:false);
	fprintf(fp, "Xvtdl.DeadlineDeleteTime:\t%d\n", default_deadline.delete_time);
	fprintf(fp, "Xvtdl.DeadlineDeleteUnits:\t%d\n", default_deadline.delete_units);
	fprintf(fp, "Xvtdl.DeadlineUpPriority:\t%s\n",
		     BIT_IS_SET(default_deadline.actions, 1)?true:false);
	fprintf(fp, "Xvtdl.DeadlineUpIncrement:\t%d\n",
      			default_deadline.priority_up_units);
   fprintf(fp, "Xvtdl.DeadlineDownPriority:\t%s\n",
		     BIT_IS_SET(default_deadline.actions, 2)?true:false);
	fprintf(fp, "Xvtdl.DeadlineDownIncrement:\t%d\n",
      			default_deadline.priority_down_units);
	fprintf(fp, "Xvtdl.DeadlineMailOn:\t%s\n",
		     BIT_IS_SET(default_deadline.actions, 3)?true:false);
	fprintf(fp, "Xvtdl.DeadlineMailOnAddress:\t%s\n",
			  default_deadline.mail_on);
	fprintf(fp, "Xvtdl.DeadlineMailAfter:\t%s\n",
		     BIT_IS_SET(default_deadline.actions, 4)?true:false);
	fprintf(fp, "Xvtdl.DeadlineMailAfterAddress:\t%s\n",
			  default_deadline.mail_after);
	fprintf(fp, "Xvtdl.DeadlineMoveAfter:\t%s\n",
		     BIT_IS_SET(default_deadline.actions, 5)?true:false);
	fprintf(fp, "Xvtdl.DeadlineMoveTime:\t%d\n", default_deadline.move_time);
	fprintf(fp, "Xvtdl.DeadlineMoveUnits:\t%d\n", default_deadline.move_units);

	fclose(fp);
	free(filename);
}
