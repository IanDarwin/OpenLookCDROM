/*
 * $Id: logger.c,v 2.1 1992/10/06 11:31:37 jipping Exp $
 * **********************************************************************
 *
 *   Logger.c ==> Logging routines for XVTDL.
 *
 * ----------------------------------------------------------------------
 * Copyright (c) 1992 by Mike Jipping and Hope College
 *
 * Permission is granted to copy and distribute this file in modified or
 * unmodified form, for noncommercial use, provided (a) this copyright notice
 * is preserved, (b) no attempt is made to restrict redistribution of this
 * file, and (c) this file is not distributed as part of any collection whose
 * redistribution is restricted by a compilation copyright.
 * ----------------------------------------------------------------------
 *
 * Revision History:
 * $Log: logger.c,v $
 * Revision 2.1  1992/10/06  11:31:37  jipping
 * fixes after beta test:  change to log message to include category.
 *
 * Revision 2.0  1992/09/15  11:46:06  jipping
 * Release 4.0 beta:
 *    changed the behavior of the "Cancel" on the user-specified log
 *    edit.  It now completely cancels the log entry -- no entry is
 *    made into the log.
 *
 * Revision 1.3  1992/07/28  15:15:34  jipping
 * Condensed "cancel_log_edit" into "log_log_edit" -- to eliminate bug in
 * cancelling log edits.
 *
 * Revision 1.2  1992/07/28  12:04:52  jipping
 * Added an unlink of temp log file with "%" attached.
 *
 * Revision 1.1  1992/07/27  18:38:29  jipping
 * Initial revision
 *
 *
 */

#include "globaldefs.h"
#include "logedit_ui.h"

int log_level, log_info_level;
char log_file[LINESIZ];

logedit_log_edit_objects	*logedit_log_edit;

/*
 ***********************************************************************
 *  A dummy routine to call a GUIDE initialize function for the 
 *  log editor.
 */
void initialize_log_editor()
{
	logedit_log_edit = logedit_log_edit_objects_initialize(NULL, tdlist);
}

/*
 * **********************************************************************
 * This routines logs a day_entry structure in a way appropriate to 
 * the setting of "log_level" and "log_info_level".
 */
void log_entry (de,cr)
struct day_entry *de;
struct category_rec *cr;
{
	FILE *logfd;
	FILE *tmp;
	char *temp_file, atime[28];
   struct timeval tv;

	logfd = fopen(log_file, "a+");
	if (logfd == NULL) {
		fprintf(stderr, "Problems opening log file (%s) -- no log made\n", log_file);
		return;
	}

   gettimeofday(&tv, 0);
   tm = localtime(&tv.tv_sec);
	strcpy(atime, asctime(tm));
	atime[strlen(atime)-1] = '\0';

	if ((log_level == LOG_AT_QUIT) || (log_info_level == LOG_TIMESTAMP)) {
		fprintf(logfd, "<%s>\n", atime);
		if (de->checked) {
			fprintf(logfd, "   COMPLETED \"%s\"\n", de->text);
		} else {
			fprintf(logfd, "   UNCHECKED \"%s\"\n", de->text);
		}
		fprintf(logfd, "   Category: %s\n", cr->name);
		fprintf(logfd, "   Priority: %d\n", de->priority);
		fprintf(logfd, "   Entered on %d/%d/%d\n\n",
				         dc_month(de->starting_day_code),
				         dc_day(de->starting_day_code),
				         dc_year(de->starting_day_code)-1900);
		fclose(logfd);
	} else {
		if ( (temp_file = (char *)tempnam(NULL, "log")) == NULL) {
			fprintf(stderr, "Unable to create temporary log file name\n");
			return;
		}
		if ( (tmp = fopen(temp_file, "w")) == NULL) {
			fprintf(stderr, "Unable to open temp file %s\n", temp_file);
			free(temp_file);
			return;
		}
		fprintf(tmp, "<%s>\n", atime);
		if (de->checked) {
			fprintf(tmp, "   COMPLETED \"%s\"\n", de->text);
		} else {
			fprintf(tmp, "   UNCHECKED \"%s\"\n", de->text);
		}
		fprintf(tmp, "   Category: %s\n", cr->name);
		fprintf(tmp, "   Priority: %d\n", de->priority);
		fprintf(tmp, "   Entered on %d/%d/%d\n\n",
				       dc_month(de->starting_day_code),
				       dc_day(de->starting_day_code),
				       dc_year(de->starting_day_code)-1900);
		fclose(tmp);

		xv_set(logedit_log_edit->log_editor_done, PANEL_CLIENT_DATA, logfd, 0);
		xv_set(logedit_log_edit->log_editor, TEXTSW_FILE, temp_file, 0);
		xv_set(logedit_log_edit->log_edit, XV_SHOW, TRUE, 0);
	}

}

/*
 * **********************************************************************
 * Callback routine for both the "Done" and "Cancel" button on the log
 * editor.  Logs the comments into the log file.
 */
void log_log_edit(item, event)
Panel_item	item;
Event		*event;
{
	logedit_log_edit_objects *ip =
		(logedit_log_edit_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	FILE *logfd;
	char fname[LINESIZ];

	logfd = (FILE *)xv_get(ip->log_editor_done, PANEL_CLIENT_DATA);

	strcpy(fname, (char *)xv_get(ip->log_editor, TEXTSW_FILE));
	if (item == ip->log_editor_done) {
		textsw_save(ip->log_editor, 20, 20);
		copyfile2(fname, logfd);
		fprintf(logfd, "\n");
	} else {
		textsw_reset(ip->log_editor, 0, 0);
	}

	fclose(logfd);
	unlink(fname);
	unlink(strcat(fname,"%"));
	xv_set(logedit_log_edit->log_editor, TEXTSW_FILE, NULL, 0);
	xv_set(logedit_log_edit->log_edit, XV_SHOW, FALSE, 0);
}
