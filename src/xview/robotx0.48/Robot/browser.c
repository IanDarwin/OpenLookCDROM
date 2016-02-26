/*
 * dirlist.c
 * This program creates two scrolling lists, one with directories and one
 * with files.  It allows the user to search through directories for files
 * and then select a file to "open".
 * This is based on code posted to alt.toollkits.xview.
 */

/*
 *      (XGed : Graph Editor for the X Window System)
 *
 *      Carlos Puchol (cpg@cs.utexas.edu)
 *
 * This file is public domain. Do with it what you please.
 * I am not to be held responsible for any damage caused by the use of this
 * code. I would appreciate if you quote the authors, if you use this code.
 *
 *  %W%     %G%
 */

#include <stdio.h>
#include <stdlib.h>
#include <xview/xview.h>
#include <xview/panel.h>
#include <dirent.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/param.h>

#include "robot.h"

#define DESELECT 0
#define SELECT 1
#define NULL_STR ""

Frame dir_frame;
Panel dir_panel;
Panel_item pi_dir, pi_file, ti_file, mi_dir, bi_open, filter;
Panel_item	filter_name;

static Bool Filter = False;	/* set to true if we're to filter names */
char filter_string[34];		/* the filter string to use */
void check_filter();
Bool check_name();

DIR *dirp;
struct dirent *entry;
char *current;

char pathname[MAXPATHLEN];

int num_files, num_dirs;

/* glyphs */
/* Server_image	file_glyph;
Server_image	folder_glyph;
Server_image	dotdot_glyph;


*/


int Open();
int Cancel();
void setup_list();
void dir_proc();
void file_proc();
int check_read();
void filter_set();

/* main(argc, argv)
int argc;
char *argv[];
*/
void
dir_frame_create(frame)
Frame	frame;
{
  char *tmpname;


/* create glyphs */

/*	folder_glyph = (Server_image) xv_create(NULL, SERVER_IMAGE,
		XV_WIDTH, 	16,
		XV_HEIGHT, 	16,
		SERVER_IMAGE_BITS, 	folder_bits,
		NULL);

	dotdot_glyph = (Server_image) xv_create(NULL, SERVER_IMAGE,
		XV_WIDTH, 	16,
		XV_HEIGHT, 	16,
		SERVER_IMAGE_BITS, 	dotdot_bits,
		NULL);

	file_glyph = (Server_image) xv_create(NULL, SERVER_IMAGE,
		XV_WIDTH, 	16,
		XV_HEIGHT, 	16,
		SERVER_IMAGE_BITS, 	folder_bits,
		NULL);

	printf("Glyphs created\n"); */


/*
 * Start in the current (PWD) directory
 */

/*  tmpname = (char *)getenv("PWD"); */
  tmpname = (char *) getwd(pathname);
  if(tmpname == NULL)
	tmpname = (char *)getenv("HOME");
  current = (char *)malloc(strlen(tmpname)+1);
  memset(current, '\0', strlen(tmpname)+1);
  strcat(current, tmpname);
/*  free(tmpname); */


  dir_frame = xv_create(frame, FRAME_CMD,
    FRAME_LABEL,		"File Selector",
    FRAME_DONE_PROC, Open,
    NULL);

  dir_panel = (Panel)xv_get(dir_frame, FRAME_CMD_PANEL);

  mi_dir = (Panel_item)xv_create(dir_panel, PANEL_MESSAGE,
    PANEL_LABEL_STRING,		current,
    PANEL_LABEL_BOLD,		TRUE,
    NULL);


  ti_file = (Panel_item)xv_create(dir_panel, PANEL_TEXT,
    PANEL_LABEL_STRING, 	"File:",
    PANEL_VALUE_DISPLAY_LENGTH,	50,
    PANEL_VALUE_STORED_LENGTH,	256,
    PANEL_NEXT_ROW,		-1,
    PANEL_VALUE,		NULL_STR,
    PANEL_NOTIFY_PROC,		Open,  /* hitting return will also get file */
    NULL);

  filter = (Panel_item)xv_create(dir_panel, PANEL_CHOICE,
		PANEL_LABEL_STRING, "Filter on:",
		PANEL_CHOICE_STRINGS, 
			"No Filter", ".rob", ".dat", "Other", NULL,
		PANEL_VALUE, 0,
		PANEL_NOTIFY_PROC, filter_set,
		XV_HELP_DATA, "robot:filter",
		NULL);
 filter_name = (Panel_item)xv_create(dir_panel, PANEL_TEXT,
		PANEL_LABEL_STRING, "Filter string:",
		PANEL_VALUE_DISPLAY_LENGTH, 20,
		PANEL_NOTIFY_PROC, filter_set,
		PANEL_INACTIVE, TRUE,
		XV_HELP_DATA, "robot:filter_name",
		NULL);
 



  pi_dir = (Panel_item)xv_create(dir_panel, PANEL_LIST,
    PANEL_LABEL_STRING,		"Directories:",
    PANEL_LIST_DISPLAY_ROWS,	10,
    PANEL_CHOOSE_ONE,		TRUE,
    /* PANEL_CHOOSE_NONE,          TRUE, */
    PANEL_READ_ONLY,		TRUE,
    PANEL_NOTIFY_PROC,		dir_proc,
    PANEL_NEXT_ROW,		-1,
    PANEL_LIST_WIDTH,		150,
    /* PANEL_LIST_ROW_HEIGHT,	16, */
    PANEL_LAYOUT,		PANEL_VERTICAL,
    NULL);

  pi_file = (Panel_item)xv_create(dir_panel, PANEL_LIST,
    PANEL_LABEL_STRING,		"Files:",
    PANEL_LIST_DISPLAY_ROWS,	10,
    PANEL_CHOOSE_ONE,		TRUE,
    PANEL_READ_ONLY,		TRUE,
    PANEL_NOTIFY_PROC,		file_proc,
    PANEL_LIST_WIDTH,		150,
   /* PANEL_LIST_ROW_HEIGHT,	16, */
    PANEL_LAYOUT,		PANEL_VERTICAL,
    NULL);

  bi_open = (Panel_item)xv_create(dir_panel, PANEL_BUTTON,
    PANEL_LABEL_STRING,		"Select",
    PANEL_NOTIFY_PROC,		Open,
    XV_X,			370,
    XV_Y,			103,
    NULL);

/* and set this button as the default */
   xv_set(dir_panel, PANEL_DEFAULT_ITEM, bi_open, NULL);

  (void)xv_create(dir_panel, PANEL_BUTTON,
    PANEL_LABEL_STRING,		"Cancel",
    PANEL_NOTIFY_PROC,		Cancel,
    XV_X,			370,
    XV_Y,			133,
    NULL);
/* following line gave no initial files on DECstation - should 
 * probably fix rather than just commenting out! */
  /* if (check_read(current)) */
    setup_list();

  window_fit(dir_panel);
  window_fit(dir_frame);

}

void
dir_show()
{
	xv_set(dir_frame, XV_SHOW, TRUE,
			FRAME_CMD_PUSHPIN_IN,	TRUE, NULL);
}

void
filter_set()
{
char	*tmpname;
int	i;


	xv_set(dir_panel,
	WIN_CONSUME_EVENTS,
			WIN_NO_EVENTS, WIN_REPAINT, NULL,
			NULL);

	check_filter();
/*
 * First delete the old entries
 */
/* hide list during operations on it */
	xv_set(pi_file, XV_SHOW, FALSE, NULL);

  for (i = num_files - 1; i >= 0; i--)
    xv_set(pi_file, PANEL_LIST_DELETE, i, NULL);
	num_files = 0;


/* recheck directory list */

  dirp = opendir(current);

  while ((entry = readdir(dirp)) != NULL) {
    tmpname = (char *)malloc(strlen(current) + entry->d_namlen + 2);
    memset(tmpname, '\0', strlen(current) + entry->d_namlen + 2);
    strcat(tmpname, current);
    strcat(tmpname, "/");
    strcat(tmpname, entry->d_name);
    if (dir_test(tmpname)) {

/*
 * A directory
 */

      if (strcmp(entry->d_name, ".")) {



      }
    }
    else {

/*
 * A file
 */

      if (strncmp(entry->d_name, ".", 1)
		&& check_name(entry->d_name)) {
			for (i=0; i < num_files; i++) {
				if (0 > strcmp (entry->d_name,
				(char *)xv_get (pi_file, PANEL_LIST_STRING,
						i, NULL, NULL)))
				break;
				}
				xv_set (pi_file,
					PANEL_LIST_INSERT, i,
					PANEL_LIST_STRING, i, entry->d_name,
					NULL);
				num_files++;
			}
    }
    free(tmpname);
  }
	xv_set(pi_file, XV_SHOW, TRUE, NULL);
/* set ti_file if appropriate */
  if(xv_get(pi_file, PANEL_LIST_NROWS) >= 1){
	xv_set(pi_file, PANEL_LIST_SELECT, 0, TRUE, NULL);
	xv_set(ti_file, PANEL_VALUE, 
			xv_get(pi_file, PANEL_LIST_STRING, 0, NULL), NULL);
  }

  else
  	xv_set(ti_file,
    		PANEL_VALUE,	NULL_STR,
    		NULL);
	xv_set(dir_panel,
	WIN_CONSUME_EVENTS,
			WIN_ASCII_EVENTS, WIN_MOUSE_BUTTONS, 
				LOC_WINENTER, LOC_WINEXIT, 
				WIN_STRUCTURE_NOTIFY, NULL,
			NULL);
}


void
check_filter()
{
	int             i;
	i = xv_get(filter, PANEL_VALUE);
	Filter = True;
	xv_set(filter_name, PANEL_INACTIVE, TRUE, NULL);

	if (i == 0)
		Filter = False;
	else if (i == 1)
		strcpy(filter_string, ".rob");
	else if (i == 2)
		strcpy(filter_string, ".dat");
	else if (i == 3){
		strcpy(filter_string, (char *) xv_get(filter_name,
						      PANEL_VALUE));
		xv_set(filter_name, PANEL_INACTIVE, FALSE, NULL);
	}
	else;
}


Bool
check_name(name)
char	*name;
{

	if(Filter){	


	  if (streq(name + strlen(name) - strlen(filter_string),
			filter_string))
			return (True);
	  else
			return (False);
	}
	return(True);
}

/* reset directory list to show current directory */
void
resetd_()
{
  char *tmpname;


/*
 * Start in the current (PWD) directory
 */

  tmpname = (char *) getwd(pathname);
  if(tmpname == NULL)
	tmpname = (char *)getenv("HOME");
  current = (char *)malloc(strlen(tmpname)+1);
  memset(current, '\0', strlen(tmpname)+1);
  strcat(current, tmpname);
  /* free(tmpname); */
  setup_list();
}



/*
 * Re-read the directory entries for the scrolling lists
 */

void setup_list()

{
  char *tmpname;
  int i;



  xv_set(mi_dir,
    PANEL_LABEL_STRING,	current,
    NULL);

/*
 * Hide the scroll lists while re-computing the lists.  Saves on re-draw
 * time. Delete the old entries.
 */


  check_filter();

  xv_set(pi_file,
    XV_SHOW,		FALSE,
    NULL);

  for (i = num_files - 1; i >= 0; i--)
    xv_set(pi_file, PANEL_LIST_DELETE, i, NULL);



  xv_set(pi_dir,
    XV_SHOW,		FALSE,
    NULL);

  for (i = num_dirs - 1; i >= 0; i--)
    xv_set(pi_dir, PANEL_LIST_DELETE, i, NULL);


  num_files = 0;
  num_dirs = 0;

/*
 * Now read the new entries
 */

  dirp = opendir(current);

  while ((entry = readdir(dirp)) != NULL) {
    tmpname = (char *)malloc(strlen(current) + entry->d_namlen + 2);
    memset(tmpname, '\0', strlen(current) + entry->d_namlen + 2);
    strcat(tmpname, current);
    strcat(tmpname, "/");
    strcat(tmpname, entry->d_name);
    if (dir_test(tmpname)) {

/*
 * A directory
 */

	if (strcmp (entry->d_name, ".")) {
		for (i=0; i < num_dirs; i++) {
			if (0 > strcmp (entry->d_name,
			    (char *) xv_get (pi_dir, PANEL_LIST_STRING,
					i, NULL, NULL)))
				break;
				}
				xv_set (pi_dir,
					PANEL_LIST_INSERT, i,
					PANEL_LIST_STRING, i, entry->d_name,
					NULL);
         				/* PANEL_LIST_GLYPH,  file_glyph, */
				num_dirs++;
			}
		}

    else {

/*
 * A file
 */

	if (strncmp (entry->d_name, ".", 1)
		&& check_name(entry->d_name)) {
		for (i=0; i < num_files; i++) {
			if (0 > strcmp (entry->d_name,
				(char *)xv_get (pi_file, PANEL_LIST_STRING,
					i, NULL, NULL)))
				break;
				}
				xv_set (pi_file,
					PANEL_LIST_INSERT, i,
					PANEL_LIST_STRING, i, entry->d_name,
					NULL);
				num_files++;
			}
    }
    free(tmpname);
  }

/*
 * Done, show the new scroll panels
 */

  xv_set(pi_dir,
    XV_SHOW,		TRUE,
    NULL);
  xv_set(pi_file,
    XV_SHOW,		TRUE,
    NULL);

  closedir(dirp);

  if(xv_get(pi_file, PANEL_LIST_NROWS) >= 1){
	xv_set(pi_file, PANEL_LIST_SELECT, 0, TRUE, NULL);
	xv_set(ti_file, PANEL_VALUE, 
			xv_get(pi_file, PANEL_LIST_STRING, 0, NULL), NULL);
  }

  else
  	xv_set(ti_file,
    		PANEL_VALUE,	NULL_STR,
    		NULL);
}


/*
 * Handle the selection of a directory entry in the scroll list
 */

void dir_proc(item, string, client_data, op, event)
Panel_item item;
char *string;
caddr_t client_data;
Panel_list_op op;
Event *event;

{

void Directory();
/*
 * Make sure the user selected and didn't drag.  Dragging may cause unwanted
 * changes
 */
	static char        last_string[80] = "";
        static struct timeval   then = {0, 0};
        static struct timeval   now = {0, 0};

  if (op == SELECT && event_id(event) != LOC_DRAG){

	 now = event_time(event);
	 if(double_click(last_string, &then, string, &now))
		Directory(item, string, client_data, op, event);
	 strcpy(last_string, string);
	 then = now;
	}
}

void
Directory(item, string, client_data, op, event)
Panel_item item;
char *string;
caddr_t client_data;
Panel_list_op op;
Event *event;
{
  char *oldcurrent;
  int i;

/*
 * Make sure we can read the directory before making any changes
 */

      if(check_read(current, string)) {

      oldcurrent = (char *)malloc(strlen(current)+1);
      memset(oldcurrent, '\0', strlen(current)+1);
      strcat(oldcurrent, current);
      free(current);

      if (strcmp(string, "..")) {

/*
 * Add the new directory to the "current" directory string
 */

        current = (char *)malloc(strlen(oldcurrent) + strlen(string) + 2);
        memset(current, '\0', strlen(oldcurrent) + strlen(string) + 2);
        strcat(current, oldcurrent);
        if (strlen(oldcurrent) > 1)
          strcat(current, "/");
        strcat(current, string);
      }
      else {

/*
 * Go back one directory
 */

        i = strlen(oldcurrent);

        while(oldcurrent[i] != '/') {
	  oldcurrent[i] = '\0';
	  i--;
        }

/*
 * Tried to go back from the root directory?
 */

        if (i < 1) {
	  oldcurrent[0] = '/';
	  oldcurrent[1] = '\0';
	  i = 1;
        }
        else
          oldcurrent[i] = '\0';

        current = (char *)malloc(i);
        memset(current, '\0', i);
        strcat(current, oldcurrent);
      }

      free(oldcurrent);

      setup_list();
  }
}


/*
 * Handle a selection on the file list
 */

void file_proc(item, string, client_data, op, event)
Panel_item item;
char *string;
Xv_opaque client_data;
Panel_list_op op;
Event *event;

{
	static char        last_string[80] = "";
        static struct timeval   then = {0, 0};
        static struct timeval   now = {0, 0};



  if (op == SELECT) {
    xv_set(ti_file,
      PANEL_VALUE,	string,
      NULL);
	now = event_time(event);
	if(double_click(last_string, &then, string, &now))
		Open();
	strcpy(last_string, string);
	then = now;



  }
/*  else if (op == DESELECT) {
    xv_set(ti_file,
      PANEL_VALUE,	NULL_STR,
      NULL);
  } */
}


/*
 * Acknowledge an Open button press
 */

int Open()

{
char	name[120], name2[120];
int	ilen;
/* if someone unpinned the frame stick the pin back in! */
  if(xv_get(dir_frame, FRAME_CMD_PUSHPIN_IN) == FALSE)
	xv_set(dir_frame, FRAME_CMD_PUSHPIN_IN, TRUE, NULL);
/* first see if we can read the specified string as a directory
 * rather than it being a file name */
  strcpy(name, (char *)xv_get(ti_file, PANEL_VALUE));
/* Is this non-null? */
   if(streq(name, NULL_STR)){
	toarkt_("No file name given!");
	return XV_OK;
   }
/* cd to home if needed */
  if(name[0] == '~'){
	strcpy(name2, (char *) getenv("HOME"));
	strcat(name2, name+1);
	if(opendir(name2) != NULL){
	strcpy(current, name2);
	setup_list();
	return XV_OK;
	}
}

/* if it begins with a slash assume an absolute pathname */
  else if(name[0] == '/'){
  if(opendir(name) != NULL){
	strcpy(current, name);
	setup_list();
	return XV_OK;
	}
  }
/* otherwise check whether when combined with "current" it makes
 * a directory or not */
  strcpy(name2, current); 
  if(name2[strlen(name2)-1] != '/') strcat(name2,"/");
  strcat(name2, name);
  if(opendir(name2) != NULL){
	strcpy(current, name2);
	setup_list();
	return XV_OK;
  }

/* check whether we can read this */
  if(fopen(name2, "r") == NULL){
	toarkt_("That file/directory couldn't be opened");
	return XV_OK;
  }
  if (strcmp(NULL_STR, name)) {
	xv_set(dir_frame, XV_SHOW, FALSE,
			FRAME_CMD_PUSHPIN_IN,	FALSE, NULL);
	xv_set(dir_frame, XV_SHOW, FALSE,
			FRAME_CMD_PUSHPIN_IN,	FALSE, NULL);
/* without the duplication of the above line a call back
 * from the text item failed to hide the command window!
 * maybe a timing problem???
 */

	/* change the working directory */
	ilen = strlen(current);
	cd_(current, &ilen);
	dir_finish(current, name);
  }
}

int Cancel(item, event)
Panel_item item;
Event *event;

{
	xv_set(dir_frame, XV_SHOW, FALSE,
			FRAME_CMD_PUSHPIN_IN,	FALSE, NULL);
	dir_finish("CANCEL", "CANCEL");
}


/*
 * This routine tests to see if a file name is a directory entry
 */

dir_test(filename)
char *filename;

{
  struct stat statbuf;

  stat(filename, &statbuf);
  return(S_ISDIR(statbuf.st_mode));
}


/*
 * Check to see if the directory is readable
 */

int check_read(oldpath, newdir)
char *oldpath, *newdir;

{
  char *both;
  DIR *test_open;

  both = (char *)malloc(strlen(oldpath) + strlen(newdir) + 2);
  memset(both, '\0', strlen(oldpath) + strlen(newdir) + 2);
  strcat(both, oldpath);
  strcat(both, "/");
  strcat(both, newdir);

  if (!(test_open = opendir(both))) {
    free(both);
    return 0;
  }
  else {
    free(both);
    closedir(test_open);
    return 1;
  }
}

/*---------------- end of dirlist.c ------------------------------------------*/
