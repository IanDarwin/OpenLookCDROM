/* (C) Universitaet Passau 1986-1991 */
/************************************************************************/
/*									*/
/*	fileselect.c		implementation module of fileselector	*/
/*				library routines			*/
/*									*/
/*		10.10.91 ,	by Lamshoeft Thomas			*/
/*									*/
/************************************************************************/

/************************************************************************

		AT FIRST:  ALL PROCEDURES AND FUNCTIONS ON BLOCK
		
	Fs_item	fls_create()					/+ creates a fileselector and sets its default values +/
	void	fls_close(fsi)					/+ removes fileselector from screen and releases display-list +/
	void	fls_destroy(fsi)				/+ destroys a fileselector and releases all its allocated memory +/
	void	fls_set_user_panel_items_create_proc(fsi, user_proc)	/+ user's chance to add an own panel +/
	void	fls_set_extension(fsi, no, value)		/+ sets an extension to 'value' +/
	void	fls_set_current_extension(fsi, no)		/+ makes the extension associated with 'no' to the current mask +/
	void	fls_copy_attributes(dest, source)		/+ copies the user-relevant data of a fileselector +/
	void	fls_set_working_directory(fsi, dir)		/+ sets the default value for selecting a directory +/
	void	fls_set_default_filename(fsi, file)		/+ sets the default value for selecting a filename +/
	void	fls_set_info(fsi, info)				/+ sets the fileselector's label +/
	int	fls_busy(fsi)					/+ returns TRUE if the fileselector is on screen +/
	void	fls_set_auto_destroy(fsi, bool_value)		/+ set the auto_destroy-status of a fileselector +/
	void	fileselect(fsi, parent_frame, done_proc)	/+ pushes a fileselector on screen +/
	void	fls_setup_from_file(fsi, filename, identification)	/+ loads fileselector-contents from file +/
	void	fls_write_to_file(fsi, file, identification)	/+ writes fileselector-contents on stream +/
static	void	nf_set_directory(item, event)			/+ called if CR has been pressed while editing directory +/
static	void	nf_ignore_event(item, event)			/+ empty event proc. Ignores all events +/
static	void	nf_file_input_event_proc(item, event)		/+ special event procedure for the file_input_item +/
static	void	nf_react_to_selection(item, event)		/+ called if user has chosen directory or file with mouse +/
static	void	nf_set_current_mask(item, value, event)		/+ called if user has chosen an extension with mouse +/
static	void	nf_edit_extension(item, event)			/+ called if user has pressed CR while editing an extension +/
static	void	nf_quit_proc(item, event)			/+ called if user has chosen 'done' in frame menu +/
static	void	nf_refresh_proc(item, event)			/+ called if user has pressed refresh-button +/
static	void	nf_addextension_proc(item, event)		/+ called if user has pressed 'fit extension'-button +/
static	void	nf_point_files(item, event)			/+ called if user has pressed ' '.'-files '-button +/
static	void	nf_set_display_status(item, value, even)	/+ called if user has changed display-choice +/
static	Fs_item	fls_get_fsi(item)				/+ returns fileselector-descriptor of 'item' +/
static	int	fls_is_directory(name, eno)			/+ returns TRUE if file 'name' is a directory; accesses drive +/
static	int	fits_reg_expr(str, exp)				/+ evaluation of very small reg. expressions. Only '*' and '?'   +/
								/+ are allowed as wildcards.  +/
static	void	fls_set_filename(item)				/+ special procedure: adapts the value of file_item to +/
								/+ the value of file_input_item +/
static	void	fls_create_window(fsi)				/+ creates whole fileselector window +/
static	void	fls_finish(fsi, dir, file, close)		/+ does the final jobs of the fileselector +/
static	void	quick_dir_sort(**filent, nr_of_filent)		/+ sorts an array of directory entries alphabetically +/
static	void	fls_browse_proc(fsi)				/+ browses directory to show_panel; uses selection service +/
static	void	scan_open_file(name)				/+ opens file 'name' for reading fileselector-contents +/
static	void	scan_close_file()				/+ closes an open setup-file +/
static	int	scan_get_next_line()				/+ reads next line from scanfile; returns TRUE upon success +/
static	int	scan_find_string(str)				/+ tries to find 'str' in scanfile beginning at scan_position +/
static	int	is_number(s)					/+ returns TRUE if 's' represents an integer value +/

*********************************************************************************************/

/************************************************/
/*		INCLUDES			*/
/************************************************/

#include <xview/xview.h>
#include <xview/canvas.h>
#include <xview/panel.h>
#include <xview/scrollbar.h>
#include <xview/expandname.h>
#include <sys/types.h>
#include <dirent.h>
#include <sys/stat.h>
#include <string.h>
#include <stdio.h>
#include <ctype.h>
#include <malloc.h>
#include <sys/param.h>
#include <sys/time.h>
#include "auto_extension.h"


/************************************************/
/*		CONSTANTS			*/
/************************************************/

#define	MASKNO	14	/* max. number of mask entries */
#define MASKLEN	14	/* displayed length of mask strings */
#define	WDLEN	1024	/* length of working directory string */
#define	FNLEN	128	/* length of working filename string */
#define	SILEN	64	/* length of fileselector info string */
#define	UCLEN	80	/* length of user extension string */

#define	CR	015	/* carriage return */


/************************************************/
/*	type declarations			*/
/************************************************/

struct filent {
	char		filename[MAXNAMLEN+1];
	int		is_file;		/* is_file == FALSE, if directory; TRUE otherwise		*/
};

typedef struct fls_record {

	char		cwd[WDLEN],		/* current working directory 					*/
			cwd_old[WDLEN],		/* backup of cwd; used to hold enter state of fileselector	*/
			filename[FNLEN],	/* filename							*/
			fileinput[FNLEN],	/* file-input-string						*/
			select_info[SILEN],	/* select_frame's label						*/
			*extension[MASKNO];	/* extension-buttons						*/

	Frame		parent_frame,
			select_frame;		/* 'baseframe' of fileselector					*/

	Panel		control_panel,		/* directory-, filename-text-items and 'ok'- and 'abort'-buttons	*/
			ext_panel,		/* user-extension-text-item, extension-buttons and a display-mode-cycle	*/
			show_panel;		/* displays directory entries.					*/

	Panel_item	dir_item,		/* text-item for current directory */
			file_item,		/* text-item for current filename */
			file_input_item,	/* text-item */
			message_item,
			edit_item,		/* text-item */
			ext_choice,		/* extension buttons */
			sort_choice,		/* display mode cycle */
			last_selected_item,	/* item to detect a double-click */
			panel_list_item,
			refresh_button, ok_button, abort_button;

	struct timeval	last_selection_time;	/* used to proof validity of a double-click */

	void		(*quit_proc)();		/* called when quitting a fileselector */
	void		(*user_panel_proc)();	/* if not NULL, then try to create a user_panel.
							   if successful then call the procedure */
	struct filent	**file_list;

	int		last_extension,		/* currently selected extension */
			fit_extension,		/* TRUE if 'fit extension'-mode is ON */
			show_point_files,	/* TRUE if '.'-files are displayed */
			dir_cycle,		/* current display-mode */
			ok_implies_close,	/* set FALSE if fileselector should not disappear when
							   pushing 'O.k.'-button */
			window_open,		/* TRUE while fileselector is on screen */
			auto_destroy,		/* if TRUE, then 'this_item' is released when leaving
							   the fileselector. default: FALSE */
			nr_of_filent;

	struct fls_record	*this_item;		/* pointer to this record */
} *Fs_item;


/************************************************/
/*	local variables				*/
/************************************************/

static char	fls_empty_string[] = { '\0' };
static FILE	*scanfile = (FILE *)NULL;
static char	buffer[1024], *scan_position;
static int	linecount;


/************************************************/
/*	local procedures : forward declaration	*/
/************************************************/

static void 	nf_set_directory();	/* called if CR was pressed while editing directory	*/
static void 	nf_react_to_selection();/* called if user chose directory or file with mouse	*/
static void 	nf_set_current_mask();	/* called if user chose an extension with mouse		*/
static void 	nf_quit_proc();		/* called if user chose 'done' in frame menu		*/
static void 	nf_set_display_status();/* called if user changed display-choice		*/

static Fs_item	fls_get_fsi();		/* returns fileselector-descriptor of 'item'		*/

static int	fls_is_directory();	/* returns TRUE if file 'name' is a directory; accesses drive	*/
static int	fits_reg_expr();	/* returns TRUE if 'str' fits the "regular expression" 'exp' 	*/
static void 	fls_set_filename();	/* called while editing file-input-entry		*/

static void	fls_create_window();	/* creates whole fileselector window			*/
static void	fls_finish();		/* always called when quitting a fileselector		*/
static void	fls_browse_proc();	/* browses directory to show_panel; uses selection service */

static	void	scan_open_file();	/* opens file 'name' for reading fileselector-contents	*/
static	void	scan_close_file();	/* closes an open setup-file				*/
static	int	scan_get_next_line();	/* reads next line from scanfile; returns TRUE upon success	*/
static	int	scan_find_string();	/* tries to find 'str' in scanfile beginning at scan_position	*/
static	int	is_number();		/* returns TRUE if 's' represents an integer value	*/


/********************************************************************************/
/*										*/
/*	exported procedures :							*/
/*										*/
/********************************************************************************/

void	fls_set_extension(); 		/* forward declaration */

Fs_item	fls_create()			/* creates a fileselector and sets its default values */
{					/* returns NULL (nil) if creation failed	      */
	Fs_item	local_fsi;
	int	i;
	
	local_fsi = (Fs_item)malloc(sizeof(struct fls_record));
	if(local_fsi != (Fs_item)NULL) {
		local_fsi->user_panel_proc = NULL;
		local_fsi->auto_destroy = FALSE;

		strcpy(local_fsi->cwd, "");
		strcpy(local_fsi->cwd_old, "");
		strcpy(local_fsi->filename, "");
		strcpy(local_fsi->fileinput, "");
		strcpy(local_fsi->select_info, "Fileselector (v 2.2a)");
		for(i=0; i<MASKNO; i++)
			local_fsi->extension[i] = fls_empty_string;

		local_fsi->select_frame = (Frame)NULL;
		local_fsi->parent_frame = (Frame)NULL;

		local_fsi->control_panel = (Panel)NULL;
		local_fsi->show_panel = (Panel)NULL;
		local_fsi->ext_panel = (Panel)NULL;

/*fisss Panel_items, struct timeval, lokale Prozeduren fehlen*/

		local_fsi->last_selected_item = (Panel_item)NULL;

		local_fsi->file_list = (struct filent**)NULL;
		local_fsi->nr_of_filent = 0;

		local_fsi->last_extension = 0;
		local_fsi->fit_extension = FALSE;
		local_fsi->show_point_files = FALSE;
		local_fsi->dir_cycle = 1;
		local_fsi->ok_implies_close = TRUE;
		local_fsi->window_open = FALSE;
		local_fsi->this_item = local_fsi;

		fls_set_extension(local_fsi, 0, "*");
	}
	return local_fsi;
}


void	fls_close(fsi)			/* removes fileselector from screen and releases display-list-items */
Fs_item	fsi;
{
	int	i;

	if(fsi != (Fs_item)NULL) {
		for(i=0; i<fsi->nr_of_filent; i++)
			free(fsi->file_list[i]);
		free(fsi->file_list);
		fsi->nr_of_filent = 0;

		if (fsi->select_frame != (Frame)NULL){
			xv_destroy_safe(fsi->select_frame);
		}
		fsi->select_frame = (Frame)NULL;
		fsi->window_open = FALSE;
	}
}


void	fls_destroy(fsi)		/* destroys a fileselector and releases all its allocated memory */
Fs_item	*fsi;
{
	Fs_item	hilf;
	int	i;
		
	if(*fsi != (Fs_item)NULL){
		hilf = *fsi;
		fls_close(hilf);
		for(i=0; i<MASKNO; i++){
			if(hilf->extension[i] != fls_empty_string){
				free(hilf->extension[i]);
			}
		}
		*fsi = (Fs_item)NULL;
		free(hilf);
	}
}


void	fls_set_user_panel_items_create_proc(fsi, user_proc)	/* user's chance to add an own panel to */
Fs_item	fsi;							/* the fileselector */
void	(*user_proc)();
{
	if(fsi != (Fs_item)NULL){
		fsi->user_panel_proc = user_proc;
	}
}				


void	fls_set_extension(fsi, no, value)	/* sets the extension associated with 'no' to 'value' */
Fs_item	fsi;
int	no;
char	*value;
{
	if(fsi != (Fs_item)NULL){
		if ((no >= 0) && (no < MASKNO)){
			if (fsi->extension[no] != fls_empty_string){
				free(fsi->extension[no]);
			}
			if(!strcmp(value, fls_empty_string)) {
				fsi->extension[no] = fls_empty_string;
			} else {
				fsi->extension[no] = (char *)malloc(strlen(value) + 1);
				if(fsi->extension[no] != (char *)NULL) {
					strcpy(fsi->extension[no], value);
				} else {
					fsi->extension[no] = fls_empty_string;
				}
			}
		}
	}
}


void	fls_set_current_extension(fsi, no)	/* the value of the extension 'no' becomes the current mask */
Fs_item	fsi;
int	no;
{
	if(fsi != (Fs_item)NULL){
		if ((no >= 0) && (no < MASKNO)){
			fsi->last_extension = no;
		}
	}
}


void	fls_copy_attributes(dest, source)	/* copies the user-relevant data of a fileselector */
Fs_item	dest, source;				/* to an other */
{
	int	i;
	
	if(source != (Fs_item)NULL && dest != (Fs_item)NULL){
		strcpy(dest->cwd, source->cwd_old);
		strcpy(dest->cwd_old, source->cwd_old);
		strcpy(dest->filename,source->filename);
		strcpy(dest->select_info, source->select_info);
		for(i=0; i<MASKNO; i++) {
			fls_set_extension(dest, i, source->extension[i]);
		}
		dest->last_extension = source->last_extension;
		dest->dir_cycle = source->dir_cycle;
	}
}


void	fls_set_working_directory(fsi, dir)	/* sets the default value for the directory item */
Fs_item	fsi;
char	*dir;
{
	if(fsi != (Fs_item)NULL){
		if (strlen(dir) < WDLEN) {
			strcpy(fsi->cwd, dir);
		} else {
			printf("fls_set_working_directory : string too long !\n Please recompile fileselect.c with increased WDLEN.\n");
		}
	}
}


void	fls_set_default_filename(fsi, file)	/* sets the default value for the file_item */
Fs_item	fsi;
char	*file;
{
	if(fsi != (Fs_item)NULL){
		if (strlen(file) < FNLEN) {
			strcpy(fsi->filename, file);
		} else {
			printf("fls_set_default_filename : string too long !\n Please recompile fileselect.c with increased FNLEN.\n");
		}
	}
}


void	fls_set_info(fsi, info)		/* sets the fileselector's label */
Fs_item	fsi;
char	*info;
{
	if(fsi != (Fs_item)NULL){
		if (strlen(info) < SILEN) {
			strcpy(fsi->select_info, info);
		} else {
			printf("fls_set_info : string too long !\n Please recompile fileselect.c with increased SILEN.\n");
		}
	}
}


int	fls_busy(fsi)			/* returns TRUE if the fileselector is on screen */
Fs_item	fsi;
{
	if(fsi != (Fs_item)NULL){
		return fsi->window_open;
	} else {
		return FALSE;
	}
}


void	fls_set_auto_destroy(fsi, bool_value)	/* set the auto_destroy-status of a fileselector */
Fs_item	fsi;
int	bool_value;
{
	if(fsi != (Fs_item)NULL){
		fsi->auto_destroy = bool_value;
	}
}


void	fileselect(fsi, parent_frame, done_proc)	/* pushes a fileselector on screen and calls */
Fs_item	fsi;						/* done_proc when the user has made up his/her mind */
Frame	parent_frame;
void	(*done_proc)();
{
	Fs_item	local_fsi;
	
	if (fls_busy(fsi)){
		local_fsi = fls_create();
		if(local_fsi == (Fs_item)NULL){
			done_proc("", "NOTHING SELECTED");
		} else {
			fls_copy_attributes(local_fsi, fsi);
			fls_set_auto_destroy(local_fsi, TRUE);
			strcat(local_fsi->select_info, " - ORIGINAL STILL BUSY!");
			local_fsi->quit_proc = done_proc;
			local_fsi->parent_frame = parent_frame;
			fls_create_window(local_fsi);
		}
	} else {
		if(fsi != (Fs_item)NULL) {
			fsi->quit_proc = done_proc;
			fsi->parent_frame = parent_frame;
			fls_create_window(fsi);
		} else {
			printf("failed to create fileselector because Fs_item is NULL\n");
			done_proc("", "NOTHING SELECTED");
		}
	}
}


/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
/* two more exported procedures (fls_setup_from_file & fls_write_to_file)	*/
/* at 'file read/write procedures' below					*/
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/


/********************************************************************************/
/*										*/
/*	notify procedures : 	(all beginning with  nf_...)			*/
/*										*/
/********************************************************************************/

static void nf_set_directory(item, event)	/* called if CR has been pressed while editing directory */
Panel_item	item;
Event		*event;
{
	Fs_item	fsi;

	fsi = fls_get_fsi(item);
	fsi->last_selected_item = (Panel_item)NULL;
	strcpy(fsi->cwd, (char *)xv_get(item, PANEL_VALUE));
	if(!strcmp(fsi->cwd, "")){
		strcpy(fsi->cwd, fsi->cwd_old);
	}
	fls_browse_proc(fsi);
}


static void nf_file_input_event_proc(item, event)	/* notify-proc. for file_input_item	*/
Panel_item	item;
Event		*event;
{
	panel_default_handle_event(item, event);	/* at first the normal event handling on file_input_item */
	if(event_is_ascii(event)) {			/* and then adapting the value of file_item to the */
		fls_set_filename(item);			/* value of file_input_item */
	}
}


static void nf_react_to_selection(item, string, client_data, op, event)
Panel_item	item;
char		*string;
caddr_t		client_data;
Panel_list_op	op;
Event		*event;
{
	int		i;
	Fs_item		fsi;
	struct timeval	this_event_time;

	if(op == 1) {
		fsi = fls_get_fsi(item);

		this_event_time = event_time(event);

/*fisss: es muss doch einen besseren weg geben den index herauszufinden!?*/

		i=0;
		while(!xv_get(item, PANEL_LIST_SELECTED, i)) i++;

		if(fsi->file_list[i]->is_file) {
			if((item == fsi->last_selected_item) &&
			   (this_event_time.tv_sec - fsi->last_selection_time.tv_sec <= 1) &&
			   (((this_event_time.tv_sec - fsi->last_selection_time.tv_sec) * 1000000 +
			   this_event_time.tv_usec - fsi->last_selection_time.tv_usec) <= 400000)) {
				/* puuuh, in this case we have a double-click to a file (no directory!)	*/
				/* a double-click means two clicks on one item within 400 msec 		*/
				nf_quit_proc(item, event);	/* the user's mind is made up; now let's go home */
			} else {	/* first click on this item */
				strcpy(fsi->filename, fsi->file_list[i]->filename);
				xv_set(fsi->file_item, PANEL_VALUE, fsi->filename, NULL);
				fsi->last_selected_item = item;
				fsi->last_selection_time = this_event_time;
			}
		} else {	/* user selected a directory, so we have to update the display-panel */ 
			fsi->last_selected_item = (Panel_item)NULL;
			strcat(fsi->cwd, "/");
			strcat(fsi->cwd, fsi->file_list[i]->filename);
			xv_set(fsi->file_item, PANEL_VALUE, fsi->filename, NULL);
			fls_browse_proc(fsi);
		}
	}
}


static void nf_set_current_mask(item, value, event)	/* called if user has chosen an extension with mouse */
Panel_item	item;
unsigned int	value;
Event		*event;
{
	Fs_item	fsi;
	struct timeval	this_event_time;
	static int	last_choice = -1;

	fsi = fls_get_fsi(item);
	this_event_time = event_time(event);

	if((item == fsi->last_selected_item) &&	(value == last_choice) &&
	   (this_event_time.tv_sec - fsi->last_selection_time.tv_sec <= 1) &&
	   (((this_event_time.tv_sec - fsi->last_selection_time.tv_sec) * 1000000 +
	   this_event_time.tv_usec - fsi->last_selection_time.tv_usec) <= 400000)) {
		/* puuuh, in this case we have a double-click to an extension	*/
		/* a double-click means two clicks on one item within 400 msec	*/
		/* now show a text item and let the user edit the extension	*/
		xv_set(fsi->message_item, XV_SHOW, FALSE, NULL);
		xv_set(fsi->edit_item,
			PANEL_VALUE, 		fsi->extension[value],
			PANEL_CLIENT_DATA,	value,
			XV_SHOW,		TRUE,
			NULL);
		fsi->last_selected_item = (Panel_item)NULL;
	} else {	/* first click on this extension */
		fsi->last_extension = value;
		fls_browse_proc(fsi);
		fls_set_filename(fsi->file_input_item);
		last_choice = value;
		fsi->last_selected_item = (Panel_item)item;
		fsi->last_selection_time = this_event_time;
	}
}


static void nf_edit_extension(item, event)	/* called if the user edited an extension and pressed RETURN */
Panel_item	item;
Event		*event;
{
	Fs_item fsi;
	int	hilflen, ext_choice_value;
	char	*new_string, hilfstring[UCLEN];
	
	fsi = fls_get_fsi(item);
	new_string = (char *)xv_get(item, PANEL_VALUE);
	ext_choice_value = (int)xv_get(item, PANEL_CLIENT_DATA);	/* number of extension to be changed */
	xv_set(fsi->edit_item, XV_SHOW, FALSE, NULL);
	xv_set(fsi->message_item, XV_SHOW, TRUE, NULL);
   	fls_set_extension(fsi, ext_choice_value, new_string);		/* set the new extensnion */
   	hilflen = strlen(new_string);					/* and create a new PANEL_CHOICE_STRING */
   	if(hilflen >= MASKLEN){
   		strcpy(hilfstring, &(new_string[hilflen - MASKLEN]));
   	} else {
   		strcpy(hilfstring, new_string);
   		while(hilflen < MASKLEN) {
   			hilfstring[ hilflen++ ] = ' ';
   		}
   		hilfstring[MASKLEN] = '\0';
   	}
	xv_set(fsi->ext_choice,
		PANEL_VALUE,		ext_choice_value,
		PANEL_CHOICE_STRING, 	ext_choice_value, hilfstring ,
		NULL);
	nf_set_current_mask(item, ext_choice_value, event);		/* new extension becomes current mask */
}


static void nf_frame_done_proc(frame)
Frame		frame;
{
	Fs_item	fsi;

	fsi = (Fs_item)xv_get(frame, WIN_CLIENT_DATA);
	fsi->last_selected_item = (Panel_item)NULL;

	fls_finish(fsi, "", "NOTHING SELECTED", TRUE);
}


static void nf_quit_proc(item, event)
Panel_item	item;
Event		*event;
{
	Fs_item	fsi;

	fsi = fls_get_fsi(item);
	fsi->last_selected_item = (Panel_item)NULL;

	if(item == fsi->select_frame || item == fsi->abort_button) {
		fls_finish(fsi, "", "NOTHING SELECTED", TRUE);
	}
	else if(item == fsi->file_input_item || item == fsi->ok_button || fsi->panel_list_item) {
		strcpy(fsi->cwd, (char *)xv_get(fsi->dir_item, PANEL_VALUE));
		strcpy(fsi->filename, (char *)xv_get(fsi->file_item, PANEL_VALUE));
		fls_finish(fsi, fsi->cwd, fsi->filename, fsi->ok_implies_close);
	}
}


static void nf_refresh_proc(item, event)	/* called if user has pressed refresh-button */
Panel_item	item;				/* forces the fileselector to rescan the directory */
Event		*event;
{
	Fs_item	fsi;

	fsi = fls_get_fsi(item);
	fsi->last_selected_item = (Panel_item)NULL;
	fls_browse_proc(fsi);
}


static void nf_addextension_proc(item, event)	/* called if user has pressed 'fit extension'-button */
Panel_item	item;				/* switches the value of fit_extension (boolean) */
Event		*event;
{
	Fs_item	fsi;

	fsi = fls_get_fsi(item);
	fsi->last_selected_item = (Panel_item)NULL;
	fsi->fit_extension = !(fsi->fit_extension);
	if(fsi->fit_extension){
		xv_set(item, PANEL_LABEL_STRING, "fit extension : on ", NULL);
	} else {
		xv_set(item, PANEL_LABEL_STRING, "fit extension : off", NULL);
	}
	fls_set_filename(fsi->file_input_item);
}


static void nf_point_files(item, event)		/* called if user has pressed '.'-files -button */
Panel_item	item;				/* switches the value of show_point_files (boolean) */
Event		*event;
{
	Fs_item	fsi;
	
	fsi = fls_get_fsi(item);
	fsi->last_selected_item = (Panel_item)NULL;
	fsi->show_point_files = !(fsi->show_point_files);
	if(fsi->show_point_files){
		xv_set(item, PANEL_LABEL_STRING, "'.'-files : displayed", NULL);
	} else {
		xv_set(item, PANEL_LABEL_STRING, "'.'-files :  hidden  ", NULL);
	}
	fls_browse_proc(fsi);
}


static void nf_set_display_status(item, value, event)	/* called if user has changed display-choice */
Panel_item	item;
unsigned int	value;
Event		*event;
{
	Fs_item	fsi;

	fsi = fls_get_fsi(item);
	fsi->last_selected_item = (Panel_item)NULL;
	if(value >= 0 && value <= 2) {
		fsi->dir_cycle = value;
		fls_browse_proc(fsi);
	}
}


/********************************************************************************/
/*										*/
/*	diverse internal procedures and functions				*/
/*										*/
/********************************************************************************/

static Fs_item fls_get_fsi(item)	/* returns fileselector-descriptor of 'item' */
Panel_item	item;			/* via WIN_CLIENT_DATA */
{
	Panel	parent_panel;
	Fs_item	result;
	
	parent_panel = (Panel)xv_get(item, XV_OWNER);
	result = (Fs_item)xv_get(parent_panel, WIN_CLIENT_DATA);
	return result;
}


static int fls_is_directory(name, eno)		/* returns TRUE if file 'name' is a directory; accesses drive */
char	*name;					/* if something got wrong: returns FALSE and sets eno to 1 */
int	*eno;
{
	struct stat	buf_rec, *buf = &buf_rec;
	int		stat_value;
	
	if((stat_value = stat(name, buf)) == 0){
		if((buf->st_mode & S_IFMT) == S_IFDIR){
			*eno = 0;
			return TRUE;
		} else {
			*eno = 0;
			return FALSE;
		}
	} else {
		*eno = 1;
		return FALSE;
	}
}


static int fits_reg_expr(str, exp)	/* evaluation of very small regular expressions with only */
char	*str, *exp;			/* '*' and '?' as wildcards */
{
	int	el = 0, sl = 0, last_star = -1, pp = -1;
	
	do {
		while(exp[el] == '*') {
			el++;
			pp = sl;
			last_star = el;
		}
		if(exp[el] == '\0') {
			if((last_star == el) || (str[sl] == '\0')) {
				return TRUE;
			} else {
				return FALSE;
			}
		}
		if((str[sl] == exp[el]) || (exp[el] == '?')) {
			el++;
			sl++;
		} else {
			if(last_star < 0) {
				return FALSE;
			} else {
				el = last_star;
				sl = ++pp;
			}
		}
		if(str[sl] == '\0') {
			if((exp[el] == '\0') || ((exp[el]=='*') && (exp[el+1]=='\0'))){
				return TRUE;
			} else {
				return FALSE;
			}
		}
	} while(TRUE);
}


static void fls_create_window(fsi)	/* creates whole fileselector window */
Fs_item	fsi;
{
	int		i, col, row_count=0;
	Panel		user_panel;
	static char	directory_notify_string[] = { CR, '\0' };
	char		hilfstring[2*MASKLEN];
	int		hilflen;
	

	if(fsi->window_open) return;

	fsi->window_open = TRUE;
	
	if(fsi->select_frame != (Frame)NULL) {			/* if already created	*/
		xv_set(fsi->select_frame, XV_SHOW, TRUE, NULL);	/* just show on top	*/
		return;
	}

	fsi->select_frame = (Frame)xv_create(fsi->parent_frame, FRAME,
		FRAME_SHOW_RESIZE_CORNER,	FALSE,
		XV_LABEL,			fsi->select_info,
		FRAME_SHOW_LABEL,		TRUE,
		FRAME_DONE_PROC,		nf_frame_done_proc,
		FRAME_NO_CONFIRM,		TRUE,
		WIN_CLIENT_DATA,		fsi,
		NULL);

	fsi->control_panel = (Panel)xv_create(fsi->select_frame, PANEL,
		WIN_CLIENT_DATA,		fsi,
		NULL);
	
	fsi->dir_item = xv_create(fsi->control_panel, PANEL_TEXT,
		PANEL_LABEL_X,			xv_col(	fsi->control_panel, 0),
		PANEL_LABEL_Y,			xv_row(	fsi->control_panel, row_count),
		PANEL_VALUE,			fsi->cwd,
		PANEL_NOTIFY_STRING,		directory_notify_string,
		PANEL_NOTIFY_LEVEL,		PANEL_SPECIFIED,
		PANEL_NOTIFY_PROC,		nf_set_directory,
		PANEL_LABEL_STRING,		"Dir : ",
		PANEL_VALUE_DISPLAY_LENGTH,	40,
		NULL);

	fsi->ok_button = xv_create(fsi->control_panel, PANEL_BUTTON,
		PANEL_LABEL_STRING,		"O.k.",
		PANEL_NOTIFY_PROC,		nf_quit_proc,
		NULL);

	row_count++;
	fsi->file_item = xv_create(fsi->control_panel, PANEL_TEXT,
		PANEL_LABEL_X,			xv_col(fsi->control_panel, 0),
		PANEL_LABEL_Y,			xv_row(fsi->control_panel, row_count),
		PANEL_VALUE,			fsi->filename,
		PANEL_LABEL_STRING,		"File: ",
		PANEL_VALUE_DISPLAY_LENGTH,	40,
		NULL);
		
	fsi->abort_button = xv_create(fsi->control_panel, PANEL_BUTTON,
		PANEL_LABEL_STRING,		"Abort",
		PANEL_NOTIFY_PROC,		nf_quit_proc,
		NULL);

	row_count++;
	fsi->file_input_item = xv_create(fsi->control_panel, PANEL_TEXT,
		PANEL_LABEL_X,			xv_col(fsi->control_panel, 0),
		PANEL_LABEL_Y,			xv_row(fsi->control_panel, row_count),
		PANEL_VALUE,			fsi->fileinput,
		PANEL_CARET_ITEM,		fsi->file_input_item,
		PANEL_EVENT_PROC,		nf_file_input_event_proc,
		PANEL_NOTIFY_STRING,		directory_notify_string,
		PANEL_NOTIFY_LEVEL,		PANEL_SPECIFIED,
		PANEL_NOTIFY_PROC,		nf_quit_proc,
		PANEL_LABEL_STRING,		"FILE INPUT: ",
		PANEL_VALUE_DISPLAY_LENGTH,	34,
		NULL);

	row_count++;	
	fsi->refresh_button = xv_create(fsi->control_panel, PANEL_BUTTON,
		PANEL_LABEL_STRING,	"refresh",
		PANEL_LABEL_X,		xv_col(fsi->control_panel, 0),
		PANEL_LABEL_Y,		xv_row(fsi->control_panel, row_count),
		PANEL_NOTIFY_PROC,	nf_refresh_proc,
		NULL);
	
	(void)xv_create(fsi->control_panel, PANEL_BUTTON,
		PANEL_LABEL_STRING,	fsi->show_point_files ?
					"'.'-files: displayed" : "'.'-files:  hidden  ",
		PANEL_NOTIFY_PROC,	nf_point_files,
		NULL);

	(void)xv_create(fsi->control_panel, PANEL_BUTTON,
		PANEL_LABEL_STRING,	fsi->fit_extension ?
					"fit extension : on " : "fit extension : off",
		PANEL_NOTIFY_PROC,	nf_addextension_proc,
		NULL);

	xv_set(fsi->control_panel, PANEL_CARET_ITEM, fsi->file_input_item, NULL);

	window_fit(fsi->control_panel);


	fsi->show_panel = (Panel)xv_create(fsi->select_frame, PANEL,
		WIN_CLIENT_DATA,	fsi,
		WIN_BELOW,		fsi->control_panel,
		WIN_X,			0,
		NULL);

	fsi->panel_list_item = xv_create(fsi->show_panel, PANEL_LIST,
		PANEL_DISPLAY_ROWS,	15,
		PANEL_LIST_WIDTH,	200,
		PANEL_CHOOSE_ONE,	TRUE,		
		PANEL_NOTIFY_PROC,	nf_react_to_selection,
		NULL);

	window_fit(fsi->show_panel);


	fsi->ext_panel = (Panel)xv_create(fsi->select_frame, PANEL,
		WIN_CLIENT_DATA,	fsi,
		WIN_COLUMNS,		2*MASKLEN+3,
		XV_HEIGHT,		xv_get(fsi->show_panel, XV_HEIGHT),
		NULL);
	
	fsi->message_item = xv_create(fsi->ext_panel, PANEL_MESSAGE,
		PANEL_LABEL_STRING,	"(double click to edit)",
		PANEL_LABEL_X,		xv_col(fsi->ext_panel, 1),
		PANEL_LABEL_Y,		xv_row(fsi->ext_panel, 0),
		NULL);
	
	fsi->edit_item = xv_create(fsi->ext_panel, PANEL_TEXT,
		PANEL_LABEL_X,		xv_col(fsi->ext_panel, 1),
		PANEL_LABEL_Y,		xv_row(fsi->ext_panel, 0),
		PANEL_LABEL_STRING,	"new:",
		PANEL_VALUE,		"",					
		PANEL_NOTIFY_STRING,	directory_notify_string,
		PANEL_NOTIFY_LEVEL,	PANEL_SPECIFIED,
		PANEL_NOTIFY_PROC,	nf_edit_extension,
		XV_SHOW,		FALSE,
		NULL);
	
	fsi->ext_choice = xv_create(fsi->ext_panel, PANEL_CHOICE,
		PANEL_NOTIFY_PROC,	nf_set_current_mask,
		NULL);
	
	row_count = 1; col = 1;
	for(i=0;i<MASKNO;i++) {
		hilflen = strlen(fsi->extension[i]);
	   	if(hilflen >= MASKLEN){
	   		strcpy(hilfstring, &fsi->extension[i][hilflen - MASKLEN]);
	   	} else {
	   		strcpy(hilfstring, fsi->extension[i]);
	   		while(hilflen < MASKLEN) {
	   			hilfstring[hilflen++] = ' ';
	   		}
	   		hilfstring[MASKLEN] = '\0';
	   	}
		xv_set(fsi->ext_choice,
			PANEL_CHOICE_STRING,	i, hilfstring,
			PANEL_CHOICE_X,		i, xv_col(fsi->ext_panel, col),
			PANEL_CHOICE_Y,		i, xv_row(fsi->ext_panel, row_count),
			NULL);
		col = (MASKLEN + 2) - col;
		if (col == 1) row_count++;
	}

	(void)xv_create(fsi->ext_panel, PANEL_CYCLE,
		PANEL_LAYOUT,		PANEL_HORIZONTAL,
		PANEL_LABEL_STRING,	"displaying :",
		PANEL_LABEL_X,		xv_col(fsi->ext_panel, 1),
		PANEL_LABEL_Y,		xv_row(fsi->ext_panel, row_count),
		PANEL_CHOICE_STRINGS,	"files", "all",	"directories", NULL,
		PANEL_VALUE,		fsi->dir_cycle,
		PANEL_NOTIFY_PROC,	nf_set_display_status,
		NULL);
	
	xv_set(fsi->ext_choice, PANEL_VALUE, fsi->last_extension, NULL);	/* now setting current extension */

	xv_set(fsi->control_panel,
		XV_WIDTH,	xv_get(fsi->show_panel, XV_WIDTH) +
				xv_get(fsi->ext_panel, XV_WIDTH),
		NULL);


	if(fsi->user_panel_proc != NULL){
		user_panel = (Panel)xv_create(fsi->select_frame, PANEL,
			WIN_BELOW,		fsi->show_panel,
			WIN_X,			0,
			XV_WIDTH,		xv_get(fsi->control_panel, XV_WIDTH),
			NULL);
		if(user_panel == (Panel)NULL){
			fls_finish(fsi, "", "NOTHING SELECTED", TRUE);
			return;
		} else {
			fsi->user_panel_proc(user_panel);
		}
	}


	window_fit(fsi->select_frame);

	strcpy(fsi->cwd_old, fsi->cwd);

	if(fsi->parent_frame != (Frame)NULL){
		xv_set(fsi->select_frame,
			WIN_X,	((1152 - xv_get(fsi->select_frame, XV_WIDTH))/2 -
				xv_get(fsi->parent_frame, WIN_X)),
			WIN_Y,	((900 - xv_get(fsi->select_frame, XV_HEIGHT))/2 -
				xv_get(fsi->parent_frame, WIN_Y)),
			NULL);
	}

	xv_set(fsi->select_frame, XV_SHOW, TRUE, NULL);

	fls_browse_proc(fsi);
}


static void fls_finish(fsi, dir, file, close)	/* does the final jobs of the fileselector */
Fs_item	fsi;
char	*dir, *file;
int	close;
{
	if(fsi != (Fs_item)NULL){
		if(close) {
			fls_close(fsi);
		}
		fsi->quit_proc(dir, file);
		if(close && fsi->auto_destroy) {
			fls_destroy(&fsi);
		}
	}
}    


static int namecmp(filent1, filent2)
struct filent	*filent1, *filent2;
{
	if (filent1->is_file > filent2->is_file)
		return 1;
	else if (filent1->is_file < filent2->is_file)
		return -1;
	else
		return strcmp(filent1->filename, filent2->filename);
}


static void quick_dir_sort(file_list, nr_of_filent)
struct filent	**file_list;
int		nr_of_filent;
{
	struct filent	*hilf, *vgl;
	long		lu, lo;

	if(nr_of_filent < 2)
		return;
	else if(nr_of_filent < 3) {
		if(namecmp(file_list[0], file_list[1]) > 0) { 
			hilf = file_list[0]; file_list[0] = file_list[1]; file_list[1] = hilf;
		}
	} else {
		vgl = file_list[0]; lu = 1; lo = nr_of_filent - 1;
		while(lu < lo) {
			while(namecmp(file_list[lo], vgl) > 0)
				lo--;
			while((namecmp(file_list[lu], vgl) <= 0) && (lu < lo))
				lu++;
			if(lu < lo) {
				hilf = file_list[lu]; file_list[lu] = file_list[lo]; file_list[lo] = hilf;
			}
		}
		hilf = file_list[0]; file_list[0] = file_list[lo]; file_list[lo] = hilf;
		quick_dir_sort(file_list, lo);
		quick_dir_sort(&file_list[lo+1], nr_of_filent-lo-1);
	}
}


static void fls_browse_proc(fsi)	/* browses directory to show_panel; uses selection mask */
Fs_item	fsi;
{
	int		i, nr_of_filent, eno;
	char		name[MAXNAMLEN+6], help[WDLEN], **name_list;
	struct dirent	**nl;
	struct filent	**file_list;

	getwd(help);
	chdir(fsi->cwd);
	getwd(fsi->cwd);
	xv_set(fsi->dir_item, PANEL_VALUE, fsi->cwd, NULL);

	if(fsi->nr_of_filent) {
		for(i=0; i<fsi->nr_of_filent; i++)
			free(fsi->file_list[i]);
		free(fsi->file_list);
	}

	if((nr_of_filent = scandir(fsi->cwd, &nl, NULL, NULL)) != -1) {
		file_list = (struct filent**)malloc(nr_of_filent*sizeof(int));

		file_list[0] = (struct filent*)malloc(sizeof(struct filent));
		strcpy(file_list[0]->filename, "..");
		file_list[0]->is_file = FALSE;
		fsi->nr_of_filent = 1;

		for(i=1; i<nr_of_filent; i++) {
			file_list[i] = (struct filent*)NULL;
			if(fsi->show_point_files || nl[i]->d_name[0] != '.') {
				if(fls_is_directory(nl[i]->d_name, &eno)) {
					if(fsi->dir_cycle > 0) {
						file_list[i] = (struct filent*)malloc(sizeof(struct filent));
						strcpy(file_list[i]->filename, nl[i]->d_name);
						file_list[i]->is_file = FALSE;
						fsi->nr_of_filent++;
					}
				} else {
					if(fsi->dir_cycle < 2 && fits_reg_expr(nl[i]->d_name, fsi->extension[fsi->last_extension])) {
						file_list[i] = (struct filent*)malloc(sizeof(struct filent));
						strcpy(file_list[i]->filename, nl[i]->d_name);
						file_list[i]->is_file = TRUE;
						fsi->nr_of_filent++;
					}
				}
			}
			free(nl[i]);
		}
		free(nl);

		fsi->file_list = (struct filent**)malloc(fsi->nr_of_filent*sizeof(int));

		i = fsi->nr_of_filent;

		while(--nr_of_filent >= 0)
			if(file_list[nr_of_filent] != (struct filent*)NULL)
				fsi->file_list[--i] = file_list[nr_of_filent];

		free(file_list);

		quick_dir_sort(&fsi->file_list[1], fsi->nr_of_filent-1);

		xv_set(fsi->panel_list_item, XV_SHOW, FALSE, NULL);

		for(i=0; i<fsi->nr_of_filent; i++) {
			sprintf(name, "%s%s", fsi->file_list[i]->is_file ? "     " : "<> ", fsi->file_list[i]);
			xv_set(fsi->panel_list_item,
				PANEL_LIST_STRING,	i, name,
				NULL);
		}
		for(i=xv_get(fsi->panel_list_item, PANEL_LIST_NROWS)-1; i>=fsi->nr_of_filent; i--)
			xv_set(fsi->panel_list_item, PANEL_LIST_DELETE, i, NULL);

		xv_set(fsi->panel_list_item,
			XV_SHOW,		TRUE,
			NULL);

	}
	chdir(help);
}


static void fls_set_filename(item)	/* 'item' is always the file_input_item of a fileselector. 	*/
					/* this procedure serves to adapt the value of the file_item 	*/
					/* to the value of the file_input_item				*/
Panel_item	item;
{
	Fs_item	fsi;
	Komp_list	fn, exp;
	char		*result;
	Panel_setting	hilf_result;
	
	fsi = fls_get_fsi(item);
	fsi->last_selected_item = (Panel_item)NULL;
	if(fsi->fit_extension) {
		exp = rexp_komponents((fsi->extension)[fsi->last_extension]);
	} else {
		exp = rexp_komponents("*");
	}
	strcpy(fsi->fileinput, (char *)xv_get(item, PANEL_VALUE));
	fn = filename_komponents((char *)xv_get(fsi->file_input_item, PANEL_VALUE));
	result = get_name_of_lists(fn, exp);
	strcpy(fsi->filename, result);
	free(result);
	dispose_komplist(fn); dispose_komplist(exp);
	xv_set(fsi->file_item, PANEL_VALUE, fsi->filename, NULL);
	
}	


/********************************************************************************/
/*										*/
/*	file read/write	procedures						*/
/*										*/
/********************************************************************************/

static void scan_open_file(name)
char *name;
{
	if(scanfile != (FILE *)NULL) {
		fclose(scanfile);
	}
	scanfile = fopen(name, "r");
	buffer[0] = '\0';
	scan_position = buffer;
	linecount = 0;
}

static void scan_close_file()
{
	if(scanfile != (FILE *)NULL) {
		fclose(scanfile);
	}
}

static int scan_get_next_line()
{
	char dummy;
	if(scanfile == (FILE *)NULL) {
		return FALSE;
	}
	buffer[0] = '\0';
	if(fscanf(scanfile, "%[^\n]", buffer) != EOF) {
		fscanf(scanfile, "%c", &dummy);
		scan_position = buffer;
		linecount++;
		return TRUE;
	} else {
		return FALSE;
	}
}

static int scan_find_string(str)
char *str;
{
	do {
		scan_position = strstr(scan_position, str);
	} while(scan_position == (char *)NULL && scan_get_next_line());
	if(scan_position == (char *)NULL) {
		return FALSE;
	} else {
		return TRUE;
	}
}

static int is_number(s)
char *s;
{
	int result = FALSE;
	char *pos;
	pos = s;
	if(pos == NULL || *pos == '\0') {
		return FALSE;
	}
	if(*pos == '-') {
		pos++;
	}
	while(*pos != '\0') {
		if(isdigit(*pos)) {
			result = TRUE;
		} else {
			return FALSE;
		}
		pos++;
	}
	return result;
}

void	fls_setup_from_file(fsi, filename, identification)
Fs_item	fsi;
char	*filename, *identification;
{
	int found = FALSE, finished = FALSE, val;
	char	key[128];
	
	scan_open_file(filename);
	while(!found && scan_find_string("FILESELECTOR:")) {
		scan_position += 13;
		scan_position += strspn(scan_position, " \t");
		key[0] = '\0';
		sscanf(scan_position, "%s", key);
		if(!strcmp(key, identification)) {
			found = TRUE;
		}
	}
	if(found) {
		while(!finished && scan_get_next_line()) {
			scan_position = strchr(scan_position, '#');
			if(scan_position != NULL) {
				scan_position++;
				key[0] = '\0';
				sscanf(scan_position, "%[^:]", key);
				scan_position += strlen(key) + 1;
				if(!strcmp(key, "directory")) {
					key[0] = '\0';
					sscanf(scan_position, "%s", key);
					if(!strcmp(key, "\"\"")) {
						fls_set_working_directory(fsi, "");
					} else {
						fls_set_working_directory(fsi, key);
					}
				}
				if(!strcmp(key, "'.'-files")) {
					key[0] = '\0';
					sscanf(scan_position, "%s", key);
					if(!strcmp(key, "hidden")) {
						fsi->show_point_files = FALSE;
					} else {
						if(!strcmp(key, "displayed")) {
							fsi->show_point_files = TRUE;
						} else {
							fprintf(stderr, "fls_setup - error in \"%s\", line %d:\n expecting 'hidden' or 'displayed' instead of '%s'.\n",
								filename, linecount, key);
						}
					}
				}
				if(!strcmp(key, "fit extension")) {
					key[0] = '\0';
					sscanf(scan_position, "%s", key);
					if(!strcmp(key, "off")) {
						fsi->fit_extension = FALSE;
					} else {
						if(!strcmp(key, "on")) {
							fsi->fit_extension = TRUE;
						} else {
							fprintf(stderr, "fls_setup - error in \"%s\", line %d:\n expecting 'on' or 'off' instead of '%s'.\n",
								filename, linecount, key);
						}
					}
				}
				if(!strcmp(key, "mask")) {
					scan_position += strspn(scan_position, " \t");
					sscanf(scan_position, "%d", &val);
					key[0] = '\0';
					sscanf(scan_position, "%s", key);
					scan_position += strlen(key);
					if(is_number(key)) {
						val--;
						if(val >= 0 && val < MASKNO) {
							key[0] = '\0';
							sscanf(scan_position, "%s", key);
							if(!strcmp(key, "\"\"")) {
								fls_set_extension(fsi, val, fls_empty_string);
							} else {
								fls_set_extension(fsi, val, key);
							}
						} else {
							fprintf(stderr, "fls_setup - error in \"%s\", line %d:\n number out of range [1..%d]\n",
								filename, linecount, MASKNO);
						}
					} else {
						fprintf(stderr, "fls_setup - error in \"%s\", line %d:\n expecting number instead of '%s'.\n",
							filename, linecount, key);
					}
				}
				if(!strcmp(key, "current mask")) {
					scan_position += strspn(scan_position, " \t");
					sscanf(scan_position, "%d", &val);
					key[0] = '\0';
					sscanf(scan_position, "%s", key);
					if(is_number(key)) {
						val--;
						if(val >= 0 && val < MASKNO) {
							fls_set_current_extension(fsi, val);
						} else {
							fprintf(stderr, "fls_setup - error in \"%s\", line %d:\n number out of range [1..%d]\n",
								filename, linecount, MASKNO);
						}
					} else {
						fprintf(stderr, "fls_setup - error in \"%s\", line %d:\n expecting number instead of '%s'.\n",
							filename, linecount, key);
					}
				}
				if(!strcmp(key, "displaying")) {
					key[0] = '\0';
					sscanf(scan_position, "%s", key);
					if(!strcmp(key, "all")) {
						fsi->dir_cycle = 1;
					} else {
						if(!strcmp(key, "files")) {
							fsi->dir_cycle = 0;
						} else {
							if(!strcmp(key, "directories")) {
								fsi->dir_cycle = 2;
							} else {
								fprintf(stderr, "fls_setup - error in \"%s\", line %d:\n expecting 'files', 'all' or 'directories' instead of '%s'.\n",
									filename, linecount, key);
							}
						}
					}
				}
			} else {
				scan_position = strstr(buffer, "FILESELECTOR:");
				if(scan_position != NULL) {
					finished = TRUE;
				}
			}
		}
	} else {
		fprintf(stderr, "fls_setup - error in \"%s\":\n fileselector information \"%s\" not found.\n",
			filename, identification);
	}
	scan_close_file();
}


void	fls_write_to_file(fsi, file, identification)
Fs_item	fsi;
FILE	*file;
char	*identification;
{
	int i;
	fprintf(file, "FILESELECTOR: %s\n", identification);
	if(!strcmp(fsi->cwd, "")) {
		fprintf(file, "\t#directory:\t\t\"\"\n");
	} else {
		fprintf(file, "\t#directory:\t\t%s\n", fsi->cwd);
	}
	fprintf(file, "\t#'.'-files:\t\t");
	if(fsi->show_point_files) {
		fprintf(file, "displayed\n");
	} else {
		fprintf(file, "hidden\n");
	}
	fprintf(file, "\t#fit extension:\t\t");
	if(fsi->fit_extension) {
		fprintf(file, "on\n");
	} else {
		fprintf(file, "off\n");
	}
	for(i=0; i<MASKNO; i++) {
		fprintf(file, "\t#mask:\t\t\t%d\t", i+1);
		if(fsi->extension[i] == fls_empty_string) {
			fprintf(file, "\"\"\n");
		} else {
			fprintf(file, "%s\n", fsi->extension[i]);
		}
	}
	fprintf(file, "\t#current mask:\t\t%d\n", fsi->last_extension + 1);
	fprintf(file, "\t#displaying:\t\t");
	switch(fsi->dir_cycle) {
		case	2:	fprintf(file, "directories\n");
				break;
		case	1:	fprintf(file, "all\n");
				break;
		case	0:	fprintf(file, "files\n");
				break;
	}
}

/****************************** the end *****************************************/
