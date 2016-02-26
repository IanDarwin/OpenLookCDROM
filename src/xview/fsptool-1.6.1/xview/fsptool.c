/********************************************************************************/
/* fsptool.c --									*/
/*	An OpenLook front end for the fsp client programs, this program		*/
/*	calls out to the appropriate fsp command ie. fgetcmd to carry out the	*/
/*	actual fsp actions. This seemed appropriate as the fsp client programs	*/
/*	get updated every other week, just about :~).				*/
/*										*/
/* Author : A.J.Doherty								*/
/* Date   : 06/11/93								*/
/* Version: 1.6.0								*/
/*										*/
/* (c)1993 Andrew J. Doherty							*/
/********************************************************************************/

#include "fsptool.h"
#include "icon.h"
#include "frame.h"
#include "create.h"
#include "menu.h"
#include "resource.h"
#include "file.h"
#include "system.h"

#include "../lib/unix.h"
#include "../lib/fsp.h"
#include "../lib/file.h"

#ifdef _FSP_LOGGING
#include "../lib/logging.h"
#endif

#include "../config.h"

#include <xview/scrollbar.h>

#include <stdarg.h>

/********************************************************************************/

extern Base_frame	baseframe;
extern Transfer_frame	transferframe;
extern Local_frame	localframe;
extern Sethost_frame	sethostframe;

extern int		files_found, files_selected,
			local_files_found, local_files_selected,
			exec_mode;

extern int		pipe_io[2][2];

extern Notify_client	client1;

extern HostInfo	       *hosts_list[MAX_HOSTS];

extern Icon		fsptoolicon;

extern char		*fsp_ls_cmd, *fsp_get_cmd, *fsp_put_cmd;

/********************************************************************************/

long int	total_transfer_size	= 0,
		transfer_done		= 0,
		current_file_size	= 0;

char 		*buf;

/********************************************************************************/

long int remote_transfer_size(void);

/********************************************************************************/

int main ( int argc, char *argv[] )

{ char *tmp, helpbuf[BUFSIZ];

buf = c_malloc(512);


(void)xv_init(XV_INIT_ARGC_PTR_ARGV, &argc, argv, NULL);

initialise_fsptool_lib();
register_error_handler(error_report);

sprintf(helpbuf,"HELPPATH=%s:",FSPTOOL_HELP);

if ((tmp = getenv_var("HELPPATH")))
    strcat(helpbuf,tmp);

setenv_var(helpbuf);

load_icons();		/* -- load icon and set icons specific data "icon.c"	*/
load_resources();	/* -- load resources and set specific data "resource.c"	*/
load_hosts();		/* -- load file of hosts into memory "resource.c"	*/
set_environ();		/* -- set FSP environment specific data "fsp.c"		*/

make_baseframe();	/* -- create base frame					*/
make_child_frames();	/* -- create children of base frame			*/

set_defaults();		/* -- set defaults to compiled in values if no resource	*/

xv_set(baseframe.frame, FRAME_ICON, fsptoolicon, NULL);

#ifdef _FSP_LOGGING
log_startup();
#endif

xv_main_loop(baseframe.frame);

#ifdef _FSP_LOGGING
log_exit();
#endif

free(buf);
return(0);
}

/********************************************************************************/

void fget_files ( CacheData *file_data )

{ char      *argv[] = { fsp_get_cmd, "-r", NULL };
  CacheData *ptr;
  int	     pid;


if (files_selected) {
    if (xv_get(baseframe.dir_list, PANEL_LIST_SELECTED, 0)) {
	xv_set(baseframe.dir_list, PANEL_LIST_SELECT, 0, FALSE, NULL);
	files_selected--;
	}
    }
else
    return;

set_frame_busy(TRUE);

transfer_done = 0;

if (file_data) { int i = 1;
    do
	ptr = (CacheData*)xv_get(baseframe.dir_list,PANEL_LIST_CLIENT_DATA,i++);
    while ((i <= files_found) && (strcmp(ptr->name,file_data->name) != 0));

    xv_set(baseframe.dir_list, PANEL_LIST_SELECT, --i, TRUE, NULL);
    ptr = file_data;
    total_transfer_size = ptr->size;
    files_selected++;
    }
else { int i;

    left_footer(baseframe.frame,"Determining transfer size ...");
    left_footer(transferframe.frame,"Determining transfer size ...");

    total_transfer_size = remote_transfer_size();

    left_footer(transferframe.frame,"Transfer size %s",
			unit_file_size(total_transfer_size));

    i   = get_next_selected(baseframe.dir_list);
    ptr = (CacheData*) xv_get(baseframe.dir_list, PANEL_LIST_CLIENT_DATA, i);
    }

pid = do_spawn(argv,(void*)read_get_stream);
xv_set(baseframe.dir_list, PANEL_CLIENT_DATA, pid, NULL);

current_file_size = ptr->size;
left_footer(baseframe.frame,"Getting %s ...",ptr->name);
set_transferframe(ptr->name,ptr->size,total_transfer_size,DOWNLOAD);
strcpy(buf,ptr->name);

#ifdef _FSP_LOGGING
log_transfer(buf,ptr->size);
#endif

strcat(buf,"\n");
write(pipe_io[0][1],buf,strlen(buf));
right_footer(baseframe.frame,files_found,files_selected);
}

/********************************************************************************/

void fput_files ( CacheData *file_data )

/* this fn puts files to the current FSP remote directory, if file then is used	*/
/* as the name of a local file to upload, otherwise selected files in the local	*/
/* directory listing frame will be used						*/


{ char    *argv[] = { fsp_put_cmd, NULL };
  int      i;
static int pid;
CacheData *ptr;


set_frame_busy(TRUE);
total_transfer_size = 0;
transfer_done = 0;

if (file_data == NULL) {
    if (local_files_selected) {
	if (xv_get(localframe.dir_list, PANEL_LIST_SELECTED, 0)) {
	    xv_set(localframe.dir_list, PANEL_LIST_SELECT, 0, FALSE, NULL);
	    local_files_selected--;
	    }
	}
    else {
	set_frame_busy(FALSE);
	return;
	}

    pid = do_spawn_put(argv,(void*)read_put_stream);
    xv_set(baseframe.dir_list, PANEL_CLIENT_DATA, pid, NULL);

    for ( i = 1; i <= local_files_found; i++ )
	if ( xv_get(localframe.dir_list, PANEL_LIST_SELECTED, i) ) {
	    ptr = (CacheData*)
			xv_get(localframe.dir_list,PANEL_LIST_CLIENT_DATA,i);
	    total_transfer_size += ptr->size;
	    }

    i   = get_next_selected(localframe.dir_list);
    ptr = (CacheData*) xv_get(localframe.dir_list, PANEL_LIST_CLIENT_DATA, i);
    xv_set(localframe.dir_list, PANEL_LIST_SELECT, i, FALSE, NULL);
    right_footer(localframe.frame,local_files_found,--local_files_selected);
    left_footer(localframe.frame,"Sending %s ...",ptr->name);
    }
else {
    (void) do_spawn_put(argv,(void*)read_single_stream);
    ptr = file_data;
    left_footer(baseframe.frame,"Sending %s ...",ptr->name);
    }

current_file_size = ptr->size;
set_transferframe(ptr->name,ptr->size,total_transfer_size,UPLOAD);
strcpy(buf,ptr->name);
strcat(buf,"\n");
write(pipe_io[0][1],buf,strlen(buf));
}

/********************************************************************************/

Notify_value read_get_stream ( Notify_client client, int fd )

{ static char  buffer[256];
          int  i, transferred;
    CacheData *ptr;


switch (read_client_data(fd,&transferred,buffer))
    {
    case CLIENT_STATUS:
	set_transferdone(transferred,transfer_done+transferred);
	set_icon(DOWNLOAD,transfer_done+transferred,total_transfer_size);
	break;

    case CLIENT_COMPLETE:
	xv_set(transferframe.frame, FRAME_RIGHT_FOOTER, buffer, NULL);
	transfer_done += current_file_size;
	--files_selected;
	update_local_dir_list();
	
	if (!exec_mode) {
	    i = get_next_selected(baseframe.dir_list);
	    xv_set(baseframe.dir_list, PANEL_LIST_SELECT, i, FALSE, NULL);

	    if ((i = get_next_selected(baseframe.dir_list))) {
		ptr = (CacheData*) xv_get(baseframe.dir_list,
					PANEL_LIST_CLIENT_DATA,i);

		strcpy(buffer,ptr->name);
		set_transferframe(buffer,ptr->size,total_transfer_size,DOWNLOAD);
		set_transferdone(0,transfer_done);
		current_file_size = ptr->size;
		left_footer(baseframe.frame,"Getting %s ...",buffer);

#ifdef _FSP_LOGGING
		log_transfer(buffer,ptr->size);
#endif

		right_footer(baseframe.frame,files_found,files_selected);
		strcat(buffer,"\n");
		write(pipe_io[0][1],buffer,strlen(buffer));
		return(NOTIFY_DONE);
		}
	    }

	notify_set_input_func(client,NOTIFY_FUNC_NULL,pipe_io[1][0]);
	close(pipe_io[0][1]);
	right_footer(baseframe.frame,files_found,files_selected);
	left_footer(baseframe.frame,"File get completed.");
	set_transferframe("Not Transferring",0,0,FALSE);
	set_transferdone(0,0);

	if (exec_mode) {
	    ptr = (CacheData*) xv_get(baseframe.dir_list,
					PANEL_LIST_CLIENT_DATA,
					get_next_selected(baseframe.dir_list));
 
	    left_footer(baseframe.frame,"Starting %s ...",ptr->name);
	    exec_file(ptr);
	    exec_mode = FALSE;
	    }

	xv_set(baseframe.dir_list, PANEL_LIST_SELECT,
			get_next_selected(baseframe.dir_list),
			FALSE, NULL);

	set_icon(FALSE,0,100);
	set_frame_busy(FALSE);
	break;

    case CLIENT_ERROR:
	notify_set_input_func(client,NOTIFY_FUNC_NULL,pipe_io[1][0]);
	done_proc(buffer);
	break;

    case CLIENT_NONE:
    case CLIENT_WARNING:
	break;

    default:
	error_report(WARNING,
			"FSPtool: unidentified return value from FSP client");
    }

return(NOTIFY_DONE);
}

/********************************************************************************/

Notify_value read_put_stream ( Notify_client client, int fd )

{ char *buffer  = c_malloc(256);

   int	i, transferred;

 CacheData *ptr;


switch(read_client_data(fd,&transferred,buffer))
    {
    case CLIENT_STATUS:
	set_transferdone(transferred,transfer_done+transferred);
	set_icon(UPLOAD,transfer_done+transferred,total_transfer_size);
	break;

    case CLIENT_COMPLETE:
	xv_set(transferframe.frame, FRAME_RIGHT_FOOTER, buffer, NULL);
	transfer_done += current_file_size;

	if ((i = get_next_selected(localframe.dir_list))) {
	    xv_set(localframe.dir_list, PANEL_LIST_SELECT, i, FALSE, NULL);
	    ptr=(CacheData*)xv_get(localframe.dir_list, PANEL_LIST_CLIENT_DATA, i);
	    strcpy(buffer,ptr->name);
	    set_transferframe(buffer,ptr->size,total_transfer_size,UPLOAD);
	    set_transferdone(0,transfer_done);
	    current_file_size = ptr->size;
	    left_footer(localframe.frame,"Sending %s ...",buffer);
	   right_footer(localframe.frame,local_files_found,--local_files_selected);
	    strcat(buffer,"\n");
	    write(pipe_io[0][1],buffer,strlen(buffer));
	    free(buffer);
	    return(NOTIFY_DONE);
 	    }

	notify_set_input_func(client,NOTIFY_FUNC_NULL,pipe_io[1][0]);
	done_proc("File send completed.");
	break;
	
    case CLIENT_ERROR:
	notify_set_input_func(client,NOTIFY_FUNC_NULL,pipe_io[1][0]);
	done_proc(buffer);

	if (strstr(buffer,"no permission"))
	    error_report(ERROR,"No Permission to copy into remote directory");
	break;

    case CLIENT_NONE:
	break;

    default:
	error_report(WARNING,
			"FSPtool: unidentified return value from FSP client");
    }

free(buffer);
return(NOTIFY_DONE);
}

/********************************************************************************/

Notify_value read_single_stream ( Notify_client client, int fd )

/* used to read the stream from a single file fputcmd				*/

{ char *buffer = c_malloc(256);
   int	transferred;


switch(read_client_data(fd,&transferred,buffer))
    {
    case CLIENT_STATUS:
	set_transferdone(transferred,transfer_done+transferred);
	set_icon(UPLOAD,transfer_done+transferred,total_transfer_size);
	break;

    case CLIENT_COMPLETE:
	xv_set(transferframe.frame, FRAME_RIGHT_FOOTER, buffer, NULL);
	transfer_done += current_file_size;
	notify_set_input_func(client,NOTIFY_FUNC_NULL,pipe_io[1][0]);
	done_proc("File send completed.");
	break;

    case CLIENT_ERROR:
	notify_set_input_func(client,NOTIFY_FUNC_NULL,pipe_io[1][0]);
	done_proc(buffer);
	break;

    case CLIENT_NONE:
    case CLIENT_WARNING:
	break;

    default:
	error_report(WARNING,
			"FSPtool: unidentified return value from FSP client");
    }

free(buffer);
return(NOTIFY_DONE);
}

/********************************************************************************/

void update_dir_list()

/* this function updates this directory list to reflect the new current dir */

{
set_frame_busy(TRUE);

left_footer(baseframe.frame,"Initialising ...");

#ifdef _FSP_LOGGING
log_directory(get_fsp_dir());
#endif

xv_set(baseframe.dir_list, PANEL_LIST_DELETE_ROWS, 0, files_found+1, NULL);

read_fsp_directory(baseframe.dir_list);

files_selected = 0;

xv_set(xv_get(baseframe.dir_list, PANEL_LIST_SCROLLBAR),
		SCROLLBAR_VIEW_START,
		0,
		NULL);

set_frame_busy(FALSE);
right_footer(baseframe.frame,files_found,files_selected);
xv_set(baseframe.cache_button, PANEL_ITEM_MENU, make_cache_menu(), NULL);
}

/********************************************************************************/

void select_files ( Panel_item item, char *string, caddr_t client_data,
		    Panel_list_op op, Event *event )

{ static char	 dir_buf[MAXPATHLEN], ts_string[512];

  CacheData	*file_data = (CacheData*) client_data;

  static long	 ts_val;


if (op == PANEL_LIST_OP_SELECT) {
    files_selected++;
    strcpy(ts_string,string);
    ts_val = csectime();
    }
else {
    if ((op == PANEL_LIST_OP_DESELECT) && (strcmp(string,ts_string) == 0)) {
	if ((csectime()-ts_val) < 40) {
	    strcpy(dir_buf,(char*)xv_get(baseframe.dir_text,PANEL_VALUE));

	    if (strcmp("../",ts_string) == 0) {	/* -- go up a directory */
		strip_pathname(dir_buf);
		xv_set(baseframe.dir_text, PANEL_VALUE, dir_buf, NULL);
		set_fsp_dir(dir_buf);
		update_dir_list();
		files_selected++;
		}
	    else
		switch (file_data->filetype)
		    {
		    case DIRECTORY:
			if (strcmp("/",dir_buf) != 0) /* -- already at root dir */
		            strcat(dir_buf,"/");
	
			strcat(dir_buf,file_data->name);
			xv_set(baseframe.dir_text, PANEL_VALUE, dir_buf, NULL);
			set_fsp_dir(dir_buf);
			update_dir_list();
			files_selected++;
		    	break;

		    case FILE_LINK:
			strcpy(dir_buf,file_data->name);
			xv_set(baseframe.dir_text, PANEL_VALUE, dir_buf, NULL);
			set_fsp_dir(dir_buf);
			update_dir_list();
			files_selected++;
			break;

		    default:
			do_exec_file(file_data);
		    }
	    }
	}

    files_selected--;
    }

right_footer(baseframe.frame,files_found,files_selected);
}

/********************************************************************************/

void select_local_files ( Panel_item item, char *string, caddr_t client_data,
		    Panel_list_op op, Event *event )

{ static char  dir_buf[MAXPATHLEN], ts_string[512];
  CacheData   *file_data = (CacheData*) client_data;

  static long  ts_val;


if (op == PANEL_LIST_OP_SELECT) {
    local_files_selected++;
    strcpy(ts_string,string);
    ts_val = csectime();
    }
else {
    if ((op == PANEL_LIST_OP_DESELECT) && (strcmp(string,ts_string) == 0)) {
	if ((csectime()-ts_val) < 40) {
	    strcpy(dir_buf,(char*)xv_get(localframe.localdir,PANEL_VALUE));

	    if (strcmp("../",ts_string) == 0) {	/* -- go up a directory */
		strip_pathname(dir_buf);
		xv_set(localframe.localdir, PANEL_VALUE, dir_buf, NULL);
		set_fsp_local_dir(dir_buf);
		update_local_dir_list();
		local_files_selected++;
		}
	    else
		switch (file_data->filetype)
		    {
		    case DIRECTORY:
			if (strcmp("/",dir_buf) != 0) /* -- already at root dir */
		            strcat(dir_buf,"/");
	
			strcat(dir_buf,file_data->name);
			xv_set(localframe.localdir, PANEL_VALUE, dir_buf, NULL);
			set_fsp_local_dir(dir_buf);
			update_local_dir_list();
			local_files_selected++;
		    	break;

		    case FILE_LINK:
			strcpy(dir_buf,file_data->name);
			xv_set(localframe.localdir, PANEL_VALUE, dir_buf, NULL);
			set_fsp_local_dir(dir_buf);
			update_local_dir_list();
			local_files_selected++;
			break;

		    default:
			do_exec_local_file(file_data);
		    }
	    }
	}

    local_files_selected--;
    }

right_footer(localframe.frame,local_files_found,local_files_selected);
}

/********************************************************************************/

void done_proc ( const char *message )

{
close(pipe_io[0][1]);
right_footer(baseframe.frame,files_found,files_selected);
left_footer(baseframe.frame,"%s",(char*) message);
set_frame_busy(FALSE);
set_icon(FALSE,0,100);
set_transferframe("Not Transferring",0,0,FALSE);
left_footer(transferframe.frame,"");
}

/********************************************************************************/

Panel_setting remote_dir_proc ( Panel_item item, Event *event )

/* this function is called when a notify char happens for the dir panel item if */
/* is space or escape or tab then is ignored, if return or new line then new    */
/* dir list if obtained for the specified directory is possible			*/

{
switch (event_action(event))
    {
    case '\n' :
    case '\r' :
	set_fsp_dir((char*) xv_get(item, PANEL_VALUE));
	update_dir_list();
	return(PANEL_NONE);

    case '\t' :
    case '\033':
    case '\040':
	return(PANEL_NONE);

    default:
	return(panel_text_notify(item,event));
    }
}

/********************************************************************************/

Panel_setting local_dir_proc ( Panel_item item, Event *event )

/* this function is called when a notify char happens for the dir panel item if */
/* is space or escape or tab then is ignored, if return or new line then this	*/
/* becomes the new local directory list	-- created if it doesn't exist		*/

{ int result;


switch (event_action(event))
    {
    case '\n' :
    case '\r' :
	if (chdir((char*)xv_get(item, PANEL_VALUE)) == -1)
	    {
	    result = notice_prompt(xv_get(item,PANEL_PARENT_PANEL), NULL,
			NOTICE_FOCUS_XY,	event_x(event), event_y(event),
			NOTICE_MESSAGE_STRINGS,
				(char*)xv_get(item, PANEL_VALUE),
				"does not exist, create it?",
				NULL,
			NOTICE_BUTTON_YES,	"YES",
			NOTICE_BUTTON_NO,	"NO",
			NULL);

	    if (result == NOTICE_NO)
		return(PANEL_NONE);

	    if (!make_dir_hier((char*)xv_get(item, PANEL_VALUE)))
		{
   	        (void) notice_prompt(xv_get(item,PANEL_PARENT_PANEL), NULL,
			NOTICE_FOCUS_XY,	event_x(event), event_y(event),
			NOTICE_MESSAGE_STRINGS,
				"Unable to create new directory",
				(char*)xv_get(item, PANEL_VALUE),
				"local directory not changed",
				NULL,
			NOTICE_BUTTON,		"OK",	100,
			NULL);

		return(PANEL_NONE);
		}
	    }

	set_fsp_local_dir((char*) xv_get(item, PANEL_VALUE));
	update_local_dir_list();
	return(PANEL_NONE);

    case '\t' :
    case '\033':
    case '\040':
	return(PANEL_NONE);

    default:
	return(panel_text_notify(item,event));
    }
}

/********************************************************************************/

int get_next_selected ( Panel_item dir_list )

/* this functions returns the next selected item off of the list item		*/

{ register int loop;

loop = xv_get(dir_list,PANEL_LIST_FIRST_SELECTED);

return(loop == -1 ? 0 : loop);
}

/********************************************************************************/

void update_local_dir_list()

/* this function updates this directory list to reflect the new current dir */

{ int	     i;
  CacheData *client_data;


xv_set(localframe.dir_list, PANEL_INACTIVE, TRUE, NULL);
left_footer(localframe.frame, "Initialising...");

for (i = 1; i < local_files_found; i++ ) {
    client_data = (CacheData*)xv_get(localframe.dir_list,PANEL_LIST_CLIENT_DATA,i);
    free(client_data->name);
    free(client_data);
    }

xv_set(localframe.dir_list, PANEL_LIST_DELETE_ROWS, 0, local_files_found+1, NULL);
read_local_directory(localframe.dir_list,get_fsp_local_dir());
panel_paint(localframe.dir_list, PANEL_CLEAR);

xv_set(xv_get(baseframe.dir_list, PANEL_LIST_SCROLLBAR),
		SCROLLBAR_VIEW_START,
		0,
		NULL);

xv_set(localframe.dir_list, PANEL_INACTIVE, FALSE, NULL);
}

/********************************************************************************/

void abort_proc ( Panel_item item, int value, Event *event )

{ int pid;

notify_set_input_func(client1, NOTIFY_FUNC_NULL, pipe_io[1][0]);
close(pipe_io[0][1]);

pid = (int) xv_get(baseframe.dir_list, PANEL_CLIENT_DATA);
terminate_process(pid);

right_footer(baseframe.frame,files_found,files_selected);
left_footer(baseframe.frame,"Aborted...");
set_frame_busy(FALSE);
set_icon(FALSE,0,100);
set_transferframe("Not Transferring",0,0,FALSE);
}

/********************************************************************************/

void error_report ( int level, ... )

/* this fn reports an error on different levels, a warning is simply displayed	*/
/* on STDERR, an error is displayed on STDERR and in the GUI interface.		*/

{    va_list  args;
	char *fmt;
 static char  lbuf[512];


va_start(args,level);
fmt = va_arg(args,char*);

if (!level) {
    (void) vfprintf(stderr, fmt, args);
    (void) fprintf(stderr, "\n");
    va_end(args);
    return;
    }

if (level == ERROR) {
    (void) fprintf(stderr, "FSPtool:");
    (void) vfprintf(stderr, fmt, args);
    (void) fprintf(stderr, "\n");

    (void) vsprintf(lbuf, fmt, args);
    left_footer(baseframe.frame, "%s", lbuf);

    (void) notice_prompt(baseframe.frame, NULL,
			NOTICE_MESSAGE_STRINGS,
				"FSPtool Error",
				lbuf,
				NULL,
			NOTICE_BUTTON,	"OK",	0,
			NULL);

    va_end(args);
    return;
    }

(void) vfprintf(stderr, fmt, args);
(void) fprintf(stderr, "\n");
va_end(args);
exit(1);
}

/********************************************************************************/

long int remote_transfer_size()

/* determine total file size (bytes) of all selected items in remote transfer	*/

{ long int   total = 0;
  int	     i, rows;
  CacheData *ptr;


rows = (int) xv_get(baseframe.dir_list, PANEL_LIST_NROWS);

for ( i = 0; i < rows; i++ )
    if ((int) xv_get(baseframe.dir_list, PANEL_LIST_SELECTED, i)) {
	ptr = (CacheData*) xv_get(baseframe.dir_list, PANEL_LIST_CLIENT_DATA, i);

	if (ptr->filetype == DIRECTORY) {
	    int	     status;
	    long int dircount = 0;

	    status = cache_dir_size(ptr->name,&dircount);

	    if (status != CACHE_OK) {
		error_report(ERROR,"Error determining transfer size");
		return(total);
		}

	    ptr->size = dircount;
	    total += dircount;
	    }
	else
	    total += ptr->size;
	}

return(total);
}

/********************************************************************************/


