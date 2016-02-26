/********************************************************************************/
/* batch.c --									*/
/*										*/
/* Author : A.J.Doherty								*/
/********************************************************************************/

#include "fsptool.h"
#include "resource.h"
#include "create.h"
#include "frame.h"
#include "batch.h"
#include "icon.h"
#include "system.h"

#include "../lib/cache.h"
#include "../lib/file.h"
#include "../lib/unix.h"
#include "../lib/fsp.h"

/********************************************************************************/

static char host_buf[128],		/* -- so we can remember host name	*/
	    port_buf[128],		/* -- and port during batch transfers	*/
	    dir_buf[MAXPATHLEN];	/* -- remote directory also		*/

static char lhostname[128],		/* -- to remember the current host/port	*/
	    lportname[128];		/* -- combination during transfer ops	*/

static long int  transfer_done       = 0,	/* -- amount of transfer done	*/
	         current_file_size   = 0,	/* -- size of current file	*/
	         total_transfer_size = 0;	/* -- size of batch transfer	*/

/********************************************************************************/

long int batch_transfer_size(void);

/********************************************************************************/

extern Base_frame	baseframe;
extern Batch_frame	batchframe;
extern Filer_frame	filerframe;
extern Transfer_frame	transferframe;

extern Xv_font		fixed_font, bold_fixed_font;

extern char		*buf, *fsp_get_cmd;

extern int		exec_mode, files_found, files_selected, pipe_io[2][2];

/********************************************************************************/

int batch_list_notify_proc ( Panel_item item, char *string, Xv_opaque client_data,
			     Panel_list_op op, Event *event, int row )

{
switch (op)
    {
    case PANEL_LIST_OP_DELETE:
	delete_batch_item(row);
	break;

    case PANEL_LIST_OP_SELECT:
    case PANEL_LIST_OP_DESELECT:
    case PANEL_LIST_OP_VALIDATE:
	break;
    }

return(XV_OK);
}

/********************************************************************************/

void add_to_batch_list()

{ CacheData *file_ptr;
  BatchItem *item_ptr;
	int  i, list_items = (int)xv_get(batchframe.batch_list, PANEL_LIST_NROWS);


if (get_next_selected(baseframe.dir_list) == 0)
    return;

while ((i = get_next_selected(baseframe.dir_list)) > 0 )
    {
    item_ptr = (BatchItem*) c_malloc(sizeof(BatchItem));
    file_ptr = (CacheData*) xv_get(baseframe.dir_list, PANEL_LIST_CLIENT_DATA, i);

    item_ptr->hostname = c_strdup(get_fsp_host());
    item_ptr->hostport = c_strdup(get_fsp_port());
    item_ptr->filetype = file_ptr->filetype;

    strcpy(buf,get_fsp_dir());

    if (strcmp(get_fsp_dir(),"/") != 0)
	strcat(buf,"/");

    strcat(buf,file_ptr->name);
    item_ptr->filename = c_strdup(buf);

    if (isdir(file_ptr))
	item_ptr->filesize = -1;
    else
	item_ptr->filesize = file_ptr->size;

    strcpy(buf,item_ptr->hostname);
    strcat(buf," (");
    strcat(buf,item_ptr->hostport);
    strcat(buf,") ");
    strcat(buf,item_ptr->filename);

    xv_set(batchframe.batch_list,
		PANEL_LIST_INSERT, 	list_items,
		PANEL_LIST_STRING, 	list_items,	buf,
		PANEL_LIST_FONT,   	list_items,
			isdir(file_ptr) ? bold_fixed_font : fixed_font,
		PANEL_LIST_CLIENT_DATA,	list_items,	item_ptr,
		NULL);

    xv_set(baseframe.dir_list, PANEL_LIST_SELECT, i, FALSE, NULL);
    files_selected--;
    list_items++;
    }

left_footer(batchframe.frame, "Added files to batch list.");
right_footer(baseframe.frame, files_found, files_selected);
}

/********************************************************************************/

void do_batch_transfer()

{ int loop, rows;

if ((rows = (int) xv_get(batchframe.batch_list, PANEL_LIST_NROWS)) == 0)
    return;

left_footer(batchframe.frame, "Transfering all items.");
set_frame_busy(TRUE);
xv_set(batchframe.frame, FRAME_BUSY, TRUE, NULL);

for ( loop = 0; loop < rows; loop++ )
    xv_set(batchframe.batch_list, PANEL_LIST_SELECT, loop, TRUE, NULL);

strcpy(host_buf,get_fsp_host());
strcpy(port_buf,get_fsp_port());
strcpy(dir_buf,get_fsp_dir());

transfer_done = 0;
left_footer(transferframe.frame,"Determining transfer size ...");
left_footer(baseframe.frame,"Determining transfer size ...");
total_transfer_size = batch_transfer_size();
left_footer(transferframe.frame,"Transfer size %s",
		unit_file_size(total_transfer_size));
batch_transfer_proc();
}

/********************************************************************************/

void do_selected_batch_transfer()

{
if (((int) xv_get(batchframe.batch_list, PANEL_LIST_NROWS)) == 0)
    return;

if (((int) xv_get(batchframe.batch_list, PANEL_LIST_FIRST_SELECTED)) == -1)
    return;

left_footer(batchframe.frame, "Transfering selected items.");
set_frame_busy(TRUE);
xv_set(batchframe.frame, FRAME_BUSY, TRUE, NULL);

strcpy(host_buf,get_fsp_host());
strcpy(port_buf,get_fsp_port());
strcpy(dir_buf,get_fsp_dir());

transfer_done = 0;
left_footer(transferframe.frame,"Determining transfer size ...");
left_footer(baseframe.frame,"Determining transfer size ...");
total_transfer_size = batch_transfer_size();
left_footer(transferframe.frame,"Transfer size %s",
		unit_file_size(total_transfer_size));
batch_transfer_proc();
}

/********************************************************************************/

void batch_transfer_proc()

/* this fn does the actual transfer set-up, it is called at the start of the	*/
/* batch transfer and each time the host/port values change and a new client	*/
/* most be forked with these new values.					*/

{ char      *argv[] = { fsp_get_cmd, "-r", NULL };
  BatchItem *ptr;
  int	     pid, i;


if ((i = (int)xv_get(batchframe.batch_list, PANEL_LIST_FIRST_SELECTED)) == -1)
    return;

ptr = (BatchItem*) xv_get(batchframe.batch_list, PANEL_LIST_CLIENT_DATA, i);

set_fsp_host(ptr->hostname);
set_fsp_port(ptr->hostport);

strcpy(lhostname,ptr->hostname);
strcpy(lportname,ptr->hostport);

left_footer(baseframe.frame,"Getting %s ...",ptr->filename);
set_transferframe(ptr->filename,ptr->filesize,total_transfer_size,DOWNLOAD);
set_transferdone(0,transfer_done);
current_file_size = ptr->filesize;

exec_mode = FALSE;
pid = do_batch_spawn(argv,(void*)read_batch_stream);
xv_set(baseframe.dir_list, PANEL_CLIENT_DATA, pid, NULL);
strcpy(buf,ptr->filename);
strcat(buf,"\n");
write(pipe_io[0][1],buf,strlen(buf));
}

/********************************************************************************/

Notify_value read_batch_stream ( Notify_client client, int fd )

{ static char buffer[256];
  int	      i, transferred;
  BatchItem   *ptr;

switch (read_client_data(fd,&transferred,buffer))
    {
    case CLIENT_STATUS:
	set_transferdone(transferred,transfer_done+transferred);
	set_icon(DOWNLOAD,transfer_done+transferred,total_transfer_size);
	break;

    case CLIENT_COMPLETE:
	xv_set(transferframe.frame, FRAME_RIGHT_FOOTER, buffer, NULL);
	transfer_done += current_file_size;
	update_local_dir_list();
	
	i = get_next_selected(batchframe.batch_list);
	delete_batch_item(i);

	if ((i=(int)xv_get(batchframe.batch_list,PANEL_LIST_FIRST_SELECTED))>-1) {
	    ptr = (BatchItem*) xv_get(batchframe.batch_list,
					PANEL_LIST_CLIENT_DATA,i);

	    if ((strcmp(lhostname,ptr->hostname) != 0) ||
		(strcmp(lportname,ptr->hostport) != 0)) {
		notify_set_input_func(client,NOTIFY_FUNC_NULL,pipe_io[1][0]);
		close(pipe_io[0][1]);
		batch_transfer_proc();
		return(NOTIFY_DONE);
		}
	    else {
		strcpy(buffer,ptr->filename);
		set_transferframe(buffer,ptr->filesize,
					total_transfer_size,DOWNLOAD);
		set_transferdone(0,transfer_done);
		current_file_size = ptr->filesize;
		left_footer(baseframe.frame,"Getting %s ...",buffer);
		strcat(buffer,"\n");
		write(pipe_io[0][1],buffer,strlen(buffer));
		return(NOTIFY_DONE);
		}
	    }

	notify_set_input_func(client,NOTIFY_FUNC_NULL,pipe_io[1][0]);
	close(pipe_io[0][1]);
	left_footer(batchframe.frame,"Batch transfer completed.");
	left_footer(baseframe.frame,"Transfer completed.");
	set_transferframe("Not Transferring",0,0,FALSE);
	set_transferdone(0,0);
	set_icon(FALSE,0,100);
	post_batch_tidy();
	break;

    case CLIENT_ERROR:
	notify_set_input_func(client,NOTIFY_FUNC_NULL,pipe_io[1][0]);
	error_report(ERROR,"Error when getting file\n %s",buffer);
	done_proc(buffer);
	post_batch_tidy();
	break;

    case CLIENT_NONE:
    case CLIENT_WARNING:
	break;

    default:
	error_report(WARNING,
			"FSPtool: unidentified return value from FSP client\n");
    }

return(NOTIFY_DONE);
}

/********************************************************************************/

void post_batch_tidy()

/* this fn does the tidying up after a batch transfer. Restoring original host	*/
/* port values etc.								*/

{
set_frame_busy(FALSE);
xv_set(batchframe.frame, FRAME_BUSY, FALSE, NULL);
set_fsp_host(host_buf);
set_fsp_port(port_buf);
set_fsp_dir(dir_buf);
}

/********************************************************************************/

void delete_batch_item ( int item_no )

/* delete an item from the batch item list. Freeing any associated data		*/

{ BatchItem *ptr;

ptr = (BatchItem*) xv_get(batchframe.batch_list, PANEL_LIST_CLIENT_DATA, item_no);
free(ptr->hostname);
free(ptr->hostport);
free(ptr->filename);
free(ptr);
xv_set(batchframe.batch_list, PANEL_LIST_DELETE, item_no, NULL);
}

/********************************************************************************/

long int batch_transfer_size()

/* determine total file size (bytes) of all selected items in batch transfer	*/
/* list.									*/

{ long int   total = 0;
  int	     i, rows;
  BatchItem *ptr;


rows = (int) xv_get(batchframe.batch_list, PANEL_LIST_NROWS);

for ( i = 0; i < rows; i++ )
    if ((int) xv_get(batchframe.batch_list, PANEL_LIST_SELECTED, i)) {
	ptr = (BatchItem*) xv_get(batchframe.batch_list,PANEL_LIST_CLIENT_DATA,i);

	if (ptr->filetype == DIRECTORY) {
	    int	     status;
	    long int dircount = 0;

	    set_fsp_host(ptr->hostname);
	    set_fsp_port(ptr->hostport);

	    status = cache_dir_size(ptr->filename,&dircount);

	    if (status != CACHE_OK) {
		error_report(ERROR,"Error determining transfer size");
		return(total);
		}

	    ptr->filesize = dircount;
	    total += dircount;
	    }
	else
	    total += ptr->filesize;
	}

return(total);
}

/********************************************************************************/

void batch_write_text()

{
if (((int) xv_get(batchframe.batch_list, PANEL_LIST_NROWS)) == 0)
    return;

xv_set(filerframe.frame, FRAME_LABEL, "FSPtool:Save", NULL);
xv_set(filerframe.dirname, PANEL_VALUE, get_cwd(), NULL);

xv_set(filerframe.apply_button,
		PANEL_LABEL_STRING,	"Save",
		PANEL_NOTIFY_PROC,	write_text_callback,
		NULL);

xv_set(filerframe.frame, XV_SHOW, TRUE, NULL);
}

/********************************************************************************/

void write_text_callback ( Panel_item item, Event *event )

/* this fn handles the callback from the batch_write_text dialog box to save	*/
/* the batch list as plain text in the specified dir filename path		*/

{ FILE *outfile;
   int  i, rows = (int) xv_get(batchframe.batch_list, PANEL_LIST_NROWS);


strcpy(buf, (char*) xv_get(filerframe.dirname, PANEL_VALUE));

if (strlen(buf) > 0)
    strcat(buf, "/");

strcat(buf, (char*) xv_get(filerframe.filename, PANEL_VALUE));

if ((outfile = fopen(buf,"w")) == NULL) {
    error_report(ERROR,"Couldn't write file %s\n",buf);
    xv_set(filerframe.frame, XV_SHOW, FALSE, NULL);
    }

for ( i = 0; i < rows; i++ )
    fprintf(outfile,"%s\n",
	(char*) xv_get(batchframe.batch_list, PANEL_LIST_STRING, i));

fclose(outfile);
xv_set(filerframe.frame, XV_SHOW, FALSE, NULL);
left_footer(batchframe.frame, "Written batch list as textfile.");
}

/********************************************************************************/

