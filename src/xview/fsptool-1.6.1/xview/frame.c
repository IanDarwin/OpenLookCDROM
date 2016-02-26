/********************************************************************************/
/* frame.c --									*/
/*										*/
/* (c)1993 A.J.Doherty								*/
/********************************************************************************/

#include "fsptool.h"
#include "frame.h"
#include "icon.h"
#include "menu.h"
#include "file.h"
#include "resource.h"
#include "batch.h"

#include "../lib/cache.h"
#include "../lib/unix.h"
#include "../lib/fsp.h"

#include <stdarg.h>

/********************************************************************************/

extern char 		*buf, *fsp_ver_cmd, *fsp_ls_cmd, *fsp_get_cmd,
			*fsp_put_cmd;

extern Server_image	fsptool_glyph, drop_busy_glyph, drop_idle_glyph;

extern int		hosts_loaded;

extern HostInfo		*hosts_list[MAX_HOSTS];

/********************************************************************************/

extern Tool_Properties		tool_properties;

extern Base_frame		baseframe;
extern Transfer_frame		transferframe;
extern Local_frame		localframe;
extern Filter_frame		filterframe;
extern Sethost_frame		sethostframe;
extern Hostlist_frame		hostlistframe;
extern Action_frame		actionframe;
extern Tool_frame		toolframe;
extern Dirlist_frame		dirlistframe;
extern Fsp_frame		fspframe;
extern Clients_frame		clientsframe;
extern Generic_frame		aboutfsptoolframe, aboutfspframe;
extern Batch_frame		batchframe;

extern Xv_font			fixed_font, bold_fixed_font;

/********************************************************************************/

void handle_frame ( Panel_item item, Event *event )

{ Frame frame = (Frame) xv_get(xv_get(item, PANEL_PARENT_PANEL), XV_OWNER);

if (tool_properties.openlook)
    if ((xv_get(frame, FRAME_CMD_PIN_STATE) == FRAME_CMD_PIN_IN) &&
		(tool_properties.cancelclose == FALSE))
	return;

if (tool_properties.cancelclose) {
    unmapframe(frame);
    return;
    }

xv_set(frame, XV_SHOW, FALSE, NULL);
}

/********************************************************************************/

void unmapframe ( Frame frame )

/* this fn unmaps a frame from the screen regardless of whether the pushpin is	*/
/* in or not.									*/

{
if (tool_properties.openlook)
    xv_set(frame, FRAME_CMD_PIN_STATE, FRAME_CMD_PIN_OUT, NULL);

xv_set(frame, XV_SHOW, FALSE, NULL);
}

/********************************************************************************/

void set_dirlist_size ( Panel panel, Panel_item list )

/* this fn resets the size of the dir_list panel to an appropriate size for the	*/
/* current window size. Width varies from min 5 chars upto the maximum stored	*/
/* length for a list string. Height varies from min 1 line to the limit of the	*/
/* current window length. Also moves the drag_drop item to far right of panel	*/

{ int	height  = (int) xv_get(panel, XV_HEIGHT),
	width   = (int) xv_get(panel, XV_WIDTH)-30,
	rowsize = (int) xv_get(list, PANEL_LIST_ROW_HEIGHT),
	rows    = (height-((int) xv_get(list, PANEL_ITEM_Y))-25)/rowsize;

if ( panel == batchframe.panel )
    rows = (rows <= 1 ? 1 : rows-1);

xv_set(list, 
	PANEL_LIST_DISPLAY_ROWS,	rows <= 0 ? 1 : rows,
	PANEL_LIST_WIDTH,		width < 30 ? 30 : width, 
	PANEL_PAINT,			PANEL_NONE,
	NULL);

panel_paint(list, PANEL_CLEAR);

if ( panel == baseframe.panel ) {
    width = xv_get(panel, XV_WIDTH)-30;
    xv_set(baseframe.drop_target, XV_X, width < 390 ? 390:width, NULL);
    }
}

/********************************************************************************/

void handle_baseframe ( Xv_Window window, Event *event, Notify_arg arg )

/* this fn handles the initial setup of saved layout information and the resize	*/
/* event which calls for the dir_list panel item to be re-sized to fit the new	*/
/* window. Initial set up is needed as we cannot do some things until the base	*/
/* frame window has been realised by Xview and the server			*/

{ static int set_frame_sizes = FALSE;

switch (event_action(event))
    {
    case WIN_RESIZE:			/* -- resizing panel frames */
	set_dirlist_size(baseframe.panel, baseframe.dir_list);
	break;

    case ACTION_OPEN:			/* -- initialise, frames & locations */
	if (!set_frame_sizes) {
	    set_layout();
	    set_frame_sizes = TRUE;
	    }
	break;
    }
}

/********************************************************************************/

void handle_batch_panel_resize ( Xv_Window window, Event *event, Notify_arg arg )

{
switch (event_action(event))
    {
    case WIN_RESIZE:			/* -- resizing panel frames */
	set_dirlist_size(batchframe.panel, batchframe.batch_list);
	break;
    }
}

/********************************************************************************/

int drop_notify_proc ( Panel_item item, unsigned int value, Event *event )

{ Selection_requestor sel_req = xv_get(item, PANEL_DROP_SEL_REQ);
  char	       *string;
  int		length, format;
  CacheData    *fdata = c_malloc(sizeof(CacheData));


xv_set(sel_req, SEL_TYPE_NAME, "FILE_NAME", NULL);

string = (char*) xv_get(sel_req, SEL_DATA, &length, &format);

switch (event_action(event))
    {
    case ACTION_DRAG_COPY:
    case ACTION_DRAG_MOVE:
	fdata->name = string;
	fdata->size = 0;
	return_filetype(string,fdata);
	fput_files(fdata);
	free(fdata);
	break;

    default:
	break;
    }

return(XV_ERROR);	/* -- otherwise filemanager will remove file */
}

/********************************************************************************/

void handle_local_panel_resize ( Xv_Window window, Event *event, Notify_arg arg )

{
switch (event_action(event))
    {
    case WIN_RESIZE:
	set_dirlist_size(localframe.panel, localframe.dir_list);
	break;

    default:
	break;
    }
}

/********************************************************************************/

void realize_aboutfsp()

{
xv_destroy_safe(aboutfspframe.frame);
make_aboutfspframe(TRUE);
xv_set(aboutfspframe.frame, XV_SHOW, TRUE, NULL);
}

/********************************************************************************/

void footer_message ( char *fmtstr, ... )

/* this fn is used by external routines to allow them to display message thru'	*/
/* the toolkit in use. Has exact functionality of left_footer.			*/

{ static char tmpbuf[512];
      va_list args;

va_start(args,fmtstr);
(void) vsprintf(tmpbuf, fmtstr, args);
xv_set(baseframe.frame, FRAME_LEFT_FOOTER, tmpbuf, NULL);
va_end(args);
}

/********************************************************************************/

void left_footer ( Frame frame, char *fmtstr, ... )

/* this function sets the left footer part of the baseframe frame. Takes a	*/
/* var-arg list which is formatted printf style to produce display string.	*/

{ static char tmpbuf[512];
      va_list args;

va_start(args,fmtstr);
(void) vsprintf(tmpbuf, fmtstr, args);
xv_set(frame, FRAME_LEFT_FOOTER, tmpbuf, NULL);
va_end(args);
}

/********************************************************************************/

void right_footer ( Frame frame, int items, int selected )

/* this function sets the right footer with items found and items selected */

{ static char tmpbuf[64];

sprintf(tmpbuf,"%d items, %d selected",items,selected);
xv_set(frame, FRAME_RIGHT_FOOTER, tmpbuf, NULL);
}

/********************************************************************************/

Panel_setting handle_port_entry ( Panel_item item, Event *event )

{
switch (event_action(event))
    {
    case '\n' :
    case '\r' :
	set_host(item,0,event);
	xv_set(sethostframe.frame, XV_SHOW, FALSE, NULL);
	return(PANEL_NONE);

    case '\t' :
	return(PANEL_PREVIOUS);

    case '\033':
    case '\040':
	return(PANEL_NONE);

    default:
	return(panel_text_notify(item,event));
    }
}

/********************************************************************************/

Panel_setting handle_host_entry ( Panel_item item, Event *event )

{
switch (event_action(event))
    {
    case '\n' :
    case '\r' :
    case '\t' :
	return(PANEL_NEXT);

    case '\033':
    case '\040':
	return(PANEL_NONE);

    default:
	return(panel_text_notify(item,event));
    }
}

/********************************************************************************/

void set_tool_properties ( Panel_item item, int value, Event *event )

{
tool_properties.openlook    = (int) xv_get(toolframe.openlook, PANEL_VALUE);
tool_properties.cancelclose = (int) xv_get(toolframe.cancelclose, PANEL_VALUE);
tool_properties.menuclose   = (int) xv_get(toolframe.menuclose, PANEL_VALUE);
tool_properties.hostread    = (int) xv_get(toolframe.hostread, PANEL_VALUE);

if (!tool_properties.openlook) {
    XV_SET_TRANSIENT(transferframe.frame);
    XV_SET_TRANSIENT(filterframe.frame);
    XV_SET_TRANSIENT(sethostframe.frame);
    XV_SET_TRANSIENT(actionframe.frame);
    XV_SET_TRANSIENT(aboutfsptoolframe.frame);
    XV_SET_TRANSIENT(aboutfspframe.frame);
    XV_SET_TRANSIENT(localframe.frame);
    XV_SET_TRANSIENT(hostlistframe.frame);
    XV_SET_TRANSIENT(toolframe.frame);
    XV_SET_TRANSIENT(dirlistframe.frame);
    XV_SET_TRANSIENT(fspframe.frame);
    XV_SET_TRANSIENT(clientsframe.frame);
    }

if (item_name(item,"Save")) {
    set_bool_resource("Fsptool.Tool.Openlook-wm",tool_properties.openlook);
    set_bool_resource("Fsptool.Tool.Cancelclose",tool_properties.cancelclose);
    set_bool_resource("Fsptool.Tool.Menuclose",tool_properties.menuclose);
    set_bool_resource("Fsptool.Tool.Hostread",tool_properties.hostread);
    save_resources();
    }

xv_set(dirlistframe.frame, XV_SHOW, FALSE, NULL);
}

/********************************************************************************/

void set_dirlist_properties ( Panel_item item, int value, Event *event )

{ SortFormat name   = Alpha;
	 int normal = TRUE, cachesize, cachetimeout;
        char sbuf[5];

cachesize =    (int) xv_get(dirlistframe.cachesize, PANEL_VALUE);
cachetimeout = (int) xv_get(dirlistframe.cachetimeout, PANEL_VALUE);

set_cache_size(cachesize);
set_cache_timeout(cachetimeout);

switch ((int) xv_get(dirlistframe.sorttype, PANEL_VALUE)) {
    case 0 : name = Alpha;
	     normal = (int)xv_get(dirlistframe.alphasort, PANEL_VALUE) == 0;
	     strcpy(sbuf,"Name");
	     if (normal) set_fsp_sorttype(Alpha); else set_fsp_sorttype(RevAlpha);
	     break;

    case 1 : name   = Date;
	     normal = (int)xv_get(dirlistframe.timesort, PANEL_VALUE) == 0;
	     strcpy(sbuf,"Date");
	     if (normal) set_fsp_sorttype(Date); else set_fsp_sorttype(RevDate);
	     break;

    case 2 : name   = Size;
	     normal = (int)xv_get(dirlistframe.sizesort, PANEL_VALUE) == 0;
	     strcpy(sbuf,"Size");
	     if (normal) set_fsp_sorttype(Size); else set_fsp_sorttype(RevSize);
	     break;
    }

if (item_name(item,"Save")) {
    set_string_resource("Fsptool.Remotesort.Order",sbuf);
    set_bool_resource("Fsptool.Remotesort.Reversed",!normal);
    set_int_resource("Fsptool.Cache.Size",cachesize);
    set_int_resource("Fsptool.Cache.Timeout",cachetimeout);
    save_resources();
    }

update_dir_list();
xv_set(dirlistframe.frame, XV_SHOW, FALSE, NULL);
}

/********************************************************************************/

void set_order_type ( Panel_item item, int value, Event *event )

{
switch ((int) xv_get(dirlistframe.sorttype, PANEL_VALUE)) {
    case 1 :	xv_set(dirlistframe.alphasort, PANEL_INACTIVE, TRUE, NULL);
		xv_set(dirlistframe.timesort, PANEL_INACTIVE, FALSE, NULL);
		xv_set(dirlistframe.sizesort, PANEL_INACTIVE, TRUE, NULL);
		break;

    case 2 :	xv_set(dirlistframe.alphasort, PANEL_INACTIVE, TRUE, NULL);
		xv_set(dirlistframe.timesort, PANEL_INACTIVE, TRUE, NULL);
		xv_set(dirlistframe.sizesort, PANEL_INACTIVE, FALSE, NULL);
		break;

    default:	xv_set(dirlistframe.alphasort, PANEL_INACTIVE, FALSE, NULL);
		xv_set(dirlistframe.timesort, PANEL_INACTIVE, TRUE, NULL);
		xv_set(dirlistframe.sizesort, PANEL_INACTIVE, TRUE, NULL);
    }
}

/********************************************************************************/

void set_host_clear ( Panel_item item, int value, Event *event )

{
xv_set(sethostframe.host, PANEL_VALUE, "", NULL);
xv_set(sethostframe.port, PANEL_VALUE, "", NULL);
xv_set(sethostframe.panel, PANEL_CLIENT_DATA, -1, NULL);
xv_set(sethostframe.host, PANEL_CLIENT_DATA, "", NULL);
xv_set(sethostframe.port, PANEL_CLIENT_DATA, "", NULL);
}

/********************************************************************************/

void set_fsp_properties ( Panel_item item, int value, Event *event )

{ int lport_val = (int) xv_get(fspframe.localport, PANEL_VALUE),
      time_val  = (int) xv_get(fspframe.timeout, PANEL_VALUE),
      buf_val   = (int) xv_get(fspframe.bufsize, PANEL_VALUE),
      sz, delay;


switch (buf_val)
    {
    case 0:
	sz = 128;
	break;

    case 1:
	sz = 256;
	break;

    case 2:
	sz = 512;
	break;

    case 3:
	sz = 1024;
	break;

    default:
	sz = 512;
    }

switch((int) xv_get(fspframe.delaysize, PANEL_VALUE))
    {
    case 0:
	delay = 500;
	break;

    case 1:
	delay = 1000;
	break;

    case 2:
	delay = 2000;
	break;

    case 3:
	delay = 3000;
	break;

    case 4:
	delay = 5000;
	break;

    default:
	delay = 3000;
    }

if (item_name(item,"Save")) {
    set_int_resource("Fsptool.Fsp.Localport",lport_val);
    set_int_resource("Fsptool.Fsp.Timeout",time_val);
    set_int_resource("Fsptool.Fsp.Buffer",sz);
    set_int_resource("Fsptool.Fsp.Delay",delay);
    save_resources();
    }

set_fsp_local_port(lport_val);
set_fsp_timeout(time_val);
set_fsp_bufsize(sz);
set_fsp_delay(delay);
xv_set(fspframe.frame, XV_SHOW, FALSE, NULL);
}

/********************************************************************************/

void set_fsp_clients ( Panel_item item, int value, Event *event )

{
fsp_ver_cmd = c_strdup((char*) xv_get(clientsframe.fver, PANEL_VALUE));
fsp_ls_cmd  = c_strdup((char*) xv_get(clientsframe.fls, PANEL_VALUE));
fsp_get_cmd = c_strdup((char*) xv_get(clientsframe.fget, PANEL_VALUE));
fsp_put_cmd = c_strdup((char*) xv_get(clientsframe.fput, PANEL_VALUE));

if (item_name(item,"Save")) {
    set_string_resource("Fsptool.Client.Fver",fsp_ver_cmd);
    set_string_resource("Fsptool.Client.Fls",fsp_ls_cmd);
    set_string_resource("Fsptool.Client.Fget",fsp_get_cmd);
    set_string_resource("Fsptool.Client.Fput",fsp_put_cmd);
    save_resources();
    }

xv_set(clientsframe.frame, XV_SHOW, FALSE, NULL);
}

/********************************************************************************/

void set_transferframe ( const char *filename,int fsize,int transize,int active )

/* this function sets the status and size fields of the transfer frame pop-up	*/

{ static char temp[BUFSIZ];

switch (active)
    {
    case DOWNLOAD:
	strcpy(temp,"Receiving ");
	strcat(temp,filename);
	break;

    case UPLOAD:
	strcpy(temp,"Sending ");
	strcat(temp,filename);
	break;

    default:
	strcpy(temp,filename);
	fsize = 100;
	transize = 100;
    }

xv_set(transferframe.status, PANEL_VALUE, temp, NULL);
sprintf(temp,"%1d Bytes",fsize);
xv_set(transferframe.size, PANEL_VALUE, temp, NULL);

xv_set(transferframe.gauge, PANEL_MAX_VALUE, fsize, NULL);
xv_set(transferframe.gauge, PANEL_VALUE, 0, NULL);

xv_set(transferframe.full_gauge, PANEL_MAX_VALUE, transize, NULL);
xv_set(transferframe.full_gauge, PANEL_VALUE, 0, NULL);
}

/********************************************************************************/

void set_transferdone ( int transdone, int alldone )

/* this function updates the gauge in the transfer status pop-up to reflect the	*/
/* precentage of the file transferred so far.					*/

{
xv_set(transferframe.gauge, PANEL_VALUE, transdone, NULL);
xv_set(transferframe.full_gauge, PANEL_VALUE, alldone, NULL);
}

/********************************************************************************/

void set_frame_busy ( int busy_count )

/* when busy_flag set the busy attributes of all frames and buttons that are	*/
/* inactive during [up/down]load when not, then unset them.			*/

{
xv_set(baseframe.file_button, PANEL_INACTIVE, busy_count, NULL);
xv_set(baseframe.view_button, PANEL_INACTIVE, busy_count, NULL);
xv_set(baseframe.properties_button, PANEL_INACTIVE, busy_count, NULL);
xv_set(baseframe.sethost_button, PANEL_INACTIVE, busy_count, NULL);
xv_set(baseframe.abort_button, PANEL_INACTIVE, !busy_count, NULL);
xv_set(baseframe.drop_target, PANEL_INACTIVE, busy_count, NULL);
xv_set(baseframe.cache_button, PANEL_INACTIVE, busy_count, NULL);
xv_set(baseframe.dir_text, PANEL_INACTIVE, busy_count, NULL);
xv_set(baseframe.dir_list, PANEL_INACTIVE, busy_count, NULL);

xv_set(localframe.file_button, PANEL_INACTIVE, busy_count, NULL);
xv_set(localframe.localdir, PANEL_INACTIVE, busy_count, NULL);
xv_set(localframe.dir_list, PANEL_INACTIVE, busy_count, NULL);
}

/********************************************************************************/

void select_dir_files ( Panel_item dir_list )

/* select all files in specified directory listing				*/

{ int rows = (int) xv_get(dir_list, PANEL_LIST_NROWS), loop;

for ( loop = 1; loop <= rows; loop++ )
    xv_set(dir_list, PANEL_LIST_SELECT, loop, TRUE, NULL);
}

/********************************************************************************/

void unselect_dir_files ( Panel_item dir_list )

/* unselect all files in specified directory listing				*/

{ int row;

while ((row = (int) xv_get(dir_list, PANEL_LIST_FIRST_SELECTED)) != -1)
    xv_set(dir_list, PANEL_LIST_SELECT, row, FALSE, NULL);
}

/********************************************************************************/

void hosts_select_proc ( Panel_item item, char *string, caddr_t client_data,
			 Panel_list_op op, Event *event )

/* this fn is called when an item is acted upon in the hosts panel list		*/

{ HostInfo *host = (HostInfo*) client_data;

if (op == PANEL_LIST_OP_SELECT) {
    xv_set(hostlistframe.alias, PANEL_VALUE, host->alias, NULL);
    xv_set(hostlistframe.host, PANEL_VALUE, host->hostname, NULL);
    sprintf(buf,"%1d",host->hostport);
    xv_set(hostlistframe.port, PANEL_VALUE, buf, NULL);
    xv_set(hostlistframe.description, PANEL_VALUE, host->description, NULL);
    xv_set(hostlistframe.rdir, PANEL_VALUE, host->directory, NULL);    
    }
}

/********************************************************************************/

void hostlist_edit_proc ( Panel_item item, int value, Event *event )

/* this fn changes the information for an alias if the alias field matches one	*/
/* which is already present, and creates a new entry if it does not		*/

{ int	    i = 0, matchval = 0, match = FALSE,
	    rsel = (int)xv_get(hostlistframe.host_list,PANEL_LIST_FIRST_SELECTED);
  HostInfo *host;
  char     *ptr  = (char*) xv_get(hostlistframe.alias, PANEL_VALUE);


host = (HostInfo*)xv_get(hostlistframe.host_list,PANEL_LIST_CLIENT_DATA,rsel,NULL);

while ((i < hosts_loaded) && !match)
    if (strcmp(ptr,hosts_list[i++]->alias) == 0) {
	match = TRUE;
	matchval = i-1;
	}

if (match) {
    host = hosts_list[matchval];
    free(host->hostname);
    free(host->directory);
    free(host->description);
    left_footer(hostlistframe.frame,
			"Edited details of alias \"%s\"",host->alias);
    }
else
    if (hosts_loaded < MAX_HOSTS) {
	hosts_list[hosts_loaded] = c_malloc(sizeof(HostInfo));
	host = hosts_list[hosts_loaded];
	host->alias = c_strdup(ptr);

	xv_set(hostlistframe.host_list,
		PANEL_LIST_INSERT,	hosts_loaded,
		PANEL_LIST_STRING,	hosts_loaded,	host->alias,
		PANEL_LIST_CLIENT_DATA,	hosts_loaded,	host,
		NULL);

	xv_set(hostlistframe.host_list, PANEL_LIST_SELECT,
		hosts_loaded, TRUE, NULL);

	hosts_loaded++;

	xv_set(sethostframe.hostlist_button,
		PANEL_ITEM_MENU, make_hosts_menu(hosts_menu_proc),
				/* - if new then update hosts menu */
		NULL);

	left_footer(hostlistframe.frame,"Added new alias \"%s\"",host->alias);
	}
    else {
	left_footer(hostlistframe.frame,
			"Couldn't add new alias, alias table full.");
	return;
	}

host->hostname   = c_strdup((char*)xv_get(hostlistframe.host, PANEL_VALUE));
host->description= c_strdup((char*)xv_get(hostlistframe.description,PANEL_VALUE));
host->directory  = c_strdup((char*)xv_get(hostlistframe.rdir, PANEL_VALUE));
host->hostport	 = atoi((char*)xv_get(hostlistframe.port, PANEL_VALUE));

xv_set(hostlistframe.save_button, PANEL_LABEL_STRING, "<Save>", NULL);
}

/********************************************************************************/

void hostlist_delete_proc ( Panel_item item, int value, Event *event )

{ int loop, rsel = (int)xv_get(hostlistframe.host_list,PANEL_LIST_FIRST_SELECTED);
  HostInfo *host;

if (rsel == -1)
    return;

host=(HostInfo*)xv_get(hostlistframe.host_list,PANEL_LIST_CLIENT_DATA,rsel,NULL);

for ( loop = rsel; loop < hosts_loaded-1; loop++ )
    hosts_list[loop] = hosts_list[loop+1];

hosts_loaded--;

xv_set(hostlistframe.host_list, PANEL_LIST_DELETE, rsel, NULL);
xv_set(hostlistframe.save_button, PANEL_LABEL_STRING, "<Save>", NULL);
left_footer(hostlistframe.frame, "Deleted alias \"%s\"",host->alias);

free(host->alias);
free(host->hostname);
free(host->directory);
free(host->description);
free(host);

xv_set(sethostframe.hostlist_button,
	PANEL_ITEM_MENU, make_hosts_menu(hosts_menu_proc), /* - update menu */
	NULL);

if (!hosts_loaded)
    return;

xv_set(hostlistframe.host_list, PANEL_LIST_SELECT, 0, TRUE, NULL);
xv_set(hostlistframe.host, PANEL_VALUE, hosts_list[0]->hostname, NULL);
sprintf(buf,"%1d",hosts_list[0]->hostport);
xv_set(hostlistframe.port, PANEL_VALUE, buf, NULL);
xv_set(hostlistframe.description, PANEL_VALUE, hosts_list[0]->description, NULL);
xv_set(hostlistframe.ldir, PANEL_VALUE, "", NULL);
xv_set(hostlistframe.rdir, PANEL_VALUE, hosts_list[0]->directory, NULL);
}

/********************************************************************************/

void hostlist_clear_proc ( Panel_item item, int value, Event *event )

{
xv_set(hostlistframe.host, PANEL_VALUE, "", NULL);
xv_set(hostlistframe.port, PANEL_VALUE, "", NULL);
xv_set(hostlistframe.description, PANEL_VALUE, "", NULL);
xv_set(hostlistframe.ldir, PANEL_VALUE, "", NULL);
xv_set(hostlistframe.rdir, PANEL_VALUE, "/", NULL);
left_footer(hostlistframe.frame, "Cleared current alias info.");
}

/********************************************************************************/

void hostlist_load_proc ( Panel_item item, int value, Event *event )

{
if (load_hosts()) {
    return_hosts_list(hostlistframe.host_list);
    xv_set(hostlistframe.alias, PANEL_VALUE, hosts_list[0]->alias, NULL);
    xv_set(hostlistframe.host, PANEL_VALUE, hosts_list[0]->hostname, NULL);
    sprintf(buf,"%1d",hosts_list[0]->hostport);
    xv_set(hostlistframe.port, PANEL_VALUE, buf, NULL);
    xv_set(hostlistframe.description,PANEL_VALUE,hosts_list[0]->description,NULL);
    xv_set(hostlistframe.ldir, PANEL_VALUE, "", NULL);
    xv_set(hostlistframe.rdir, PANEL_VALUE, hosts_list[0]->directory, NULL);
    xv_set(hostlistframe.save_button, PANEL_LABEL_STRING, "Save", NULL);
    left_footer(hostlistframe.frame, "Loaded ~/.fsphosts to hosts list.");

    xv_set(sethostframe.hostlist_button,
	    PANEL_ITEM_MENU, make_hosts_menu(hosts_menu_proc),
		/* - if new then update hosts menu */
	    NULL);
    }
else
    left_footer(hostlistframe.frame, "Unable to load ~/.fsphosts");
}

/********************************************************************************/

void hostlist_save_proc ( Panel_item item, int value, Event *event )

{
if (save_hosts()) {
    xv_set(hostlistframe.save_button, PANEL_LABEL_STRING, "Save", NULL);
    left_footer(hostlistframe.frame, "Saved hosts list to ~/.fsphosts");
    }
else
    left_footer(hostlistframe.frame, "Unable to write file ~/.fsphosts");
}

/********************************************************************************/

void sethost_proc ( Panel_item item, int value, Event *event )

{
xv_set(sethostframe.frame, XV_SHOW, TRUE, NULL);
}

/********************************************************************************/

void do_transfer_callback ( Panel_item item, int value, Event *event )

{
do_batch_transfer();
}

/********************************************************************************/

void set_host ( Panel_item item, int value, Event *event )

{ int   host_id;

  char *host_name = (char *) xv_get(sethostframe.host, PANEL_VALUE),
       *port_name = (char *) xv_get(sethostframe.port, PANEL_VALUE);


if ((strlen(host_name) == 0) || (strlen(port_name) == 0))
     return;

set_fsp_host(host_name);
set_fsp_port(port_name);

left_footer(baseframe.frame,
		(char*)xv_get(sethostframe.host,PANEL_VALUE),
	    	" - ",
	    	(char*)xv_get(sethostframe.port,PANEL_VALUE));

xv_set(baseframe.dir_list, PANEL_INACTIVE, FALSE, NULL);
(void) clear_cache();

if ((strcmp(host_name,(char*)xv_get(sethostframe.host, PANEL_CLIENT_DATA))==0) &&
   (strcmp(port_name,(char*)xv_get(sethostframe.port, PANEL_CLIENT_DATA))==0)) {
    host_id = (int) xv_get(sethostframe.panel, PANEL_CLIENT_DATA);
    xv_set(baseframe.dir_text,PANEL_VALUE,hosts_list[host_id]->directory,NULL);
    set_fsp_dir(hosts_list[host_id]->directory);
    }
else {
    xv_set(baseframe.dir_text, PANEL_VALUE, "/", NULL);
    set_fsp_dir("/");
    }

update_dir_list();
xv_set(baseframe.cache_button, PANEL_ITEM_MENU, make_cache_menu(), NULL);
}

/********************************************************************************/
