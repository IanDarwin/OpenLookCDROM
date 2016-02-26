/********************************************************************************/
/* xview/resource.c --								*/
/*										*/
/* This module handles both X11 and local resource management. 			*/
/*										*/
/* (c)1993 A.J.Doherty								*/
/********************************************************************************/

#include <X11/Xlib.h>
#include <X11/Xresource.h>

#include "../xview/fsptool.h"
#include "../xview/frame.h"

#include "../lib/cache.h"
#include "../lib/unix.h"
#include "../lib/fsp.h"
#include "../config.h"

#include "fsptool.h"
#include "resource.h"
#include "file.h"

/********************************************************************************/

Tool_Properties			tool_properties;

/********************************************************************************/

extern Base_frame		baseframe;
extern Transfer_frame		transferframe;
extern Filter_frame		filterframe;
extern Sethost_frame		sethostframe;
extern Action_frame		actionframe;
extern Generic_frame		aboutfsptoolframe, aboutfspframe;
extern Local_frame		localframe;
extern Hostlist_frame		hostlistframe;
extern Tool_frame		toolframe;
extern Dirlist_frame		dirlistframe;
extern Fsp_frame		fspframe;
extern Clients_frame		clientsframe;
extern Batch_frame		batchframe;

extern int file_mask;

extern char *fsp_ls_cmd, *fsp_ver_cmd, *fsp_get_cmd, *fsp_put_cmd;

/********************************************************************************/

HostInfo	       *hosts_list[MAX_HOSTS];

int			hosts_loaded = 0;

static XrmDatabase	fsptoolDB;

/********************************************************************************/

void set_fileaction_defaults(void);
void set_tool_defaults(void);
void set_fsp_defaults(void);
void set_clients_defaults(void);
void set_dirlist_defaults(void);

void get_geometry(Frame,const char*);
void set_geometry(Frame,const char*,int);
void set_cmd_geometry(Frame,const char*);

char *capitalise_resource(const char*);

/********************************************************************************/

void return_hosts_list ( Panel_item host_list )

/* places the currently known host alias strings into the panel list item	*/

{ int loop;

xv_set(host_list, PANEL_LIST_DELETE_ROWS,
			0, (int)xv_get(host_list, PANEL_LIST_NROWS), NULL);

for ( loop = 0; loop < hosts_loaded; loop++ )
    xv_set(host_list,	PANEL_LIST_INSERT,	loop,
			PANEL_LIST_STRING,	loop, hosts_list[loop]->alias,
			PANEL_LIST_CLIENT_DATA, loop, hosts_list[loop],
			NULL);
}

/********************************************************************************/

void get_geometry(Frame frame, const char *resource )

/* get the geometry specifications for the specified frame, and write them	*/
/* to a string in the Fsptool."resource".Geometry / Open resources.		*/

{ Rect rect;
  char buf[256], name[256];


frame_get_rect(frame,&rect);

sprintf(buf,"+%1d+%1dx%1dx%1d",rect.r_left,rect.r_top,rect.r_width,rect.r_height);

strcpy(name,"Fsptool.");
strcat(name,resource);
strcat(name,".Geometry");
XrmPutStringResource(&fsptoolDB,name,buf);

strcpy(name,"Fsptool.");
strcat(name,resource);
strcat(name,".Open");

set_bool_resource(name,(int) xv_get(frame, XV_SHOW));
}

/********************************************************************************/

void get_layout()

/* this fn gets the location and open/closed state of each FSPtool window or	*/
/* command frame, these are updated into the server resource database.		*/

{
get_geometry(baseframe.frame,"Baseframe");
get_geometry(localframe.frame,"Localframe");
get_geometry(transferframe.frame,"Transferframe");
get_geometry(filterframe.frame,"Filterframe");
get_geometry(sethostframe.frame,"Sethostframe");
get_geometry(hostlistframe.frame,"Hostlistframe");
get_geometry(actionframe.frame,"Actionframe");
get_geometry(toolframe.frame,"Toolframe");
get_geometry(dirlistframe.frame,"Dirlistframe");
get_geometry(fspframe.frame,"Fspframe");
get_geometry(clientsframe.frame,"Clientsframe");
get_geometry(aboutfsptoolframe.frame,"Aboutfsptoolframe");
get_geometry(batchframe.frame,"Batchframe");
save_resources();
left_footer(baseframe.frame, "Layout saved to ~/.fsptooldefaults");
}

/********************************************************************************/

void set_cmd_geometry ( Frame frame, const char *nm )

/* this fn sets the geometry for command frames, ie: those which are not re-	*/
/* sizeable, as a result only the x and y parts are set	and mapping state	*/

{ Rect  rect;
  int   x_pos, y_pos;
  char *ptr, buf[128];

frame_get_rect(frame,&rect);

strcpy(buf,"fsptool.");
strcat(buf,nm);
strcat(buf,".geometry");

if ((ptr = string_resource(buf,NULL)) == NULL)
    return;

if (sscanf(ptr,"%*c%d%*c%d%*c%*d%*c%*d",&x_pos, &y_pos) != 2) {
    error_report(WARNING,"FSPtool: error in frame geometry : \"%s\"\n",buf);
    return;
    }

rect.r_left = x_pos;
rect.r_top = y_pos;
frame_set_rect(frame,&rect);

strcpy(buf,"fsptool.");
strcat(buf,nm);
strcat(buf,".open");
xv_set(frame, XV_SHOW, bool_resource(buf,FALSE), NULL);

if (bool_resource(buf,FALSE))
    xv_set(frame, FRAME_CMD_PIN_STATE, FRAME_CMD_PIN_IN, NULL);
}

/********************************************************************************/

void set_geometry ( Frame frame, const char *nm, int mapdeflt )

/* this fn sets the geometry for frames: x,y and width and height are set and	*/
/* mapping state.								*/

{ Rect  rect;
   int  x, y, width, height;
  char *ptr, buf[128];

strcpy(buf,"fsptool.");
strcat(buf,nm);
strcat(buf,".geometry");

if ((ptr = string_resource(buf,NULL)) == NULL)
    return;

if (sscanf(ptr,"%*c%d%*c%d%*c%d%*c%d",&x, &y, &width, &height) != 4) {
    error_report(WARNING,"FSPtool: error in frame geometry : \"%s\"\n",buf);
    return;
    }

rect.r_left = x;
rect.r_top = y;
rect.r_width = width;
rect.r_height = width;

frame_set_rect(frame,&rect);

strcpy(buf,"fsptool.");
strcat(buf,nm);
strcat(buf,".open");
xv_set(frame, XV_SHOW, bool_resource(buf,mapdeflt), NULL);
}

/********************************************************************************/

void set_layout()

/* this fn sets the location and open/closed state of each FSPtool window or	*/
/* command frame, these are updated from the server resource database.		*/

{
set_geometry(baseframe.frame,"baseframe",TRUE);
set_cmd_geometry(transferframe.frame,"transferframe");
set_cmd_geometry(filterframe.frame,"filterframe");
set_cmd_geometry(sethostframe.frame,"sethostframe");
set_cmd_geometry(actionframe.frame,"actionframe");
set_cmd_geometry(fspframe.frame,"fspframe");
set_cmd_geometry(dirlistframe.frame,"dirlistframe");
set_cmd_geometry(aboutfsptoolframe.frame,"aboutfsptoolframe");
set_geometry(localframe.frame,"localframe",FALSE);
set_cmd_geometry(hostlistframe.frame,"hostlistframe");
set_cmd_geometry(toolframe.frame,"toolframe");
set_cmd_geometry(clientsframe.frame,"clientsframe");
set_geometry(batchframe.frame,"batchframe",FALSE);
}

/********************************************************************************/

void save_resources()

/* this fn writes out the Server resource files containg any FSPtool resources	*/
/* that may have been set to the users ~/.fsptooldefaults file in that order	*/

{ char filenm[512], *ptr;


if ((ptr = getenv("HOME")) == NULL) {
    error_report(ERROR,"FSPtool: couldn't obtain $HOME unable to write file\n");
    left_footer(baseframe.frame, "Unable to save, couldn't get $HOME");
    return;
    }

strcpy(filenm,ptr);
strcat(filenm,"/.fsptooldefaults");

XrmPutFileDatabase(fsptoolDB,filenm);

left_footer(baseframe.frame, "Resources saved to ~/.fsptooldefaults");
}

/********************************************************************************/

int load_hosts()

/* this fn loads in the users ~/.fsphosts file, if it does not exist then no	*/
/* hosts are loaded and the program host list will be empty and returns FALSE	*/

{ char *hostname  = c_malloc(64),
       *alias     = c_malloc(32),
       *directory = c_malloc(64),
       *desc      = c_malloc(128),
       *filename  = c_malloc(1024),
       *ptr, ch	  = 0;

  int   port, count = 0;

  FILE *host_file;


hosts_loaded = 0;

if ((ptr = getenv("HOME"))) {
    strcpy(filename,ptr);
    strcat(filename,"/.fsphosts");
    }
else
    strcpy(filename,"/.fsphosts");

if ((host_file = fopen(filename,"r")) == NULL) {
    return(FALSE);
    }

while (!feof(host_file))
    {
    if (fscanf(host_file," %c",&ch) == 1) {
	if ( ch != '#' ) {
	    ungetc(ch,host_file);

	    if ( fscanf(host_file,"%63s%d%31s%63s ",
				hostname,&port,alias,directory) != 4)
		error_report(WARNING,"FSPtool: error in ~/.fsphosts file\n");
	    else {
		fgets(filename,1023,host_file);
		strncpy(desc,filename,127);

		if ((ptr = strchr(desc,'\n')) != NULL)
		    *ptr = 0;

		hosts_list[count] = c_malloc(sizeof(HostInfo));
		hosts_list[count]->hostname    = c_strdup(hostname);
		hosts_list[count]->hostport    = port;
		hosts_list[count]->alias       = c_strdup(alias);
		hosts_list[count]->directory   = c_strdup(directory);
		hosts_list[count]->description = c_strdup(desc);
		count++;
		}
	    }
	else
	    fgets(filename,1023,host_file);
	}
    else
	if (!feof(host_file)) {
	    error_report(WARNING,"FSPtool: error in ~/.fsphosts file\n");
	    fgets(filename,1023,host_file);
	    }
    }

hosts_loaded = count;

fclose(host_file);

free(filename);
free(hostname);
free(alias);
free(directory);
free(desc);

return(hosts_loaded);
}

/********************************************************************************/

int save_hosts()

/* this fn saves the current hosts list to the ~/.fsphosts file			*/

{ FILE *host_file;
  char *ptr, *filename = c_malloc(512);
   int  loop;


if ((ptr = getenv("HOME"))) {
    strcpy(filename,ptr);
    strcat(filename,"/.fsphosts");
    }
else
    strcpy(filename,"/.fsphosts");

if ((host_file = fopen(filename,"w")) == NULL) {
    error_report(ERROR,"FSPtool: unable to write %s file\n",filename);
    free(filename);
    return(FALSE);
    }

for ( loop = 0; loop < hosts_loaded; loop++ ) {
    fprintf(host_file,"%s\t %1d\t %s\t %s\t %s\n",
			hosts_list[loop]->hostname,
			hosts_list[loop]->hostport,
			hosts_list[loop]->alias,
			hosts_list[loop]->directory,
			hosts_list[loop]->description);
    }

fclose(host_file);
free(filename);
return(TRUE);
}

/********************************************************************************/

void set_defaults()

/* this fn checks all fsptool specific database options, and sets program data	*/
/* as appropriate.								*/

{
set_fileaction_defaults();
set_tool_defaults();
set_dirlist_defaults();
set_fsp_defaults();
set_clients_defaults();
}

/********************************************************************************/

void load_resources()

/* this fn loads up all of the fsptool defaults. Load from ~/.fsptooldefaults	*/

{ char filenm[512], *ptr;


if ((ptr = getenv("HOME")) == NULL) {
    error_report(WARNING,"FSPtool: couldn't obtain $HOME unable to load file\n");
    return;
    }

strcpy(filenm,ptr);
strcat(filenm,"/.fsptooldefaults");

fsptoolDB = XrmGetFileDatabase(filenm);
}

/********************************************************************************/

char *string_resource(const char *nme, char *deflt)

/* this fn gets a string resource from the database, if it doesn't exist then	*/
/* the specified default string is used instead. Returned ptrs should not be	*/
/* altered in any way and should not be stored long term only copied.		*/

{    char *str_type[20], *cls = capitalise_resource(nme);
 XrmValue  value;


if (XrmGetResource(fsptoolDB, nme, cls, str_type, &value) != True)
    return(deflt);
else
    return((char*) value.addr);
}

/********************************************************************************/

int int_resource(const char *nme, int deflt)

/* this fn gets a integer resource from the database, if it doesn't exist then	*/
/* the specified default integer is used instead.				*/

{    char *str_type[20], *cls = capitalise_resource(nme);
 XrmValue  value;
      int  val;


if (XrmGetResource(fsptoolDB, nme, cls, str_type, &value) != True)
    return(deflt);

if (sscanf((char*)value.addr,"%d",&val) != 1) {
    error_report(WARNING,"FSPtool: error in resource '%s' - not integer.\n",cls);
    return(deflt);
    }

return(val);
}

/********************************************************************************/

int bool_resource(const char *nme, int deflt)

/* this fn gets a boolean resource from the database, if it doesn't exist then	*/
/* the specified default boolean is used instead.				*/

{    char *str_type[20], *cls = capitalise_resource(nme);
 XrmValue  value;


if (XrmGetResource(fsptoolDB, nme, cls, str_type, &value) != True)
    return(deflt);

if (strncasecmp((char*)value.addr,"False",(int) value.size) == 0)
    return(FALSE);

if (strncasecmp((char*)value.addr,"True",(int) value.size) == 0)
    return(TRUE);
  
error_report(WARNING,"FSPtool: error in resource '%s' - not boolean.\n",cls);
return(deflt);
}

/********************************************************************************/

void set_string_resource ( const char *name, const char *value )

/* this fn saves a resource string to the resource database			*/

{
XrmPutStringResource(&fsptoolDB,name,value);
}

/********************************************************************************/

void set_int_resource ( const char *name, int value )

/* this fn saves a resource integer to the resource database			*/

{ char *buf = c_malloc(64);

sprintf(buf,"%1d",value);
XrmPutStringResource(&fsptoolDB,name,buf);
free(buf);
}

/********************************************************************************/

void set_bool_resource ( const char *name, int value )

/* this fn saves a resource boolean to the resource database			*/

{
XrmPutStringResource(&fsptoolDB, name,(value ? "True" : "False"));
}

/********************************************************************************/

char *capitalise_resource ( const char *resource )

/* returns a capitalised version of specified resource string, data overwritten	*/
/* with each call. Only capitalises resource strings not anything else.		*/

{ static char  buf[128];
	 char *ptr;

if (!resource)
    return(NULL);

strcpy(buf,resource);
ptr = buf;

*ptr = toupper(*ptr);

while (*ptr)
    if (*ptr++ == '.')
	*ptr = toupper(*ptr);

return(buf);
}

/********************************************************************************/

void set_clients_defaults()

{
fsp_ver_cmd = string_resource("fsptool.client.fver",FSP_VER_CMD);
fsp_ls_cmd  = string_resource("fsptool.client.fls",FSP_LS_CMD);
fsp_get_cmd = string_resource("fsptool.client.fget",FSP_GET_CMD);
fsp_put_cmd = string_resource("fsptool.client.fput",FSP_PUT_CMD);

xv_set(clientsframe.fver, PANEL_VALUE, fsp_ver_cmd);
xv_set(clientsframe.fls, PANEL_VALUE, fsp_ls_cmd);
xv_set(clientsframe.fput, PANEL_VALUE, fsp_put_cmd);
xv_set(clientsframe.fget, PANEL_VALUE, fsp_get_cmd);
}

/********************************************************************************/

void set_fileaction_defaults()

{
set_file_action(UNKNOWN,string_resource("fsptool.exec.default",ACTION_DEFAULT));
set_file_action(TEXT_FILE,string_resource("fsptool.exec.text",ACTION_TEXT));
set_file_action(H_FILE,string_resource("fsptool.exec.h",ACTION_H));
set_file_action(C_FILE,string_resource("fsptool.exec.c",ACTION_C));
set_file_action(GIF_FILE,string_resource("fsptool.exec.gif",ACTION_GIF));
set_file_action(PBM_FILE,string_resource("fsptool.exec.pbm",ACTION_PBM));
set_file_action(X11_FILE,string_resource("fsptool.exec.x11",ACTION_X11));
set_file_action(RAS_FILE,string_resource("fsptool.exec.ras",ACTION_RAS));
set_file_action(PS_FILE,string_resource("fsptool.exec.ps",ACTION_PS));
set_file_action(JPEG_FILE,string_resource("fsptool.exec.jpeg",ACTION_JPEG));
set_file_action(TIFF_FILE,string_resource("fsptool.exec.tiff",ACTION_TIFF));
set_file_action(MPG_FILE,string_resource("fsptool.exec.mpg",ACTION_MPG));
set_file_action(GL_FILE,string_resource("fsptool.exec.gl",ACTION_GL));
set_file_action(RLE_FILE,string_resource("fsptool.exec.rle",ACTION_RLE));
set_file_action(AU_FILE,string_resource("fsptool.exec.au",ACTION_AU));

if (bool_resource("fsptool.file.filter",FALSE))
    xv_set(filterframe.filter, PANEL_VALUE, 0, NULL);
else
    xv_set(filterframe.filter, PANEL_VALUE, 1, NULL);

file_mask = int_resource("fsptool.file.mask",-1);
xv_set(filterframe.checkboxes, PANEL_VALUE, file_mask, NULL);
}

/********************************************************************************/

void set_tool_defaults()

{
tool_properties.openlook    = bool_resource("fsptool.tool.openlook-wm",TRUE);
tool_properties.cancelclose = bool_resource("fsptool.tool.cancelclose",FALSE);
tool_properties.menuclose   = bool_resource("fsptool.tool.menuclose",FALSE);
tool_properties.hostread    = bool_resource("fsptool.tool.hostread",FALSE);

if (tool_properties.openlook == FALSE) {
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

xv_set(toolframe.openlook, PANEL_VALUE, tool_properties.openlook, NULL);
xv_set(toolframe.cancelclose, PANEL_VALUE, tool_properties.cancelclose, NULL);
xv_set(toolframe.menuclose, PANEL_VALUE, tool_properties.menuclose, NULL);
xv_set(toolframe.hostread, PANEL_VALUE, tool_properties.hostread, NULL);
}

/********************************************************************************/

void set_dirlist_defaults()

{ char *ptr = string_resource("fsptool.remotesort.order","Name");
   int  cachesize, cachetimeout;


cachesize    = int_resource("fsptool.cache.size",get_cache_size());
cachetimeout = int_resource("fsptool.cache.timeout",get_cache_timeout());

set_cache_size(cachesize);
set_cache_timeout(cachetimeout);

xv_set(dirlistframe.cachesize, PANEL_VALUE, cachesize, NULL);
xv_set(dirlistframe.cachetimeout, PANEL_VALUE, cachetimeout, NULL);

if (strcmp(ptr,"Name") == 0) {
    xv_set(dirlistframe.sorttype, PANEL_VALUE, 0, NULL);
    xv_set(dirlistframe.alphasort, PANEL_VALUE,
		bool_resource("fsptool.remotesort.reversed",FALSE), NULL);

    xv_set(dirlistframe.alphasort, PANEL_INACTIVE, FALSE, NULL);
    xv_set(dirlistframe.sizesort, PANEL_INACTIVE, TRUE, NULL);
    xv_set(dirlistframe.timesort, PANEL_INACTIVE, TRUE, NULL);

    if (bool_resource("fsptool.remotesort.reversed",FALSE))
	set_fsp_sorttype(RevAlpha);
    else
	set_fsp_sorttype(Alpha);

    return;
    }

if (strcmp(ptr,"Date") == 0) {
    xv_set(dirlistframe.sorttype, PANEL_VALUE, 1, NULL);
    xv_set(dirlistframe.timesort, PANEL_VALUE,
		bool_resource("fsptool.remotesort.reversed",FALSE), NULL);

    xv_set(dirlistframe.alphasort, PANEL_INACTIVE, TRUE, NULL);
    xv_set(dirlistframe.timesort, PANEL_INACTIVE, FALSE, NULL);
    xv_set(dirlistframe.sizesort, PANEL_INACTIVE, TRUE, NULL);

    if (bool_resource("fsptool.remotesort.reversed",FALSE))
	set_fsp_sorttype(RevDate);
    else
	set_fsp_sorttype(Date);

    return;
    }
    
if (strcmp(ptr,"Size") == 0) {
    xv_set(dirlistframe.sorttype, PANEL_VALUE, 2, NULL);
    xv_set(dirlistframe.sizesort, PANEL_VALUE,
		bool_resource("fsptool.remotesort.reversed",FALSE), NULL);

    xv_set(dirlistframe.alphasort, PANEL_INACTIVE, TRUE, NULL);
    xv_set(dirlistframe.timesort, PANEL_INACTIVE, TRUE, NULL);
    xv_set(dirlistframe.sizesort, PANEL_INACTIVE, FALSE, NULL);

    if (bool_resource("fsptool.remotesort.reversed",FALSE))
	set_fsp_sorttype(RevSize);
    else
	set_fsp_sorttype(Size);

    return;
    }
    
error_report(WARNING,"FSPtool: invalid 'Fsptool.Remotesort.Order' value\n");
}

/********************************************************************************/

void set_fsp_defaults()

{ int value = int_resource("fsptool.fsp.localport",0),
      setting;


if (value)
   if ((value > -1) && (value < 100000)) {
	xv_set(fspframe.localport, PANEL_VALUE, value, NULL);
	set_fsp_local_port(value);
	}
    else
	error_report(WARNING,"FSPtool: invalid 'Fsptool.Fsp.Localport'\n");

value = int_resource("fsptool.fsp.timeout",4);

if ((value > 0) && (value < 100)) {
    xv_set(fspframe.timeout, PANEL_VALUE, value, NULL);
    set_fsp_timeout(value);
    }
else
    error_report(WARNING,"FSPtool: invalid 'Fsptool.Fsp.Timeout' value\n");

value = int_resource("fsptool.fsp.buffer",512);

switch (value) {
    case 128:
	setting = 0;
	break;

    case 256:
	setting = 1;
	break;

    case 512:
	setting = 2;
	break;

    case 1024:
	setting = 3;
	break;

    default:
	error_report(WARNING,"FSPtool: invalid 'Fsptool.Fsp.Buffer' value\n");
	value = 512;
	setting = 2;
    }

xv_set(fspframe.bufsize, PANEL_VALUE, setting, NULL);
set_fsp_bufsize(value);

value = int_resource("fsptool.fsp.delay",3000);

switch (value) {
    case 5000:
	setting = 4;
	break;

    case 3000:
	setting = 3;
	break;

    case 2000:
	setting = 2;
	break;

    case 1000:
	setting = 1;
	break;

    case 500:
	setting = 0;
	break;

    default:
	error_report(WARNING,"FSPtool: invalid 'Fsptool.Fsp.Delay' value\n");
	value = 3000;
	setting = 3;
    }

xv_set(fspframe.delaysize, PANEL_VALUE, setting, NULL);
set_fsp_delay(value);
}

/********************************************************************************/
