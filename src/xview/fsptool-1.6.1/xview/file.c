/********************************************************************************/
/* file.c --									*/
/*										*/
/* Author : A.J.Doherty								*/
/********************************************************************************/

#include "fsptool.h"
#include "icon.h"
#include "frame.h"
#include "file.h"
#include "resource.h"
#include "system.h"

#include "../lib/cache.h"
#include "../lib/file.h"
#include "../lib/unix.h"
#include "../lib/fsp.h"

#include "../config.h"

/********************************************************************************/

extern Base_frame	baseframe;
extern Local_frame	localframe;
extern Filter_frame	filterframe;
extern Action_frame	actionframe;

extern Xv_font		fixed_font, bold_fixed_font;

extern char		*buf;

/********************************************************************************/

int  local_files_found = 0,
     local_files_selected = 0,
     files_found = 0,
     files_selected = 0,
     file_mask = -1,
     local_files = 0,
     exec_mode = 0;

char execfile[512], filenm[512];

/********************************************************************************/

static char fileactions[NO_FILETYPES+1][256];

/********************************************************************************/

char *get_file_action ( int filetype )

{
return(fileactions[filetype]);
}

/********************************************************************************/

void set_file_action ( int filetype, const char *action )

{
strcpy(fileactions[filetype],action);
xv_set(actionframe.fileactions[filetype], PANEL_VALUE, action, NULL);
}

/********************************************************************************/

void set_filter ( Panel_item item, int value, Event *event )

{
file_mask = (int) xv_get(filterframe.checkboxes, PANEL_VALUE);

if (item_name(item,"Save")) {
    set_bool_resource("Fsptool.File.Filter",
				!((int)xv_get(filterframe.filter, PANEL_VALUE)));
    set_int_resource("Fsptool.File.Mask",file_mask);
    save_resources();
    }
}

/********************************************************************************/

void set_file_actions ( Panel_item item, int value, Event *event )

/* this fn takes the panel values for the applications to run for particular	*/
/* file types and transfers them to the appropriate storage areas so they will	*/
/* be used. NB: this routine is order dependent on the order of the panel items	*/

{ int loop = TEXT_FILE, count = 0;

 char *types[] = { "Text", "H", "C", "Gif", "Pbm", "X11", "Ras", "Ps", "Jpeg",
		   "Tiff", "Mpg", "Gl", "Rle", "Au", "Default" };

 char *ptr = c_malloc(256);


for ( ; loop <= UNKNOWN; loop++ ) {
    strcpy(fileactions[loop],
		(char*)xv_get(actionframe.fileactions[loop], PANEL_VALUE));

    if (item_name(item,"Save")) {
	strcpy(ptr,"Fsptool.Exec.");
	strcat(ptr,types[count]);
	set_string_resource(ptr,fileactions[loop]);
	}

    count++;
    }

if (item_name(item,"Save"))
    save_resources();

free(ptr);
}

/********************************************************************************/

void exec_file ( CacheData *file_data )

/* this fn handles the actual execution of a file once it has been downloaded	*/
/* from the remote filing system. This includes decompressing it and then	*/
/* running it if necessary.							*/

{ static char  namebuf[512];
         char *argv[5];
	  int  pid;

if (file_data->compression) {

#ifndef HAS_NO_GZIP

    left_footer(baseframe.frame, "Decompressing %s ...",file_data->name);
    argv[0] = UNIX_GZIP_CMD;
    strcpy(namebuf,get_fsp_local_dir());
    strcat(namebuf,"/");
    strcat(namebuf,file_data->name);
    argv[1] = "-d";
    argv[2] = "-f";
    argv[3] = namebuf;
    argv[4] = NULL;

    xv_set(baseframe.frame,
		XV_KEY_DATA,
			do_decompress(argv),
			file_data,
		NULL);

#else

    left_footer(baseframe.frame, "Uncompressing %s ...",file_data->name);
    argv[0] = UNIX_UNCOMPRESS_CMD;
    strcpy(namebuf,get_fsp_local_dir());
    strcat(namebuf,"/");
    strcat(namebuf,file_data->name);
    argv[1] = "-f";
    argv[2] = namebuf;
    argv[3] = NULL;

    xv_set(baseframe.frame,
		XV_KEY_DATA,
			do_decompress(argv),
			file_data,
		NULL);
#endif
    }
else {
    strcpy(namebuf,get_fsp_local_dir());
    strcat(namebuf,"/");
    strcat(namebuf,file_data->name);
    pid = do_command(string_to_args(get_file_action(file_data->filetype),namebuf));
    xv_set(baseframe.frame, XV_KEY_DATA, pid, c_strdup(namebuf), NULL);
    }
}

/********************************************************************************/

void do_exec_file ( CacheData *file_data )

/* this function takes a filetype and appropriate filename and file_data and	*/
/* determines what to do with this file. Usually this will be download it to	*/
/* the current directory and then execute an appropriate application with the	*/
/* downloaded file as a parameter - eg: textedit README				*/

{
if (file_data->filetype == DIRECTORY)
    return;

exec_mode = TRUE;
fget_files(file_data);
}

/********************************************************************************/

void do_exec_local_file ( CacheData *file_data )

/* this function takes a filetype and appropriate filename and file_data and	*/
/* determines what to do with this file. Usually this will be to execute an	*/
/* appropriate application with the file as a parameter - eg: textedit README	*/

{
if (file_data->filetype == DIRECTORY)
    return;

strcpy(execfile,get_file_action(file_data->filetype));
left_footer(localframe.frame,"Starting %s ...",execfile);
set_frame_busy(TRUE);
strcpy(filenm,get_fsp_local_dir());
strcat(filenm,"/");
strcat(filenm,file_data->name);

xv_set(baseframe.frame,
	XV_KEY_DATA,
		do_command(string_to_args(execfile,filenm)),
					/* -- key by pid reference number	*/
		NULL,			/* -- local file so don't delete it	*/
	NULL);

set_frame_busy(FALSE);
}

/********************************************************************************/

int infilter ( CacheData *file_data )

/* this fn returns TRUE if the specified file is one which can be passed by	*/
/* the current filter settings - if filter is on - otherwise returns FALSE	*/

{
if (isdir(file_data) || xv_get(filterframe.filter, PANEL_VALUE))
    return(TRUE);

return(file_mask & (1<<(file_data->filetype-3)));
}

/********************************************************************************/

void read_fsp_directory ( Panel_item list )

/* this fn displays the listing of the current FSP directory in the passed	*/
/* panel list item. It uses the FSP cache routines, so the difference between	*/
/* data polled for and data from the cache is transparent to it.		*/

{ CacheData *data;
  int        op_status;

files_found    = 0;
files_selected = 0;

xv_set(list,
	PANEL_LIST_INSERT,	0,
	PANEL_LIST_STRING,	0, "../",
	PANEL_LIST_FONT,	0, bold_fixed_font,
	PANEL_LIST_CLIENT_DATA,	0, NULL,
	PANEL_PAINT,		PANEL_NONE,
	NULL);

if ((op_status = open_cache(get_fsp_dir())) == CACHE_OK) {
    while ((data = query_file_data()))
	if ( infilter(data) ) {
	    files_found++;

	    xv_set(list,
		PANEL_LIST_INSERT, 	files_found,
		PANEL_LIST_STRING, 	files_found,	cachedata_to_line(data),
		PANEL_LIST_FONT,   	files_found,
			(isdir(data) ? bold_fixed_font : fixed_font),
		PANEL_LIST_GLYPH,	files_found,
			return_file_glyph(data->filetype),
		PANEL_LIST_CLIENT_DATA,	files_found,	data,
		PANEL_PAINT,		PANEL_NONE,
		NULL);
	    }

    left_footer(baseframe.frame,"Directory read.");
    close_cache();
    return;
    }

files_found = 0;
files_selected = 0;
error_report(ERROR,cache_perror(op_status));
}

/********************************************************************************/

void read_local_directory ( Panel_item list, char *dirname )

/* this fn calls out to the local file reading routines to return a CacheData	*/
/* formatted list of the files in the named directory.				*/

{ CacheData *ptr;
  int	     status;


local_files_found = 1;
local_files_selected = 0;
left_footer(localframe.frame,"Reading directory ...");

xv_set(list,
	PANEL_LIST_INSERT,	0,
	PANEL_LIST_STRING,	0, "../",
	PANEL_LIST_FONT,	0, bold_fixed_font,
	PANEL_LIST_CLIENT_DATA,	0, NULL,
	PANEL_PAINT,		PANEL_NONE,
	NULL);

if (( status = open_file_dir(dirname)) == FILEDIR_OK ) {
    while ((ptr = read_file_dir()))
	{
	xv_set(list,
		PANEL_LIST_INSERT, 	local_files_found,
		PANEL_LIST_STRING, 	local_files_found,
			cachedata_to_line(ptr),
		PANEL_LIST_FONT,   	local_files_found,
			(isdir(ptr) ? bold_fixed_font : fixed_font),
		PANEL_LIST_GLYPH,	local_files_found,
			return_file_glyph(ptr->filetype),
		PANEL_LIST_CLIENT_DATA,	local_files_found,	ptr,
		PANEL_PAINT,		PANEL_NONE,
		NULL);

	local_files_found++;
	}

    left_footer(localframe.frame, "Directory read.");
    right_footer(localframe.frame, --local_files_found, local_files_selected);
    close_file_dir();
    return;
    }

local_files_found    = 0;
local_files_selected = 0;
left_footer(localframe.frame, "Couldn't open directory.");
right_footer(localframe.frame,local_files_found,local_files_selected);
}

/********************************************************************************/

