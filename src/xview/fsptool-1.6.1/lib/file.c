/********************************************************************************/
/* lib/file.c --								*/
/*										*/
/* These are file handling and filing system related functions common to all	*/
/* FSPtool clients.								*/
/*										*/
/* Author : A.J.Doherty								*/
/********************************************************************************/

#include <string.h>

#include <sys/types.h>
#include <sys/time.h>
#include <sys/stat.h>

#include <dirent.h>
#include <time.h>

#include "common.h"
#include "cache.h"
#include "file.h"
#include "unix.h"

#include "../config.h"

/********************************************************************************/
/* Local module static var for local directory reading operations.		*/

static DIR	*dirp;		/* -- pointer to current local directory read	*/
static char	*dirname;	/* -- pointer to name of local directory	*/

/********************************************************************************/

CacheData *interpret_filename ( FILE *stream_out, char ch )

{ static char  name_str[256], temp_buf[256], month_buf[4];
         char  c;
    CacheData *data = c_malloc(sizeof(CacheData));
     DateData  dateinfo;
	  int  hour, min;


fscanf(stream_out,"%*s%*d");			/* -- skip perms char/value	*/

while (!isdigit(c = fgetc(stream_out)));       /* -- skip all stuff before size	*/
ungetc(c,stream_out);

fscanf(stream_out,"%ld",&(data->size));		/* -- read size value		*/
fscanf(stream_out,"%3s",month_buf);		/* -- read month		*/
fscanf(stream_out,"%d",&(dateinfo.day));	/* -- read day			*/
fscanf(stream_out,"%s",temp_buf);		/* -- read time	or year		*/

dateinfo.month = return_month_val(month_buf);

if (strstr(temp_buf,":")) {			/* -- is time rather than year	*/
    sscanf(temp_buf,"%d:%d",&hour,&min);
    dateinfo.year = 0;
    }
else {
    hour = min = 0;
    sscanf(temp_buf,"%d",&(dateinfo.year));
    }

(void) fgetc(stream_out);			/* -- skip the space character	*/
fgets(name_str,255,stream_out);			/* -- read filename		*/

name_str[strlen(name_str)-1] = NULL; 	       /* -- remove \n from end of line */

dateinfo.time     = hour*60+min;
data->date	  = dateinfo;
data->compression = NULL;
data->access	  = NULL;

switch (ch)
    {
    case 'd':
	data->filetype = DIRECTORY;
	data->name     = c_strdup(name_str);
	break;

    case 'l':
	data->filetype = FILE_LINK;

	if (sscanf(name_str,"%*s %*s %s",temp_buf) == 1)
	    data->name = c_strdup(temp_buf);
	else
	    data->name = c_strdup(name_str);

	break;

    default:
	data->name = c_strdup(name_str);
	return_filetype(data->name,data);
    }

return(data);
}

/********************************************************************************/

void strip_pathname ( char *pathname )

/* this fn strips the tail item of of a /etc/etc/etc type pathname if the path	*/
/* is simply / then it is unaltered.						*/

{ char *strptr = pathname+strlen(pathname);

while ((strptr > pathname) && (*strptr != '/'))
    *strptr-- = 0;

if ((strlen(pathname) > 1) && (*strptr == '/'))
    *strptr = 0;
}

/********************************************************************************/

void return_filetype ( char *filename, CacheData *file_data )

/* this function takes a filename and tries to make an intelligent guess about	*/
/* the type of the file. Such as foo.Z being a Z compressed file .jpg for JPEG  */
/* and so on.									*/

{ char *tmpptr, *ptr = filename+strlen(filename), *ptr2;


file_data->filetype    = NULL;
file_data->compression = NULL;

while (( ptr > filename ) && (*ptr != '.'))
    ptr--;

if (ptr == filename) {
    file_data->filetype = UNKNOWN;  /* -- not found a . in string or at start */
    return;
    }

ptr++;

if (strcmp(ptr,"z") == 0)			/* -- a "little z" .z file */
    file_data->compression = z_FILE;
else
    if (strcmp(ptr,"Z") == 0)			/* -- a Z file */
        file_data->compression = Z_FILE;
    else
    	if (strcmp(ptr,"gz") == 0)		/* -- a z "gzip" file */
            file_data->compression = z_FILE;

if (file_data->compression) {
    ptr--;
    ptr--;

    if (ptr < filename)
	ptr = filename;

    while (( ptr >= filename ) && (*ptr != '.'))
	ptr--;

    ptr++;
    }

if (ptr == filename) {
    file_data->filetype = UNKNOWN;  /* -- not found a . in string or at start */
    return;
    }

tmpptr = ptr;
ptr    = c_malloc(strlen(tmpptr)+1);
strcpy(ptr,tmpptr);

if ((ptr2 = strchr(ptr,'.')))
    *ptr2  = 0;

tmpptr = ptr;
ptr    = c_malloc(strlen(tmpptr)+2);
strcpy(ptr,".");
strcat(ptr,tmpptr);
strcat(ptr,".");
free(tmpptr);

if (strstr(".tar.TAR.Tar.",ptr))
    file_data->filetype = TAR_FILE;

if (strstr(".zip.ZIP.",ptr))
    file_data->filetype = ZIP_FILE;

if (strstr(".txt.text.TXT.readme.README.Readme.TEXT.",ptr))
    file_data->filetype = TEXT_FILE;

if (strstr(".h.H.",ptr))
    file_data->filetype = H_FILE;

if (strstr(".c.C",ptr))
    file_data->filetype = C_FILE;

if (strstr(".gif.GIF.g.",ptr))
    file_data->filetype = GIF_FILE;

if (strstr(".pbm.PBM.",ptr))
    file_data->filetype = PBM_FILE;

if (strstr(".xbm.XBM.",ptr))
    file_data->filetype = X11_FILE;

if (strstr(".ras.RAS.raster.RASTER.",ptr))
    file_data->filetype = RAS_FILE;

if (strstr(".ps.PS.postscript.POSTSCRIPT.PostScript.",ptr))
    file_data->filetype = PS_FILE;

if (strstr(".jpg.JPG.jpeg.Jpeg.JPEG.",ptr))
    file_data->filetype = JPEG_FILE;

if (strstr(".tiff.TIFF.",ptr))
    file_data->filetype = TIFF_FILE;

if (strstr(".mpg.MPG.",ptr))
    file_data->filetype = MPG_FILE;

if (strstr(".gl.grasp.GRASP.GL.",ptr))
    file_data->filetype = GL_FILE;

if (strstr(".rle.RLE.",ptr))
    file_data->filetype = RLE_FILE;

if (strstr(".au.audio.AU.AUDIO.",ptr))
    file_data->filetype = AU_FILE;

if (!file_data->filetype)
    file_data->filetype = UNKNOWN;

free(ptr);
}

/********************************************************************************/

char *cachedata_to_line ( CacheData *data )

/* this fn takes a cache data item and produces a character line of the form	*/
/* used in FSPtool directory listings. Return line is not reliable between	*/
/* calls to the routines.							*/

{ static char linebuf[256];


if (data->date.year == 0)
    sprintf(linebuf,"%3s %2d %02d:%02d %11ld %s",
		return_month_str(data->date.month), data->date.day,
		data->date.time/60, data->date.time%60,
		data->size, data->name);
else
    sprintf(linebuf,"%3s %2d %5d %11ld %s",
		return_month_str(data->date.month), data->date.day,
		data->date.year,
		data->size, data->name);

return(linebuf);
}

/********************************************************************************/

int return_month_val ( char *month_str )

/* this fn takes a month name and returns equivalent month of year.		*/

{
if (strcmp(month_str,"Jan") == 0)
    return(1);

if (strcmp(month_str,"Jun") == 0)
    return(6);

if (strcmp(month_str,"Mar") == 0)
    return(3);

if (strcmp(month_str,"Apr") == 0)
    return(4);

switch (*month_str) {
    case 'F' : return(2);
    case 'M' : return(5);
    case 'J' : return(7);
    case 'A' : return(8);
    case 'S' : return(9);
    case 'O' : return(10);
    case 'N' : return(11);
    case 'D' : return(12);
    }

return(1);
}

/********************************************************************************/

char *return_month_str ( int month )

/* this fn takes a month name and returns equivalent month of year.		*/

{ static char *month_table[] = { "Jan", "Feb", "Mar",
				 "Apr", "May", "Jun",
				 "Jul", "Aug", "Sep",
				 "Oct", "Nov", "Dec" };

return(month_table[month-1]);
}

/********************************************************************************/

int compare_date ( DateData date1, DateData date2 )

/* this fn compares dates in a similar manner to strcmp, if date1 < date2 then	*/
/* <0 is returned, if they are equal then =0 otherwise >0			*/

{
if (date1.year > date2.year)
    return(1);

if (date1.year < date2.year)
    return(-1);

if (date1.month < date2.month)
    return(1);

if (date1.month > date2.month)
    return(-1);

if (date1.day < date2.day)
    return(1);

if (date1.day > date2.day)
    return(-1);

if (date1.time < date2.time)
    return(1);

if (date1.time > date2.time)
    return(-1);

return(0);
}

/********************************************************************************/

int open_file_dir ( char *pathname )

/* this fn opens up the named directory if possible. Returns appropriate error	*/
/* code from <file.h> if unable to open or read named directory.		*/

{
if (!(dirp = opendir(pathname)))
    return(FILEDIR_UNOPENED);

dirname = c_strdup(pathname);
return(FILEDIR_OK);
}

/********************************************************************************/

int close_file_dir()

/* this fn closes the directory which was/is currently being read. Returns code	*/
/* if error occurred attempting to close the directory.				*/

{
if (dirp)
    closedir(dirp);

if (dirname)
    free(dirname);

return(FILEDIR_OK);
}

/********************************************************************************/

CacheData *read_file_dir()

/* this fn reads in the next entry from the currently open directory. Will	*/
/* return CacheData* NULL when end of directory is encountered. Or if no more	*/
/* entries could be read for some other reason. Won't return . or ..		*/

{ struct dirent *dp;
  struct stat    s_buf;
  CacheData	*ptr;
  struct tm	 ltime, *ctime;
  time_t	 timeval;

  static char	 pname[MAXPATHLEN];


if (!(dp = readdir(dirp)))
    return((CacheData*)NULL);

ptr = c_malloc(sizeof(CacheData));

strcpy(pname,dirname);
strcat(pname,"/");
strcat(pname,dp->d_name);
stat(pname,&s_buf);

ptr->size = (int) s_buf.st_size;

ltime   = *(localtime(&(s_buf.st_mtime)));
timeval = time(NULL);
ctime   = localtime(&timeval);

ptr->date.day	= ltime.tm_mday;
ptr->date.month	= ltime.tm_mon+1;

if (ctime->tm_year == ltime.tm_year)
    ptr->date.year = 0;
else
    ptr->date.year = ltime.tm_year+1900; /* -- should check century :-> */

ptr->date.time	= (ltime.tm_hour*60)+ltime.tm_min;
ptr->access	= s_buf.st_mode;

if (S_ISDIR(s_buf.st_mode) && (*(dp->d_name) != '.')) {
    ptr->name = c_strdup(dp->d_name);
    ptr->filetype = DIRECTORY;
    ptr->compression = 0;
    return(ptr);
    }

if (S_ISREG(s_buf.st_mode) && (*(dp->d_name) != '.')) {
    ptr->name = c_strdup(dp->d_name);
    return_filetype(dp->d_name,ptr);
    return(ptr);
    }

/* -- if we got this far then we have an abnormal file call out for next */

free(ptr);
return(read_file_dir());
}

/********************************************************************************/

int file_exists ( const char *pathname )

/* this fn returns TRUE if the file exists and is a dir or regular file		*/

{ struct stat s_buf;


if (stat(pathname,&s_buf) == -1)
    return(0);

return(S_ISREG(s_buf.st_mode) || S_ISDIR(s_buf.st_mode));
}

/********************************************************************************/

int make_dir_hier ( const char *pathname )

/* this fn creates an entire directory hierarchy (if required) to create the	*/
/* directory on the end of pathname. Returns FALSE if couldn't create dir	*/

{
if (file_exists(pathname))
    return(TRUE);

mkdir((char*)pathname,448);
return(TRUE);

}

/********************************************************************************/

char *get_cwd()

/* this fn returns the current working directory. Masks differences between	*/
/* UNIX variants in doing this.	Returned pointer is overwritten on next call	*/

{ static char buffer[MAXPATHLEN];

#if defined(SVR4) || defined(SYSV)
(void) getcwd(buffer,(size_t) MAXPATHLEN);
#else
(void) getwd(buffer);
#endif

return(buffer);
}

/********************************************************************************/

char *unit_file_size ( long int file_size )

/* this fn returns the size of the file as a string in whatever are the most	*/
/* sizeable units it can be measured in (bytes -> Kbytes -> Mbytes)		*/

{ static char nbuf[128];

if (file_size/10485760) {	/* -- greater than 10 mbytes ?	*/
    sprintf(nbuf,"%1.2f Mbytes",(((float)file_size)/1048576.0));
    return(nbuf);
    }

if (file_size/10240) {		/* -- greater than 10 Kbytes ?	*/
    sprintf(nbuf,"%1.2f Kbytes",(((float)file_size)/1024.0));
    return(nbuf);
    }

sprintf(nbuf,"%1ld Bytes",file_size);
return(nbuf);
}

/********************************************************************************/
