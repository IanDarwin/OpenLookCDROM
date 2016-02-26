/*
 * xman - X window system manual page display program.
 *
 * $XConsortium: misc.c,v 1.19 89/12/10 17:19:10 rws Exp $
 *
 * Copyright 1987, 1988 Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of M.I.T. not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  M.I.T. makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * Author:    Chris D. Peterson, MIT Project Athena
 * Created:   October 27, 1987
 */

#if ( !defined(lint) && !defined(SABER))
  static char rcs_version[] = "$Athena: misc.c,v 4.6 88/12/19 13:48:01 kit Exp $";
#endif

#include <unistd.h>
#include <malloc.h>
#include <string.h>
#include <sys/stat.h>
#include <errno.h>
#include <xview/xview.h>
#include <xview/panel.h>
#include <xview/textsw.h>
#include <xview/xv_xrect.h>
#include <xview/notice.h>
#include <gdd.h>
#include "olxvman_ui.h"
#include "globals.h"

static FILE * Uncompress();
static Bool UncompressNamed(), UncompressUnformatted();
extern int errno;		/* error codes. */

/*
 * It would be very nice if these would pop up their own windows for 
 * error messages, whould anyone like to implement this???
 */

/*	Function Name: PrintWarning
 *	Description: This function prints a warning message to stderr.
 *	Arguments: string - the specific warning string.
 *	Returns: none
 */

void
PrintWarning(string)
char * string;
{
    int	x,y;

	x = xv_get (OLXVMan->OLXVMan, XV_X, NULL);
	y = xv_get (OLXVMan->OLXVMan, XV_Y, NULL);
	x += xv_get (OLXVMan->OLXVMan, XV_WIDTH, NULL) / 2;
	y += xv_get (OLXVMan->OLXVMan, XV_HEIGHT, NULL) / 2;
	notice_prompt (OLXVMan->controls1, NULL,
		       NOTICE_MESSAGE_STRINGS, string, NULL,
		       NOTICE_BUTTON,	"Ok", 101,
		       NOTICE_FOCUS_XY,	x, y,
		       NULL);
}

/*	Function Name: PrintError
 *	Description: This Function prints an error message and exits.
 *	Arguments: string - the specific message.
 *	Returns: none. - exits tho.
 */

void
PrintError(string)
char * string;
{
  fprintf(stderr,"Xman Error: %s\n",string);
#ifdef DEBUG
  fprintf(stderr,"\n\nbye,bye\n\n\n\n\nsniff...\n");
#endif
  exit(42);
}


/*
 * Function Name: FilterText Description: Removes "_^H" from text buffer.
 */

char           *
  FilterText(buf, size)
char           *buf;
int             size;
{
    char           *start;
    char           *actual;
    char           *newbuf;
    
    newbuf = malloc(size);
    
    for (start = buf, actual = newbuf; start < (buf + size); start++)
    {
	if (*start == '_' && *(start + 1) == 8)
	  start += 1;
	else
	  *actual++ = *start;
	
	if (*start == 'o' && *(start + 1) == 8)
	  start += 2;
    }
    
    *actual = 0;
    free(buf);
    
    return (newbuf);
}

/*	Function Name: OpenFile
 *	Description: Assignes a file to the manpage.
 *	Arguments: man_globals - global structure.
 *                 file        - the file pointer.
 *	Returns: none
 */

void
OpenFile(wText, file)
Xv_opaque wText;
FILE * file;
{
    char	*buf;
    int		size;

    fseek (file, 0, 2);
    size = ftell (file);
    rewind (file);
    buf = malloc (size + 1);
    fread (buf, 1, size, file);
    buf[size] = 0;

    buf = FilterText (buf, size);

    xv_set (wText, 
	    TEXTSW_CONTENTS, buf, 
	    TEXTSW_FIRST, 0,
	    NULL);
    free (buf);
    return;
}


/*	Function Name: FindManualFile
 *	Description: Opens the manual page file given the entry information.
 *	Arguments: man_globals - the globals info for this manpage.
 *                 section_num - section number of the man page.
 *                 entry_num   - entry number of the man page.
 *	Returns: fp - the file pointer
 *
 * NOTES:
 *
 * If there is a uncompressed section it will look there for uncompresed 
 * manual pages first and then for individually comressed file in the 
 * uncompressed section.
 * 
 * If there is a compressed directory then it will also look there for 
 * the manual pages.
 *
 * If both of these fail then it will attempt to format the manual page.
 */

FILE *
FindManualFile(section_num, entry_num)
int section_num, entry_num;
{
  FILE * file;
  char path[BUFSIZ], page[BUFSIZ], section[BUFSIZ], *temp;
  char filename[BUFSIZ];
  char * entry = manual[section_num].entries[entry_num];
  int len_cat = strlen(CATC);

  ParseEntry(entry, path, section, page);

/*
 * Look for uncompressed files first.
 */

  sprintf(filename, "%s/%s%s/%s", path, CATC, section + len_cat, page);
  if ( (file = fopen(filename,"r")) != NULL)
    return(file);

/*
 * Then for compressed files in an uncompressed directory.
 */

  sprintf(filename, "%s/%s%s/%s.%s", path, CATC, 
	  section + len_cat, page, COMPRESSION_EXTENSION);
  if ( (file = Uncompress(filename)) != NULL) 
    return(file);

/*
 * And lastly files in a compressed directory.
 *
 * The directory is not actually compressed it is just named man#.Z
 * and all files in it are compressed without the .Z extension.
 * HP does it this way (really :-).
 */

  sprintf(filename, "%s/%s%s.%s/%s", path, CATC, section + len_cat,
	  COMPRESSION_EXTENSION, page);
  if ( (file = Uncompress(filename)) != NULL)
    return(file);
/*
 * We did not find any preformatted manual pages, try to format it.
 */

  return(Format(entry));
}

/*	Function Namecompress
 *	Description: This function will attempt to find a compressed man
 *                   page and uncompress it.
 *	Arguments: man_globals - the psuedo global info.
 *                 filename - name of file to uncompress.
 *	Returns:; a pointer to the file or NULL.
 */

static FILE *
Uncompress(filename)
char * filename;
{
  char tmpfile[BUFSIZ], error_buf[BUFSIZ];
  FILE * file;

  if ( !UncompressNamed(filename, tmpfile) )
    return(NULL);

  else if ((file = fopen(tmpfile, "r")) == NULL) {  
    sprintf(error_buf, "Something went wrong in retrieving the %s",
	    "uncompressed manual page try cleaning up /tmp.");
    PrintWarning(error_buf);
  }

  unlink(tmpfile);		/* remove name in tree, it will remain
				   until we close the fd, however. */
  return(file);
}

/*	Function Name: UncompressNamed
 *	Description: This function will attempt to find a compressed man
 *                   page and uncompress it.
 *	Arguments: man_globals - the psuedo global info.
 *                 filename - name of file to uncompress.
 * RETURNED        output - the file name output (must be an allocated string).
 *	Returns:; TRUE if the file was found.
 */

static Bool
UncompressNamed(filename, output)
char * filename, * output;
{
  char tmp[BUFSIZ], cmdbuf[BUFSIZ], error_buf[BUFSIZ];
  struct stat junk;

  if (stat(filename, &junk) != 0) { /* Check for existance of the file. */
    if (errno != ENOENT) {
      sprintf(error_buf, "Error while stating file %s, errno = %d",
	      filename, errno);
      PrintWarning(error_buf);
    }
    return(FALSE);
  }

/*
 * Using stdin is necessary to fool zcat since we cannot guarentee
 * the .Z extension.
 */

  strcpy(tmp, MANTEMP);		/* get a temp file. */
  strcpy(output, mktemp(tmp));

  sprintf(cmdbuf, UNCOMPRESS_FORMAT, filename, output);
  if(system(cmdbuf) == 0) 	/* execute search. */
    return(TRUE);

  sprintf(error_buf, "Error while uncompressing, command was: %s", cmdbuf);
  PrintWarning(error_buf);
  return(FALSE);
}

/*
 *	Function Name: PrintFormat
 *	Description: This function formats the manual page for the printer.
 */

void
PrintFormat (entry)
char	*entry;
{
    char	filename[BUFSIZ], error_buf[BUFSIZ], cmdbuf[BUFSIZ];
    char	path[BUFSIZ];

    if ( !UncompressUnformatted(entry, filename) ) {
	/* We Really could not find it, this should never happen, yea right. */
	sprintf(error_buf, "Could open manual page, %s", entry);
	PrintWarning(error_buf);
	return;
    }
    
    ParseEntry(entry, path, NULL, NULL);
    
    sprintf(cmdbuf,"cd %s ; %s %s %s", path, TBL,
	    filename, PFORMAT);
    
    if(system(cmdbuf) != 0) 
    {	/* execute search. */
	sprintf(error_buf,
		"Something went wrong trying to run the command: %s", cmdbuf);
	PrintWarning(error_buf);
    }
    return;
}

/*	Function Name: Format
 *	Description: This funtion formats the manual pages and interfaces
 *                   with the user.
 *	Arguments: man_globals - the psuedo globals
 *                 file - the file pointer to use and return
 *                 entry - the current entry struct.
 *                 current_box - The current directory being displayed. 
 *	Returns: none.
 */

/* ARGSUSED */

FILE *
  Format(entry)
char * entry;
{
    FILE * file;
    char cmdbuf[BUFSIZ], tmp[BUFSIZ], filename[BUFSIZ], error_buf[BUFSIZ];
    char tmpfile[BUFSIZ];
    char path[BUFSIZ];
    int	result;
    int x, y;
    
    xv_set (OLXVMan->OLXVMan, FRAME_BUSY, TRUE, NULL);
    
    if ( !UncompressUnformatted(entry, filename) ) {
	/* We Really could not find it, this should never happen, yea right. */
	sprintf(error_buf, "Could open manual page, %s", entry);
	PrintWarning(error_buf);
	xv_set (OLXVMan->OLXVMan, FRAME_BUSY, FALSE, NULL);
	return(NULL);
    }
    
    strcpy(tmp,MANTEMP);		          /* Get a temp file. */
    strcpy(tmpfile,mktemp(tmp));
    ParseEntry(entry, path, NULL, NULL);
    
    sprintf(cmdbuf,"cd %s ; %s %s %s > %s %s", path, TBL,
	    filename, FORMAT, tmpfile, "2> /dev/null");

    
    if(system(cmdbuf) != 0) {	/* execute search. */
	xv_set (OLXVMan->OLXVMan, FRAME_BUSY, FALSE, NULL);
	sprintf(error_buf,
		"Something went wrong trying to run the command: %s", cmdbuf);
	PrintWarning(error_buf);
	file = NULL;
    }
    else 
    {
	xv_set (OLXVMan->OLXVMan, FRAME_BUSY, FALSE, NULL);

	if ((file = fopen(tmpfile,"r")) == NULL) 
	{
	    sprintf(error_buf, "Something went wrong in retrieving the %s",
		    "temp file, try cleaning up /tmp");
	    PrintWarning(error_buf);
	}
	else 
	{
	    
	    char * ptr, catdir[BUFSIZ];
	    
	    /*
	     * If the catdir is writeable then ask the user if he/she wants to
	     * write the man page to it. 
	     */
	    
	    strcpy(catdir, SaveFile);
	    if ( (ptr = rindex(catdir, '/')) != NULL) 
	    {
		*ptr = '\0';
		
		if ( access(catdir, W_OK) != 0 )
		  unlink(tmpfile);
		else 
		{
		    x = xv_get (OLXVMan->ManualPages, XV_X, NULL);
		    y = xv_get (OLXVMan->ManualPages, XV_Y, NULL);
		    x += xv_get (OLXVMan->ManualPages, XV_WIDTH, NULL) / 2;
		    y += xv_get (OLXVMan->ManualPages, XV_HEIGHT, NULL) / 2;
		    result = notice_prompt (OLXVMan->controls1, NULL,
			     NOTICE_MESSAGE_STRINGS, "Do you want to save the formatted manual page?", NULL,
			     NOTICE_FOCUS_XY, x, y,
			     NOTICE_BUTTON_YES, "Yes",
			     NOTICE_BUTTON_NO, "No",
					    NULL);
		    if (result == NOTICE_YES)
		      SaveFormattedPage (tmpfile);
		}
	    }
	    else 
	      unlink(tmpfile);
	}
    }
    
    if (WasCompressed)	/* If the original was compressed
				   then this is a tempory file. */
      unlink(filename);
    
    return(file);
}

/*	Function Name: UncompressUnformatted
 *	Description: Finds an uncompressed unformatted manual page.
 *	Arguments: man_globals - the psuedo global structure.
 *                 entry - the manual page entry.
 * RETURNED        filename - location to put the name of the file.
 *	Returns: TRUE if the file was found.
 */

static Bool
UncompressUnformatted(entry, filename)
char * entry, * filename;
{
  char path[BUFSIZ], page[BUFSIZ], section[BUFSIZ], input[BUFSIZ];
  int len_cat = strlen(CATC), len_man = strlen(MAN);

  ParseEntry(entry, path, section, page);

/*
 * Look for uncompressed file first.
 */

  sprintf(filename, "%s/%s%s/%s", path, MAN, section + len_man, page);
  if ( access( filename, R_OK ) == 0 ) {
    WasCompressed = FALSE;
    sprintf(SaveFile, "%s/%s%s/%s", path,
	    CATC, section + len_cat, page);
    return(TRUE);
  }

/*
 * Then for compressed files in an uncompressed directory.
 */

  sprintf(input, "%s.%s", filename, COMPRESSION_EXTENSION);
  if ( UncompressNamed(input, filename) ) {
    WasCompressed = TRUE;
    sprintf(SaveFile, "%s/%s%s/%s.%s", path,
	    CATC, section + len_cat, page, COMPRESSION_EXTENSION);
    return(TRUE);
  }
/*
 * And lastly files in a compressed directory.
 */

  sprintf(input, "%s/%s%s.%s/%s", path, 
	  MAN, section + len_man, COMPRESSION_EXTENSION, page);
  if ( UncompressNamed(input, filename) ) {
    WasCompressed = TRUE;
    sprintf(SaveFile, "%s/%s%s.%s/%s", path, 
	    CATC, section + len_cat, COMPRESSION_EXTENSION, page);
    return(TRUE);
  }
  return(FALSE);
}

/*	Function Name: ParseEntry(entry, path, sect, page)
 *	Description: Parses the manual pages entry filenames.
 *	Arguments: str - the full path name.
 *                 path - the path name.      RETURNED
 *                 sect - the section name.   RETURNED
 *                 page - the page name.      RETURNED
 *	Returns: none.
 */

void
ParseEntry(entry, path, sect, page)
char *entry, *path, *page, *sect;
{
  char *c, temp[BUFSIZ];

  strcpy(temp, entry);

  c = rindex(temp, '/');
  if (c == NULL) 
    PrintError("index failure in ParseEntry.");
  *c++ = '\0';
  if (page != NULL)
    strcpy(page, c);

  c = rindex(temp, '/');
  if (c == NULL) 
    PrintError("index failure in ParseEntry.");
  *c++ = '\0';
  if (sect != NULL)
    strcpy(sect, c);

  if (path != NULL)
    strcpy(path, temp);
}

/*	Function Name: CreateManpageName
 *	Description: Creates the manual page name for a given item.
 *	Arguments: entry - the entry to convert.
 *	Returns: the manual page properly allocated.
 */

/*
 * If the filename is foo.3     - Create an entry of the form:  foo
 * If the filename is foo.3X11  - Create an entry of the form:  foo(X11)
 * IF the filename is a.out.1   - Create an entry of the form:  a.out
 */

char *
CreateManpageName(entry)
char * entry;
{
  char * cp;
  char page[BUFSIZ];

  ParseEntry(entry, NULL, NULL, page);

  if ( (cp = rindex(page, '.')) != NULL)
    if ( (strlen(cp) > 2) ) {
      *cp++ = '(';
      while( (cp[1] != '\0') ) {
	*cp = *(cp + 1); 
	cp++;
      }
      *cp++ = ')';
      *cp = '\0';
    }
    else
      *cp = '\0';  
  
  return(StrAlloc(page));
}

char *
OlRealloc (ptr, size)
char	*ptr;
int	size;
{
    char	*newptr;

    if (ptr)
      newptr = realloc (ptr, size);
    else
      newptr = malloc (size);

    return (newptr);
}

void
SaveFormattedPage (tmpfile)
char	*tmpfile;
{
    char	cmdbuf[BUFSIZ], error_buf[BUFSIZ];

    if (!WasCompressed)
      sprintf (cmdbuf, "%s %s %s", COPY, tmpfile, SaveFile);
    else
      sprintf (cmdbuf, "%s < %s > %s", COMPRESS, tmpfile, SaveFile);

    if (system (cmdbuf) != 0)
    {
	sprintf (error_buf, "Something went wrong executing command '%s'.",
		 cmdbuf);
	PrintWarning (error_buf);
    }

    return;
}
