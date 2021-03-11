/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */

/*
	$Disclaimer: 
 * Permission to use, copy, modify, and distribute this software and its 
 * documentation for any purpose is hereby granted without fee, 
 * provided that the above copyright notice appear in all copies and that 
 * both that copyright notice, this permission notice, and the following 
 * disclaimer appear in supporting documentation, and that the names of 
 * IBM, Carnegie Mellon University, and other copyright holders, not be 
 * used in advertising or publicity pertaining to distribution of the software 
 * without specific, written prior permission.
 * 
 * IBM, CARNEGIE MELLON UNIVERSITY, AND THE OTHER COPYRIGHT HOLDERS 
 * DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT 
 * SHALL IBM, CARNEGIE MELLON UNIVERSITY, OR ANY OTHER COPYRIGHT HOLDER 
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
 * DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, 
 * WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS 
 * ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
 * OF THIS SOFTWARE.
 *  $
*/

#ifndef NORCSID
#define NORCSID
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/class/lib/RCS/classind.c,v 1.5 1992/12/15 21:00:13 rr2b R6tape $";
#endif


#include <sys/param.h>
#include <andrewos.h> 
#include <stdio.h>


#include <classind.h>


/*
 *
 * local functions
 *
 */

/**
 ** Our own free that doesn't botch it if 
 ** we try to free a NULL pointer.  This 
 ** makes alot of code easier.
 **/
static void
SafeFree(buffer)
void * buffer;

{
    if (buffer != NULL) {
	free(buffer);
    }

    return;
}





/*
 *
 * external functions
 *
 */


/**
 ** Return the next entry from the file
 ** passed in fd.  The file's position is 
 ** advanced to the next '\n'.  If there is
 ** any error read to the next '\n' (if 
 ** possible) and return NULL.
 **/
struct IndexEntry *
ReadEntry(fd)
FILE *fd;

{
struct IndexEntry * ThisEntry;
char buffer[MAXPATHLEN];
unsigned long ThisVersion;
int ch;


    ThisEntry = NewEntry();
    if (ThisEntry != NULL) {
	if (fscanf(fd, "%s", buffer) < 0) {	/* read name */
	    goto error;
	} else {
	    if (SetEntryName(ThisEntry, buffer) != TRUE) {
		goto error;
	    }
	}

	if (fscanf(fd, "%s", buffer) < 0) {	/* read key */
	    goto error;
	} else {
	    if (SetEntryKey(ThisEntry, buffer) != TRUE) {
		goto error;
	    }
	}

	if (fscanf(fd, "%lx", &ThisVersion) < 0) {	/* read version */
	    goto error;
	} else {
	    if (SetEntryVersion(ThisEntry, ThisVersion) != TRUE) {
		goto error;
	    }
	}

	if (fscanf(fd, "%s", buffer) < 0) {	/* read data */
	    goto error;
	} else {
	    if (SetEntryData(ThisEntry, buffer) != TRUE) {
		goto error;
	    }
	}

	while (((ch = fgetc(fd)) != EOF) && (ch != '\n')) ;	/* gobble up the '\n' */
	if (ch == EOF) {
	    goto error;
	}
    }

    return ThisEntry;

error:
    DestroyEntry(ThisEntry);
    return NULL;
}


boolean
WriteEntry(fd, entry)
FILE *fd;
struct IndexEntry * entry;

{
    if (fprintf(fd, "%s\t%s\t%lx\t%s\n", entry->Name, entry->Key, entry->Version, entry->Data) < 0) {
	return FALSE;
    }
    return TRUE;
}


/**
 ** Return a new, empty entry or NULL if no space is available.
 **/
struct IndexEntry *
NewEntry()

{
struct IndexEntry * ThisEntry;

    ThisEntry = (struct IndexEntry *) malloc(sizeof(struct IndexEntry));
    if (ThisEntry == NULL) {
	return NULL;
    }

    ThisEntry->Name = NULL;
    ThisEntry->Key = NULL;
    ThisEntry->Version = class_VERSIONNOTKNOWN;
    ThisEntry->Data = NULL;

    return ThisEntry;
}


/**
 ** Return a new, initialized entry or NULL if no space is available.
 **/
struct IndexEntry *
CreateEntry(name, key, version, data)

{
struct IndexEntry * ThisEntry;

    ThisEntry = NewEntry();
    if (ThisEntry == NULL) {
	goto error;
    }

    if (SetEntryName(ThisEntry, name) != TRUE) {
	goto error;
    }
    if (SetEntryKey(ThisEntry, key) != TRUE) {
	goto error;
    }
    if (SetEntryVersion(ThisEntry, version) != TRUE) {
	goto error;
    }
    if (SetEntryData(ThisEntry, data) != TRUE) {
	goto error;
    }

    return ThisEntry;

error:
    DestroyEntry(ThisEntry);
    return NULL;
}


/**
 ** Destroy the entry and all the auxillary data
 ** allocated for that entry.
 **/
void
DestroyEntry(entry)
struct IndexEntry * entry;

{
    if (entry != NULL) {
	SafeFree(entry->Name);
	SafeFree(entry->Key);
	SafeFree(entry->Data);
	SafeFree(entry);
    }

    return;
}
    


char * 
GetEntryName(entry)
struct IndexEntry * entry;

{
    if (entry == NULL) {
	return NULL;
    } else {
	return entry->Name;
    }
}



char * 
GetEntryKey(entry)
struct IndexEntry * entry;

{
    if (entry == NULL) {
	return NULL;
    } else {
	return entry->Key;
    }
}



unsigned long
GetEntryVersion(entry)
struct IndexEntry * entry;

{
    if (entry == NULL) {
	return NULL;
    } else {
	return entry->Version;
    }
}



char * 
GetEntryData(entry)
struct IndexEntry * entry;

{
    if (entry == NULL) {
	return NULL;
    } else {
	return entry->Data;
    }
}


boolean
SetEntryName(entry, name)
struct IndexEntry * entry;
char * name;

{
char * FreeString;

    if (entry == NULL) {
	return FALSE;
    }

    FreeString = entry->Name;

    if (name == NULL) {
	entry->Name = NULL;
    } else {
	entry->Name = (char *) malloc(strlen(name) + 1);
	if (entry->Name == NULL) {
	    entry->Name = FreeString;	/* restore this */
	    return FALSE;
	}
	strcpy(entry->Name, name);
    }
    SafeFree(FreeString);
    return TRUE;
}



boolean
SetEntryKey(entry, key)
struct IndexEntry * entry;
char * key;

{
char * FreeString;

    if (entry == NULL) {
	return FALSE;
    }

    FreeString = entry->Key;

    if (key == NULL) {
	entry->Key = NULL;
    } else {
	entry->Key = (char *) malloc(strlen(key) + 1);
	if (entry->Key == NULL) {
	    entry->Key = FreeString;	/* restore this */
	    return FALSE;
	}
	strcpy(entry->Key, key);
    }
    SafeFree(FreeString);
    return TRUE;
}



boolean
SetEntryVersion(entry, version)
struct IndexEntry * entry;
unsigned long version;

{
    if (entry == NULL) {
	return FALSE;
    }

    entry->Version = version;
    return TRUE;
}



boolean
SetEntryData(entry, data)
struct IndexEntry * entry;
char * data;

{
char * FreeString;

    if (entry == NULL) {
	return FALSE;
    }

    FreeString = entry->Data;

    if (data == NULL) {
	entry->Data = NULL;
    } else {
	entry->Data = (char *) malloc(strlen(data) + 1);
	if (entry->Data == NULL) {
	    entry->Data	= FreeString;	/* restore this */
	    return FALSE;
	}
	strcpy(entry->Data, data);
    }
    SafeFree(FreeString);
    return TRUE;
}

















#ifdef NOTDEF
/** 
 ** Update index file.
 **/
int
UdateIndex(entry)
IndexEntry * entry;


{
int infd;
int outfd;
struct IndexEntry ThisEntry;
struct IndexEntry * TestEntry;

    infd = open(INDEXFILE, O_RDONLY, 0);
    if (infd >= 0) {
	if (unlink(INDEXFILE) != 0) {
	    Error("can not remove old version of index file", "");
	}
    }




    


}

#endif


