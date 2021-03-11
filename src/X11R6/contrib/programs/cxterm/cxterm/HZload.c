/*
 *	$Id: HZload.c,v 3.1 1994/06/06 09:41:36 ygz Exp $
 */

/***********************************************************
Copyright 1992, 1994 by Yongguang Zhang.  All Rights Reserved.

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the names of the authors not
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.

THE AUTHOR(S) DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
THE AUTHOR(S) BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

******************************************************************/

/*
 *  HZload.c		Routines to load external files (CIT, AssocList)
 */

#include "HZinput.h"

#include <stdio.h>
#include <ctype.h>
#include <errno.h>
#include <sys/param.h>
#include "data.h"

static int getprompt ();

/*
 * LoadCIT -- load the input table from .cit file
 */
int LoadCIT (screen, inputdir, name, encode, hztbl)
    TScreen *screen;
    char *inputdir;
    char *name;
    int encode;
    HZinputTable *hztbl;
{
    char filename[MAXPATHLEN];
    char tmpstr[80], magic[2];
    Boolean found = FALSE; 
    FILE *file;

    if (name[0] == '/') {
	/* try root directory first */
	strcpy (filename, name);
	strcat (filename, CIT_SUFFIX);
	if (access (filename, R_OK) == 0)
	    found = TRUE;
    }

    /* search name in different dirs */
    if ((! found) && inputdir) {
	register char *dir = inputdir;
	register char *pfilename;

	while (*dir && (! found)) {
	    /* copy from (dir) to (filename), till ':' or end of string */
	    pfilename = filename;
	    while ((*dir != '\0') && (*dir != ':'))
		*pfilename++ = *dir++ ;
	    *pfilename = '\0';

	    strcat (filename, "/");
	    strcat (filename, name);
	    strcat (filename, CIT_SUFFIX);
	    if (access (filename, R_OK) == 0)
		found = TRUE;

	    if (*dir == ':')
		dir++ ;	/* skip this ':', ready for next component dir */
	}
    }

    if ((! found) && (name[0] != '/')) {
	/* try current directory */
	strcpy (filename, name);
	strcat (filename, CIT_SUFFIX);
	if (access (filename, R_OK) == 0)
	    found = TRUE;
    }

    if (! found) {
	sprintf (tmpstr, "Unable to locate the input table for %s", name);
	HZiaShowMesg (screen, tmpstr);
	return(-1);
    }
    file = fopen (filename, "r");
    if (! file) {
	sprintf (tmpstr, "Unable to open the input table for %s, errno = %d",
		 name, errno);
	HZiaShowMesg (screen, tmpstr);
	return(-1);
    }
    HZiaShowMesg (screen, "Loading input table ...");
    (void) fread (magic, 2, 1, file);
    if (strncmp (magic, MAGIC_CIT, 2) != 0) {
	sprintf (tmpstr, "Not a legal %s file for %s", CIT_SUFFIX, name);
	HZiaShowMesg (screen, tmpstr);
	return(-1);
    }
    if (fread((char *)hztbl, sizeof(HZinputTable), 1, file) == 0) {
	sprintf(tmpstr, "Error in loading input table for %s", name);
	HZiaShowMesg (screen, tmpstr);
	return(-1);
    }
    if (hztbl->encode != encode) {
	sprintf(tmpstr, "%s is not a \"%s\" encoding input method", name,
		term->misc.hz_encoding);
	HZiaShowMesg (screen, tmpstr);
	return(-1);
    }
    if (hztbl->version != CIT_VERSION) {
	sprintf(tmpstr, "Version mismatched for %s, rerun tit2cit", name);
	HZiaShowMesg (screen, tmpstr);
	return(-1);
    }
    hztbl->trieList = (trieNode *) calloc (hztbl->sizeTrieList,
					   sizeof(trieNode));
    hztbl->hzList = (XChar2b *) calloc (hztbl->sizeHZList, sizeof(XChar2b));
    if ((! hztbl->hzList) || (! hztbl->trieList)) {
	    sprintf(tmpstr, "No memory to load input table for %s", name);
	    HZiaShowMesg (screen, tmpstr);
	    if (hztbl->trieList)  free ((char *)(hztbl->trieList));
	    if (hztbl->hzList)  free ((char *)(hztbl->hzList));
	    return(-1);
    }
    if ((fread ((char *)(hztbl->trieList), sizeof(trieNode),
		(int)hztbl->sizeTrieList, file) != hztbl->sizeTrieList) ||
	(fread ((char *)hztbl->hzList, sizeof(XChar2b),
		(int)(hztbl->sizeHZList), file) != hztbl->sizeHZList))
    {
	    sprintf (tmpstr, "Error in loading input table for %s", name);
	    HZiaShowMesg (screen, tmpstr);
	    free ((char *)(hztbl->trieList));
	    free ((char *)(hztbl->hzList));
	    return(-1);
    }
    fclose (file);
    return(0);
}

/*
 * UnloadCIT -- unload the input table, free the memory
 */
void UnloadCIT (hztbl)
    HZinputTable *hztbl;
{
    if (! hztbl)
	return;
    if (hztbl->trieList)
	free ((char *)(hztbl->trieList));
    if (hztbl->hzList)
	free ((char *)(hztbl->hzList));
    free ((char *)(hztbl));
}

/*************************** ASSOCIATION ***************************/

/* the maximum length of an association list? */
#define	MAX_ASSOC_LEN	65536		/* 64K x 2 bytes */
#define	MAX_LINE_LEN	256

AssocList *HZassocLoad (name, inputdir, term_encname)
    char *name, *inputdir;
    char *term_encname;
{
    char filename[MAXPATHLEN];
    Char strbuf[MAX_LINE_LEN];
    Boolean found = False;
    Boolean has_encode = False;
    Boolean has_prompt = False;
    FILE *file;
    int totallen = MAX_ASSOC_LEN;
    AssocList *alist;
    unsigned short lasthzidx = 0;	/* the last hz index */
    XChar2b *phptr;			/* append point in the phrase list */
    int phlen;				/* length of the phrase list */

    if (name == NULL)
	return (NULL);

    if (name[0] == '/') {
	/* try root directory first */
	strcpy (filename, name);
	if (access (filename, R_OK) == 0)
	    found = True;
    }

    /* search name in different dirs */
    if ((! found) && inputdir) {
	register char *dir = inputdir;
	register char *pfilename;

	while (*dir && (! found)) {
	    /* copy from (dir) to (filename), till ':' or end of string */
	    pfilename = filename;
	    while ((*dir != '\0') && (*dir != ':'))
		*pfilename++ = *dir++ ;
	    *pfilename = '\0';

	    strcat (filename, "/");
	    strcat (filename, name);
	    if (access (filename, R_OK) == 0)
		found = TRUE;

	    if (*dir == ':')
		dir++ ;	/* skip this ':', ready for next component dir */
	}
    }

    if ((! found) && (name[0] != '/')) {
	/* try current directory */
	strcpy (filename, name);
	if (access (filename, R_OK) == 0)
	    found = TRUE;
    }

    if (! found) {
	/* Don't use HZiaShowMesg(), 'cause the window is not yet created! */
	fprintf (stderr, "Unable to locate the association file %s\n", name);
	return (NULL);
    }
    file = fopen (filename, "r");
    if (! file) {
	(void) perror (filename);
	fprintf (stderr, "Unable to load the association list %s\n", name);
	return (NULL);
    }

    alist = XtNew (AssocList);
    alist->phrases = (XChar2b *) malloc (totallen * sizeof(XChar2b));
    if (! alist->phrases) {
	perror ("malloc");
	fprintf (stderr, "Unable to load the association list \"%s\"\n", name);
	XtFree ((char *)alist);
	fclose (file);
	return (NULL);
    }
    bzero ((char *)(alist->idxtbl), sizeof(alist->idxtbl));
    phptr = alist->phrases;
    phlen = 0;

    while (fgets ((char *)strbuf, MAX_LINE_LEN, file)) {

	if (strbuf[0] & 0x80)	/* reach the hanzi text */
	    break;

	if (strncmp ((char *)strbuf, "ENCODE:", 7) == 0) {
	    char encname[MAX_LINE_LEN];

	    getprompt(strbuf, encname);
	    if (HZencode (encname) != HZencode(term_encname)) {
		fprintf (stderr, "%s is not a \"%s\" association file\n",
			 name, term_encname);
		fclose (file);
		fprintf (stderr, "Unable to load the association list %s\n",
			 name);
		return (NULL);
	    }
	    has_encode = True;
	} else if (strncmp ((char *)strbuf, "PROMPT:", 7) == 0) {
	    getprompt(strbuf, (char *)(alist->prompt));
	    has_prompt = True;
	}
    }
    if (! has_encode) {
	fprintf (stderr, "keyword ENCODE: missing in file \"%s\"\n", name);
	fclose (file);
	return (NULL);
    }
    if (! has_prompt) {
	fprintf (stderr, "keyword PROMPT: missing in file \"%s\"\n", name);
	fclose (file);
	return (NULL);
    }

    do {
	Char *sptr;
	XChar2b abuf[MAX_LINE_LEN/2], *abufptr = abuf;
	int abuflen;
	unsigned short hzidx;

	if (! (strbuf[0] & 0x80))
	    continue;	/* not start with hanzi, ignore this line */

	hzidx = ((unsigned char)(strbuf[0] & 0x7f)) * 256
		+ (unsigned char)strbuf[1];
	sptr = strbuf + 2;
	while (*sptr) {
	    int nhz;
	    Char *sptr2;

	    while (*sptr && !(*sptr & 0x80))
		sptr++;	/* skip ASCII */
	    if (! *sptr)
		break;	/* end of this line */

	    sptr2 = sptr;
	    while (*sptr & 0x80) {
		sptr++;	 sptr++;	/* jump to the end of hz */
	    }
	    nhz = (sptr - sptr2) / 2;

	    if (nhz == 1) {
		abufptr->byte1 = *sptr2++;
		(abufptr++)->byte2 = *sptr2++;
	    } else {
		abufptr->byte1 = HZ_PHRASE_TAG;
		(abufptr++)->byte2 = nhz;
		while (nhz-- > 0) {
		    abufptr->byte1 = *sptr2++;
		    (abufptr++)->byte2 = *sptr2++;
		}
	    }
	}
	abuflen = abufptr - abuf;
	if (phlen + abuflen + 1 >= totallen) {
	    totallen += MAX_ASSOC_LEN;
	    alist->phrases = (XChar2b *) realloc ((char *)(alist->phrases),
						  totallen * sizeof(XChar2b));
	    phptr = alist->phrases + phlen;	/* the new position */
	}

	if (hzidx == lasthzidx) {	/* continue from last line ... */

	    memmove ((char *)phptr, (char *)abuf, abuflen * sizeof(XChar2b));

	} else {			/* start a new hanzi */

	    /* done with lasthzidx, move one step for the null terminator */
	    phptr++;  phlen++;
	    lasthzidx = hzidx;

	    if (alist->idxtbl[ hzidx ] == 0) {

		/* appending ... */
		alist->idxtbl[ hzidx ] = phlen;
		memmove ((char *)phptr, (char *)abuf,
			 abuflen * sizeof(XChar2b));

	    } else if (alist->idxtbl[ hzidx ] < phlen) {

		/* need to insert something in the middle */
		XChar2b *inspoint = alist->phrases + alist->idxtbl[hzidx];
		int i;

		/*
		 *  |<------- phlen ------------------->|
		 *    alist->
		 *    idxtbl[hzidx]       /-- for hzidx
		 *  |<------------->|    V
		 *  0...............XXXXXXXX0............
		 *  ^                       ^            ^
		 *  alist->phrases          inspoint     phptr
		 *  
		 * 
		 *                      a hole of abuflen
		 *			    <----->
		 *  0...............XXXXXXXX_______0............
		 *  ^                       ^                   ^
		 *  alist->phrases          inspoint            phptr
		 */

		/* find the insertion point and make a hole */
		while ((inspoint++)->byte1) ;
		inspoint--;
		memmove ((char *)(inspoint + abuflen), (char *)inspoint,
			 (phptr - inspoint) * sizeof(XChar2b));

		/* fill in the hole with new associations */
		memmove ((char *)inspoint, (char *)abuf,
			 abuflen * sizeof(XChar2b));

		/* shift all those indices that are after the inspoint */
		for (i = 0; i < MAX_NUM_HZ; i++ )
		    if (alist->phrases + alist->idxtbl[i] > inspoint)
			alist->idxtbl[i] += abuflen;

	    } else {

		/* Very wrong!  I don't know what is going on. */
		/* discard the previous one if any. */
		alist->idxtbl[ hzidx ] = phlen;
		memmove ((char *)phptr, (char *)abuf, abuflen*sizeof(XChar2b));
	    }
	}
	phlen += abuflen;
	phptr += abuflen;
	phptr->byte1 = '\0';	/* just null-pad it */
	phptr->byte2 = '\0';

    } while (fgets ((char *)strbuf, MAX_LINE_LEN, file));

    fclose (file);

    /* release some extra space */
    alist->phrases = (XChar2b *) realloc ((char *)(alist->phrases),
			(phlen + 1) * sizeof(XChar2b));
    return (alist);
}

static int getprompt (buffer, strbuf)
     char *buffer, *strbuf;
{
#define	is7space(c)	(isspace(c) && (!((c) & 0x80)))

    while (isspace (*buffer))  buffer++;
    while (*buffer && (! isspace(*buffer)))  buffer++;	/* skip 1st field */
    while (is7space (*buffer))  buffer++;
    if (! *buffer) {
	*strbuf = '\0';
	return (0);
    }
    while (*buffer && (! is7space(*buffer)))
	*strbuf++ = *buffer++;
    *strbuf = '\0';
    return(1);
}
