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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/supportviews/RCS/lprruler.c,v 2.12 1992/12/15 21:42:39 rr2b R6tape $";
#endif


 

/* lprruler.c		

	Code for the lprruler data object

*/

#include <class.h>
#include <lprruler.eh>

#define MAXFILELINE 255

	long
lprruler__Read( self, file, id )
	register struct lprruler  *self;
	register FILE  *file;
	register long  id;			/* !0 if data stream, 0 if direct from file*/
{
	/* reads a lprruler from -file-.  See file format in lprruler.ch */
	/* This routine reads the \enddata, if any. Its syntax is not checked */

	char s[MAXFILELINE + 2];
	char c;

	if ((c=getc(file)) == '\n') {}		/* COMPATIBILITY KLUDGE */
	else if (c == '\\') fgets(s, MAXFILELINE+2, file);	/* skip header */
	else ungetc(c, file);

	while (TRUE) {

		/* read the lines of the data stream */

		char *nl;
		if ((fgets(s, MAXFILELINE + 2, file)) == 0) 
			/* EOF or error */
			break;
		if (*s == '\\') 
			/* \enddata */
			break;

		nl = s + strlen(s) - 1;		/* point at last character */
		if (*nl = '\n')
			*nl = '\0';	/* delete newline*/

		/* process an input line of the data stream */
		
	}
	lprruler_NotifyObservers(self, lprruler_DATACHANGED);
	return dataobject_NOREADERROR;
}
	  
	long
lprruler__Write( self, file, writeID, level )
	register struct lprruler  *self;
	FILE  *file;
 	long  writeID;
	int  level;
{
	char head[50];
	long id = lprruler_UniqueID(self);
	if (self->header.dataobject.writeID != writeID) {
		/* new instance of write, do it */
		self->header.dataobject.writeID = writeID;
		sprintf(head, "data{%s, %d}\n", class_GetTypeName(self), id);
		fprintf(file, "\\begin%s", head);

		/* no contents */

		fprintf(file, "\\end%s", head);
	}
	return id;
}

	boolean
lprruler__InitializeObject(ClassID, self)
	struct classheader *ClassID;
	struct lprruler *self;
{
	return TRUE;
}

	void
lprruler__FinalizeObject(ClassID, self)
	struct classheader *ClassID;
	struct lprruler *self;
{
}
