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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/supportviews/RCS/strtbl.c,v 2.14 1993/12/15 22:44:02 rr2b Exp $";
#endif


 


/* strtbl.c		

String table data object

deferred:
	Probably ought to include some form of file header and trailer 
		even when not embedded in text
*/


#include <andrewos.h> /* strings.h */

#include <class.h>
#include <strtbl.eh>

#define MAXFILELINE 255


	boolean
stringtbl__InitializeObject(ClassID, self)
	struct classheader *ClassID;
	register struct stringtbl  *self;
{
	self->used = self->numacc = 0;
	self->ContainsInitialStrings = FALSE;
	stringtbl_AddString(self, "FIRST");
	stringtbl_AddString(self, "SECOND");
	stringtbl_AddString(self, "THIRD");
	self->ContainsInitialStrings = TRUE;

	return TRUE;
}

	void 
stringtbl__FinalizeObject(ClassID, self)
	struct classheader *ClassID;
	register struct stringtbl  *self;
{
	register short i;
	for (i = 0; i < self->used; i++)
		free (self->item[i]);
}


	long
stringtbl__Read( self, file, id )
	register struct stringtbl  *self;
	register FILE  *file;
	register long  id;			/* !0 if data stream, 0 if direct from file*/
{
	/* reads a stringtbl from -file-.  See file format in strtbl.ch */
	/* This routine reads the \enddata, if any. Its syntax is not checked */

	long highlight;
	char s[MAXFILELINE + 2];
	char c;

	if ((c=getc(file)) == '\n') {}		/* COMPATIBILITY KLUDGE */
	else if (c == '\\') fgets(s, MAXFILELINE+2, file);	/* skip header */
	else ungetc(c, file);

	stringtbl_Clear(self);
	fscanf(file, " %x ", &highlight);
	while (TRUE) {
		char s[MAXSTRINGENTRYLENGTH + 2], *nl;
		if ((fgets(s, MAXSTRINGENTRYLENGTH + 2, file)) == 0) 
			/* EOF or error */
			break;
		if (*s == '\\') 
			/* \enddata */
			break;
		nl = s + strlen(s) - 1;		/* point at last character */
		if (*nl = '\n')
			*nl = '\0';	/* delete newline*/
		stringtbl_AddString(self, s);
	}
	self->highlight = highlight;	/* (should use SetBit) */
	stringtbl_NotifyObservers(self, stringtbl_STRINGSCHANGED);
	return dataobject_NOREADERROR;
}
	  
	long
stringtbl__Write( self, file, writeID, level )
	register struct stringtbl  *self;
	FILE  *file;
 	long  writeID;
	int  level;
{
	char head[50];
	register short i;
	long id = stringtbl_UniqueID(self);
	if (self->header.dataobject.writeID != writeID) {
		/* new instance of write, do it */
		self->header.dataobject.writeID = writeID;
		sprintf(head, "data{%s, %d}\n", class_GetTypeName(self), id);
		fprintf(file, "\\begin%s", head);

		fprintf(file, "%x\n", self->highlight);
		for (i = 0; i < self->used; i++)
			fprintf(file, "%s\n", self->item[i]);

		fprintf(file, "\\end%s", head);
	}
	return id;
}

	void 
stringtbl__Clear( self )
  register struct stringtbl  *self;
		/* Clears the string table of existing strings */
{
 	register short i;
	for (i = self->used; --i >= 0; )
		free (self->item[i]);
	self->used = self->numacc = 0;
	stringtbl_NotifyObservers(self, stringtbl_STRINGSCHANGED);
	self->ContainsInitialStrings = FALSE;
}

	static short
FindString(self, s, startIndex)
	register struct stringtbl *self;
	register char *s;
/* Finds string s in self and returns its index.   returns -1 for failure.*/
{
	register short i;
	if (s == NULL || *s == '\0') return (-2);
	for (i = startIndex; i < self->used; i++) {
		if (strcmp(s, self->item[i]) == 0)
			return (i);
	}
	return (-1);
}
	static void
SetIthBit(self, i, val)
	register struct stringtbl *self;
	short i;
	boolean val;
		/* finds the named string in the table 
			and sets its associated bit to the given val */
{
	register unsigned long mask;
	if (i < 0) 	/* string not found */
		return;
	mask = ((unsigned long)1)<<i;
	if ( (val==TRUE) != ((self->highlight & mask) != 0) ) {
		/* bit must change, invert it */
		self->highlight ^= mask;
		stringtbl_NotifyObservers(self, stringtbl_BITSCHANGED);
	}
}

short stringtbl__GetEntryOfString(self, s, startIndex)
	register struct stringtbl *self;
	register char *s;
	short startIndex;
{
	register short i;

	i = FindString(self, s, startIndex);
	if (i >= 0) {
	    return self->accmap[i];
	}
	return -1;
}

	short
stringtbl__AddString(self, s)
	register struct stringtbl *self;
	register char *s;
{
	short len;
	register char *t;
	short i;
	
	if(s==NULL) return 0;
	/* check for full table, length of string, backslash, and duplicate entry */
	if (self->used >= MAXSTRINGSINTABLE)
		return (0);
	if (self->ContainsInitialStrings) {
		stringtbl_Clear(self);
		self->ContainsInitialStrings = FALSE;
	}
	len = strlen(s);
	if (len > MAXSTRINGENTRYLENGTH)
	    len=MAXSTRINGENTRYLENGTH;
	t=malloc(len+1);
	if(t==NULL) return (0);
	strncpy(t, s, len);	/* copy the string to new storage */
	s = t;
	s[len] = '\0';					/* and terminate it ! */
	while ((t= index(t, '\\')))	/* delete backslashes */
		strcpy(t, t+1);
	t = s;
	while ((t= index(t, '{')))	/* delete left brackets */
		strcpy(t, t+1);
	t = s;
	while ((t= index(t, '}')))	/* delete right brackets */
		strcpy(t, t+1);
	if ((i=FindString(self, s, 0)) >= 0)
	 	/* duplicate entry */
		return self->accmap[i];
	if (i == -2) return (0);		/* NULL or  \0 */
	/* add the new item as the i'th */
	i = self->used++;	/* set i and incr -used- */
	self->item[i] = s;
	/* turn off highlight for the new string */
	self->highlight &= ~(((unsigned long)1)<<i);
	stringtbl_NotifyObservers(self, stringtbl_STRINGSCHANGED);
	self->accmap[i] = ++self->numacc;
	return self->numacc;
}

	void
stringtbl__RemoveString(self, s)
	register struct stringtbl *self;
	register char *s;
		/* Removes the specified string from the table.  
			If the string is absent, the call is ignored */
{
	register short i;
	register unsigned long mask;
	if(s==NULL) return;
	if (self->ContainsInitialStrings) {
		stringtbl_Clear(self);
		self->ContainsInitialStrings = FALSE;
	}
	i = FindString(self, s, 0);
	if (i < 0) 	/* string not found */
		return;
	free(self->item[i]);	/* free the string store */
	mask = (((unsigned long)1)<<i)-1;
	self->highlight = 	/* shift highlights */
		(self->highlight & mask)
		|  ((self->highlight & ~(mask<<1)) >> 1);
	self->used --;	/* decrement number of entries */
	/* DO NOT DECREMENT numacc */
	for ( ; i < self->used ; i++ ) {
		/* shift remaining items */
		self->item[i] = self->item[i+1];
		self->accmap[i] = self->accmap[i+1];
	}
	stringtbl_NotifyObservers(self, stringtbl_STRINGSCHANGED);
}

	void
stringtbl__SetBit(self, s, val)
	register struct stringtbl *self;
	register char *s;
	boolean val;
		/* finds the named string in the table 
			and sets its associated bit to the given val */
{
	SetIthBit(self, FindString(self, s, 0), val);
}

	boolean
stringtbl__GetBit(self, s)
	register struct stringtbl *self;
	register char *s;  
		/* finds the named string in the table and 
			returns the current value of its bit */
{
	register short i;
	register unsigned long mask;
	i = FindString(self, s, 0);
	if (i < 0) 	/* string not found */
		return FALSE;
	mask = ((unsigned long)1)<<i;
	return ((self->highlight & mask) != 0L);
}

	void
stringtbl__ClearBits( self )
	struct stringtbl *self;
{
	self->highlight = 0L;
	stringtbl_NotifyObservers(self, stringtbl_BITSCHANGED);
}

	static short 
FindEntry(self, accnum)
	struct stringtbl *self;
	short accnum;
{
	int i;
	for (i = self->used;  i--; )
		if (self->accmap[i] == accnum) return i;
	return -1;
}

	void
stringtbl__RemoveEntry(self, accnum)
		/* remove the entry with the given accnum 
			If the string is absent, the call is ignored */
	struct stringtbl *self;
	short accnum;
{
	short i;
	if (self->ContainsInitialStrings) {
		stringtbl_Clear(self);
		self->ContainsInitialStrings = FALSE;
	}
	i = FindEntry(self, accnum);
	if (i >= 0)
		stringtbl_RemoveString(self, stringtbl_IthString(self, i));
}

	void
stringtbl__SetBitOfEntry(self, accnum, val)
		/* set the bit associated with the given accnum 
			If the string is absent, the call is ignored */
	struct stringtbl *self;
	short accnum;	
	boolean val;
{
	SetIthBit(self, FindEntry(self, accnum), val);
}

	boolean
stringtbl__GetBitOfEntry(self, accnum)
		/* return the bit for the given accnum 
			If the string is absent, returns FALSE */
	struct stringtbl *self;
	short accnum;
{
	short i = FindEntry(self, accnum);
	unsigned long mask = ((unsigned long)1)<<i;
	return (i >= 0 && (self->highlight & mask) != 0L);
}

	char *
stringtbl__GetStringOfEntry(self, accnum)
		/* return the string for the given accnum 
			If the string is absent, returns NULL */
	struct stringtbl *self;
	short accnum;
{
	short i = FindEntry(self, accnum);
	if (i >= 0) 
		return stringtbl_IthString(self, i);
	else return NULL;
}


char *stringtbl__ViewName(self)
struct stringtbl *self;
{
    return ("strtblview");
}
