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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/supportviews/RCS/label.c,v 2.15 1993/05/04 01:32:13 susan Exp $";
#endif


 


/* label.c		

	Code for the label data object

*/

/* sys/types.h in AIX PS2 defines "struct label", causing a type name clash.
   Avoid this by temporarily redefining "label" to be something else in the preprocessor. */
#define label gezornenplatz
#include <andrewos.h> /* strings.h */
#undef label
#include <class.h>
#include <fontdesc.ih>
#include <label.eh>

#define MAXFILELINE 255

	boolean
label__InitializeObject(ClassID, self)
	struct classheader *ClassID;
	register struct label  *self;
{
	/* here we give initial values to any fields that need them */
	self->text = NULL;
	self->font = fontdesc_Create("andy", fontdesc_Bold, 10);
	self->flags = 0;
	label_SetText(self, "LABEL");
	return TRUE;
}

	void 
label__FinalizeObject(ClassID, self)
	struct classheader *ClassID;
	register struct label  *self;
{
	/* free any storage allocated to self */
	if (self->text) free(self->text);
}

	void
label__SetFlags( self, flags )
        register struct label *self;
        register int flags;
{
        self->flags = flags;
}



	long
label__Read( self, file, id )
	register struct label  *self;
	register FILE  *file;
	register long  id;			/* !0 if data stream, 0 if direct from file*/
{
	unsigned char fontfamily[50];
	long style, size;
	char text[257], *tail;
	char s[MAXFILELINE + 2];
	long c;

	if ((c=getc(file)) == '\n') {}		/* COMPATIBILITY KLUDGE */
	else if (c == '\\') fgets(s, MAXFILELINE+2, file);	/* skip header */
	else ungetc(c, file);

	/* reads a label from -file-.  See file format in label.ch */
	/* This routine reads the \enddata, if any. Its syntax is not checked */

	fscanf(file, " %s %d %d ", fontfamily, &style, &size);
	label_SetFont(self, fontfamily, style, size);

	*text = '\0';
	while (TRUE) {
		/* read the lines of the data stream */
		if ((fgets(s, MAXFILELINE + 2, file)) == 0) 
			/* EOF or error */
			break;
		if (*s == '\\') 
			/* \enddata */
			break;
		if (strlen(text) + strlen(s) <= 255)
			strcat(text, s);		
	}
	tail = text + strlen(text) - 1;		/* point at last character */
	if (*tail = '\n')
		*tail = '\0';				/* delete newline*/
	label_SetText(self, text);
	label_NotifyObservers(self, label_DATACHANGED);
	return dataobject_NOREADERROR;
}
	  
	long
label__Write( self, file, writeID, level )
	register struct label  *self;
	FILE  *file;
 	long  writeID;
	int  level;
{
	char head[50];
	char *fontfamily;
	long style, size;
	long id = label_UniqueID(self);
	if (self->header.dataobject.writeID != writeID) {
		/* new instance of write, do it */
		self->header.dataobject.writeID = writeID;
		sprintf(head, "data{%s, %d}\n", class_GetTypeName(self), id);
		fprintf(file, "\\begin%s", head);

		fontfamily = label_GetFont(self, &style, &size);
		fprintf(file, " %s %d %d\n", fontfamily, style, size);
	
		fprintf(file, "%s\n", label_GetText(self));

		fprintf(file, "\\end%s", head);
	}
	return id;
}

	void
label__SetText(self, text)
	register struct label *self;
	char *text;
{
	register char *s, *t;
	int length = strlen(text);
	if (length > 255) length = 255;
	if (self->text) free(self->text);
	self->text = NULL;
	s = malloc(length+1);
	strncpy(s, text, length);
	s[length] = '\0';
	t = s;
	while ((t= index(t, '\\')))	/* delete backslashes */
		strcpy(t, t+1);
	t = s;
	while ((t= index(t, '{')))	/* delete left brackets */
		strcpy(t, t+1);
	t = s;
	while ((t= index(t, '}')))	/* delete right brackets */
		strcpy(t, t+1);
	if (*s) self->text = s;
	label_NotifyObservers(self, label_DATACHANGED);
}
	void
label__SetFont(self, fontfamily, style, size)
	register struct label *self;
	char *fontfamily;
	long style, size;
{
	self->font = fontdesc_Create(fontfamily, style, size);
	label_NotifyObservers(self, label_DATACHANGED);
}
	char *
label__GetText(self)
	register struct label *self;
{
	return self->text;
}
	char *
label__GetFont(self, style, size)
	register struct label *self;
	long *style, *size;
{
	*style = fontdesc_GetFontStyle(self->font);
	*size = fontdesc_GetFontSize(self->font);
	return fontdesc_GetFontFamily(self->font);
}

