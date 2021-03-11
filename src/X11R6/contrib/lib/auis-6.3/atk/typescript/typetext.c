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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/typescript/RCS/typetext.c,v 2.14 1992/12/15 21:45:38 rr2b R6tape $";
#endif


 

/* typetext.c
 * Code for typescript to ignore styles and dataobjects in read text.
 *
 */

#define GETANDTEST(C,file) ((C = getc(file)) != EOF && C != NULL)
#define TESTEND(C) (C == EOF || C == NULL)

#include <class.h>
#include <typetext.eh>
#include <style.ih>
#include <stylesht.ih>

long typetext__HandleKeyWord(self, pos, keyword, file)
struct typetext *self;
long pos;
char *keyword;
FILE *file;  {
    register long c;
    if ((strcmp(keyword, "textdsversion") == 0) || 
	(strcmp(keyword, "define") == 0) ||
	(strcmp(keyword, "template") == 0) ){
   	 	while (GETANDTEST(c,file) && c != '}') ;
    		if(TESTEND(c)) return 0;
    		while (GETANDTEST(c,file) && c != '\n');
	}
    if(strcmp(keyword, "view") == 0)
	return super_HandleKeyWord(self, pos, keyword, file);
    return 0;
}

long typetext__HandleCloseBrace(self, pos, file)
struct typetext *self;
long pos;
FILE *file;  {
    return 0;
}

long typetext__HandleBegindata(self,pos,file)
struct typetext *self;
long pos;
FILE *file;
{
return super_HandleBegindata(self,pos,file);
}

char *typetext__ViewName(self)
    struct typetext *self;
{
    return "typescript";
}

boolean typetext__InitializeObject(classID, self)
    struct classheader *classID;
    struct typetext *self;
{
    self->hashandler = FALSE;
    typetext_SetCopyAsText(self,TRUE);
    typetext_SetWriteAsText(self,TRUE);
    typetext_SetObjectInsertionFlag(self,FALSE);
    typetext_ReadTemplate(self, "typescript", FALSE);
    return TRUE;
}

long typetext__GetModified(self)
    struct typetext *self;
{
    return 0;
}

