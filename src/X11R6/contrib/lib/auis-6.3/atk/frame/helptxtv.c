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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/frame/RCS/helptxtv.c,v 2.8 1992/12/15 21:36:09 rr2b R6tape $";
#endif


 

#define Text(self) ((struct text *) ((self)->header.view.dataobject))
#include <class.h>
#include <text.ih>
#include <message.ih>
#include <view.ih>
#include <cursor.ih>
#include <helptxtv.eh>

void helptextview__WantInputFocus(self,vw)
struct helptextview *self;
struct view *vw;
{   /* ignore requests for input focus */
}
struct view *helptextview__Hit(self, action, x, y, numberOfClicks)
struct helptextview *self;
enum view_MouseAction action;
long x;
long y;
long numberOfClicks;
{
    if (action == view_LeftDown  || action == view_LeftUp) 
	return super_Hit(self, action, x, y, numberOfClicks);
    return NULL;
}
static char *helptextview_getstartstring(self,buf)
struct helptextview *self;
char *buf;
{
    /* finds the starting string for the completion, sticks it in buf, and returns
	a pointer to the end of the string */
    /* currently assumes
      1. The completion string will be found as the first quoted string in the document.
      2. portions of the string following the last '/' are to be discarded.
      */
    /* assumes too much */
    register char *bp;
    register int i,len;
    struct text *doc = Text(self);
    len = text_GetLength(doc);
    for(i = 0;i < len; i++) if(text_GetChar(doc,i) == '`') break;
    if(i < len) i++;
    for(i++, bp = buf;i < len && ((*bp = text_GetChar(doc,i)) != '\''); i++)  bp++;
   if(i >= len) { *buf = '\0'; return buf; }
    while (*bp != '/' && bp != buf) bp--;
   if(*bp == '/') bp++;
    *bp = '\0';
    return bp;
}
void helptextview__GetClickPosition(self, position, numberOfClicks, action, startLeft, startRight, leftPos, rightPos)
struct helptextview *self;
long position;
long numberOfClicks;
enum view_MouseAction action;
long startLeft;
long startRight;
long *leftPos;
long *rightPos;
{   
    char buf[512], *bp;
    int pos;
    struct text *doc = Text(self);
    super_GetClickPosition(self, position,3, action, startLeft, startRight, leftPos, rightPos);
    if (action != view_LeftDown ) return;
    *buf = '\0';
    message_GetCurrentString(self,buf,512) ;
    if(*buf)message_DeleteCharacters(self, 0, strlen(buf));
    bp = helptextview_getstartstring(self,buf);
    /* grab the first unbroken word on the current line */
    for(pos = *leftPos; pos < *rightPos; pos++,bp++) {
	*bp = text_GetChar(doc,pos);
	if(*bp == '\n' || *bp == ' ' || *bp == '\t'){
	    break;
	}
    }
    *bp = '\0';
    message_InsertCharacters(self, 0, buf, bp - buf);
    message_SetCursorPos(self,bp - buf);
}
void helptextview__FullUpdate(self, type, left, top, width, height)
struct helptextview *self;
enum view_UpdateType type;
long left;
long top;
long width;
long height;
{
    struct rectangle tr;
    super_FullUpdate(self, type, left, top, width, height);
    helptextview_GetVisualBounds(self,&tr);
    helptextview_PostCursor(self,&tr,self->myCursor);
}
boolean helptextview__InitializeObject(classID, self)
struct classheader *classID;
struct helptextview *self;
{
    self->myCursor = cursor_Create(self);
    cursor_SetStandard(self->myCursor,Cursor_LeftPointer);
    return TRUE;
}
void helptextview__FinalizeObject(classID, self)
struct classheader *classID;
struct helptextview *self;
{
    cursor_Destroy(self->myCursor);
}
