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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/help/src/RCS/hlptextv.c,v 1.4 1992/12/15 21:36:09 rr2b R6tape $";
#endif

/* $ACIS$ */

#include <ctype.h>

#include <class.h>
#include <style.ih>
#include <envrment.ih>
#include <text.ih>
#include <hlptextv.eh>

boolean hlptextview__InitializeClass(classID)
struct classheader *classID;
{
    return TRUE;
}

boolean hlptextview__InitializeObject(classID, self)
struct classheader *classID;
struct hlptextview *self;
{
    hlptextview_SetHyperlinkCheck(self, FALSE);
    return TRUE;
}

void hlptextview__FinalizeObject(classID, self)
struct classheader *classID;
struct hlptextview *self;
{
    return;
}

/* given a buffer and a position, return the largest string containing that position which includes no whitespace. The buffer may be mutilated as a side effect.
If the character at pos and pos+1 are both whitespace, return NULL. */
static char *TrimWhiteSpace(buf, pos)
char *buf;
int pos;
{
    int val;

    /* set buf to the start of the desired string */
    val = pos;
    while (val > 0 && (buf[val]=='\0' || isspace(buf[val]))) {
	if (buf[val] == '\n')  {
	    return NULL;
	}
	val--;
    }

    if (isspace(buf[val])) {
	return NULL;
    }

    while (val > 0 && !isspace(buf[val-1])) {
	val--;
    }

    buf = (buf+val);
    val = 0;
    while (buf[val] && !isspace(buf[val]))
	val++;
    buf[val] = '\0';
    return buf;
}

/* override */
void hlptextview__GetClickPosition(self, position, numberOfClicks, action, startLeft, startRight, leftPos, rightPos)
struct hlptextview *self;
long position, numberOfClicks;
enum view_MouseAction action;
long startLeft, startRight, *leftPos, *rightPos;
{
    super_GetClickPosition(self, position, numberOfClicks, action, startLeft, startRight, leftPos, rightPos);
    if (hlptextview_GetHyperlinkCheck(self)) {
	/* Check for a hypertext style.  For now, we check for a style named
	 * 'helptopic', although we should create a new attribute called
	 * 'hyperlink' and look for any style with that attribute.
	 */
	struct environment *env;
	struct style *style;
	char *stylename;
	char *buf;
	struct text *text;
	long textstart, textlen;
	static char topic[200];

	env = hlptextview_GetEnclosedStyleInformation(self, position, &textlen);
	while (env) {
	    style = env->data.style;
	    if (style) {
		stylename = style->name;
		if (stylename && strcmp(stylename, "helptopic") == 0) {
		    textstart = environment_Eval(env);
		    textlen = environment_GetLength(env);
		    text = (struct text *)hlptextview_GetDataObject(self);
		    if (text) {
			buf = text_GetBuf(text, textstart, textlen, &textlen);
			if (buf && textlen < sizeof(topic)) {
			    strncpy(topic, buf, textlen);
			    topic[textlen] = '\0';
			    self->hyperTopic = TrimWhiteSpace(topic, position-textstart);
			    self->action = action;
			    return;
			}
		    }
		}
	    }
	    env = (struct environment *) env->header.nestedmark.parent;
	}
    }
    self->hyperTopic = NULL;	/* no topic hit */
}
