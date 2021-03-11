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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/annot/RCS/note.c,v 1.5 1992/12/15 21:50:50 rr2b R6tape $";
#endif


#include <andrewos.h>
#include <class.h>
#include <text.ih>
#include <sys/types.h>
#include <pwd.h>
#include <note.eh>


/****************************************************************/
/*		private functions				*/
/****************************************************************/



/****************************************************************/
/*		class procedures				*/
/****************************************************************/
boolean
note__InitializeClass(classID)
    struct classheader * classID;
{
    return TRUE;
}

boolean
note__InitializeObject(classID,self)
struct classheader * classID;
struct note * self;
{
    struct text * to;
    struct passwd *userentry;

    userentry = getpwuid(getuid());
    note_SetTitle(self,userentry->pw_name);

    to = text_New();
    text_SetReadOnly(to,0);
    note_SetChild(self,to);
    return TRUE;
}

/****************************************************************/
/*		instance methods				*/
/****************************************************************/

void
note__SetChild(self,child)
    struct note * self;
    struct dataobject * child;
{
    super_SetChild(self,child);
    if (child != (struct dataobject *)0)
	text_SetReadOnly((struct text *)child, 0);
}
