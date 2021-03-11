/* ********************************************************************** *\
 *         Copyright IBM Corporation 1991 - All Rights Reserved           *
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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/time/RCS/writestmpv.c,v 1.4 1992/12/15 21:57:11 rr2b R6tape $";
#endif

#include <class.h>
#include <writestmpv.eh>
#include <writestmp.ih>
#include <proctbl.ih>
#include <util.h>

/* Defined constants and macros */

/* External Declarations */

/* Forward Declarations */

/* Global Variables */
static struct menulist *writestampview_menulist = NULL;

static char *formats[] = {
  "Default~1", NULL,
  "H:MM AM/PM~10", "%u:%M %P",
  "Month DD YYYY~11", "%o %A, %Y",
  NULL, NULL};


static void MenuSetFormat(self, format)
     struct writestampview *self;
     char *format;
{
  struct writestamp *b = (struct writestamp *) writestampview_GetDataObject(self);

  writestamp_SetFormat(b, format);
  writestamp_SetModified(b);
}


boolean
writestampview__InitializeClass(c)
struct classheader *c;
{
/* 
  Initialize all the class data.
*/
  int i;
  char *menuname;
  char temp[250];
  struct proctable_Entry *proc = NULL;
  char menutitlefmt[200];
  char procname[200];

  writestampview_menulist = menulist_New();
  
  sprintf(procname, "%s-set-format", "writestamp");
  proc = proctable_DefineProc(procname, MenuSetFormat, &writestampview_classinfo, NULL, "Set the writestamp's inset's format.");
  
  sprintf(menutitlefmt, "%s,%%s", "Write Stamp");
  
  for (i=0; formats[i]; i+=2) {
    sprintf(temp, menutitlefmt, formats[i]);
    menuname = NewString(temp);
    menulist_AddToML(writestampview_menulist, menuname, proc, formats[i+1], 0);
  }
  
  return(TRUE);
}


boolean
writestampview__InitializeObject(c, self)
struct classheader *c;
struct writestampview *self;
{
/*
  Set up the data for each instance of the object.
*/
  struct menulist *ml;

  if writestampview_GetMenuList(self) menulist_Destroy(writestampview_GetMenuList(self));
  ml = menulist_DuplicateML(writestampview_menulist, self);
  writestampview_SetMenuList(self, ml);

  return(TRUE);
}


void
writestampview__FinalizeObject(c, self)
struct classheader *c;
struct writestampview *self;
{
  return;
}


