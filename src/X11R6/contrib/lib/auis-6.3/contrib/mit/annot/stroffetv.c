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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/annot/RCS/stroffetv.c,v 1.5 1993/05/04 01:42:12 susan Exp $";
#endif


static char *stroffetview_rcsid = "$Header";

#include "class.h"
#include "view.ih"
#include "textv.ih"
#include "stroffet.ih"
#include "iconview.ih"
#include "stroffetv.eh"
#include "bind.ih"
#include "menulist.ih"
#include "keymap.ih"
#include "text.ih"
#include "proctbl.ih"

#define ICONFONT "icon"
#define ICONSTYLE "fontdesc_Plain"
#define ICONPTS 12
#define ICONCHAR '4'
#define TITLEFONT "andysans"
#define TITLESTYLE "fontdesc_Plain"
#define TITLEPTS 12

struct menulist *stroffetviewMenus;
static struct keymap *stroffetviewKeyMap;


/****************************************************************/
/*		private functions				*/
/****************************************************************/

static void
Close(v,l)
struct stroffetview *v;
long l;
{
    stroffetview_Close(v);
}
static void
open(v,l)
struct stroffetview *v;
long l;
{
    stroffetview_Open(v);
}
static void
closeall(v,l)
struct view *v;
long l;
{
    iconview_CloseRelated(v);
}
static void
openall(v,l)
struct view *v;
long l;
{
    iconview_OpenRelated(v);
}

static void
insert(tv,l)
struct textview *tv;
long l;
{
    struct text *t;
    long pos;
    t = (struct text *) textview_GetDataObject(tv);
    pos = textview_GetDotPosition(tv) + textview_GetDotLength(tv);
    tv->currentViewreference = text_InsertObject(t, pos,"stroffet", NULL);
    text_NotifyObservers(t,0);
}

static struct bind_Description stroffetviewBindings[]={
    {"stroffetview-close",NULL,0,"stroffets,close~2", 0,0,Close,"close the stroffet"},
    {"stroffetview-closeall",NULL,0,"stroffets,close all~12", 0,0,closeall,"close all the stroffets"},
    {"stroffetview-openall",NULL,0,"stroffets,open all~11", 0,0,openall,"open all the stroffets"},
    NULL
};
void stroffetview__PostMenus(self, menulist)
struct stroffetview *self;
struct menulist *menulist;
{
    menulist_ClearChain(self->menus);
    menulist_ChainBeforeML(self->menus, menulist, menulist);
    super_PostMenus(self, self->menus);
}


/****************************************************************/
/*		class procedures				*/
/****************************************************************/

boolean
stroffetview__InitializeClass(classID)
    struct classheader * classID;
{
    struct classinfo *textviewtype = class_Load("textview");
    struct classinfo *viewtype = class_Load("view");

    stroffetviewMenus = menulist_New();
    stroffetviewKeyMap =  keymap_New();
    bind_BindList(stroffetviewBindings, stroffetviewKeyMap , stroffetviewMenus, &stroffetview_classinfo);
    proctable_DefineProc("stroffetview-insertstroffet",insert,textviewtype,NULL,"Insert Stroffet Object");
    proctable_DefineProc("stroffetview-openallstroffets",openall,viewtype,NULL,"open Stroffet Views");
    proctable_DefineProc("stroffetview-closeallstroffets",closeall,viewtype,NULL,"close Stroffet Views");
    return TRUE;
}


boolean
stroffetview__InitializeObject(classID,self)
struct classheader * classID;
struct stroffetview * self;
{

    self->menus = menulist_DuplicateML(stroffetviewMenus, self);
    stroffetview_SetIconFont(self,ICONFONT,ICONSTYLE,ICONPTS); 
    stroffetview_SetIconChar(self,ICONCHAR);
    stroffetview_SetTitleFont(self,TITLEFONT,TITLESTYLE,TITLEPTS);


    return TRUE;
}

/****************************************************************/
/*		instance methods				*/
/****************************************************************/

void
stroffetview__Print(self, file, processor, finalformat, toplevel)
    struct stroffetview * self;
    FILE * file;
    char * processor;
    char * finalformat;
    boolean toplevel;
{
  struct textview * textvobj;
  struct text * textobj;
  long c, pos = 0, textlength = 0;
  

  if (self->header.iconview.child == (struct view *)0)
    return;


  textvobj = (struct textview *) self->header.iconview.bottomview;
  textobj = (struct text *) textview_GetDataObject(textvobj);

  textlength = text_GetLength(textobj);


  /* 
   * We don't want to be the toplevel.  We also don't want to
   * process with anything other than troff.  If we are, then we pout,
   * and refuse to print anything. 
   */
  if (!toplevel
      && strcmp(processor,"troff") == 0) 
    {
      while ((c = text_GetChar(textobj,pos)) != EOF 
	     && pos < textlength)
	{
	  fputc(c,file);
	  /*
	   * Formatting: Just Say No.
	   */
	  pos++; 
	}

      /* 
       * We want this to be newline-terminated, even if the user
       * doesn't put a newline at the end of the code segment.
       *
       * I'm confused.  If you track down the tree, text_GetChar is 
       * actually simpletext_GetChar.  Now, go and look at the code for
       * simpletext__GetChar.  It is defined as returning a long, and it
       * happily passes back a char *. 
       */
      if ((textlength != 0) && (text_GetChar(textobj, textlength - 1) != '\n'))
	{
	  fputc('\n',file);
	}
    }
}

