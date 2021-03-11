/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1989 - All Rights Reserved      *
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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/value/RCS/controlv.c,v 2.13 1992/12/15 21:46:29 rr2b R6tape $";
#endif


 


#include <class.h>
#include <buttonv.ih>
#include <controlv.eh>
#include <proctbl.ih>
#include <message.ih>
#include <fontdesc.ih>
#include <rect.h>
#include <value.ih>
#include <atom.ih>
#include <atomlist.ih>
#include <rm.ih>
#include <im.ih>
#include <cursor.ih>

static boolean Inhibit;
static struct atomlist *  AL_bodyfont;
static struct atomlist *  AL_bodyfont_size;
static struct atomlist *  AL_label;
static struct atomlist *  AL_class;
static struct atomlist *  AL_function;
static struct atomlist *  AL_AutoInit;
static struct atomlist *  AL_forecolor;
static struct atomlist *  AL_backcolor;
static struct atomlist *  AL_style;

static struct atom *  A_long;
static struct atom *  A_string;
static struct cursor *WaitCursor;
#define InternAtoms ( \
   AL_bodyfont = atomlist_StringToAtomlist("bodyfont") ,\
   AL_bodyfont_size = atomlist_StringToAtomlist("bodyfont-size") ,\
   AL_label = atomlist_StringToAtomlist("label") ,\
   AL_class = atomlist_StringToAtomlist("class") ,\
   AL_function = atomlist_StringToAtomlist("function") ,\
   AL_AutoInit = atomlist_StringToAtomlist("Auto-Init") ,\
   AL_forecolor = atomlist_StringToAtomlist("foreground-color")  ,\
   AL_backcolor = atomlist_StringToAtomlist("background-color") ,\
   AL_style = atomlist_StringToAtomlist("style") ,\
   A_long = atom_Intern("long") ,\
   A_string = atom_Intern("string") )

static DoFunc(self)
struct controlV * self;
{
    char iname[256];
    struct proctable_Entry *pr;
    int (*proc)();
    int res;
    self->needsinit = FALSE;
    if(self->cclass && self->function){
	strcpy(iname,self->cclass);
	if(class_Load(iname) != NULL){
	    strcat(iname,"-");
	    strcat(iname,self->function);
	    if((pr = proctable_Lookup(iname)) != NULL && proctable_Defined(pr) ){
		proc = proctable_GetFunction(pr) ;
		if(WaitCursor) im_SetProcessCursor(WaitCursor);
		res = (*proc)(self,0);
		if(WaitCursor) im_SetProcessCursor(NULL);
		return res;
	    }
	}
    }
    return FALSE;
}
struct controlV *controlV__DoHit(self,type,x,y,hits )
struct controlV * self;
enum view_MouseAction type;
long x,y,hits;
{

    if (type == view_RightUp || type == view_LeftUp)
	DoFunc(self);
    return super_DoHit(self,type,x,y,hits );
}

void controlV__LookupParameters(self)
     struct controlV * self;
{
    struct buttonV *bv = (struct buttonV *) self;
  char * fontname;
  long fontsize;
  struct resourceList parameters[10];

  parameters[0].name = AL_label;
  parameters[0].type = A_string;
  parameters[1].name = AL_bodyfont;
  parameters[1].type = A_string;
  parameters[2].name = AL_bodyfont_size;
  parameters[2].type = A_long;
  parameters[3].name = AL_class;
  parameters[3].type = A_string;
  parameters[4].name = AL_function;
  parameters[4].type = A_string;
  parameters[5].name = AL_AutoInit;
  parameters[5].type = A_string;
  parameters[6].name = AL_forecolor;
  parameters[6].type = A_string;
  parameters[7].name = AL_backcolor;
  parameters[7].type = A_string;
  parameters[8].name = AL_style;
  parameters[8].type = A_string;
  parameters[9].name = NULL;
  parameters[9].type = NULL;

  controlV_GetManyParameters(self, parameters, NULL, NULL);

  if (parameters[0].found)
    bv->label = (char *)parameters[0].data;
  else
    bv->label = NULL;

  if (parameters[1].found)
    fontname = (char *)parameters[1].data;
  else
    fontname = "andytype";

  if (parameters[2].found)
    fontsize = parameters[2].data;
  else
    fontsize = 10;

  if (parameters[3].found)
    self->cclass = (char *)parameters[3].data;
  else
    self->cclass = NULL;

  if (parameters[4].found)
    self->function = (char *) parameters[4].data;
  else
      self->function = "start";

    if(bv->mono == -10)
	bv->mono = (buttonV_DisplayClass(bv) & graphic_Monochrome);

    if(!bv->mono){

      if (parameters[6].found)
	  bv->prefs->colors[sbutton_FOREGROUND] = (char *) parameters[6].data;

      if (parameters[7].found)
	  bv->prefs->colors[sbutton_BACKGROUND] = (char *) parameters[7].data;
  }
  if (parameters[8].found)
	buttonV_HandleStyleString(bv, (char *)parameters[8].data);

  self->autoinit = FALSE;
  if (parameters[5].found){
      char *foo;
      foo = (char *) parameters[5].data;
      if(*foo == 't' || *foo == 'T')
	  self->autoinit = TRUE;
  }

  if (fontsize != bv->fontsize || fontname != bv->fontname)
    {
      bv->fontsize = fontsize;
      bv->fontname = fontname;
    }
  controlV_CacheSettings(self);

}

boolean controlV__InitializeClass(classID)
struct classheader *classID;
{
  InternAtoms;
  WaitCursor = cursor_Create(NULL);
  if(WaitCursor) cursor_SetStandard(WaitCursor,Cursor_Wait);
  Inhibit = FALSE;
  return TRUE;
}

boolean controlV__InitializeObject(classID, self )
struct classheader *classID;
struct controlV * self;
{
    self->cclass = NULL;
    self->function = NULL;
    self->autoinit = FALSE;
    self->needsinit = TRUE;
    return TRUE;
}
void controlV__DrawFromScratch(self,x,y,width,height)
struct controlV * self;
long x,y,width,height;
{
    super_DrawFromScratch(self,x,y,width,height);
    if(self->autoinit && self->needsinit && !Inhibit){
	DoFunc(self);
    }
}

void controlV__InhibitAutoInit(ClassID)
struct classheader *ClassID;
{
    Inhibit = TRUE;
}
void controlV__SetAutoInit(ClassID,val)
struct classheader *ClassID;
boolean val;
{
    Inhibit = !val;
}

