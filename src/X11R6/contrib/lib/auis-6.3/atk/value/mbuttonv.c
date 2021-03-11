

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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/value/RCS/mbuttonv.c,v 1.5 1992/12/15 21:47:14 rr2b R6tape $";
#endif
#include <class.h>
#include <mbuttonv.eh>
#include <atom.ih>
#include <atomlist.ih>
#include <rm.ih>
#include <buttonv.ih>
#include <ctype.h>

static struct atomlist *  AL_bodyfont;
static struct atomlist *  AL_bodyfont_size;
static struct atomlist *  AL_label;
static struct atomlist *  AL_placement;
static struct atomlist *  AL_forecolor;
static struct atomlist *  AL_backcolor;
static struct atomlist *  AL_style;
static struct atom *  A_long;
static struct atom *  A_string;

#define InternAtoms ( \
AL_bodyfont = atomlist_StringToAtomlist("bodyfont") ,\
AL_bodyfont_size = atomlist_StringToAtomlist("bodyfont-size") ,\
AL_label = atomlist_StringToAtomlist("label") ,\
AL_placement = atomlist_StringToAtomlist("placement") ,\
AL_forecolor = atomlist_StringToAtomlist("foreground-color")  ,\
AL_backcolor = atomlist_StringToAtomlist("background-color") ,\
AL_style = atomlist_StringToAtomlist("style") ,\
A_long = atom_Intern("long") ,\
A_string = atom_Intern("string") )


void mbuttonv__LookupParameters(self)
     struct mbuttonv * self;
{
  char * fontname;
  long fontsize;
  struct resourceList parameters[8];
  char *plc;
  struct buttonV *bv = (struct buttonV *) self;

  parameters[0].name = AL_label;
  parameters[0].type = A_string;
  parameters[1].name = AL_bodyfont;
  parameters[1].type = A_string;
  parameters[2].name = AL_bodyfont_size;
  parameters[2].type = A_long;
  parameters[3].name = AL_placement;
  parameters[3].type = A_string;
  parameters[4].name = AL_forecolor;
  parameters[4].type = A_string;
  parameters[5].name = AL_backcolor;
  parameters[5].type = A_string;
  parameters[6].name = AL_style;
  parameters[6].type = A_string;
  parameters[7].name = NULL;
  parameters[7].type = NULL;

  mbuttonv_GetManyParameters(self, parameters, NULL, NULL);

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
      plc = (char *)parameters[3].data;
  else
      plc = NULL;

  if(bv->mono == -10)
      bv->mono = (buttonV_DisplayClass(bv) & graphic_Monochrome);

  if(!bv->mono){
      if (parameters[4].found)
	  bv->prefs->colors[sbutton_FOREGROUND] = (char *) parameters[4].data;

      if (parameters[5].found)
	  bv->prefs->colors[sbutton_BACKGROUND] = (char *) parameters[5].data;
  }
  bv->columns = 1;
  bv->topdown = FALSE;
  bv->vertical = TRUE;

  if(plc != NULL){
      if(*plc) {
	  /* restore defaults */
	  bv->columns = 1;
	  bv->topdown = FALSE;
	  bv->vertical = TRUE;
      }
      while (*plc != '\0'){
	  switch (isupper(*plc)?tolower(*plc):*plc){
	      case 'h':
	      case 'c':
		  bv->vertical = FALSE;
		  break;
	      case 'v':
	      case 'r':
		  bv->vertical = TRUE;
		  break;
	      case 't':
		  bv->topdown = TRUE;
		  break;
	      case 'b':
		  bv->topdown = FALSE;
		  break;
	      case '1': case '2': case '3':
	      case '4': case '5': case '6':
	      case '7': case '8': case '9':
		  if(bv->fixedcolumns == 0) bv->columns = (int) (*plc - '0');
		  break;
	  }
	  plc++;
      }
  }
  else bv->vertical = TRUE;
  if (parameters[6].found)
	buttonV_HandleStyleString(bv, (char *)parameters[6].data);

  bv->fontsize = fontsize;
  bv->fontname = fontname;
  mbuttonv_CacheSettings(self);

}

boolean mbuttonv__InitializeObject(classID, self )
struct classheader *classID;
struct mbuttonv * self;
{
    mbuttonv_SetFixedCount(self,0);
    mbuttonv_SetFixedColumns(self,0);
    return TRUE;
}

boolean mbuttonv__InitializeClass(classID)
struct classheader *classID;
{
    InternAtoms;
    return TRUE;
}


