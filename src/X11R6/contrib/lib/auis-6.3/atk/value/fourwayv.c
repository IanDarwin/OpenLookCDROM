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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/value/RCS/fourwayv.c,v 2.12 1992/12/15 21:46:29 rr2b R6tape $";
#endif


 

#include <class.h>
#include <fourwayv.eh>
#include <atom.ih>
#include <atomlist.ih>
#include <rm.ih>
#include <buttonv.ih>
static struct atomlist *  AL_bodyfont;
static struct atomlist *  AL_bodyfont_size;
static struct atomlist *  AL_label;
static struct atomlist *  AL_forecolor;
static struct atomlist *  AL_backcolor;
static struct atomlist *  AL_style;

static struct atom *  A_long;
static struct atom *  A_string;
#define rectangle_TempSet(X,Y,W,H,R) ((R)->top = (Y), (R)->left = (X), \
				      (R)->height = (H), (R)->width = (W), (R))

#define InternAtoms ( \
   AL_bodyfont = atomlist_StringToAtomlist("bodyfont") ,\
   AL_bodyfont_size = atomlist_StringToAtomlist("bodyfont-size") ,\
   AL_label = atomlist_StringToAtomlist("':' separated labels") ,\
   AL_forecolor = atomlist_StringToAtomlist("foreground-color") ,\
   AL_backcolor = atomlist_StringToAtomlist("background-color") ,\
   AL_style = atomlist_StringToAtomlist("style") ,\
   A_long = atom_Intern("long") ,\
   A_string = atom_Intern("string") )



/****************************************************************/
/*		class procedures 				*/
/****************************************************************/




boolean fourwayV__InitializeClass(classID)
struct classheader *classID;
{
    InternAtoms;
    return TRUE;
}
boolean fourwayV__InitializeObject(classID,self)
struct classheader *classID;
struct fourwayV *self;
{
    fourwayV_SetFixedCount(self,4);
    fourwayV_SetFixedColumns(self,2);
    fourwayV_SetFourway(self,TRUE);
    return TRUE;
}

void fourwayV__LookupParameters(self)
struct fourwayV * self;
{
    char * fontname;
    long fontsize;
    struct resourceList parameters[7];
    struct buttonV *bv;
    bv = (struct buttonV *) self;

    parameters[0].name = AL_label;
    parameters[0].type = A_string;
    parameters[1].name = AL_bodyfont;
    parameters[1].type = A_string;
    parameters[2].name = AL_bodyfont_size;
    parameters[2].type = A_long;
    parameters[3].name = AL_forecolor;
    parameters[3].type = A_string;
    parameters[4].name = AL_backcolor;
    parameters[4].type = A_string;
    parameters[5].name = AL_style;
    parameters[5].type = A_string;
    parameters[6].name = NULL;
    parameters[6].type = NULL;

    fourwayV_GetManyParameters(self, parameters, NULL, NULL);

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

     if(bv->mono == -10)
	bv->mono = (buttonV_DisplayClass(bv) & graphic_Monochrome);

     if(!bv->mono){
	 if (parameters[3].found)
	     bv->prefs->colors[sbutton_FOREGROUND] = (char *) parameters[3].data;

	 if (parameters[4].found)
	     bv->prefs->colors[sbutton_BACKGROUND] = (char *) parameters[4].data;
     }
    if (parameters[5].found)
	buttonV_HandleStyleString(bv, (char *)parameters[5].data);

    if (fontsize != bv->fontsize || fontname != bv->fontname)
    {
	bv->fontsize = fontsize;
	bv->fontname = fontname;
    }
    bv->vertical = FALSE;
    fourwayV_CacheSettings(self);

}

