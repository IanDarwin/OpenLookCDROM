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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/text/RCS/textrefv.c,v 1.9 1993/10/26 22:43:58 gk5g Exp $";
#endif

#include <andrewos.h>
#include <class.h>
#include <textref.ih>
#include <textv.ih>
#include <text.ih>
#include <viewref.ih>
#include <environ.ih>
#include <envrment.ih>
#include <texttag.ih>
#include <fontdesc.ih>
#include <textrefv.eh>

#define DataObject(A) (A->header.view.dataobject)
#define Data(A) ((struct textref *)(DataObject(A)))

/*
#define PROCESSOR "/afs/andrew.cmu.edu/usr20/tpn/itc/amos/text/ezpostprocess"
#define TFILE "/afs/andrew.cmu.edu/usr20/tpn/itc/amos/text/tmac.arf"
*/

#define PROCESSOR environ_AndrewDir("/bin/ezpostprocess")
#define TFILE environ_AndrewDir("/lib/tmac/tmac.arf")

static boolean findtag(self,text,pos,env)
struct textrefv *self;
struct text *text;
long pos;
struct environment *env;
{
    char *foo,*name,buf[256];
    struct viewref *vr;
    if(self->cref != NULL && *self->cref != '\0' && env->type == environment_View){
	vr = env->data.viewref;
	name = class_GetTypeName(vr->dataObject);
	if(!class_IsTypeByName(name,"texttag"))
		return FALSE;
	foo = texttag_GetTag((struct texttag *)vr->dataObject,255,buf);
	if(strcmp(foo,self->cref) == 0) {
	    self->loc = pos;
	    return TRUE;
	}
    }
    return FALSE;
}
void textrefv__Print(self, f, process, final, toplevel)
struct textrefv *self;
FILE *f;
char *process;
char *final;
int toplevel;
{
    struct textref *ref;
    char buf[256];
    ref = Data(self);
    textref_GetRef(ref,255,buf);
    fprintf(f,"XXX  \\\"TEXTREF %s\n",buf);
    if(environ_Get("troffpostprocessor") == NULL){
	sprintf(buf,"%s ",PROCESSOR);
	strcat(buf,TFILE);
	environ_Put("troffpostprocessor",buf);
    }
}

struct view *textrefv__Hit(self,action,mousex,mousey,numberOfClicks) 
struct textrefv *self;
enum view_MouseAction action;
long mousex, mousey, numberOfClicks;
{
    struct textref *ref;
    char buf[256];
    struct text *txt;
    struct textview *tv;
 /*   long cpos,clen; */
    ref = Data(self);
    self->loc = -1;
    if( action == view_LeftUp && 
	(textref_IsOpen(ref) == FALSE) &&
	((txt = textrefv_GetParentText(self)) != NULL) &&
	((tv = textrefv_GetParentTextview(self)) != NULL) &&
	((self->cref = textref_GetRef(ref,255,buf)) != NULL) && 
	(text_EnumerateEnvironments(txt,0,text_GetLength(txt),findtag,(long) self) != NULL) &&
	self->loc >= 0){
/*
	cpos = textview_GetDotPosition(tv);
	clen = textview_GetDotLength(tv);
*/
	textview_SetDotPosition(tv,self->loc);   
	textview_SetDotLength(tv,1);   
/*
	textview_FrameDot(tv);
	textview_SetDotPos(tv,cloc);   
	textview_SetDotLength(tv,clen);   
*/
	textview_FrameDot(tv,self->loc);
	textview_WantUpdate(tv,tv);
	self->cref = NULL;
	return((struct view *)self);
    }
    self->cref = NULL;
    return super_Hit(self,action,mousex,mousey,numberOfClicks) ;
}
#define FONTNAME "andy"
#define FONTSIZE 12
#define OFNAME "andy"
#define OFSIZE 8
boolean textrefv__InitializeObject(classID,self)
struct classheader *classID;
struct textrefv *self;
{
    struct fnotev *fv = (struct fnotev *) self;
    textrefv_SetDisplayStr(self,"?");
    fv->fd = fontdesc_Create(FONTNAME,0,FONTSIZE);
    fv->ofd = fontdesc_Create(OFNAME,0,OFSIZE);
    return TRUE;
}
boolean textrefv__InitializeClass(classID)
struct classheader *classID;
{   
    return TRUE;
}
