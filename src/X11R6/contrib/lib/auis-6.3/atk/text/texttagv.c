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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/text/RCS/texttagv.c,v 1.6 1993/10/26 22:43:58 gk5g Exp $";
#endif

#include <andrewos.h>
#include <class.h>
#include <texttag.ih>
#include <environ.ih>
#include <fontdesc.ih>
#include <texttagv.eh>

#define DataObject(A) (A->header.view.dataobject)
#define Data(A) ((struct texttag *)(DataObject(A)))

void texttagv__Print(self, f, process, final, toplevel)
struct texttagv *self;
FILE *f;
char *process;
char *final;
int toplevel;
{
    struct texttag *tag;
    char buf[256];
    if(environ_Get("IndexOnly") != NULL) return;
    tag = Data(self);
    texttag_GetTag(tag,255,buf);
    fprintf(f,".iy \"TEXTTAG %s\"\n",buf);
}
struct view *texttagv__Hit(self,action,mousex,mousey,numberOfClicks) 
struct texttagv *self;
enum view_MouseAction action;
long mousex, mousey, numberOfClicks;
{
    return super_Hit(self,action,mousex,mousey,numberOfClicks) ;
}
#define FONTNAME "andy"
#define FONTSIZE 12
#define OFNAME "andy"
#define OFSIZE 8
boolean texttagv__InitializeObject(classID,self)
struct classheader *classID;
struct texttagv *self;
{
    struct fnotev *fv = (struct fnotev *) self;
    texttagv_SetDisplayStr(self,"@");
    fv->fd = fontdesc_Create(FONTNAME,0,FONTSIZE);
    fv->ofd = fontdesc_Create(OFNAME,0,OFSIZE);
    return TRUE;
}
boolean texttagv__InitializeClass(classID)
struct classheader *classID;
{
    return TRUE;
}
