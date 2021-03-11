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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/text/RCS/bpv.c,v 1.7 1993/10/26 22:43:58 gk5g Exp $";
#endif

#include <andrewos.h>
#include <class.h>
#include <view.ih>
#include <bpv.eh>

enum view_DSattributes bpv__DesiredSize(self, width, height, pass, desiredwidth, desiredheight)
struct bpv *self;
long width, height;
enum view_DSpass pass;
long *desiredwidth, *desiredheight;
{
    *desiredwidth = width;
    *desiredheight = 6;
    return(view_HeightFlexible | view_WidthFlexible);
}
void bpv__FullUpdate(self,type,left,top,width,height)
struct bpv *self;
enum view_UpdateType type;
long left,top,width,height;
{
    struct rectangle enclosingRect;
    enclosingRect.top = 0; enclosingRect.left = 0;
    enclosingRect.width  = bpv_GetLogicalWidth(self) -1 ;
    enclosingRect.height = bpv_GetLogicalHeight(self) -1 ;
    bpv_SetTransferMode(self,graphic_WHITE);
    bpv_EraseRect(self,&(enclosingRect));
    bpv_SetTransferMode( self, graphic_COPY );
    bpv_FillRect(self,&(enclosingRect) ,bpv_GrayPattern(self,4,16));
}
void bpv__Print(self, f, process, final, toplevel)
    struct bpv *self;
    FILE *f;
    char *process;
    char *final;
    boolean toplevel;
{
	fputs(".OC\n.bp\n",f);
    }
boolean bpv__InitializeObject(classID,self)
struct classheader *classID;
struct bpv *self;
{
    return TRUE;
}
