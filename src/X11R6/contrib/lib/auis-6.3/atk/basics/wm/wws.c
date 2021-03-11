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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/basics/wm/RCS/wws.c,v 1.10 1992/12/15 21:26:22 rr2b R6tape $";
#endif


 

#include <class.h>
#include <andyenv.h>
#include <wmclient.h>
#include <wcursor.ih>
#include <wfontd.ih>
#include <wgraphic.ih>
#include <wim.ih>
#include <mrl.ih>
#include <wws.eh>

struct wmcursor *wmws__CreateCursor(self)
    struct wmws *self;
{
    return wmcursor_New();
}

struct wmfontdesc *wmws__CreateFontdesc(self)
    struct wmws *self;
{
    return wmfontdesc_New();
}

struct wmgraphic *wmws__CreateGraphic(self)
    struct wmws *self;
{
    return wmgraphic_New();
}

struct wmim *wmws__CreateIM(self)
    struct wmws *self;
{
    return wmim_New();
}

struct wmoffscrwin *wmws__CreateOffScreenWindow(self,host,width,height)
    struct wmws *self;
char * host;
long width, height;
{
/*
    return wmoffscrwin_Create(host,width,height);
*/
    return NULL;
}

boolean wmws__HandleFiles(self, WaitTime, beCheap)
    struct wmws *self;
    long WaitTime;
    boolean beCheap;
{
    return wmim_HandleFiles(WaitTime, beCheap);
}

void wmws__FlushAllWindows(self)
    struct wmws *self;
{
    wmim_FlushAllWindows();
}


boolean wmws__InitializeClass(classID)
struct classheader * classID;
{
    wmim_StaticLoadOnlyThisClass();
    wmfontdesc_StaticLoadOnlyThisClass();
    wmgraphic_StaticLoadOnlyThisClass();
    wmcursor_StaticLoadOnlyThisClass();
    mrl_StaticLoadOnlyThisClass();

    /* slimy way of getting all wm related modules together */
/*	%%% removed by pgc to get this running faster 
    wmim_StaticEntry;
    wmfontdesc_StaticEntry;
    wmgraphic_StaticEntry;
    wmcursor_StaticEntry;
    mrl_StaticEntry;
*/
    return TRUE;
}
