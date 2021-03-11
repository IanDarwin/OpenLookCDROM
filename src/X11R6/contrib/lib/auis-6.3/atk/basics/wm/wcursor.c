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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/basics/wm/RCS/wcursor.c,v 1.9 1992/12/15 21:26:22 rr2b R6tape $";
#endif


 

#include <class.h>
#include <andyenv.h>
#include <fontdesc.ih>
#include <wgraphic.ih>
#include <view.ih>
#include <cursor.ih>
#include <im.ih>
#include <wmclient.h>
#include <mrl.ih>
#include <wcursor.eh>


	void 
wmcursor__ChangeShape(self)
	struct wmcursor *self;
{     
	struct  cursor *castedSelf  = (struct cursor *) self;
	struct mrl *mp;

	if(cursor_IsPosted(castedSelf) && castedSelf->posted->CursorsAreActive){
		wm_SelectWindow( 
			((struct wmgraphic *)im_GetDrawable(castedSelf->posted))
			->window);
		/* we expand wmcursor_SelectWMCursor() inline to
			avoid duplicate computations */
		if (castedSelf->fillFont == NULL) {
			for(mp = self->mrlist; mp != NULL; mp = mp->next)
			    if(mp->wmregion > 0){
				wm_SelectRegion(mp->wmregion);
				wm_SetStandardCursor(castedSelf->fillChar);
			    }
		}
		else {
			struct font *fd;
			fd = fontdesc_GetRealFontDesc( 
				castedSelf->fillFont, view_GetDrawable(castedSelf->view));
			for(mp = self->mrlist; mp != NULL; mp = mp->next)
			    if(mp->wmregion > 0){
				wm_SelectRegion(mp->wmregion);
				wm_SetCursor(fd, castedSelf->fillChar);
			    }
		}
	}

	if (cursor_IsWindowCursor(castedSelf)) {
		castedSelf->changed = TRUE;
		im_SetWindowCursor(castedSelf->windowim,  self);
	}
	if (cursor_IsProcessCursor(castedSelf)) {
		castedSelf->changed = TRUE;
		im_SetProcessCursor(self);
	}
}



	boolean 
wmcursor__InitializeObject(classID, self)
	struct classheader *classID;
	struct wmcursor *self;
{
	self->mrlist = NULL;
	return TRUE;
}

	void
wmcursor__FinalizeObject(classID, self)
	struct classheader *classID;
	struct wmcursor *self;
{
}

	boolean 
wmcursor__InitializeClass(classID)
	struct classheader *classID; 
{
	return TRUE;
}
