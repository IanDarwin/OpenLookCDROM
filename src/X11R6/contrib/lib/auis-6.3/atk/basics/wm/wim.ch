/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */

/*
	$Disclaimer: 
*Permission to use, copy, modify, and distribute this software and its 
*documentation for any purpose is hereby granted without fee, 
*provided that the above copyright notice appear in all copies and that 
*both that copyright notice, this permission notice, and the following 
*disclaimer appear in supporting documentation, and that the names of 
*IBM, Carnegie Mellon University, and other copyright holders, not be 
*used in advertising or publicity pertaining to distribution of the software 
*without specific, written prior permission.
*
*IBM, CARNEGIE MELLON UNIVERSITY, AND THE OTHER COPYRIGHT HOLDERS 
*DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
*ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT 
*SHALL IBM, CARNEGIE MELLON UNIVERSITY, OR ANY OTHER COPYRIGHT HOLDER 
*BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
*DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, 
*WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS 
*ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
*OF THIS SOFTWARE.
* $
*/


 

/* 
 *  wim.ch
 * 	Class header file for the wm interface manager.
 *
 */

#define wmim_PROGRAMMERVERSION 1


class wmim[wim] : im  {
overrides:
    WhichWS() returns unsigned char *;
    PostMenus(struct menulist *menulist);
    PostCursor(struct rectangle *rec,struct cursor *cursor) ;

    ClearCursors(struct wmcursor *C);
    ClearCursorList();
    UpdateCursors();
    SetTitle(char *titleString);
    FromCutBuffer() returns FILE *;
    CloseToCutBuffer(FILE *writeFile);
    RotateCutBuffers(long count);
    AppendToCutBuffer(FILE *writeFile);
    /* These are window manager interface calls */
    SetWMFocus();
    ExposeWindow();
    HideWindow();
    VanishWindow();
    HandleRedraw();
    CreateWindow(char * host) returns boolean;

    RedrawWindow();

classprocedures:
    InitializeClass() returns boolean;
    InitializeObject(struct im *self) returns boolean;
    FinalizeObject(struct im *self);
    HandleFiles(long WaitTime, boolean beCheap) returns boolean;
    FlushAllWindows();

data:
    int cursorRegion; /* The region on which process and window cursors for this wmim are posted. */
    int menuRegion; /* Current wm region menus are installed on. WM dependent. */
    struct mlcacheNode *mlcache;
    struct cacheregion *freeRegions,*activeRegions;
    long buttonState;	/* button state as read in */
};

