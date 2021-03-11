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

/* $ACIS$ */

 

/* boxview.ch - box view definition and interface */
#define boxview_VERSION 1

/* internal interfaces */

#define getView(self) ((self) -> header.view)
#define getBox(self) ((struct box *)boxview_GetDataObject(self))


/* Interface definition */

class boxview: view {

overrides:
  FullUpdate(enum view_UpdateType how, long left, long top, long width, long height);
  Update();
  Hit(enum view_MouseAction action, long x, long y, long numberOfClicks) returns struct view *;
  DesiredSize(long width, long height, enum view_DSpass pass, long *dWidth, long *dheight) returns enum view_DSattributes;
  WantNewSize(struct view *requestor);
  LinkTree(struct view *parent);
  UnlinkNotification(struct view *unlinkedview);
  ObservedChanged(struct observable *changed, long status);
  Print(FILE *f, char *processor, char *finalFormat, boolean toplevel);

methods:
  ToggleDebug();
  Paste();
  DrawBox();
  BoxWidth() returns int;

classprocedures:
  InitializeClass() returns boolean;
  InitializeObject(struct boxview *self) returns boolean;
  FinalizeObject(struct	boxview *self);

data:
	boolean	updateNeeded;		    /* the box itself needs to be updated */
	long lastUpdate;		    /* modification timestamp of last update */
	struct view *child;		    /* view of contents */
};

/* end of boxview.ch */

