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

 

/* fillerview.ch - filler view definition and interface */
#define fillerview_VERSION 1

/* reference to the associated data object */

#define getView(V) ((V) -> header.view)
#define getIM(V) view_GetIM(&getView(V))
#define getDrawable(V) view_GetDrawable(&getView(V))
#define MyFiller(V) ((struct filler *)(getView(V).dataobject))

/* Interface definition */

class fillerview[fillerv]: celview[celv] {

overrides:
  FullUpdate(enum view_UpdateType how, long left, long top, long width, long height);
  Update();
  Hit(enum view_MouseAction action, long x, long y, long numberOfClicks) returns struct view *;
  ReceiveInputFocus();
  LoseInputFocus();

methods:
  SetDataObjectByName(char *dataname);	/* replace myself */

classprocedures:
  InitializeClass() returns boolean;	/* initialize class */
  InitializeObject(struct fillerview *V) returns boolean;	/* initialize instance */
  FinalizeObject(struct	fillerview *V);	/* clean up instance */

data:
	boolean hasInputFocus;
        struct menulist *menulist;
	int hitindex;
};

/* end of fillerview.ch */

