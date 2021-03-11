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


 

#define cltextview_PREPROCESS 0
#define cltextview_POSTPROCESS 1

struct cltextview_observer
{
  struct basicobject * observer;
  procedure callBack;
  long rock;
};

class cltextview[cltextv] : textview[textv]  { 
    overrides:
      GetClickPosition(long position, long numberOfClicks, enum view_MouseAction action, long startLeft, long startRight, long *leftPos, long *rightPos);
      FullUpdate(enum view_UpdateType type, long left, long top, long width, long height);

    methods:
      AddClickObserver( struct basicobject * observer,
			procedure callBack, long rock );
      RemoveClickObserver( struct basicobject * observer );
      RemoveClick( struct basicobject * observer,
		 procedure callBack );
    classprocedures:
      FinalizeObject(struct cltextview *self);
      InitializeObject(struct cltextview *self) returns boolean;
    data:
      short maxObservers;			/* number of entries in observers table */
      struct cltextview_observer *observers;	/* table of observers */
      struct cursor *cursor;
};

