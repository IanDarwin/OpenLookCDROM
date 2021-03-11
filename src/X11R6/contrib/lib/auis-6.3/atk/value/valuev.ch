/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1989 - All Rights Reserved      *
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


 

#include <updateq.ih>

enum valueview_MouseStates
{
  valueview_LeftIsDown,
  valueview_RightIsDown
};


class valueview[valuev] : view {
overrides:
  FullUpdate( enum view_UpdateType type, long x, long y, long width, long height );
  Update();
  ObservedChanged( struct observable * observed, long status );
  Hit(enum view_MouseAction type, long x, long y, long numberOfClicks)
    returns struct valueview *;
  DesiredSize(long width, long height, enum view_DSpass pass, long *dWidth, long *dheight) returns enum view_DSattributes;
  GetManyParameters( struct resourceList * resources,
		     struct atomlist * name,
		     struct atomlist * class );
  ReceiveInputFocus();
  LoseInputFocus();
methods:
  RequestUpdateFunction( procedure fp );
  RequestFullUpdate();

  DeactivationMask( struct graphic * mask );
  SetDeactivationTransfer( short transferMode );

  LookupParameters();
  GetDesiredParameters() returns struct resourceList *;
  Activate();
  Deactivate();
  Highlight();
  Dehighlight();

  DrawFromScratch( long x, long y, long width, long height );
  DrawActivation();
  DrawDeactivation();
  DrawNewValue();
  DrawBadValue();
  DrawNoChange();
  DrawDestroyed();
  DrawHighlight();
  DrawDehighlight();

  NewValue();			/* should be overridden if Update is inappropriate */
  BadValue();			/* defaults to a visible bell */
  NoChange();			/* override if nec. noop by defult */
  Changed(long status);		/* for status not understood by ObservedChanged */
  Destroyed();
  DoHit(enum view_MouseAction type, long x, long y, long numberOfClicks)
    returns struct valueview *; /* override hook. def. noop */
  OnTarget(long x , long y) returns boolean;
macromethods:
  Xval() (self->x)
  Yval() (self->y)
  Width() (self->width)
  Height() (self->height)
  Value() ((struct value *) self->header.view.dataobject)
classprocedures:
  InitializeClass() returns boolean;

data:
  long borderPixels;
  struct graphic * back;
  struct graphic * border;
  struct graphic * deactivationMask; /* a graphic for graying out (or */
				    /* otherwise clearing) the value. */
  short deactivationTransferMode;
  struct updateq * updateq;
  short active;			/* is this value on? or off? */
  enum valueview_MouseStates mouseState; /* the condition of the mouse. */
  short mouseIsOnTarget;	/* meaningful if a mouse button is */
				/* down, true if the mouse is over the */
				/* value.  This allows a universal */
				/* `pull away to abort' behaviour. */
  long x,y,width,height;	/* the area in which the distinctive */
				/* part of the value should be drawn */
  long viewx, viewy, viewwidth, viewheight;
  boolean HasInputFocus;
};






/* Parameters to a valueview

   border-width		long		Defaults to 1
   border		graphic		Defaults to black
   background		graphic		Defaults to white

*/

 /* Facts: */
 /* Valueviews are simple controls such as buttons, sliders, graphs, */
 /* and other bells and whistles.  Their behaviour is very */
 /* constrained when compared to views in general.  In particular, no */
 /* valueview should override FullUpdate, Update, or Hit.  Instead, */
 /* the methods listed below are for overriding. */

 /* The state of a valueview:

    A valueview has a background graphic, a border graphic, a border
    size, a mouse state, an activation state and graphic (maybe also a
    transfer function), and an update queue.

    The background and border stuff should be pretty self explanatory.
    Subclass creators need only be aware that by the time
    DrawFromScratch is invoked, the graphic will be properly dressed
    with a border and background.


    The mouse state is a member of
    ({Left down, Right down} x {On target, Off target}) U {Neither down}

    The use of a mouse with a value is fairly constrained.  When the
    mouse state is in {(__, On target)}, it should be highlighted (DrawHighlight will be
    called automagically).  Otherwise, the value should never be
    highlighted (DrawDehighlight will be called automagically).
    ValueViews should be predominantly up-transition sensative.  That
    is, Downing a valueview, pulling away, and upping, should under
    most circumstances be a noop (hence the highlight dehighlight
    rules).  The exceptions are those valueviews sensative not only
    to buttoning, but to dragging.  The rule here is that dragging
    should have no effect when dehighlighted: not that previous
    dragging should be undone by moving off the value.  NB: all downing
    is passed to valueviews via DoHit BUT, offtarget upping
    and offtarget moving are not.  Do not assume in valuecode that
    what goes down must come up.

    The activation concept relates to the idea that it maybe useful to
    present a control even at times when the luser should not use it.
    For example, a `next page' button when there are no more pages.
    At such times, the application should valueview_Deactivate the
    control.  A graphical indication of the deactivation is made, and
    incoming events are ignored.  NB: (1) observedChanged events are
    still passed, and (2) a deactivated valueview may still poll its
    dataobject (in the case, for example, of a fullupdate).

    Rather than calling wantupdate directly, valueviews should use
    the methods RequestUpdateFunction and RequestFullUpdate.
    Requested update functions are enqued and updates requested.  In
    the event of an Update, the queue is executed.  In the event of a
    FullUpdate, the queue is cleared, and the valueview is simply
    drawn from scratch.

 /* This comment generated <<Wed Jul  1 01:58:25 1987>> so look to the */
 /* source file date.  Remember, all comments are LIES */

/*
  Draw_________ methods are all invoked while the view has the
  right-to-update.  They should feel free to make graphic calls. 


  DrawFromScratch( x, y, width, height );  
  Draw this value assuming only a clean background and a restored
  graphics state..  The indicated
  rectangle is the area inside the border of the graphic for this
  valuev. (no default)


  The rest of the Draw______ functions may assume on entry that their
  graphic looks just like it did at the end of the last Draw______ method.

  DrawNewValue();
  The observed data has changed values.  Indicate the change.  (no default)

  DrawBadValue();
  The observed data feels maligned.  Punish the luser. (defaults to a
  bletcherous visible bell)

  DrawNoChange();
  Somebody set the observed data's value, but the value did not
  change. (default - do nothing)

  DrawDestroyed();
  The observed data has DISAPPEARED.  This should not be allowed to
  happen. (no default).
 
  DrawActivation();
  DrawDeactivation();
  Show the activation status of the value.  Defaults to laying on the
  activation graphic and pulling it off again.  As far as I know, these
  don't *have* to be idempostent, but I don't want to hear about bugs
  that arise should you override them with nonidempotent methods.

  DrawHighlight();
  DrawDehighlight();
  similarly. (no default)

  DoHit();
  Handle a mouse hit.  See the above discussion of mousing to see what
  will be passed through and what won't.

  NewValue, BadValue, NoChange, Changed();
  Handle a change in the observed data.  By default, each requests the
  corresponding Draw_____ function.

*/



