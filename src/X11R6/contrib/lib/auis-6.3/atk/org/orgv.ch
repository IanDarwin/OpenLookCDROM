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






/**  SPECIFICATION -- External Facility Suite  *********************************

TITLE	The Org View-Class

MODULE	org.ch

VERSION	1.0

AUTHOR	TC Peters
	Information Technology Center, Carnegie-Mellon University 

DESCRIPTION
	This is the suite of Methods that support the Org View-Class.

    NB: The comment-symbol "===" indicates areas which are:
	    1 - Questionable
	    OR
	    2 - Arbitrary
	    OR
	    3 - Temporary Hacks
    Such curiosities need be resolved prior to Project Completion...


HISTORY
  01/19/89	Created (TCP)
  05/24/89	Add ExposeDescription (TCP)
  05/25/89	Add Arrangement variable (TCP)
  06/06/89	Add override for Update method (TCP)
  06/07/89	Add First-time variable (TCP)
  08/23/89	Remove Create method (TCP)

END-SPECIFICATION  ************************************************************/


#define  orgv_VERSION	      1

class orgv : aptv
  {
overrides:

  SetDataObject( struct org *data );
  DesiredSize( long width, long height, enum view_DSpass pass, long *dWidth, long *dheight )
								returns enum view_DSattributes;
  FullUpdate( enum view_UpdateType type, long left, long top, long width, long height );
  ReceiveInputFocus();
  LoseInputFocus();
  Hit( enum view_MouseAction action, long x, long y, long n)	returns struct view *;
  Print( FILE *file, char *processor, char *finalFormat, boolean topLevel );
  ObservedChanged( struct view *changed, long value );
  LinkTree( struct view *parent );

methods:

  SetHitHandler( (long *handler)(), struct view *anchor );
  SetDebug( boolean state );

macromethods:

  CurrentNode()    		      ((self)->current_node)

classprocedures:

  InitializeClass()					returns boolean;
  InitializeObject( struct orgv *self )			returns boolean;
  FinalizeObject( struct orgv *self );

data:

  struct org			 *data_object;
  struct lpair			 *pair_view;
  struct treev			 *tree_view_object;
  struct text			 *description_text;
  struct textview		 *description_textview;
  struct view			 *description_textview_scroll;
  struct tree_node		 *previous_node, *exploded_node;
  struct suite			 *suite_object;
  struct view			 *anchor;
  struct view		       *(*hit_handler)();
  struct rectangle		  bounds;
  struct menulist		 *menu;
  struct keystate		 *keystate;
  long				  last_modified, 
                                  initial_node_count,
				  description_last_modified;
  short				  arrangement, 
                                  node_border_style, 
                                  node_connector_style,
				  background_shade;
  boolean			  input_focus, 
                                  expose_controls, 
                                  controls_exposed,
				  fold, first_time,
				  expose_description, 
                                  description_exposed, 
                                  read_only,
				  exploded, 
                                  ignore_loseinputfocus, 
                                  ignore_fullupdate;
  };


/*
    $Log: orgv.ch,v $
*Revision 1.15  1993/05/04  01:27:02  susan
*RCS Tree Split
*
*Revision 1.14.1.1  1993/02/02  03:17:34  rr2b
*new R6tape branch
*
*Revision 1.14  1992/12/19  04:37:09  gk5g
*added LinkTree override
*.- there will be several checkins like this one.
*Many views that have children incorrectly link these children once, in InitializeObject.  These views must also provide a LinkTree because you can't assume that you are being linked into a valid viewtree (one with a path to the im), but it is assurred that once the tree is valid, the message will be distributed through the viewtree.
*.
*
*Revision 1.13  1992/12/14  23:20:33  rr2b
*add $Logs back after disclaimerization took them out
*
Revision 1.11  1992/11/11  22:39:12  gk5g
added call to lpair_LinkTree in FullUpdate... you must call LinkTree before InsertGraphic
.

Revision 1.10  1991/09/12  19:45:38  bobg
Update copyright notice

Revision 1.9  1990/03/02  18:45:04  gk5g
Removed the menu items (and suite items) Print, Save, and Quit.  These are not necessary (and, in fact, wrong) for org to have.  Frame (and buffer) handle those operations.  Removed the menu masks menu_application & menu_applicationlayer.  Removed the methods orgv_{Get,Delete}ApplicationLayer because they did nothing.  Removed the instance vars readonly, application, and org_app.  These are no longer necessary.

Revision 1.8  89/08/23  15:39:33  tom
Revise to Version 1.0

*/
