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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/chart/RCS/chartcsn.c,v 1.8 1993/05/04 01:11:11 susan Exp $";
#endif

int chartscn_debug = 0;

#define debug chartscn_debug


/* $Header $ */
/* $Source $ */





/**  SPECIFICATION -- External Facility Suite  *********************************

TITLE	The Cartesian Chart View-object

MODULE	chartcsn.c

VERSION	0.0

AUTHOR	TC Peters
	Information Technology Center, Carnegie-Mellon University 

DESCRIPTION
	This is the suite of Methods that support the Chart View-object.

    NB: The comment-symbol "===" indicates areas which are:
	    1 - Questionable
	    OR
	    2 - Arbitrary
	    OR
	    3 - Temporary Hacks
    Such curiosities need be resolved prior to Project Completion...


HISTORY
  03/28/89	Created (TCP)
  05/31/89	Add classID parameter in FinalizeObject (TCP)

END-SPECIFICATION  ************************************************************/

#include <stdio.h>
#include <math.h>
#include "class.h"
#include "graphic.ih"
#include "observe.ih"
#include "view.ih"
#include "im.ih"
#include "rect.h"
#include "apt.h"
#include "aptv.ih"
#include "chartobj.ih"
#include "chart.ih"
#include "chartcsn.eh"
#include <ctype.h>

#define  Screen			1
#define  Paper			2

#define  Bounds			(chartcsn_BodyBounds(self))
#define  Left			(chartcsn_BodyLeft(self))
#define  Top			(chartcsn_BodyTop(self))
#define  Width			(chartcsn_BodyWidth(self))
#define  Height			(chartcsn_BodyHeight(self))
#define  Right			(chartcsn_BodyRight(self))
#define  Bottom			(chartcsn_BodyBottom(self))

#define  PixelsPerUnit		(chartcsn_PixelsPerUnit( self ))
#define  PixelsPerInterval	(chartcsn_PixelsPerInterval( self ))

#define  Items			(chartcsn_Items(self))
#define  ItemBounds(shadow)	(chartcsn_ItemBounds(self,(shadow)))
#define  ItemLeft(shadow)	(chartcsn_ItemLeft(self,(shadow)))
#define  ItemTop(shadow)	(chartcsn_ItemTop(self,(shadow)))
#define  ItemWidth(shadow)	(chartcsn_ItemWidth(self,(shadow)))
#define  ItemHeight(shadow)	(chartcsn_ItemHeight(self,(shadow)))
#define  ItemCenter(shadow)	(chartcsn_ItemCenter(self,(shadow)))
#define  ItemMiddle(shadow)	(chartcsn_ItemMiddle(self,(shadow)))
#define  NextItem(shadow)	(chartcsn_NextItem(self,(shadow)))

boolean 
chartcsn__InitializeObject( classID, self)
  register struct classheader	 *classID;
  register struct chartcsn	 *self;
  {
  IN(chartcsn_InitializeObject);
  chartcsn_SetShrinkIcon( self, 'e', "icon12", "CartesianChart", "andysans10b" );
  chartcsn_SetHelpFileName( self, "/usr/andy/help/ez.help"/*=== ===*/ );
  OUT(chartcsn_InitializeObject);
  return  TRUE;
  }

void
chartcsn__FinalizeObject( classID, self )
  register struct classheader	 *classID;
  register struct chartcsn	 *self;
  {}

void
chartcsn__SetDebug( self, state )
  register struct chartcsn	 *self;
  register char			  state;
  {
  IN(chartcsn_SetDebug);
  super_SetDebug( self, debug = state );
  OUT(chartcsn_SetDebug);
  }

struct view *
chartcsn__HitChart( self, action, x, y, clicks )
  register struct chartcsn	     *self;
  register enum view_MouseAction      action;
  register long			      x, y, clicks;
  {

  IN(chartcsn_HitChart);
/*===*/
  OUT(chartcsn_HitChart);
  return  (struct view *) self;
  }

void
chartcsn__DrawChart( self )
  register struct chartcsn	     *self;
  {
  IN(chartcsn_DrawChart);
/*===*/
  OUT(chartcsn_DrawChart);
  }

void
chartcsn__PrintChart( self )
  register struct chartcsn	     *self;
  {
  IN(chartcsn_PrintChart);
/*===*/
  OUT(chartcsn_PrintChart);
  }


/*
    $Log: chartcsn.c,v $
 * Revision 1.8  1993/05/04  01:11:11  susan
 * RCS Tree Split
 *
 * Revision 1.7.1.1  1993/02/02  01:16:31  rr2b
 * new R6tape branch
 *
 * Revision 1.7  1992/12/15  21:29:20  rr2b
 * more disclaimerization fixing
 *
 * Revision 1.6  1992/12/14  23:20:33  rr2b
 * add $Logs back after disclaimerization took them out
 *
 * Revision 1.4  1992/08/25  19:40:53  rr2b
 * hacked to avoid global name conflicts
 * .
 *
 * Revision 1.3  1991/09/12  16:04:28  bobg
 * Update copyright notice and rcsid
 *
 * Revision 1.2  1989/05/31  17:24:27  tom
 * y
 * Added classID as parameter to FinalizeObject.
 *
 * Revision 1.1  89/04/28  22:44:56  tom
 * Initial revision
 * 
*/
