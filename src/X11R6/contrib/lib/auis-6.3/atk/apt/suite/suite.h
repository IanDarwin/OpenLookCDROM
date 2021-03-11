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


/* $Header $ */
/* $Source $ */





/**  SPECIFICATION -- External Facility Suite  *********************************

TITLE	The Suite-object

MODULE	suite.h

VERSION	0.0

AUTHOR	TC Peters & GW Keim
 	Information Technology Center, Carnegie-Mellon University

DESCRIPTION
	This is the suite of Methods that support the Suite-object.

    NB: The comment-symbol "===" indicates areas which are:
	    1 - Questionable
	    OR
	    2 - Arbitrary
	    OR
	    3 - Temporary Hacks
    Such curiosities need be resolved prior to Project Completion...


HISTORY
  11/01/88	Created (GW Keim)

END-SPECIFICATION  ************************************************************/

#define item_Normalized			    (1)
#define item_Highlighted		    (1<<1)
#define item_Active			    (1<<2)

#define	item_SetUpperLeft(item,X,Y) (item->bounds).left = (X);(item->bounds).top = (Y)
#define	item_SetDimensions(item,WIDTH,HEIGHT)\
    (item->bounds).width = (WIDTH);(item->bounds).height = (HEIGHT)

#define FONTHEIGHT(f) ((f)->max_bounds.ascent + (f)->max_bounds.descent)

#define item_Caption (item->caption?item->caption:item->name)
#define	item_Name (item->name?item->name:item->caption)
#define item_CaptionPlacement \
 ((item->captionplacement)?(item->captionplacement):(item->suite->captionplacement))
#define item_CaptionAlignment \
 ((item->captionalignment)?(item->captionalignment):(item->suite->captionalignment))
#define item_CaptionFontName \
 ((item->captionfontname)?(item->captionfontname):(item->suite->captionfontname))
#define item_CaptionFont \
 ((item->captionfont)?(item->captionfont):(item->suite->captionfont))

#if 1
#define item_CaptionFontSize \
 ((item->captionfontsize)?(item->captionfontsize):(item->suite->captionfontsize))
#else
#define item_CaptionFontSize (fontdesc_FontSummary(item_CaptionFont,suite_GetDrawable(item->suite))->newlineHeight)
#endif

#define item_CaptionFontType \
 ((item->captionfonttype)?(item->captionfonttype):(item->suite->captionfonttype))
#define item_Title				(item->title)
#define item_TitlePlacement \
 ((item->titleplacement)?(item->titleplacement):(item->suite->titleplacement))
#define item_TitleCaptionAlignment \
 ((item->titlecaptionalignment)?(item->titlecaptionalignment):\
 (item->suite->itemtitlecaptionalignment))
#define item_TitleFontName \
 ((item->titlefontname)?(item->titlefontname):(item->suite->titlefontname))
#define item_TitleFont \
 ((item->titlefont)?(item->titlefont):(item->suite->titlefont)
#define item_TitleFontSize \
 ((item->titlefontsize)?(item->titlefontsize):(item->suite->titlefontsize))
#define item_TitleFontType \
 ((item->titlefonttype)?(item->titlefonttype):(item->suite->titlefonttype))
#define item_BorderStyle \
 ((item->borderstyle)?(item->borderstyle):(item->suite->itemborderstyle))
#define item_BorderSize \
 ((item->bordersize)?(item->bordersize):(item->suite->itembordersize))
#define item_DataObjectName \
 ((item->dataobjectname)?(item->dataobjectname):(item->suite->itemdataobjectname))
#define item_DataObject				(item->dataobject)
#define item_DataObjectHandler \
 ((item->dataobjecthandler)?(item->dataobjecthandler):(item->suite->itemdataobjecthandler))
#define item_ViewObjectName \
 ((item->viewobjectname)?(item->viewobjectname):(item->suite->itemviewobjectname))
#define item_ViewObject				(item->viewobject)
#define item_ViewObjectHandler \
 ((item->viewobjecthandler)?(item->viewobjecthandler):(item->suite->itemviewobjecthandler))
#define item_HitHandler \
 ((item->hithandler)?(item->hithandler):(item->suite->hithandler))
#define item_Data \
 ((item->data)?(item->data):(item->suite->data))
#define item_Mode \
 ((item->mode)?(item->mode):(item->suite->mode))
#define item_AccessType \
 ((item->accesstype)?(item->accesstype):(item->suite->accesstype))
#define item_PassiveStyle \
 ((item->passivestyle)?(item->passivestyle):(item->suite->itempassivestyle))
#define item_HighlightStyle \
 ((item->highlightstyle)?(item->highlightstyle):(item->suite->itemhighlightstyle))
#define item_Debug				(item->debug)
#define item_Cursor \
    (item->cursor?item->cursor:item->suite->itemcursor)
#define item_CursorByte \
    (item->cursorbyte?item->cursorbyte:item->suite->itemcursorbyte)
#define item_CursorType \
    (item->cursortype?item->cursortype:item->suite->itemcursortype)
#define item_CursorFontName \
    (item->cursorfontname?item->cursorfontname:(item->suite->itemcursorfontname?item->suite->itemcursorfontname:item->suite->cursorfontname))
#define item_CursorFont \
    (item->cursorfont?item->cursorfont:item->suite->itemcursorfont)

#define NOFORCEDMODE				    (-1)

#define	Breaks(item)				    (item->breaks)
#define	BreakCount(item)			    (vector_Count(item->breaks))
#define	BreakPos(item,i)			    (vector_Item(item->breaks,i))

#define	item_ActiveForegroundColor (item->color ? item->color->foreground_name : item->suite->activeItemColor->foreground_name)
#define	item_ActiveBackgroundColor (item->color ? item->color->background_name : item->suite->activeItemColor->background_name)

#define	item_PassiveForegroundColor (item->color ? item->color->foreground_name : item->suite->passiveItemColor->foreground_name)
#define	item_PassiveBackgroundColor (item->color ? item->color->background_name : item->suite->passiveItemColor->background_name)

#define	item_CaptionColor (item->color ? item->color->caption_name : ((item->mode & item_Active) ? item->suite->activeItemColor->caption_name : item->suite->passiveItemColor->caption_name))


/*
    $Log: suite.h,v $
 * Revision 1.13  1993/05/04  01:06:17  susan
 * RCS Tree Split
 *
 * Revision 1.11.1.1  1993/02/02  00:45:07  rr2b
 * new R6tape branch
 *
 * Revision 1.11  1992/12/14  23:20:33  rr2b
 * add $Logs back after disclaimerization took them out
 *
 * Revision 1.9  1992/09/04  17:14:20  gk5g
 * Added caption color attributes
 * .
 *
 * Revision 1.8  1992/07/23  18:02:51  gk5g
 * Many changes:
 * 1) item borders are now drawn via sbutton
 * 2) several attributes have been removed and are not supported (font scaling attributes mainly -- CaptionFontHigh, CaptionFontLow, etc.)
 * 3) New attributes have been added to support color: suite_ForegroundColor, suite_BackgroundColor, suite_ActiveItemForegroundColor, .., suite_PassiveItemForegoundColor)
 * .
 *
 * Revision 1.7  1991/09/12  19:20:24  bobg
 * Update copyright notice
 *
 * Revision 1.6  1990/04/27  15:36:59  gk5g
 * Added item_Name macro.
 *
 * Revision 1.5  89/09/29  15:55:06  gk5g
 * Added macro item_RealCaptionFontSize.
 * 
 * Revision 1.4  89/09/08  17:00:31  gk5g
 * Added item_CaptionAlignment macro.
 * 
 * Revision 1.3  89/08/24  19:48:00  gk5g
 * Changes in support of V1.0 of the SuiteProgGuide.doc.
 * 
 * Revision 1.2  89/07/13  16:10:42  gk5g
 * Added macros for List item break points.
 * 
 * Revision 1.1  89/04/28  20:27:08  tom
 * Initial revision
 * 
*/
