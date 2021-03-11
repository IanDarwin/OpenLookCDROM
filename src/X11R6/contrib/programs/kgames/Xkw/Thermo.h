/* $XConsortium: Thermo.h,v 1.5 90/12/19 18:46:00 converse Exp $ */

/* Copyright	Massachusetts Institute of Technology	1987, 1988
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of M.I.T. not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  M.I.T. makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * M.I.T. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL M.I.T.
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
 * OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 */

#ifndef _Thermo_h
#define _Thermo_h

/****************************************************************
 *
 * Thermo widget
 *
 ****************************************************************/

/* Resources:

 Name		     Class		RepType		Default Value
 ----		     -----		-------		-------------
 background	     Background		Pixel		XtDefaultBackground
 border		     BorderColor	Pixel		XtDefaultForeground
 borderWidth	     BorderWidth	Dimension	1
 destroyCallback     Callback		Pointer		NULL
 height		     Height		Dimension	0
 mappedWhenManaged   MappedWhenManaged	Boolean		True
 sensitive	     Sensitive		Boolean		True
 width		     Width		Dimension	0
 x		     Position		Position	0
 y		     Position		Position	0

*/

/* define any special resource names here that are not in <X11/StringDefs.h> */

#define XtNmercuryColor	"mercuryColor"
#define XtNtextColor	"textColor"
#define XtNtickColor	"tickColor"
#define XtNminimum	"minimum"
#define XtCMinimum	"Minimum"
#define XtNmaximum	"maximum"
#define XtCMaximum	"Maximum"
#define XtNcurrent	"current"
#define XtCCurrent	"Current"
#define XtNminorStep	"minorStep"
#define XtCMinorStep	"MinorStep"
#define XtNmajorStep	"majorStep"
#define XtCMajorStep	"MajorStep"
#define XtNminorStart	"minorStart"
#define XtCMinorStart	"MinorStart"
#define XtNmajorStart	"majorStart"
#define XtCMajorStart	"MajorStart"
#define XtNstartPad	"startPad"
#define XtCStartPad	"StartPad"
#define XtNendPad	"endPad"
#define XtCEndPad	"EndPad"
#define XtNminorTickLen	"minorTickLen"
#define XtCMinorTickLen	"MinorTickLen"
#define XtNmajorTickLen	"majorTickLen"
#define XtCMajorTickLen	"MajorTickLen"
#define XtNvertical	"vertical"
#define XtCVertical	"Vertical"

#define ThermoUnspecified   (32765)

/* declare specific ThermoWidget class and instance datatypes */

typedef struct _ThermoClassRec*	    ThermoWidgetClass;
typedef struct _ThermoRec*	    ThermoWidget;

/* declare the class constant */

extern WidgetClass thermoWidgetClass;

#endif /* _Thermo_h */
