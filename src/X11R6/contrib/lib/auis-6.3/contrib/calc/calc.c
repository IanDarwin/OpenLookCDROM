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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/calc/RCS/calc.c,v 1.4 1993/05/04 01:40:10 susan Exp $";
#endif

/* $Header $ */
/* $Source $ */

#ifndef lint
static char *rcsidcalc = "$Header $";
#endif

/*
    $Log: calc.c,v $
 * Revision 1.4  1993/05/04  01:40:10  susan
 * RCS Tree Split
 *
 * Revision 1.3.1.1  1993/02/02  05:05:33  rr2b
 * new R6tape branch
 *
 * Revision 1.3  1992/12/15  21:48:53  rr2b
 * more disclaimerization fixing
 *
 * Revision 1.2  1992/12/14  21:04:49  rr2b
 * disclaimerization
 *
 * Revision 1.1  1992/10/05  20:07:19  susan
 * Initial revision
 *
 * Revision 1.4  1991/09/12  16:02:50  bobg
 * Update copyright notice and rcsid
 *
 * Revision 1.3  1989/08/04  17:08:38  tom
 * Accept keyboard inputs;
 * Suppress Shrink & Help Icons.
 *
 * Revision 1.2  89/05/26  18:24:20  tom
 * Use apt_LeftArea, etc.
 * 
 * Revision 1.1  89/05/10  20:58:06  tom
 * Initial revision
 * 
*/

/**  SPECIFICATION -- External Facility Suite  *********************************

TITLE	The Calc Data-object

MODULE	calc.c

VERSION	1.0

DESCRIPTION
	This is the suite of Methods that suport the Calc Data-object,
	a trivial example of an ATK Inset.

HISTORY
  02/23/88	Created (TCP)
  05/26/89	Use apt_LeftArea, etc (TCP)

END-SPECIFICATION  ************************************************************/

#include <andrewos.h>
#include "dataobj.ih"
#include "apt.h"
#include "apt.ih"
#include "calc.eh"

#define  Value			      self->value


boolean
calc__InitializeObject( classID, self )
  register struct classheader	     *classID;
  register struct calc		     *self;
  {
  IN(calc_InitializeObject);
  DEBUGst(RCSID,rcsidcalc);
  calc_SetAreaTitleFontName( self, "AndySans22b", apt_TopArea );
  calc_SetAreaLegendFontName( self, "Andy20i", apt_BottomArea );
  Value = 0;
  OUT(calc_InitializeObject);
  return TRUE;
  }

void
calc__FinalizeObject( classID, self )
  register struct classheader	     *classID;
  register struct calc		     *self;
  {
  IN(calc_FinalizeObject);
  DEBUGst(RCSID,rcsidcalc);
  /* Nop */
  OUT(calc_FinalizeObject);
  }

char *
calc__ViewName( self )
  register struct calc    	      *self;
  {
  IN(calc_ViewName);
  OUT(calc_ViewName);
  return "calcv";
  }

void
calc__SetValue( self, value )
  register struct calc    	      *self;
  register double		       value;
  {
  IN(calc__SetValue);
  Value = value;
  calc_SetModified( self );
  OUT(calc__SetValue);
  }

static
Reader( self )
  register struct calc	    	     *self;
  {
  register struct apt_field	     *field;

  IN(Reader);
  while ( field = calc_ReadObjectField( self ) )
    {
    if ( strcmp( "Value", field->name ) == 0 )
       sscanf( field->content, "%F", &Value );
    }
  OUT(Reader);
  }

long
calc__Read( self, file, id )
  register struct calc	    	     *self;
  register FILE			     *file;
  register long			      id;
  {
  register long			      status; 

  IN(calc__Read);
  if ( (status = calc_ReadObject( self, file, id, Reader )) ==
	dataobject_NOREADERROR )
    {
    calc_NotifyObservers( self, calc_value_changed );
    }
  OUT(calc__Read);
  return status;
  }

static
Writer( self )
  register struct calc		     *self;
  {
  struct apt_field		      field;
  char				      value[25];

  IN(Writer);
  field.name = "Value";
  field.content = (char *)sprintf( value, "%f", Value );
  calc_WriteObjectField( self, &field );
  OUT(Writer);
  }

long
calc__Write( self, file, id, level )
  register struct calc		     *self;
  register FILE			     *file;
  register long			      id;
  register int			      level;
  {
  IN(calc_Write);
  calc_WriteObject( self, file, id, level, Writer );
  OUT(calc_Write);
  return  id;
  }
