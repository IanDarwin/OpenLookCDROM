/* -*-C-*-
*******************************************************************************
*
* File:         xtangoloc.c
* RCS:          $Header: /disk3/npm/src/winterp/src-server/xtango/RCS/xtangoloc.c,v 2.8 1994/06/09 01:28:10 npm Exp $
* Description:  XTANGO ANIMATION PACKAGE (loc)
* Author:       John T. Stasko, Doug Hayes, Niels Mayer
* Created:      1990
* Modified:     Sun Jun  5 05:23:48 1994 (Niels Mayer) npm@indeed
* Language:     C
* Package:      N/A
* Status:	X11r6 contrib release
*
* Xtango 1.52 Copyright 1990-1994 Georgia Institute of Technology
* 			     (by John T. Stasko and Doug Hayes).
* WINTERP 2.0 Copyright (C) 1994, Enterprise Integration Technologies Corp. and Niels Mayer.
* WINTERP 1.15-1.99, Copyright (c) 1993, Niels P. Mayer.
* WINTERP 1.0-1.14, Copyright (c) 1989-1992 Hewlett-Packard Co. and Niels Mayer.
* 
* This version of Xtango 1.52 (varargs version) represents a subset of
* the Xtango distribution that has been modified specifically for use with
* WINTERP. Non-WINTERP uses of Xtango should use the complete, standard
* version of Xtango, which is available under separate copyright via
* anonymous ftp from par.cc.gatech.edu:pub/xtangovarargs.tar.Z and
* par.cc.gatech.edu:pub/xtango.tar.Z.
* 
* Permission to use, copy, modify, distribute, and sell this software and its
* documentation for any purpose is hereby granted without fee, provided that
* the above copyright notice appear in all copies and that both that
* copyright notice and this permission notice appear in supporting
* documentation, and that the name of Georgia Institute of Technology, 
* John T. Stasko, Doug Hayes, Enterprise Integration Technologies, 
* Hewlett-Packard Company, or Niels Mayer not be used in advertising or
* publicity pertaining to distribution of the software without specific,
* written prior permission. Georgia Institute of Technology, John T. Stasko,
* Doug Hayes, Enterprise Integration Technologies, Hewlett-Packard Company,
* and Niels Mayer makes no representations about the suitability of this 
* software for any purpose.  It is provided "as is" without express or
* implied warranty.
* 
* GEORGIA INSTITUTE OF TECHNOLOGY, JOHN T. STASKO, DOUG HAYES, ENTERPRISE
* INTEGRATION TECHNOLOGIES, HEWLETT-PACKARD COMPANY AND NIELS MAYER
* DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL IMPLIED
* WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL GEORGIA
* INSTITUTE OF TECHNOLOGY, JOHN T. STASKO, DOUG HAYES, ENTERPRISE INTEGRATION
* TECHNOLOGIES, HEWLETT-PACKARD COMPANY OR NIELS MAYER BE LIABLE
* FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER
* RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF
* CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
* CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
*
*******************************************************************************
*/
static char rcs_identity[] = "@(#)$Header: /disk3/npm/src/winterp/src-server/xtango/RCS/xtangoloc.c,v 2.8 1994/06/09 01:28:10 npm Exp $";

/* Modification Date  Description			      */
/* -----------------  --------------------------------------- */
/* 90/10/26 JDH	      Baselined source.                       */
/*							      */
/**************************************************************/

/**************************************************************/
/*****************	 include files       ******************/ 
/**************************************************************/

#include "xtangolocal.h"

#ifdef WINTERP
#include "xtangoversn.h" /* MIT X11r6 contrib requires < 14 char filenames */
#else /* !defined(WINTERP) */
#include "xtangoversion.h"
#endif /* WINTERP */


/**************************************************************/
/*****************	LOCAL data types     ******************/
/**************************************************************/

/**************************************************************/
/*****************	GLOBAL variables     ******************/
/**************************************************************/

ANIMATION_PTR	 TANGO__data = NULL;

/**************************************************************/
/*****************	LOCAL variables      ******************/
/**************************************************************/

/**************************************************************/
/*****************      LOCAL functions      ******************/
/**************************************************************/

/**************************************************************/
/*****************	  entry points       ******************/
/**************************************************************/

void	  TANGOinit();
TANGO_LOC TANGOloc_create();
WIN_COORD TANGOloc_X();
WIN_COORD TANGOloc_Y();
void	  TANGOloc_inquire();
TANGO_LOC TANGOloc_modify();
int	  TANGOloc_equal();


/**************************************************************/
/* TANGOinit -- Initialize TANGO globals and setup windows.   */
/*		(Only allowed once).			      */
/* 							      */
/* RETURNS:  None.					      */
/**************************************************************/
void
TANGOinit()
{
#ifdef WINTERP
  /* NPM: do nothing -- in WINTERP, this may get called for primitives which
     don't depend on TANGO__data structure since all xtango primitives have
     the following code:
	     "if (!TANGO__data) TANGOinit();"
     In some situations, TANGO__data==NULL after a TANGO:WIDGET_CLASS instance
     gets destroyed. In such cases, all TANGOpath_*() procedures would call
     TANGOinit() and do uneccesary things. Note that all WINTERP/Xtango 
     methods on TANGO:WIDGET_CLASS or TANGO:IMAGE_CLASS instances set
     TANGO__data prior to calling the associated xtango primitive. */
#else
   if (TANGO__data) return;

   fprintf(stderr, "\nXTANGO Version %4.02f\n\n", VERSION);

   TANGO_image_init();
   TANGO_setup_windows();
#endif /* WINTERP */
}



/**************************************************************/
/* TANGOloc_create -- Create a loc object given its two       */
/*		      coordinates.			      */
/* 							      */
/* RETURNS:  Newly created TANGO_LOC.			      */
/**************************************************************/
TANGO_LOC
TANGOloc_create(x,y)
   WIN_COORD x,y;
{
   TANGO_LOC loc;

   if (!TANGO__data) TANGOinit();

   DEBUG("TANGOloc_create(%f, %f)\n", x, y);

   loc = (TANGO_LOC) malloc( sizeof( struct _LOC));
   loc->x = x;
   loc->y = y;
   return(loc);
}



/**************************************************************/
/* TANGOloc_X -- Return the X component of the given LOC.     */
/* 							      */
/* RETURNS:  The X component of the given LOC.		      */
/**************************************************************/
WIN_COORD
TANGOloc_X(loc)
   TANGO_LOC loc;
{
   if (!TANGO__data) TANGOinit();

   DEBUG("TANGOloc_X(0x%lx)\n", (unsigned long) loc);

   return(loc->x);
}



/**************************************************************/
/* TANGOloc_Y -- Return the Y component of the given LOC.     */
/* 							      */
/* RETURNS:  The Y component of the given LOC.		      */
/**************************************************************/
WIN_COORD
TANGOloc_Y(loc)
   TANGO_LOC loc;
{
   if (!TANGO__data) TANGOinit();

   DEBUG("TANGOloc_Y(0x%lx)\n", (unsigned long) loc);

   return(loc->y);
}



/**************************************************************/
/* TANGOloc_inquire -- Get the x and y coordinates of the     */
/*		       given loc.			      */
/* 							      */
/* RETURNS:  None.					      */
/**************************************************************/
void
TANGOloc_inquire(loc,x,y)
   TANGO_LOC loc;
   WIN_COORD *x,*y;
{
   if (!TANGO__data) TANGOinit();

   DEBUG("TANGOloc_inquire(0x%lx, 0x%lx, 0x%lx)\n", (unsigned long) loc, (unsigned long) x, (unsigned long) y);

   *x = loc->x;
   *y = loc->y;
}



/**************************************************************/
/* TANGOloc_modify -- Modify the given loc's coords by the    */
/*		      given x and y values.		      */
/* 							      */
/* RETURNS:  Modified TANGO_LOC.			      */
/**************************************************************/
TANGO_LOC
TANGOloc_modify(loc,x,y)
   TANGO_LOC loc;
   WIN_COORD x,y;
{
   TANGO_LOC newloc;

   if (!TANGO__data) TANGOinit();

   DEBUG("TANGOloc_modify(0x%lx, %f, %f)\n", (unsigned long) loc, x, y);

   newloc = (TANGO_LOC) malloc( sizeof( struct _LOC));
   newloc->x = x + loc->x;
   newloc->y = y + loc->y;
   return(newloc);
}



/**************************************************************/
/* TANGOloc_equal -- Return 1 if the two locs have the same   */
/*		     coordinates, 0 if not.		      */
/* 							      */
/* RETURNS:  TRUE(1) or FALSE(0).			      */
/**************************************************************/
int
TANGOloc_equal(loc1,loc2)
   TANGO_LOC loc1,loc2;
{
   if (!TANGO__data) TANGOinit();

   DEBUG("TANGOloc_equal(0x%lx, 0x%lx)\n", (unsigned long) loc1, (unsigned long) loc2);

   return( EQUAL(loc1->x,loc2->x) && EQUAL(loc1->y,loc2->y) );
}

/**************************************************************/
/*****************    end of xtangoloc.c     ******************/
/**************************************************************/
