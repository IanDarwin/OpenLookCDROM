/*		SURFMODL		*/
/*
 *	Copyright, 1987, The Regents of the University of California.  This
 *	software was produced under a U.S. Government contract (W-7405-
 *	ENG-36) by Los Alamos National Laboratory, which is operated by the
 *	University of California for the U.S. Department of Energy.  The U.S.
 *	Government is licensed to use, reproduce, and distribute this software.
 *	Permission is granted to the public to copy and use this software
 *	without charge, provided that this Notice and any statement of
 *	authorship are reproduced on all copies.  Neither the Government nor
 *	the University makes any warranty, express or implied, or assumes any
 *	liability or responsibility for the use of this software.
 */


/* Surface modeling in three dimensions, Version 1.3  May 1987

	by David W. Forslund, Los Alamos National Laboratory

	Based on the Turbo Pascal version of Surfmodl 1.1 by Kenneth Van Camp

  SURFMODL may be freely distributed, or distributed at nominal copying/mailing
  fee, but may not be otherwise charged for. It may not be incorporated into
  commercial software without express written permission of the authors.

                        Kenneth Van Camp
                        P.O. Box 784
                        Stroudsburg, PA  18360

			David W. Forslund
			MS E531
			Los Alamos National Laboratory
			Los Alamos, NM 87545
*/
static char *RCSid = "$Header: surfmodl.c,v 1.4 88/03/01 08:08:38 dwf Locked $";

/*
 * $Log:	surfmodl.c,v $
 * Revision 1.4  88/03/01  08:08:38  dwf
 * Added Copyright
 * 
 * Revision 1.3  87/05/25  12:40:32  dwf
 * Added header message
 * 
 */

#include "surfmodl.h"

/* An important function for decoding the Connect array: */

int konnec(Surf,Vert)
int Surf,Vert;
{
  return Connect[Surf][Vert];
} /* function konnec */


void graphics()
{ 
  switch (drawing_style) {
    case DRAWING_STYLE_VECTORS:
	wireframe();
	break;
    case DRAWING_STYLE_HIDDEN: 
	hiddenline();
	break;
    case DRAWING_STYLE_SURFACE: 
	if ( (Interpolate) )
       		gouraud();  
        else
         	surface();
	break;
  }
  display = 0;
} /* graphics */

main(argc,argv)
int argc;
char **argv;
{   /* SURFMODL main program */
  initial (&argc,argv);
  window_main_loop();
  closegraph();

} /* program SURFMODL */
