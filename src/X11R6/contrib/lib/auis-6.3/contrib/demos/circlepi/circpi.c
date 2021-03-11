/* ********************************************************************** *\
 *         Copyright IBM Corporation 1991 - All Rights Reserved           *
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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/demos/circlepi/RCS/circpi.c,v 1.4 1992/12/15 21:49:28 rr2b R6tape $";
#endif

#include <stdio.h>
#include <math.h>
#include <circpi.eh>
#include <andrewos.h>
#include <observe.ih>

/* Defined constants and macros */

/* External declarations */

/* Forward Declarations */

/* Global variables */


boolean
circlepi__InitializeClass(c)
struct classheader *c;
{
/* 
  Initialize all the class data.
*/
  return(TRUE);
}


boolean
circlepi__InitializeObject(c, self)
struct classheader *c;
struct circlepi *self;
{
/*
  Inititialize the object instance data.
*/
    self->depth_limit = 4;
    return(TRUE);
}


void
circlepi__FinalizeObject(c, self)
struct classheader *c;
struct circlepi *self;
{
/*
  Finalize the object instance data.
*/
  return;
}


void
circlepi__SetDepth(self, limit)
     struct circlepi *self;
     int limit;
{
    if (self->depth_limit != limit) {
	self->depth_limit = limit;
	circlepi_NotifyObservers(self, observable_OBJECTCHANGED);
    }
}
