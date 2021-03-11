/* -*-C-*-
*******************************************************************************
*
* File:         xtangoversn.h
* RCS:          $Header: /disk3/npm/src/winterp/src-server/xtango/RCS/xtangoversn.h,v 2.2 1994/06/09 01:31:50 npm Exp $
* Description:  XTANGO ANIMATION PACKAGE (version.h)
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

/**************************************************************/
/* Version history					      */
/* ---------------------------------------------------------- */
/* 1.00	  Baselined version.				      */
/* 1.10	  Added Athena version.				      */
/* 1.20	  Added more animations and changed installation      */
/*	  script to warn automount problems.		      */
/* 1.21   Modified the prime number hash array index in	      */
/*	  xtangoassoc.c					      */
/* 1.30   Added Motif version and fixed small bugs in the     */
/*	  following animations:	 spath, merge		      */
/* 1.40   Fixed small bugs, added TAP package, added          */
/*        better zooming and lots of new animations           */
/* 1.41   Fixed small panning and zooming problem, added      */
/*        some new animations, fixed bugs in exisiting ones   */
/* 1.42   Fixed bug in copying composites, color capabilities */
/*        added new animations                                */
/* 1.43   Fixed centered text and polygon copy bugs, added    */
/*        window bg color, added new animations, added        */
/*        Animator program                                    */
/* 1.50   Updated animation methodology to be much faster     */
/* 1.51   Fixed bugs in new animation methodology and         */
/*        numerous other bugs and memory leaks                */
/* 1.52   Fixed bugs in text drawing and bbox, fixed bug in   */
/*        panning and zooming while paused, changed how Motif */
/*        widgets and resources are done, added some new      */
/*        animations                                          */
/*                                                            */
/**************************************************************/

#define VERSION 1.52

/**************************************************************/
/***************    end of xtangoversion.h     ****************/
/**************************************************************/
