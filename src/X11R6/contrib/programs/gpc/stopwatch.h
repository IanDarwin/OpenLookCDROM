/* $XConsortium: stopwatch.h,v 5.1 91/02/16 10:07:53 rws Exp $ */

/*
 */

/*--------------------------------------------------------------------*\
|
|  Copyright (C) 1989,1990, 1991, National Computer Graphics Association
|
|  Permission is granted to any individual or institution to use, copy, or
|  redistribute this software so long as it is not sold for profit, provided
|  this copyright notice is retained.
|
|                         Developed for the
|                National Computer Graphics Association
|                         2722 Merrilee Drive
|                         Fairfax, VA  22031
|                           (703) 698-9600
|
|                                by
|                 SimGraphics Engineering Corporation
|                    1137 Huntington Drive  Unit A
|                      South Pasadena, CA  91030
|                           (213) 255-0900
|---------------------------------------------------------------------
|
| Author        :	jmz / SimGraphics Engineering Corportation
|
| File          :	stopwatch.h
| Date          :	8/22/89
| Project       :	PLB
| Description   :	Stopwatch type and constant definitions.
| Status        :	Version 1.0
|
| Revisions     :	
|
\*--------------------------------------------------------------------*/

/* Define the Options for the stopwatch routine */

/* Stop the watch if running.  */
/* Return last stopped time */
#define WATCH_STOP	0

/* Start the watch if stopped */
/* Return current time */
#define WATCH_START	1

/* Return current time */
#define WATCH_SPLIT	2

/* Reset current time to zero, does NOT start or stop the watch */
#define WATCH_RESET	3

/* Return the accuracy of the watch */
#define WATCH_PRECISION	4

float stopwatch();
