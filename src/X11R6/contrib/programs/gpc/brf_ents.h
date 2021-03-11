/* $XConsortium: brf_ents.h,v 5.1 91/02/16 10:07:13 rws Exp $ */

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
| Author        :	SimGraphics Engineering Corportation
|
| File          :	brf_ents.h
| Date          :	Fri Feb  9 10:46:55 PST 1990
| Project       :	PLB
| Description   :	Provides function prototyping for do_brf 
|			routines
| Status        :	Version 1.0
|
| Revisions     :	
|
\*--------------------------------------------------------------------*/
int do_brfgeneric();
int do_brfpolygon();
int do_brfpolygon3();
int do_brffillareaset();
int do_brffillareaset3();
int do_brftriangle3();
int do_brfquadmesh3();
int do_brfindexpolygons3();
int do_brfexecutestructure();
int do_brfcallstructure();
int do_brfinvokeatframe();
