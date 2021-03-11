/* $XConsortium: brf_prt.h,v 5.5 94/04/17 20:44:29 rws Exp $ */
/*

Copyright (c) 1989, 1990, 1991  X Consortium

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be included
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE X CONSORTIUM BE LIABLE FOR ANY CLAIM, DAMAGES OR
OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.

Except as contained in this notice, the name of the X Consortium shall
not be used in advertising or otherwise to promote the sale, use or
other dealings in this Software without prior written authorization
from the X Consortium.

*/
/***********************************************************
Copyright 1989, 1990, 1991 by Sun Microsystems, Inc.

						All Rights Reserved

Permission to use, copy, modify, and distribute this software and its
documentation for any purpose and without fee is hereby granted,
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in
supporting documentation, and that Sun Microsystems
not be used in advertising or publicity
pertaining to distribution of the software without specific, written
prior permission.

SUN MICROSYSTEMS DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT
SHALL SUN MICROSYSTEMS BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL
DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

******************************************************************/
/*--------------------------------------------------------------------*\
|  Copyright (C) 1989, 1990, 1991, National Computer Graphics Association
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
| Author        :	Norman D. Evangelista
|
| File          :	brf_prt.h
| Date          :	Tue Jul  4 14:08:02 PDT 1989
| Project       :	BIF Benchmark Report Format
| Description   :	Definitions and constants for report generation
| Status        :	Version 1.0
|
| Revisions     :
|
|       2/90            MFC Tektronix, Inc.: PEX-SI API implementation.
|
|      12/90            MFC Tektronix, Inc.: PEX-SI PEX5R1 Release.
|
\*--------------------------------------------------------------------*/
#include <stdio.h>
#include <sys/types.h>

#define N_TOCOLON	25
#define N_TITLELINES	5
#define N_HEADFILES	4

char *brf_title[] = 
{
"------------------------------------------------------------------------\n",
"|          Graphics Performance Characterization Committee             |\n",
"|                Standard Graphics System Benchmarks                   |\n",
"|----------------------------------------------------------------------|\n",
"| Date                    :                                            |\n",
NULL
};

char *brf_beninfo[] = 
{
"|                                                                      |\n",
"|----------------------------------------------------------------------|\n",
"|   Benchmark Information                                              |\n",
"|----------------------------------------------------------------------|\n",
"| Benchmark Title         :   Level 2: Picture Level Benchmark         |\n",
"| Benchmark Version       :   1.00                                     |\n",
"| Benchmark Date          :   Mon Jul  3 18:43:54 PDT 1989             |\n",
"| Benchmark Authors       :   SimGraphics Engineering Corporation      |\n",
"|                                                                      |\n",
NULL
};

char *brf_impinfo[] = 
{
"|----------------------------------------------------------------------|\n",
"|   Implementation Information                                         |\n",
"|----------------------------------------------------------------------|\n",
"| Implementation Title    :   PEX-SI API Port                          |\n",
"| Implementation Version  :   1.20                                     |\n",
"| Implementation Date     :   Mon Nov  5 08:45:00 PDT 1990             |\n",
"| Implementation Authors  :   Tektronix, Inc                           |\n",
"|                                                                      |\n",
"| Graphics Library        :   PEX c-binding PHIGS/PHIGS+               |\n",
"| Graphics Library Version:   PEX5R1                                   |\n",
"|                                                                      |\n",
NULL
};

#ifdef EXTERNALNOTE
	/* the figure for window size is hard wired into this
	next block. In the case of the Alliant, the window always
	opens to this size. To prevent a possible error, the window
	should be locked against a user resize using the pesc() function.
	The ultimate answer is to be able to read the window size and insert
	it into this report. Size should be read just before
	and just after the test loop. If there was any change in 
	size or position (unlikely), then someone moved the window
	and the test timing is invalid. */
#endif

char _brf_sys[] =
"|  System Make and Model  :   Unknown                                  |\n";
char _brf_hdw[] =
"|  Hardware Configuration :   Unknown                                  |\n";
char _brf_os[] =
"|  Operating System       :   Unknown                                  |\n";
char _brf_ws[] =
"|  Windowing System       :   Unknown                                  |\n";
char _brf_wz[] =
"|  PLB Window Size        :   Unknown                                  |\n";
char _brf_sa[] =
"|  Stopwatch Accuracy     :   Unknown                                  |\n";

char *brf_sysinfo[] = 
{
"|----------------------------------------------------------------------|\n",
"|   System Configuration                                               |\n",
"|----------------------------------------------------------------------|\n",
_brf_sys,
_brf_hdw,
"|                                                                      |\n",
"|                                                                      |\n",
_brf_os,
_brf_ws,
_brf_wz,
_brf_sa,
"|                                                                      |\n",
"------------------------------------------------------------------------\n",
NULL
};

char _brf_nf[] =
"|  Number of Frames        :                                           |\n";
char _brf_et[] =
"|  Elapsed Time (sec)      :                                           |\n";
char _brf_td[] =
"|  Transport Delay         :                                           |\n";
char _brf_fps[] =
"|  Avg. Frames per Second  :                                           |\n";
char _brf_tpf[] =
"|  Avg. Time per Frame     :                                           |\n";
char _brf_mm1[] =
"|  Timing Merit Mthd 1     :                                           |\n";
char _brf_mm2[] =
"|  Timing Merit Mthd 2     :                                           |\n";

char *brf_timeinfo[] = 
{
"|----------------------------------------------------------------------|\n",
"|   Test Loop Timing Information                                       |\n",
"|----------------------------------------------------------------------|\n",
_brf_nf,
_brf_et,
_brf_td,
_brf_fps,
_brf_tpf,
_brf_mm1,
_brf_mm2
,NULL
};


char *brf_exceptioninfo[] = 
{
"|----------------------------------------------------------------------|\n",
"|   Global Exceptions Encountered\n",
"|----------------------------------------------------------------------|\n"
,NULL
};

#if 0 /* WORKING : to be deleted */
char	*BRF_hedrFiles[] =
{
	"brf_title.b",
	"brf_beninf.b",
	"brf_impinf.b",
	"brf_sysinf.b"
};
#endif

char	**BRF_hedrData[] =
{
	brf_title,
	brf_beninfo,
	brf_impinfo,
	brf_sysinfo
};

char	**BRF_timeData[] =
{
	brf_timeinfo
};

char	*BRF_timeFiles[] =
{
	"brf_tmeinf.b"
};

