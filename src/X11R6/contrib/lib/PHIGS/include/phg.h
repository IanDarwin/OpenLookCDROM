/* $XConsortium: phg.h,v 5.8 94/04/17 20:41:46 rws Exp $ */

/***********************************************************

Copyright (c) 1989, 1990, 1991  X Consortium

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
X CONSORTIUM BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN
AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

Except as contained in this notice, the name of the X Consortium shall not be
used in advertising or otherwise to promote the sale, use or other dealings
in this Software without prior written authorization from the X Consortium.

Copyright 1989, 1990, 1991 by Sun Microsystems, Inc. 

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the name of Sun Microsystems,
not be used in advertising or publicity pertaining to distribution of 
the software without specific, written prior permission.  

SUN MICROSYSTEMS DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, 
INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT 
SHALL SUN MICROSYSTEMS BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL 
DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.
******************************************************************/

#ifndef PHG_H_INCLUDED
#define PHG_H_INCLUDED

/* Main INTERNAL header file for the PHIGS library */

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>
#include <X11/Xos.h>
#include <X11/Xaw/Cardinals.h>
#include <X11/Xfuncs.h>
#include <math.h>
#include <errno.h>

/* Xos.h defines index to be strchr.  We don't need to do that because all
 * uses of index and strchr are #ifdef-ed in the SI API code.
 */
#ifdef index
#undef index
#endif

/* Include these after the #undef of index so that all their uses of the
 * work "index" don't get modified to strchr.  This is a workaround for the
 * Xlib hack in Xos.h.
 */
#include "PEX.h"
#include "PEXproto.h"

#ifndef X_NOT_STDC_ENV
#include <stdlib.h>
#else
char *malloc(), *realloc(), *calloc();
#endif
#if defined(macII) && !defined(__STDC__)  /* stdlib.h fails to define these */
char *malloc(), *realloc(), *calloc();
#endif /* macII */

/* Not all environments declare errno in <errno.h> (Sony, in particular) */
extern int	 errno;
/* Not all environments' <math.h> define M_PI, so if not defined, define */
#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif

#if !defined(X_NOT_STDC_ENV) && (__STDC__ || !(defined(sun) || (defined(sony) && !defined(SYSTYPE_SYSV))))
#include <float.h>
#endif
#ifndef MAXFLOAT
#ifdef FLT_MAX
#define MAXFLOAT FLT_MAX
#else
#ifdef vax
#define MAXFLOAT ((float)1.701411733192644299e+38)
#else
#define MAXFLOAT ((float)3.40282346638528860e+38)
#endif /* vax */
#endif /* FLT_MAX */
#endif /* MAXFLOAT */

/* Not all environments' <math.h> define _ABS, so if not defined, define */
#ifndef _ABS
#define _ABS(x) ((x) < 0 ? -(x) : (x))
#endif

/* Only include common files that most everybody uses. */

#include "phigs.h"
#include "phgtype.h"
#include "util.h"
#include "cp_ops.h"
#include "err.h"
#include "ws_type.h"
#include "phgargs.h"
#include "phgretdata.h"
#include "psl.h"
#include "errnum.h"
#include "assure.h"

#endif
