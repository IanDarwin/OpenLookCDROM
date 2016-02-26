/*
 * This file is provided for unrestricted use
 * provided that this legend is included on all tape media
 * and as a part of the software program in whole or part.  Users
 * may copy or modify this file without charge, but are not authorized to
 * license or distribute it to anyone else except as part of a product
 * or program developed by the user.
 *
 * THIS FILE IS PROVIDED AS IS WITH NO WARRANTIES OF ANY KIND INCLUDING THE
 * WARRANTIES OF DESIGN, MERCHANTIBILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE, OR ARISING FROM A COURSE OF DEALING, USAGE OR TRADE PRACTICE.
 *
 * This file is provided with no support and without any obligation on the
 * part of Sun Microsystems, Inc. to assist in its use, correction,
 * modification or enhancement.
 *
 * SUN MICROSYSTEMS, INC. SHALL HAVE NO LIABILITY WITH RESPECT TO THE
 * INFRINGEMENT OF COPYRIGHTS, TRADE SECRETS OR ANY PATENTS BY THIS FILE
 * OR ANY PART THEREOF.
 *
 * In no event will Sun Microsystems, Inc. be liable for any lost revenue
 * or profits or other special, indirect and consequential damages, even
 * if Sun has been advised of the possibility of such damages.
 *
 * Sun Microsystems, Inc.
 * 2550 Garcia Avenue
 * Mountain View, California  94043
 */

#include <X11/Xos.h>
#include <stdio.h>
#include <xview/xview.h>

#ifdef __STDC__
#  include <stdarg.h>
#else
#  include <varargs.h>
#endif

#include "xvnews_ui.h"
#include "xvnews.h"

#ifdef __STDC__
extern void xvnews_err(xvnews_xvnews_window_objects *ip, char *formatstr, ...)
#else
extern void xvnews_err(ip, formatstr, va_alist)
     xvnews_xvnews_window_objects	*ip;
     char	*formatstr;
     va_dcl
#endif
{
	va_list args;
static	char label[120];

#ifdef __STDC__
	va_start(args, formatstr);
#else
	va_start(args);
#endif
	vsprintf(label, formatstr, args);
        va_end(args);
	if (strchr(label, '!') != NULL)
		window_bell(ip->xvnews_window);
        xv_set(ip->xvnews_window, FRAME_LEFT_FOOTER, label, NULL);
}
