#ifdef IDENT
#ident  "@(#)error.h	1.4    93/06/28 SMI"
#endif
 
/*
 *      (c) Copyright 1989 Sun Microsystems, Inc.
 */

/*
 *      Sun design patents pending in the U.S. and foreign countries. See
 *      LEGAL_NOTICE file for terms of the license.
 */

#ifndef _OLWM_ERROR_H
#define _OLWM_ERROR_H

#include <X11/Xlib.h>

#ifdef __STDC__
extern void ErrorSensitive(char *s);
extern void ErrorInsensitive(Display *dpy);

extern int  ErrorHandler(Display *dpy, XErrorEvent *event);
extern void ErrorGeneral(char *txt);
extern void ErrorWarning(char *txt);
#else
extern void ErrorSensitive();
extern void ErrorInsensitive();

extern int  ErrorHandler();
extern void ErrorGeneral();
extern void ErrorWarning();
#endif
#endif /* _OLWM_ERROR_H */
