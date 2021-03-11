/* $Id: ParseKey.h,v 1.1 1994/05/16 05:42:45 ishisone Rel $ */
/*
 * Copyright (c) 1991, 1994  Software Research Associates, Inc.
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of Software Research Associates not be
 * used in advertising or publicity pertaining to distribution of the
 * software without specific, written prior permission.  Software Research
 * Associates makes no representations about the suitability of this software
 * for any purpose.  It is provided "as is" without express or implied
 * warranty.
 *
 * Author:  Makoto Ishisone, Software Research Associates, Inc., Japan
 */

#ifndef _ParseKey_h
#define _ParseKey_h

extern int ParseKeyEvent(
#if NeedFunctionPrototypes
	String		/* s */,
	KeySym *	/* keysymp */,
	long *		/* modp */,
	long *		/* chkmodp */
#endif
);

#endif /* _ParseKey_h */
