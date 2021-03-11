/* $Id: main.h,v 3.0 1994/06/04 07:36:02 ygz Exp $ */
/***********************************************************************
* Copyright 1994 by Yongguang Zhang.
* Copyright 1990, 1991 by Yongguang Zhang and Pong Man-Chi.
*
* All rights reserved.  Under the same copyright and permission term as
* the original.  Absolutely no warranties of any kinds.
************************************************************************/

/*
 *	$XConsortium: main.h,v 1.5 91/02/06 16:00:15 gildea Exp $
 */

/*
 * Copyright 1987 by Digital Equipment Corporation, Maynard, Massachusetts.
 *
 *                         All Rights Reserved
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted,
 * provided that the above copyright notice appear in all copies and that
 * both that copyright notice and this permission notice appear in
 * supporting documentation, and that the name of Digital Equipment
 * Corporation not be used in advertising or publicity pertaining to
 * distribution of the software without specific, written prior permission.
 *
 *
 * DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
 * DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
 * ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
 * WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
 * ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
 * SOFTWARE.
 */

#ifdef HANZI
# define DEFFONT		"8x16"
# define DEFBOLDFONT		NULL 	/* no bold font uses overstriking */
# define DEFHZFONT		"hanzigb16st"
# define DEFHZBOLDFONT		NULL	/* no bold font uses overstriking */
# define DEFHZENCODE		"GB"
# define DEFHZMODE		NULL	/* no hanzi mode uses ASCII */
# define DEFLINESPACING		0	/* no spacing between lines */
#else  /* HANZI */
#define	DEFFONT			"fixed"
#define	DEFBOLDFONT		NULL 	/* no bold font uses overstriking */
#endif /* HANZI */
#define	DEFBORDER		2
#define	DEFBORDERWIDTH		2
