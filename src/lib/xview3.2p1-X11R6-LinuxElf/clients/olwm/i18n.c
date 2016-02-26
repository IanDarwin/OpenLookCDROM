#ident	"@(#)i18n.c	1.12	93/06/28 SMI"

/*
 *      (c) Copyright 1989 Sun Microsystems, Inc.
 */

/*
 *      Sun design patents pending in the U.S. and foreign countries. See
 *      LEGAL_NOTICE file for terms of the license.
 */

#include <stdio.h>
#include <X11/Xlib.h>
#include <X11/Xatom.h>

#include "i18n.h"
#include "mem.h"
#include "ollocale.h"
#include "olwm.h"
#include "globals.h"

/*
 * DrawText	- Displays Text.
 *		  #ifdef'd for both normal and wide text
 */
void
DrawText(dpy,drawable,font,gc,x,y,text,len)
	Display		*dpy;
	Drawable	drawable;
	DisplayFont	font;
	GC		gc;
	int		x,y;
	Text		*text;
	int		len;
{
#ifdef OW_I18N_L4
	XFontSet	fontSet;

	switch (font) {
	case TitleFont:
		fontSet = GRV.TitleFontSetInfo.fs;
		break;
	case TextFont:
		fontSet = GRV.TextFontSetInfo.fs;
		break;
	case ButtonFont:
		fontSet = GRV.ButtonFontSetInfo.fs;
		break;
	case IconFont:
		fontSet = GRV.IconFontSetInfo.fs;
		break;
	default:
		return;
	}

	XwcDrawString(dpy,drawable,fontSet,gc,x,y,text,len);
#else
	XDrawString(dpy,drawable,gc,x,y,text,len);
#endif
}

/*
 * FontInfo - returns width,height,ascent,descent for the set of
 *	      fonts used in olwm.
 *	      #ifdef'd for both XFontInfo and XFontSet
 */
int
FontInfo(font,op,text,len)
	DisplayFont	font;
	FontInfoOp	op;
	Text		*text;
	int		len;
{
#ifdef OW_I18N_L4
	XFontSetInfo	*fontInfo;
#else
	XFontStruct	*fontInfo;
#endif
	int		ret;

	switch (font) {

	case TitleFont:
#ifdef OW_I18N_L4
		fontInfo = &(GRV.TitleFontSetInfo);
#else
		fontInfo = GRV.TitleFontInfo;
#endif
		break;
	case TextFont:
#ifdef OW_I18N_L4
		fontInfo = &(GRV.TextFontSetInfo);
#else
		fontInfo = GRV.TextFontInfo;
#endif
		break;

	case ButtonFont:
#ifdef OW_I18N_L4
		fontInfo = &(GRV.ButtonFontSetInfo);
#else
		fontInfo = GRV.ButtonFontInfo;
#endif
		break;

	case IconFont:
#ifdef OW_I18N_L4
		fontInfo = &(GRV.IconFontSetInfo);
#else
		fontInfo = GRV.IconFontInfo;
#endif
		break;

	default:
		return 0;
	}

	switch (op) {
	case FontWidthOp:
#ifdef OW_I18N_L4
		ret = XwcTextEscapement(fontInfo->fs,text,len);
#else
		ret = XTextWidth(fontInfo,text,len);
#endif
		break;

	case FontHeightOp:
#ifdef OW_I18N_L4
		ret = fontInfo->fsx->max_logical_extent.height;
#else
		ret = fontInfo->ascent + fontInfo->descent;
#endif
		break;

	case FontAscentOp:
#ifdef OW_I18N_L4
		ret = - fontInfo->fsx->max_logical_extent.y;
#else
		ret = fontInfo->ascent;
#endif
		break;

	case FontDescentOp:
#ifdef OW_I18N_L4
		ret = fontInfo->fsx->max_logical_extent.height +
		      fontInfo->fsx->max_logical_extent.y;
#else
		ret = fontInfo->descent;
#endif
		break;

	default:
		return 0;
	}

	return ret;
}


#ifdef OW_I18N_L4
/*
 * When converting the wide char to CTEXT, we need to estimate the
 * space, but there are no right way to do this without actually
 * converting.  "wslen(wchar) * sizeof(wchar_t)" will give us the how
 * many bytes consume by the characters, but this does not include the
 * any control sequences.  I decided use fudge bytes for this control
 * sequnces for now.  This is absolutely bad idea to having a this
 * value, but otherwise we need to convert it twice.  One control
 * sequnce require the 3 bytes, so, following allow to switch the code
 * set 6 times.
 */
#define WCSTOCTS_FUDGE_BYTES	(3 * 6)

wchar_t *
mbstowcsdup(mbs)
register char	*mbs;
{
	register int		n;
	register wchar_t	*wcs;

	if (mbs == NULL)
	    return NULL;

	n = strlen(mbs) + 1;
	wcs = (wchar_t *) MemAlloc(n * sizeof(wchar_t));
	mbstowcs(wcs, mbs, n);
#if DEBUG > 4
fprintf(stderr, "mbstowcsdup: mbs [%s] -> wcs [%ws]\n", mbs, wcs);
#endif

	return wcs;
}


#if DEBUG > 4
ascii_dump(s)
register unsigned char	*s;
{
	while (*s)
	{
		if (*s < ' ')
			fprintf(stderr, "^%c", *s + '@');
		else if (*s > 0x80)
			fprintf(stderr, "(%2x)", *s);
		else
			fputc(*s, stderr);
		s++;
	}
}
#endif DEBUG

#endif OW_I18N_L4
