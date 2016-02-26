#ifdef IDENT
#ident	"@(#)i18n.h	1.26	93/06/28 SMI"
#endif

/*
 *      (c) Copyright 1989 Sun Microsystems, Inc.
 */

/*
 *      Sun design patents pending in the U.S. and foreign countries. See
 *      LEGAL_NOTICE file for terms of the license.
 */

#ifndef _OLWM_I18N_H
#define _OLWM_I18N_H

#include <string.h>
#include <ctype.h>

#ifdef OW_I18N_L4

#include <widec.h>
#include <locale.h>
#include <wctype.h>

#include <X11/Xlib.h>		
#if XlibSpecificationRelease < 5
#include <X11/XlibR5.h>		
#endif /* XlibSpecificationRelease < 5 */

typedef struct {
        XFontSet        fs;
        XFontSetExtents	*fsx;
} XFontSetInfo;

extern wchar_t  *mbstowcsdup();

#endif /* OW_I18N_L4 */

/*
 *	String/Text - To better handle non-i18n, Level3 and Level4
 *	we introduce two 'types'; string and text.
 *
 *	String 	- really char * - which may be a multibyte string in L4.
 *		- used in printf output
 *		- ie.  printf(GetString("msg"));
 *
 *	Text	- Text * type - either char * in L3 or wchar_t * in L4.
 *		- used in output to display via DrawText() or olgx functions
 *		- in L4 entails an allocation of memory for conversion from
 *		  multibyte to widechar, so there must be a deallocation
 *		  when done with the Text.
 *		- ie:
 *			Text *	txt = GetText("msg");
 *			DrawText(...,txt,TextLen(txt));
 *			FreeText(txt);
 *		- In non-i18n and L3, FreeText() is a no-op since GetText()
 *		  does not allocate memory
 */


/*
 *	Text Convenience Functions
 */
#ifdef OW_I18N_L4

typedef wchar_t			Text;

#define TextCat(t1, t2)		wscat((t1), (t2))
#define TextChr(t,c)		wschr((t),(wchar_t)(c))
#define TextCmp(t1, t2)		wscmp((t1), (t2))
#define TextCpy(t)		wscpy((t1),(t2))
#define TextLen(t)		wslen((t))
#define	TextSPrintf		wsprintf
#define TextTok(t,s)		wstok((t),(s))
#define	TextTolower(t)		towlower((t))

#else

typedef char			Text;

#define TextCat(t1, t2)		strcat((t1), (t2))
#define TextChr(t,c)		strchr((t),(c))
#define TextCmp(t1, t2)		strcmp((t1), (t2))
#define TextCpy(t)		strcpy((t1),(t2))
#define TextLen(t)		strlen((t))
#define	TextSPrintf		sprintf
#define TextTok(t,s)		strtok((t),(s))
#define TextTolower(t)		tolower((t))

#endif /* OW_I18N_L4 */

/*
 *	Font Info and Text Draw Functions
 */
typedef enum {	TitleFont,
		TextFont,
		ButtonFont,
		IconFont
		} DisplayFont;

typedef enum {	FontWidthOp,
		FontHeightOp,
		FontAscentOp,
		FontDescentOp
		} FontInfoOp;

extern	void	DrawText();

extern	int	FontInfo();

#define	FontWidth(font,text,len)	FontInfo(font,FontWidthOp,text,len)
#define	FontHeight(font)		FontInfo(font,FontHeightOp,(Text*)0,0)
#define	FontAscent(font)		FontInfo(font,FontAscentOp,(Text*)0,0)
#define	FontDescent(font)		FontInfo(font,FontDescentOp,(Text*)0,0)

/*
 *	String/Text Messaging Convenience Functions
 *
 * char *GetString(char *)
 *	Returns an internationalized string (perhaps multi-byte).  This
 *	string is suitable for printf output.  This string must not be 
 *	modified or freed.
 *
 * Text *GetText(char *)
 *	Returns an internationalized piece of text (perhaps composed of wide 
 *	chars).  This text is suitable for window system display (e.g. for use 
 *	with XDrawText().  This string must be disposed of with FreeText().
 *	This string should never be hooked into a persistent data structure or 
 *	freed with MemFree().
 *
 * void FreeText(Text *)
 *	Frees a string.  This string must have come from GetText().
 *
 * Text *GetNewText(char *)
 *	Returns a piece of internationalized text suitable for window system 
 *	display.  This text is guaranteed to have been allocated from the 
 *	heap, and it must be freed eventually with MemFree().  It is thus 
 *	suitable for use in persistent data structures.
 */

#ifdef OW_I18N_L4

extern		char			*gettext();
#define		GetString(s)		gettext(s)
#define		GetText(s)		mbstowcsdup(gettext(s))
#define		FreeText(s)		MemFree(s)
#define		GetNewText(s)		mbstowcsdup(gettext(s))

#elif defined OW_I18N_L3

#ifdef SVR4
extern		char			*gettext();
#else
#define 	gettext(s)		s
#endif

#define		GetString(s)		gettext(s)
#define		GetText(s)		gettext(s)
#define		FreeText(s)	
#define		GetNewText(s)		MemNewString(gettext(s))

#else

#define 	GetString(s)		s
#define		GetText(s)		s
#define		FreeText(s)
#define		GetNewText(s)		MemNewString(s)

#endif


/*
 *	OLGX macros
 */
#ifdef OW_I18N_L4

#define	olgx_main_initialize	olgx_i18n_initialize
#define	TextOLGX		OLGX_LABEL_IS_WCS

#else

#define	TextOLGX		(0)

#endif

#endif /* _OLWM_I18N_H */
