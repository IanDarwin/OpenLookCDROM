/*
 *	(c) Copyright 1991 Sun Microsystems, Inc.  All rights reserved.
 *	See LEGAL_NOTICE file for terms and restrictions.
 */

#ifndef DTEXT_IMPL_DEFINED
#define DTEXT_IMPL_DEFINED

/* @(#)dtext_impl.h 1.5 92/07/08 */


typedef struct drawtext_info {
	char		*string;
	Xv_font		font;
	XFontStruct	*font_info;
	Drawtext_justify_style justify;
	short		set_width;	/* shadow for XV_WIDTH, set by app */
	short		set_height;	/* shadow for XV_HEIGHT, set by app */
	short		strlen;		/* length of string */
	short		disp_length;	/* # of chars to calc width, attr */
	short		text_width;	/* measured size of string */
	int		editable:1;
	int		show_underline:1;
	int		free_string:1;
} Drawtext_info;


#define DRAWTEXT_PRIVATE(drawtext)	\
		XV_PRIVATE(Drawtext_info, Drawtext_struct, drawtext)

EXTERN_FUNCTION(void drawtext_set_attr, (Drawtext_info*, Rectobj_info*, Attr_attribute, void*));
EXTERN_FUNCTION(void drawtext_calc_rect, (Drawtext));

/* Used by drawicon */
extern Drawtext_info    *drawicon_private_dtinfo;

#endif

