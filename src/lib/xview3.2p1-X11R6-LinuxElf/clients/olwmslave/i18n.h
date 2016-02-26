#ident	"@(#)i18n.h	1.4	93/06/28 SMI"
/*
 *	(c) Copyright 1990 Sun Microsystems, Inc. Sun design patents 
 *	pending in the U.S. and foreign countries. See LEGAL NOTICE 
 *	file for terms of the license.
 */

#ifndef i18n_DEFINED
#define i18n_DEFINED

#ifdef OW_I18N_L4
#include <widec.h>
#include <locale.h>
#include <wctype.h>
#include <xview/xv_i18n.h>

typedef struct {
        XFontSet        fs;
        XFontSetExtents	*fsx;
        char            *fsn;
} XFontSetInfo;

extern wchar_t  *mbstowcsdup();
extern wchar_t  *ctstowcsdup();
extern char     *ctstombsdup();
extern char     *wcstoctsdup();
#endif OW_I18N_L4

#endif i18n_DEFINED


