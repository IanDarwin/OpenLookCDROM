#ifdef IDENT
#ident	"@(#)ollocale.h	1.10	93/06/28 SMI"
#endif

/*
 *      (c) Copyright 1989 Sun Microsystems, Inc.
 */

/*
 *      Sun design patents pending in the U.S. and foreign countries. See
 *      LEGAL_NOTICE file for terms of the license.
 */

#ifndef _OLLOCALE_H
#define _OLLOCALE_H

#ifdef OW_I18N_L3

#include <locale.h>
/*
 * OPEN LOOK Locale Categories.  Basic Locale must be first item
 * (least number).
 */
#define	OLLC_LC_BASIC_LOCALE	0
#define	OLLC_LC_DISPLAY_LANG	1
#define	OLLC_LC_INPUT_LANG	2
#define	OLLC_LC_NUMERIC		3
#define	OLLC_LC_DATE_FORMAT	4
#define	OLLC_LC_MAX		5

/*
 * The "ISO_LATIN_1" is default character set value
 * (GRV.CharacterSet), we could choose some other namings (such as
 * "latin1"), however choose to stay with the XLFD's charset and
 * encoding.  This way we may find other way to utilize this value in
 * the future.
 */
#define	ISO_LATIN_1		"iso8859-1"

typedef struct _OLLCItem {
	char	*locale;
	int	posixCategory;		/* Will initialize in GRVLCInit() */
	char	*envName;		/* Will initialize in GRVLCInit() */
} OLLCItem;

#endif /* OW_I18N_L3 */

#endif /* _OLLOCALE_H */
