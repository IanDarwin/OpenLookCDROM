/*
 *	$Id: HZutil.c,v 3.1 1994/06/06 09:41:36 ygz Exp $
 */

/***********************************************************
Copyright 1992 by Yongguang Zhang.  All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the names of the authors not
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.

THE AUTHOR(S) DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
THE AUTHOR(S) BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

******************************************************************/

/*
 * All utilities routines and miscellaneous routines.
 */

#include "HZtable.h"

static struct codetbl {
	char *codestr;
	int   codeint;
} codeTbl[] = {

	"GB",		GB_ENCODE,
	"Gb",		GB_ENCODE,
	"gb",		GB_ENCODE,
	"GUOBIAO",	GB_ENCODE,
	"GuoBiao",	GB_ENCODE,
	"guobiao",	GB_ENCODE,

	"BIG5",		BIG5_ENCODE,
	"Big5",		BIG5_ENCODE,
	"big5",		BIG5_ENCODE,
	"B5",		BIG5_ENCODE,
	"b5",		BIG5_ENCODE,

	"JIS",		JIS_ENCODE,
	"jis",		JIS_ENCODE,
	"J",		JIS_ENCODE,
	"j",		JIS_ENCODE,

	"KS",		KS_ENCODE,
	"ks",		KS_ENCODE,
	"KSC",		KS_ENCODE,
	"ksc",		KS_ENCODE,

	(char *)0,	UNKNOWN_ENCODE,
};

int HZencode (name)
    char *name;
{
    struct codetbl *pct = codeTbl;

    while (pct->codestr) {
	if (strcmp (name, pct->codestr) == 0)
	    return (pct->codeint);
	pct++;
    }
    return (pct->codeint);
}

char *HZencodeName (encode)
    int encode;
{
    struct codetbl *pct = codeTbl;

    while (pct->codestr) {
	if (encode == pct->codeint)
	    return (pct->codestr);
	pct++;
    }
    return (pct->codestr);
}

