#ifndef lint
static char *rcsid = "$Header: jcode.c,v 2.3 93/09/22 11:04:10 nao Exp $";
#endif
/*
 * Copyright 1991 Sony Corporation
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of Sony not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  Sony makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * SONY DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL SONY
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
 * OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN 
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */
/*
 * Author: Naoshi Suzuki, SONY Corporation.  (nao@sm.sony.co.jp)
 */

#include <stdio.h>
#ifdef X_LOCALE
#include <X11/Xlocale.h>
#else /* X_LOCALE */
#include <locale.h>
#endif /* X_LOCALE */
#include "common.h"

int code,   kana;
enum { ASCII, KANJI, GAIJI } g0;

main(argc, argv)
    int     argc;
    char  **argv;
{
    char           *locale;
    register FILE  *in,    *out;
    unsigned char   ibuf[BUFSIZ],   obuf[BUFSIZ];

#ifdef X_LOCALE
    if (locale = _Xsetlocale(LC_CTYPE, ""))
#else /* X_LOCALE */
    if (locale = setlocale(LC_CTYPE, ""))
#endif /* X_LOCALE */
        if (!strcmp(locale, "ja_JP.SJIS")||!strcmp(locale, "ja_JP.mscode"))
            code = JP_SJIS;
        else if (!strcmp(locale, "ja_JP.jis7")) code = JP_JIS7;
        else if (!strcmp(locale, "ja_JP.jis8")) code = JP_JIS8;
        else code = JP_EUC;
    else code = JP_EUC;
#ifdef FORCE_SJIS
    code = JP_SJIS;
#endif
#ifdef FORCE_JIS8
    code = JP_JIS8;
#endif
#ifdef FORCE_JIS7
    code = JP_JIS7;
#endif
    if (argc < 2) {
        in = stdin;
        out = stdout;
    } else {
        if ((in = fopen (*(++argv), "r")) == NULL) {
            perror (*argv);
            exit (1);
        }
        out = stdout;
    }
    if (code == JP_EUC)
        while (fgets((char *)ibuf, BUFSIZ, in) != NULL)
            fputs((char *)ibuf, out);
    else {
        g0 = ASCII;
        kana = OFF;
        while (fgets((char *)ibuf, BUFSIZ, in) != NULL) {
            conv(ibuf, obuf);
            fputs((char *)obuf, out);
            putc('\n', out);
        }
    }
    exit(0);
}

int
conv(p, q)
    register unsigned char *p, *q;
{
    wchar                   c;

    while (*p != '\n' && *p != '\0') {
        if (*p & 0x80) {
            if (*p == SS2) {
                p++;
                if (code != JP_SJIS && g0 != ASCII) {
                    g0 = ASCII;
                    *q++ = ESC; *q++ = '('; *q++ = 'J';
                }
                if (code == JP_JIS7) {
                    if (!kana) {
                        *q++ = SO;
                        kana++;
                    }
                    *q++ = *p++ & MASK;
                } else {
                    *q++ = *p++;
                }
            } else if (*p == SS3) {
                if (code == JP_JIS7 && kana) {
                    *q++ = SI;
                    kana = OFF;
                }
                if (code != JP_SJIS) {
                    if (g0 != GAIJI) {
                        g0 = GAIJI;
                        *q++ = ESC; *q++ = '$'; *q++ = '('; *q++ = 'D';
                    }
                    *q++ = (*p++ & MASK); *q++ = (*p++ & MASK);
                } else {
                    if (*p < 0x3b) {
                        c = (*p << 8) | *(p + 1); p += 2;
                        c = _Xsj3ceuc2sjis(c);
                        *q++ = (c >> 8) + 0x6f; *q++ = c & 0xff;
                    } else {
                        *q++ = 0xfc; *q++ = 0xfc;
                    }
                }
            } else {
                if (code == JP_JIS7 && kana) {
                    *q++ = SI;
                    kana = OFF;
                }
                if (code != JP_SJIS) {
                    if (g0 != KANJI) {
                        g0 = KANJI;
                        *q++ = ESC; *q++ = '$'; *q++ = 'B';
                    }
                    *q++ = (*p++ & MASK); *q++ = (*p++ & MASK);
                } else {
                    c = (*p << 8) | *(p + 1); p += 2;
                    c = _Xsj3ceuc2sjis(c);
                    *q++ = c >> 8; *q++ = c & 0xff;
                }
            }
        } else {
            if (code == JP_JIS7 && kana) {
                *q++ = SI;
                kana = OFF;
            }
            if (code != JP_SJIS && g0 != ASCII) {
                    g0 = ASCII;
                    *q++ = ESC; *q++ = '('; *q++ = 'J';
            }
            *q++ = *p++;
        }
    }
    if (code == JP_JIS7 && kana) {
        *q++ = SI;
        kana = OFF;
    }
    if (code != JP_SJIS && g0 != ASCII) {
        g0 = ASCII;
        *q++ = ESC; *q++ = '('; *q++ = 'J';
    }
    *q = '\0';
}
