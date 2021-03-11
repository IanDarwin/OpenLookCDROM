#ifndef lint
static char *rcsid = "$Header: libif.c,v 2.0 92/02/13 18:33:26 nao Exp $";
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

#include "common.h"

extern int      sj3_open();
extern int      sj3_close();
extern int      sj3_getkan();
extern int      sj3_douoncnt();
extern int      sj3_getdouon();
extern int      sj3_touroku();
extern int      sj3_syoukyo();
extern int      sj3_gakusyuu();
extern int      sj3_gakusyuu2();
extern int      sj3_lockserv();
extern int      sj3_unlockserv();

Xsj3cCVServerIF     serverIF[SERVER_NUM] = {
    {
        JP_SJIS,
        {
            sj3_open,
            sj3_close,
            sj3_getkan,
            sj3_douoncnt,
            sj3_getdouon,
            sj3_touroku,
            sj3_syoukyo,
            sj3_gakusyuu,
            sj3_gakusyuu2,
            sj3_lockserv,
            sj3_unlockserv
        }
    }
};
