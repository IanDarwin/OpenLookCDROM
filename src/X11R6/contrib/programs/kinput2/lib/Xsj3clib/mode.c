#ifndef lint
static char *rcsid = "$Header: mode.c,v 2.6 93/01/06 10:58:18 nao Exp $";
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

#include <ctype.h>
#include "common.h"
#include "sj3ctype.h"
#include "util.h"

extern Xsj3cCVServerIF      serverIF[SERVER_NUM];

Xsj3cEvent                  _Xsj3cModeChange();
Xsj3cEvent                  _Xsj3cModeClear();
Xsj3cEvent                  Xsj3cModeConv();

void                        _Xsj3cHiraToZKata();
void                        _Xsj3cHankakuToHira();
void                        _Xsj3cHankakuToZKata();
void                        _Xsj3cHKataToHira();
void                        _Xsj3cHKataToZKata();
void                        _Xsj3cZKanaToHKata();
void                        _Xsj3cZKataToHira();
void                        _Xsj3cHAlphaToZAlpha();
void                        _Xsj3cHAlphaToZKana();
void                        _Xsj3cHAlphaToHKata();
void                        _Xsj3cZAlphaToHAlpha();

static void                 _Xsj3cLowerToUpper();
static void                 _Xsj3cUpperToLower();
static void                 _Xsj3cRKConvs();
static void                 _Xsj3cKRConvs();

/*
 * _Xsj3cModeChange()
 *
 * Change current character mode to apointed mode(secound argument)
 */
Xsj3cEvent
_Xsj3cModeChange(buf, mode, conv)
    Xsj3cBuf        buf;
    Xsj3csMode      mode;
    Xsj3cFlag       conv;
{
    Xsj3cEvent      ret;
    unsigned char  *mbs;

    if (buf->convmode != SelectModeMask) {
        if (conv) {
            Xsj3cSeg        seg = buf->input[buf->curseg];

            if ((mbs = (unsigned char *)
                    malloc(seg->size * 2 * sizeof(wchar))) == NULL)
                Xsj3cError("Cannot allocate for mode conversion buffer");
            _Xsj3cwPStomPS(buf, mbs, seg->yomi);
            if ((ret = Xsj3cModeConv(buf, mbs, mode, seg->size))
                    & KEY_TEXT_CHANGE) {
                seg->num = _Xsj3cmPStowPSn(buf, seg->yomi, mbs, seg->size);
                if (seg->num > seg->size - YBUFSIZ) {
                    Xsj3cResizeSegment(seg, seg->size * 2);
                    seg->num = _Xsj3cmPStowPS(buf, seg->yomi, mbs);
                }
                seg->cur = seg->num;
                _Xsj3cStoreYomi(buf, seg, 0);
                seg->change = OFF;
                seg->cursegmode = mode;
            }
            free(mbs);
        } else {
            if (buf->inputmode != mode) {
                buf->dispmode = buf->inputmode = mode;
                return (_Xsj3cModeClear(buf));
            } else {
#ifdef THROUGH_CONT
                return (KEY_NULL);
#else /* THROUGH_CONT */
                if (buf->cntrlsame)
                    return (KEY_NULL);
                else
                    return (KEY_TEXT_CHANGE); /* dummy  */
#endif /* THROUGH_CONT */
            }
        }
        return ret;
    } else {
        return (KEY_BELL);
    }
}

/*
 * _Xsj3cModeClear() 
 *  Clear romaji/kana buffer after changing mode.
 */
Xsj3cEvent
_Xsj3cModeClear(buf)
    Xsj3cBuf            buf;
{
    register Xsj3cSeg   seg = buf->input[buf->curseg];

    if (!seg)
        return (KEY_MODE_CHANGE);
    *seg->str = '\0';
    seg->sp = seg->str;
    seg->n_roma = 0;
    seg->n_kana = -1;
    *seg->oldstr = '\0';
    seg->oldlen = 0;
    seg->value = 0;
    seg->change = OFF;
    seg->cursegmode = MODE_HIRA;
    if (buf->flushaconv)
        return (KEY_MODE_CHANGE|KEY_TEXT_CLEAR);
    else
        return (KEY_MODE_CHANGE);
}

/*
 * Xsj3cModeConv()
 *  Convert current segment to appointed character mode.
 */
Xsj3cEvent
Xsj3cModeConv(buf, string, postmode, size)
    Xsj3cBuf        buf;
    unsigned char  *string;
    Xsj3csMode      postmode;
    int             size;
{
    Xsj3cEvent      ret = KEY_NULL;
    unsigned char   *tmp1,   *tmp2;

    if ((tmp1 = (unsigned char *)malloc(size * 2 * sizeof(wchar))) == NULL)
        Xsj3cError("Cannot allocate for mode conversion buffer");
    if ((tmp2 = (unsigned char *)malloc(size * 2 * sizeof(wchar))) == NULL)
        Xsj3cError("Cannot allocate for mode conversion buffer");

    switch (postmode) {
    case MODE_HIRA:
        _Xsj3cZKanaToHKata(buf, tmp2, string);
        _Xsj3cHKataToHira(buf, tmp1, tmp2);
        _Xsj3cKRConvs(buf, tmp2, tmp1);
        _Xsj3cRKConvs(buf, tmp1, tmp2);
        if (buf->alphaconv)
            _Xsj3cHankakuToHira(buf, string, tmp1);
        else
            _Xsj3cZKataToHira(buf, string, tmp1);
        ret = KEY_TEXT_CHANGE;
        break;

    case MODE_ZKATA:
        _Xsj3cZKanaToHKata(buf, tmp2, string);
        _Xsj3cHKataToHira(buf, tmp1, tmp2);
        _Xsj3cKRConvs(buf, tmp2, tmp1);
        _Xsj3cRKConvs(buf, tmp1, tmp2);
        if (buf->alphaconv) {
            _Xsj3cHiraToZKata(buf, tmp2, tmp1);
            _Xsj3cHAlphaToZAlpha(buf, string, tmp2);
        } else
            _Xsj3cHiraToZKata(buf, string, tmp1);
        ret = KEY_TEXT_CHANGE;
        break;

    case MODE_HKATA:
        _Xsj3cHKataToHira(buf, tmp1, string);
        _Xsj3cKRConvs(buf, tmp2, tmp1);
        _Xsj3cRKConvs(buf, tmp1, tmp2);
        _Xsj3cZKanaToHKata(buf, string, tmp1);
        ret = KEY_TEXT_CHANGE;
        break;

    case MODE_HALPHA:
        _Xsj3cZAlphaToHAlpha(buf, tmp2, string);
        _Xsj3cRKConvs(buf, tmp1, tmp2);
        _Xsj3cZKanaToHKata(buf, tmp2, tmp1);
        _Xsj3cHKataToHira(buf, tmp1, tmp2);
        _Xsj3cKRConvs(buf, string, tmp1);
        ret = KEY_TEXT_CHANGE;
        break;

    case MODE_ZALPHA:
        _Xsj3cZAlphaToHAlpha(buf, tmp2, string);
        _Xsj3cRKConvs(buf, tmp1, tmp2);
        _Xsj3cZKanaToHKata(buf, tmp2, tmp1);
        _Xsj3cHKataToHira(buf, tmp1, tmp2);
        _Xsj3cKRConvs(buf, tmp2, tmp1);
        _Xsj3cHAlphaToZAlpha(buf, string, tmp2);
        ret = KEY_TEXT_CHANGE;
        break;

    case MODE_HANKAKU:
        _Xsj3cZKanaToHKata(buf, tmp1, string);
        strcpy(string, tmp1);
        ret = KEY_TEXT_CHANGE;
        break;

    case MODE_ZENKAKU:
        _Xsj3cHankakuToZKata(buf, tmp1, string);
        strcpy(string, tmp1);
        ret = KEY_TEXT_CHANGE;
        break;

    case MODE_UPPER:
        _Xsj3cLowerToUpper(buf, string);
        ret = KEY_TEXT_CHANGE;
        break;

    case MODE_LOWER:
        _Xsj3cUpperToLower(buf, string);
        ret = KEY_TEXT_CHANGE;
        break;

    default:
        ret = KEY_BELL;
        break;
    }
    free(tmp1);
    free(tmp2);
    return (ret);
}

/*
 * _Xsj3cRKConvs()
 *  Convert romaji to kana in 3rd argument and set to 2nd argument.
 */
static void
_Xsj3cRKConvs(buf, yomi, alpha)
    Xsj3cBuf                buf;
    unsigned char          *yomi;
    unsigned char          *alpha;
{
    unsigned char          *p;
    register unsigned char *q,     *yp,     *t;
    register int            i = 0,  rlen = 0,       tflg = 0,   value;
    unsigned char           rbuf[RBUFSIZ];      
    unsigned char           ybuf[YBUFSIZ];  
    
    yp = yomi;
    p = rbuf;
    rlen = 0;

    while (*alpha != '\0') {
        if (iskan1(*alpha, serverIF[buf->server].lang)
                && iskan2(*(alpha + 1), serverIF[buf->server].lang)) {
            *yp++ = *alpha++;
            *yp++ = *alpha++;
            i++;
            continue;
        } else if (!isascii(*alpha)) {
            *yp++ = *alpha++;
            i++;
            continue;
        }
        if (i) {
            rlen = 0;
            p = rbuf;
            i = 0;
        } 

        *p = *alpha++;
        *(p + 1) = '\0';
        if (*alpha == '\0') {
            t = buf->rkdouble;
            while (*t != '\0') {
                if (p > rbuf && *(p - 1) == *t) {
                    break;
                } else if (*t == *p) {
                    *(p + 1) = *p;
                    *(p + 2) = '\0';
                    tflg++;
                    break;
                }
                t++;
            }
        }
        if ((value = _Xsj3cRomaConv(buf->rktable, rbuf, ybuf)) > 0) {
            q = ybuf;
            yp -= rlen;
            while (*q != '\0') {
                *yp++ = *q++;
            }
            p = rbuf;
            rlen = 0;
            while (*p != '\0' && !tflg) {
                *yp++ = *p++;
                rlen++;
            }
        } else if (value < 0) {
            if ((value = _Xsj3cRomaConv(buf->rktable, p, ybuf)) > 0) {
                q = ybuf;
                while (*q != '\0') {
                    *yp++ = *q++;
                }
                rlen = 0;
                while (*p != '\0' && !tflg) {
                    *yp++ = *p++;
                    rlen++;
                }
            } else if (value < 0) {
                *yp++ = *p;
                rlen = 0;
                p = rbuf;
            } else {
                *yp++ = *p;
                q = rbuf;
                *q++ = *p++;
                *q = '\0';
                rlen = 1;
            }
        } else {
            *yp++ = *p++;
            if (rlen++ > 3) { /* non-convert limit = 4: you can change this */
                rlen = 0;
                p = rbuf;
            }
        }
    }
    *yp = '\0';
}

static void
_Xsj3cKRConvs(buf, roma, kana)
    Xsj3cBuf                buf;
    register unsigned char *roma;
    register unsigned char *kana;
{
    register unsigned char  c,  *p, *q, *r, *sp, *t, *d;
    register wchar          s;
    register Xsj3cRKTable  *rktp;
    register Xsj3cHKTable  *hktp;
    register Xsj3cZHTable  *zhtp;
    register int            ylen,   plen,   rlen,   i,  cont = 0;
    register int            zenflg = 0, plosflg = 0;
    unsigned char           tyomi[KANABUFSIZ];
    unsigned char           tmp[RBUFSIZ];

    p = tyomi;
    while ((c = *kana++) != '\0') {
        if (iskan1(c, serverIF[buf->server].lang)
                && iskan2(*kana, serverIF[buf->server].lang)) {
            s = (c << 8) + *kana;
            if (iskata(s, serverIF[buf->server].lang)) {
                for (hktp = buf->hktable; hktp != NULL; hktp = hktp->next) {
                    if (c == *hktp->zkata)
                        if (*kana == *(hktp->zkata + 1))
                            break;
                }
                if (hktp != NULL) {
                    *p++ = *hktp->hira;
                    *p++ = *(hktp->hira + 1);
                } else {
                    *p++ = c;
                    *p++ = *kana;
                    Xsj3cWarning("wrong sjhk table");
                }

            } else if (!ishira(s, serverIF[buf->server].lang)) {
                for (zhtp = buf->zhtable; zhtp != NULL; zhtp = zhtp->next) {
                    if (c == *zhtp->zkana) 
                        if (*kana == *(zhtp->zkana + 1))
                            break;
                }
                if (zhtp != NULL) {
                    *p++ = *zhtp->halpha;
                } else {
                    for (zhtp = buf->zhtable; zhtp != NULL; zhtp = zhtp->next) {
                        if (c == *zhtp->zalpha) {
                            if (*kana == *(zhtp->zalpha + 1))
                                break;
                        }
                    }
                    if (zhtp != NULL) {
                        *p++ = *zhtp->halpha;
                    } else {
                        Xsj3cWarning("can't convert to halpha %#x",s);
                        *p++ = c;
                        *p++ = *kana;
                    }
                }
            } else {
                *p++ = c;
                *p++ = *kana;
            }
            kana++;
        } else {
            *p++ = c;
        }
    }
    *p = '\0';
    p = tyomi;
    plen = strlen(tyomi);

    while ((c = *p) != '\0') {
        if (iskan1(c, serverIF[buf->server].lang)
                && iskan2(*(p + 1), serverIF[buf->server].lang)) {
            /* Multi-byte character */
            q = tmp;
            *q = '\0';
            ylen = 0;
            for (rktp = buf->rktable; rktp->next != NULL; rktp = rktp->next) {
                if (ylen < rktp->ylen) {
                    if (plen >= rktp->ylen
                            && !strncmp(rktp->yomi, p, rktp->ylen)) {
                        ylen = rktp->ylen;
                        plosflg = 0;
                        if (*(sp = rktp->str) != '\0') {
                            /* Leaving any character case */
                            if (t = buf->plosive) {
                                while (*t != '\0') {
                                    if (*t++ == *sp) {
                                        plosflg = 1;
                                        break;
                                    }
                                }
                                if (plosflg) {
                                    if (*(p + 2) == '\0') {
                                        /* Last of the segment: */
                                        /* continue to search   */
                                        plosflg = 0;
                                        ylen = 0;
                                        continue;
                                    } else if (iskan1(*(p + 2),
                                            serverIF[buf->server].lang)
                                            && iskan2(*(p + 3),
                                            serverIF[buf->server].lang)) {
                                        /* Next character is zvowel: */
                                        /* continue to search        */
                                        if (iszvowel((*(p + 2) << 8) | *(p + 3),
                                                serverIF[buf->server].lang)){
                                            plosflg = 0;
                                            ylen = 0;
                                            continue;
                                        }
                                    } else if (isvowel(*(p + 2))) {
                                        /* Next character is vowel:  */
                                        /* continue to search        */
                                        plosflg = 0;
                                        ylen = 0;
                                        continue;
                                    }
                                }
                            }
                            strcpy(tmp, rktp->roma);
                            /* Reduce leaved character */
                            i = 1;
                            while (*(++sp) != '\0')
                                i++;
                            r = (q + rktp->rlen);
                            while (*(--sp) == *(--r) && (--i));
                            *r = '\0';  
                        } else if (d = buf->rkdouble) {
                            /* Double defined by a same character case */
                            strcpy(tmp, rktp->roma);
                            while (*d != '\0') {
                                if (*d++ == *q) {
                                    if (*q == *(q + 1)) {
                                        /* Last of the segment */
                                        if (*(p + 2) == '\0')
                                            *(q + 1) = '\0';
                                    } else if (*(q + 1) == '\0') {
                                        /* Single case: continue to search */
                                        ylen = 0;
                                    }
                                    break;
                                }
                            }
                        } else {
                            strcpy(tmp, rktp->roma);
                        }
                    }
                } 
            }
            if (ylen > 0) {
                /* Succeeded in converting.    */
                if (!zenflg && !plosflg && !isvowel(*q)
                        && p > tyomi && (t = buf->plosive)) {
                    /* Same as plosive table first character */
                    if (*t == *(p - 1))
                        cont++;
                }
                if (plosflg) {
                    cont++;
                } else if (cont && !isvowel(*q)) {
                    /* Correct consonant character of previous plosive */
                    sp = roma;
                    while (cont--)
                        *(--sp) = *q;
                    cont = 0;
                } else if (cont) {
                    cont = 0;
                }
                /* Copy characters to output string */
                plen -= ylen;
                p += ylen;
                while (*q != '\0') 
                    *roma++ = *q++;
            } else {
                /* Failed to convert.       */
                cont = 0;
                /* Normal japanese character(except gaiji) is 2byte. */
                *roma++ = *p++;
                *roma++ = *p++;
                plen -= 2;
            }
            zenflg++;
        } else {
            /* Non multi-byte character */
            sp = p;
            while (*sp != '\0' && !(iskan1(*sp, serverIF[buf->server].lang)
                    && iskan2(*(sp + 1), serverIF[buf->server].lang))) {
                sp++;
            }
            ylen = sp - p;
            i = 0;
            for (rktp = buf->rktable; rktp->next != NULL; rktp = rktp->next) {
                if (rktp->rlen > i) {
                    r = p;
                    t = rktp->roma;
                    while (r < sp && *r++ == *t++)
                        ;
                    if ((rlen = r - p) > i)
                        i = rlen;
                }
            }
            if (i > 0) {
                if (!zenflg && !plosflg && !isvowel(*p) && p > tyomi
                        && (t = buf->plosive)) {
                    /* Same as plosive table first character */
                    if (*t == *(p - 1))
                        cont++;
                }
                if (cont && !isvowel(*p)) {
                    /* Correct consonant character of previous plosive */
                    sp = roma;
                    while (cont--)
                        *(--sp) = *p;
                    cont = 0;
                } else if (cont) {
                    cont = 0;
                }
            } 
            /* Copy characters to output string */
            plen -= ylen;
            while (ylen--)
                *roma++ = *p++;
            zenflg = 0;
            plosflg = 0;
        }
    }
    *roma = '\0';

    if (!zenflg && (r = buf->plosive)) {
        p--;
        while (*r != '\0') {
            if (*r == *p) {
                t = buf->plosive;
                while (*t != '\0') {
                    if (p > tyomi && *t == *p && *t == *(p - 1)) {
                        cont++;
                        break;
                    }
                    t++;
                }
                break;
            }
            r++;
        }
        if (*r != '\0') {
            /* Correct consonant character of previous plosive */
            sp = (--roma);
            while (cont--)
                *(--sp) = *p;
        }
    } else if (zenflg && cont && plosflg) {
        q = tmp;
        if (*q != '\0') {
            /* Correct consonant character of previous plosive */
            sp = (--roma);
            while (cont--) {
                if (!isvowel(*(--sp)))
                    *sp = *q;
            }
        }
    }
}

void
_Xsj3cHiraToZKata(buf, dest, src)
    Xsj3cBuf                    buf;
    register unsigned char     *dest,  *src;
{
    register wchar              s;
    register unsigned char      c;
    register Xsj3cHKTable      *hktp;
    register Xsj3cZHTable      *zhtp;

    while ((c = *src++) != '\0') {
        if (iskan1(c, serverIF[buf->server].lang)
                && iskan2(*src, serverIF[buf->server].lang)) {
            s = (c << 8) + *src;
            if (ishira(s, serverIF[buf->server].lang)) {
                for (hktp = buf->hktable; hktp != NULL; hktp = hktp->next) {
                    if (c == *hktp->hira)
                        if (*src == *(hktp->hira + 1))
                            break;
                }
                if (hktp != NULL) {
                    *dest++ = *hktp->zkata;
                    *dest++ = *(hktp->zkata + 1);
                } else {
                    *dest++ = c;
                    *dest++ = *src;
                    Xsj3cWarning("wrong sjhk table");
                }
            } else {
                *dest++ = c;
                *dest++ = *src;
            }
            src++;
        } else if (isdakuten(c)) {
            for (zhtp = buf->zhtable; zhtp != NULL; zhtp = zhtp->next) {
                if (c == *zhtp->hkata)
                    break;
            }
            if (zhtp != NULL) {
                *dest++ = *zhtp->zkana;
                *dest++ = *(zhtp->zkana + 1);
            } else {
                *dest++ = c;
            }
        } else {
            *dest++ = c;
        }
    }
    *dest = '\0';
}

void
_Xsj3cHankakuToHira(buf, dest, src)
    Xsj3cBuf                    buf;
    register unsigned char     *dest,  *src;
{
    register unsigned char      c;
    register Xsj3cHKTable      *hktp;
    register Xsj3cZHTable      *zhtp;
    register int                len;
    unsigned char               tmp[RBUFSIZ];

    while ((c = *src++) != '\0') {
        if (iskan1(c, serverIF[buf->server].lang)
                && iskan2(*src, serverIF[buf->server].lang)) {
            *dest++ = c;
            *dest++ = *src++;
            continue;
        }
        if (iskana(c)) {
            len = 0;
            for (hktp = buf->hktable; hktp != NULL; hktp = hktp->next) {
                if (len < hktp->hlen && c == *hktp->hkata) {
                    if (hktp->hlen > 1) {
                        if (iskana(*src) && *src == *(hktp->hkata + 1)) {
                            src++;
                            len = hktp->hlen;
                            strcpy(tmp, hktp->hira);
                        }
                    } else {
                        len = 1;
                        strcpy(tmp, hktp->hira);
                    }
                }
            }
            if (len) {
                strcpy(dest, tmp);
                dest += strlen(tmp);
            } else {
                for (zhtp = buf->zhtable; zhtp != NULL; zhtp = zhtp->next) {
                    if (c == *zhtp->hkata)
                        break;
                }
                if (zhtp != NULL) {
                    *dest++ = *zhtp->zkana;
                    *dest++ = *(zhtp->zkana + 1);
                } else {
                    *dest++ = c;
                }
            }
        } else {
            for (zhtp = buf->zhtable; zhtp != NULL; zhtp = zhtp->next) {
                if (c == *zhtp->hkata)
                    break;
            }
            if (zhtp != NULL) {
                *dest++ = *zhtp->zkana;
                *dest++ = *(zhtp->zkana + 1);
            } else {
                *dest++ = c;
            }
        }
    }
    *dest = '\0';
}

void
_Xsj3cHankakuToZKata(buf, dest, src)
    Xsj3cBuf                    buf;
    register unsigned char     *dest,  *src;
{
    register unsigned char      c;
    register Xsj3cHKTable      *hktp;
    register Xsj3cZHTable      *zhtp;
    register int                len;
    unsigned char               tmp[RBUFSIZ];

    while ((c = *src++) != '\0') {
        if (iskan1(c, serverIF[buf->server].lang)
                && iskan2(*src, serverIF[buf->server].lang)) {
            *dest++ = c;
            *dest++ = *src++;
            continue;
        }
        if (iskana(c)) {
            len = 0;
            for (hktp = buf->hktable; hktp != NULL; hktp = hktp->next) {
                if (len < hktp->hlen && c == *hktp->hkata) {
                    if (hktp->hlen > 1) {
                        if (iskana(*src) && *src == *(hktp->hkata + 1)) {
                            src++;
                            strcpy(tmp, hktp->zkata);
                            len = hktp->hlen;
                        }
                    } else {
                        strcpy(tmp, hktp->zkata);
                        len = hktp->hlen;
                    }
                }
            }
            if (len) {
                strcpy(dest, tmp);
                dest += strlen(tmp);
            } else {
                for (zhtp = buf->zhtable; zhtp != NULL; zhtp = zhtp->next) {
                    if (c == *zhtp->hkata)
                        break;
                }
                if (zhtp != NULL) {
                    *dest++ = *zhtp->zkana;
                    *dest++ = *(zhtp->zkana + 1);
                } else {
                    *dest++ = c;
                }
            }
        } else {
            for (zhtp = buf->zhtable; zhtp != NULL; zhtp = zhtp->next) {
                if (c == *zhtp->hkata)
                    break;
            }
            if (zhtp != NULL) {
                *dest++ = *zhtp->zkana;
                *dest++ = *(zhtp->zkana + 1);
            } else {
                *dest++ = c;
            }
        }
    }
    *dest = '\0';
}

void
_Xsj3cHKataToHira(buf, dest, src)
    Xsj3cBuf                    buf;
    register unsigned char     *dest,  *src;
{
    register unsigned char      c;
    register Xsj3cHKTable      *hktp;
    register Xsj3cZHTable      *zhtp;
    register int                len;
    unsigned char               tmp[RBUFSIZ];

    while ((c = *src++) != '\0') {
        if (iskan1(c, serverIF[buf->server].lang)
                && iskan2(*src, serverIF[buf->server].lang)) {
            *dest++ = c;
            *dest++ = *src++;
            continue;
        }
        if (iskana(c)) {
            len = 0;
            for (hktp = buf->hktable; hktp != NULL; hktp = hktp->next) {
                if (len < hktp->hlen && c == *hktp->hkata) {
                    if (hktp->hlen > 1) {
                        if (iskana(*src) && *src == *(hktp->hkata + 1)) {
                            src++;
                            len = hktp->hlen;
                            strcpy(tmp, hktp->hira);
                        }
                    } else {
                        len = 1;
                        strcpy(tmp, hktp->hira);
                    }
                }
            }
            if (len) {
                strcpy(dest, tmp);
                dest += strlen(tmp);
            } else {
                for (zhtp = buf->zhtable; zhtp != NULL; zhtp = zhtp->next) {
                    if (c == *zhtp->hkata)
                        break;
                }
                if (zhtp != NULL) {
                    *dest++ = *zhtp->zkana;
                    *dest++ = *(zhtp->zkana + 1);
                } else {
                    *dest++ = c;
                }
            }
        } else {
            *dest++ = c;
        }
    }
    *dest = '\0';
}

void
_Xsj3cHKataToZKata(buf, dest, src)
    Xsj3cBuf                    buf;
    register unsigned char     *dest,  *src;
{
    register unsigned char      c;
    register Xsj3cHKTable      *hktp;
    register Xsj3cZHTable      *zhtp;
    register int                len;
    unsigned char               tmp[RBUFSIZ];

    while ((c = *src++) != '\0') {
        if (iskan1(c, serverIF[buf->server].lang)
                && iskan2(*src, serverIF[buf->server].lang)) {
            *dest++ = c;
            *dest++ = *src++;
            continue;
        }
        if (iskana(c)) {
            len = 0;
            for (hktp = buf->hktable; hktp != NULL; hktp = hktp->next) {
                if (len < hktp->hlen && c == *hktp->hkata) {
                    if (hktp->hlen > 1) {
                        if (iskana(*src) && *src == *(hktp->hkata + 1)) {
                            src++;
                            strcpy(tmp, hktp->zkata);
                            len = hktp->hlen;
                        }
                    } else {
                        strcpy(tmp, hktp->zkata);
                        len = hktp->hlen;
                    }
                }
            }
            if (len) {
                strcpy(dest, tmp);
                dest += strlen(tmp);
            } else {
                for (zhtp = buf->zhtable; zhtp != NULL; zhtp = zhtp->next) {
                    if (c == *zhtp->hkata)
                        break;
                }
                if (zhtp != NULL) {
                    *dest++ = *zhtp->zkana;
                    *dest++ = *(zhtp->zkana + 1);
                } else {
                    *dest++ = c;
                }
            }
        } else {
            *dest++ = c;
        }
    }
    *dest = '\0';
}


void
_Xsj3cZKanaToHKata(buf, dest, src)
    Xsj3cBuf                    buf;
    register unsigned char     *dest,  *src;
{
    register unsigned char      c;
    register wchar              s;
    register Xsj3cHKTable      *hktp;
    register Xsj3cZHTable      *zhtp;

    while ((c = *src++) != '\0') {
        if (iskan1(c, serverIF[buf->server].lang)
                && iskan2(*src, serverIF[buf->server].lang)) {
            s = (c << 8) + *src;
            if (ishira(s, serverIF[buf->server].lang)) {
                for (hktp = buf->hktable; hktp != NULL; hktp = hktp->next) {
                    if (c == *hktp->hira)
                        if (*src == *(hktp->hira + 1))
                            break;
                }
                if (hktp != NULL) {
                    *dest++ = *hktp->hkata;
                    if (hktp->hlen > 1)
                        *dest++ = *(hktp->hkata + 1);
                } else {
                    *dest++ = c;
                    *dest++ = *src;
                    Xsj3cWarning("wrong sjhk table");
                }
            } else if (iskata(s, serverIF[buf->server].lang)) {
                for (hktp = buf->hktable; hktp != NULL; hktp = hktp->next) {
                    if (c == *hktp->zkata)
                        if (*src == *(hktp->zkata + 1))
                            break;
                }
                if (hktp != NULL) {
                    *dest++ = *hktp->hkata;
                    if (hktp->hlen > 1)
                        *dest++ = *(hktp->hkata + 1);
                } else {
                    *dest++ = c;
                    *dest++ = *src;
                    Xsj3cWarning("wrong sjhk table");
                }
            } else {
                for (zhtp = buf->zhtable; zhtp != NULL; zhtp = zhtp->next) {
                    if (c == *zhtp->zkana)
                        if (*src == *(zhtp->zkana + 1))
                            break;
                }
                if (zhtp != NULL) {
                    *dest++ = *zhtp->hkata;
                } else {
                    for (zhtp = buf->zhtable; zhtp != NULL; zhtp = zhtp->next) {
                        if (c == *zhtp->zalpha)
                            if (*src == *(zhtp->zalpha + 1))
                                break;
                    }
                    if (zhtp != NULL) {
                        *dest++ = *zhtp->hkata;
                    } else {
                        *dest++ = c;
                        *dest++ = *src;
                    }
                }
            }
            src++;
        } else {
            *dest++ = c;
        }
    }
    *dest = '\0';
}

void
_Xsj3cZKataToHira(buf, dest, src)
    Xsj3cBuf                    buf;
    register unsigned char     *dest,  *src;
{
    register wchar              s;
    register unsigned char      c;
    register Xsj3cHKTable      *hktp;
    register Xsj3cZHTable      *zhtp;

    while ((c = *src++) != '\0') {
        if (iskan1(c, serverIF[buf->server].lang)
                && iskan2(*src, serverIF[buf->server].lang)) {
            s = (c << 8) + *src;
            if (iskata(s, serverIF[buf->server].lang)) {
                for (hktp = buf->hktable; hktp != NULL; hktp = hktp->next) {
                    if (c == *hktp->zkata)
                        if (*src == *(hktp->zkata + 1))
                            break;
                }
                if (hktp != NULL) {
                    *dest++ = *hktp->hira;
                    *dest++ = *(hktp->hira + 1);
                } else {
                    *dest++ = c;
                    *dest++ = *src;
                    Xsj3cWarning("wrong sjhk table");
                }
            } else {
                *dest++ = c;
                *dest++ = *src;
            }
            src++;
        } else if (isdakuten(c)) {
            for (zhtp = buf->zhtable; zhtp != NULL; zhtp = zhtp->next) {
                if (c == *zhtp->hkata)
                    break;
            }
            if (zhtp != NULL) {
                *dest++ = *zhtp->zkana;
                *dest++ = *(zhtp->zkana + 1);
            } else {
                *dest++ = c;
            }
        } else {
            *dest++ = c;
        }
    }
    *dest = '\0';
}

void
_Xsj3cHAlphaToZAlpha(buf, dest, src)
    Xsj3cBuf                    buf;
    register unsigned char     *dest,  *src;
{
    register unsigned char      c;
    register Xsj3cZHTable      *zhtp;

    while ((c = *src++) != '\0') {
        if (iskan1(c, serverIF[buf->server].lang)
                && iskan2(*src, serverIF[buf->server].lang)) {
            *dest++ = c;
            *dest++ = *src++;
            continue;
        }
        for (zhtp = buf->zhtable; zhtp != NULL; zhtp = zhtp->next) {
            if (c == *zhtp->halpha)
                break;
        }
        if (zhtp != NULL) {
            *dest++ = *zhtp->zalpha;
            *dest++ = *(zhtp->zalpha + 1);
        } else {
            *dest++ = c;
        }
    }
    *dest = '\0';
}

void
_Xsj3cHAlphaToZKana(buf, dest, src)
    Xsj3cBuf                    buf;
    register unsigned char     *dest,  *src;
{
    register unsigned char      c;
    register Xsj3cZHTable      *zhtp;

    while ((c = *src++) != '\0') {
        if (iskan1(c, serverIF[buf->server].lang)
                && iskan2(*src, serverIF[buf->server].lang)) {
            *dest++ = c;
            *dest++ = *src++;
            continue;
        }
        for (zhtp = buf->zhtable; zhtp != NULL; zhtp = zhtp->next) {
            if (c == *zhtp->halpha)
                break;
        }
        if (zhtp != NULL) {
            *dest++ = *zhtp->zkana;
            *dest++ = *(zhtp->zkana + 1);
        } else {
            *dest++ = c;
        }
    }
    *dest = '\0';
}

void
_Xsj3cHAlphaToHKata(buf, dest, src)
    Xsj3cBuf                    buf;
    register unsigned char     *dest,  *src;
{
    register unsigned char      c;
    register Xsj3cHKTable      *hktp;

    while ((c = *src++) != '\0') {
        if (iskan1(c, serverIF[buf->server].lang)
                && iskan2(*src, serverIF[buf->server].lang)) {
            *dest++ = c;
            *dest++ = *src++;
            continue;
        }
        for (hktp = buf->hktable; hktp != NULL; hktp = hktp->next) {
            if (c == *hktp->halpha)
                break;
        }
        if (hktp != NULL) {
            *dest++ = *hktp->hkata;
        } else {
            *dest++ = c;
        }
    }
    *dest = '\0';
}

void
_Xsj3cZAlphaToHAlpha(buf, dest, src)
    Xsj3cBuf                    buf;
    register unsigned char     *dest,  *src;
{
    register unsigned char      c;
    register Xsj3cZHTable      *zhtp;

    while ((c = *src++) != '\0') {
        if (iskan1(c, serverIF[buf->server].lang)
                && iskan2(*src, serverIF[buf->server].lang)) {
            for (zhtp = buf->zhtable; zhtp != NULL; zhtp = zhtp->next) {
                if (c == *zhtp->zalpha)
                    if (*src == *(zhtp->zalpha + 1))
                        break;
            }
            if (zhtp != NULL) {
                *dest++ = *zhtp->halpha;
            } else {
                *dest++ = c;
                *dest++ = *src;
            }
            src++;
        } else {
            *dest++ = c;
        }
    }
    *dest = '\0';
}

static void
_Xsj3cLowerToUpper(buf, str)
    Xsj3cBuf                    buf;
    register unsigned char     *str;
{
    register unsigned char      c;
    register wchar              s;

    while ((c = *str++) != '\0') {
        if (iskan1(c, serverIF[buf->server].lang)
                && iskan2(*str, serverIF[buf->server].lang)) {
            s = (c << 8) + *str;
            if (iszlower(s, serverIF[buf->server].lang)) {
                s -= 0x21;
                *(str - 1) = (s >> 8);
                *str++ = (s & 0xff);
            } else {
                str++;
            }
            continue;
        }
        if (islower(c)) {
            *(str - 1) = toupper(c);
        }
    }
}

static void
_Xsj3cUpperToLower(buf, str)
    Xsj3cBuf                    buf;
    register unsigned char     *str;
{
    register unsigned char      c;
    register wchar              s;

    while ((c = *str++) != '\0') {
        if (iskan1(c, serverIF[buf->server].lang)
                && iskan2(*str, serverIF[buf->server].lang)) {
            s = (c << 8) + *str;
            if (iszupper(s, serverIF[buf->server].lang)) {
                s += 0x21;
                *(str - 1) = (s >> 8);
                *str++ = (s & 0xff);
            } else {
                str++;
            }
            continue;
        }
        if (isupper(c)) {
            *(str - 1) = tolower(c);
        }
    }
}
