#ifndef lint
static char *rcsid = "$Header: util.c,v 2.2 93/09/21 09:42:34 nao Exp $";
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
#include <stdio.h>
#include "common.h"
#include "sj3ctype.h"
#include "segment.h"

extern Xsj3cCVServerIF      serverIF[SERVER_NUM];

int                         in_lang = JP_EUC;
int                         out_lang = JP_EUC;
int                         locked[SERVER_NUM] = {0};
Xsj3ccMode                  KanaMask = 0;

void                        Xsj3cSetInLang();
void                        Xsj3cSetOutLang();
void                        Xsj3cError();
void                        Xsj3cWarning();

int                         _Xsj3cwPStowOUT();
int                         _Xsj3cwOUTtowPS();
int                         _Xsj3cmPStowPS();
int                         _Xsj3cmPStowOUT();
int                         _Xsj3cmPStowPSn();
int                         _Xsj3cmPStowOUTn();
void                        _Xsj3cwPStomPS();
void                        _Xsj3cwOUTtomPS();

int                         _Xsj3cCmp();
int                         _Xsj3cWcpy();
void                        _Xsj3cWcat();
int                         _Xsj3cWlen();
char                       *_Xsj3cItoa();
char                       *_Xsj3cXtoa();

void                        _Xsj3cInsertChar();
void                        _Xsj3cInsertWchar();
void                        _Xsj3cExtractChar();
void                        _Xsj3cStoreYomi();
int                         _Xsj3cStoreKanji();

void                        _Xsj3cFlushDcid();
void                        _Xsj3cClearDcid();

Xsj3csMode                  _Xsj3cCheckMode();

extern wchar                _Xsj3csjis2euc();
extern wchar                _Xsj3csjis2jis();
extern wchar                _Xsj3ceuc2sjis();
extern wchar                _Xsj3ceuc2jis();
extern wchar                _Xsj3cjis2sjis();
extern wchar                _Xsj3cjis2euc();

wchar (*CodeConvFunc[4][4])() = {
    NULL,            _Xsj3csjis2euc,  _Xsj3csjis2jis,  _Xsj3csjis2jis,
    _Xsj3ceuc2sjis,  NULL,            _Xsj3ceuc2jis,   _Xsj3ceuc2jis,
    _Xsj3cjis2sjis,  _Xsj3cjis2euc,   NULL,            NULL,
    _Xsj3cjis2sjis,  _Xsj3cjis2euc,   NULL,            NULL
};

/*
 * Xsj3cSetInLang()
 *  Set input LANG
 */
void
Xsj3cSetInLang(lang)
    int     lang;
{
    in_lang = lang;
}

/*
 * Xsj3cSetOutLang()
 *  Set output LANG
 */
void
Xsj3cSetOutLang(lang)
    int     lang;
{
    out_lang = lang;
}

/*
 * Xsj3cSetKanaMod()
 *  Set output LANG
 */
void
Xsj3cSetKanaMod(mod)
    unsigned long   mod;
{
    KanaMask = mod;
}

/*
 * Xsj3cWarning()
 *  Print warning messages. (limit of 10 args)
 */
/*VARARGS1*/
void
Xsj3cWarning(message, s0, s1, s2, s3, s4, s5, s6, s7, s8, s9)
    char   *message;
    char   *s0, *s1, *s2, *s3, *s4, *s5, *s6, *s7, *s8, *s9;
{
    (void)fprintf(stderr, "Xsj3cWarning: ");
    if (message && *message) {
       (void)fprintf(stderr, message, s0, s1, s2, s3, s4, s5, s6, s7, s8, s9);
       (void)fprintf(stderr, "\n");
    }
    return;
}

/*
 * Xsj3cError()
 *  Print error messages and exit. (limit of 10 args)
 */
/*VARARGS1*/
void
Xsj3cError(message, s0, s1, s2, s3, s4, s5, s6, s7, s8, s9)
    char   *message;
    char   *s0, *s1, *s2, *s3, *s4, *s5, *s6, *s7, *s8, *s9;
{
    (void)fprintf(stderr, "Xsj3cError: ");
    if (message && *message) {
       (void)fprintf(stderr, message, s0, s1, s2, s3, s4, s5, s6, s7, s8, s9);
       (void)fprintf(stderr, "\n");
    }
    exit(1);
}

/*
 * _Xsj3cwPStowOUT()
 *  Convert string(unsigned short) code from process code to out_lang.
 */
int
_Xsj3cwPStowOUT(buf, p, q)
    Xsj3cBuf                buf;
    register wchar         *p, *q;
{
    register int            i = 0;
    register wchar          (*conv)();

    if (p == NULL)
        return i;
    if (conv = CodeConvFunc[serverIF[buf->server].lang][out_lang]) {
        while (*q != '\0') {
            i++;
            if (iskan1(*q >> 8, serverIF[buf->server].lang)
                    && iskan2(0xff & *q, serverIF[buf->server].lang)) {
                *p++ = conv(*q++);
                continue;
            } else if (iseuckana(*q >> 8)) {
                *p++ = 0xff & *q++;
                continue;
            }
            *p++ = *q++;
        }
    } else {
        while (*p++ = *q++) i++;
        return i;
    }
    *p = '\0';
    return i;
}

/*
 * _Xsj3cwOUTtowPS()
 *  Convert string(unsigned short) code from out_lang to process code.
 */
int
_Xsj3cwOUTtowPS(buf, p, q)
    Xsj3cBuf                buf;
    register wchar         *p, *q;
{
    register int            i = 0;
    register wchar          (*conv)();

    if (p == NULL)
        return i;
    if (conv = CodeConvFunc[out_lang][serverIF[buf->server].lang]) {
        while (*q != '\0') {
            i++;
            if (*q & 0xff00) {
                *p++ = conv(*q++);
                continue;
            }
            *p++ = *q++;
        }
    } else {
        while (*p++ = *q++) i++;
        return i;
    }
    *p = '\0';
    return i;
}

/*
 * _Xsj3cCmp()
 *  Compare 2 words in lower and if they are matched, return True(1).
 * Unless they are matched, return False(0).
 */
int
_Xsj3cCmp(s1, s2)
    register char  *s1, *s2;
{
    register char   c1, c2;

    while (*s1 != '\0') {
        c1 = *s1++;
        c2 = *s2++;
        if (tolower(c1) != tolower(c2))
            return 0;
    } 
    if (s2 && *s2 != '\0')
        return 0;
    return 1;
}

/*
 * _Xsj3cmPStowPS()
 *  Multi bytes character(2nd arg.) to wide character(1st arg.)
 * conversion routine. (Only for Shift-JIS code)
 */
int
_Xsj3cmPStowPS(buf, w, m)
    Xsj3cBuf                    buf;
    register wchar             *w;
    register unsigned char     *m;
{
    register unsigned char      c;
    register int                i = 0;

    while ((c = *m++) != '\0') {
        if (iskan1(c, serverIF[buf->server].lang)
                && iskan2(*m, serverIF[buf->server].lang)) {
            *w++ = (c << 8) + *m++;
        } else {
            *w++ = 0xff & c;
        }
        i++;
    }
    *w = '\0';
    return i;
}

/*
 * _Xsj3cmPStowOUT()
 *  Multi bytes character(2nd arg.) to wide character(1st arg.)
 * conversion routine.
 */
int
_Xsj3cmPStowOUT(buf, w, m)
    Xsj3cBuf                    buf;
    register wchar             *w;
    register unsigned char     *m;
{
    register unsigned char      c;
    register int                i = 0;
    register wchar              (*conv)();

    if (conv = CodeConvFunc[serverIF[buf->server].lang][out_lang]) {
        while ((c = *m++) != '\0') {
            if (iskan1(c, serverIF[buf->server].lang)
                    && iskan2(*m, serverIF[buf->server].lang)) {
                *w++ = conv((c << 8) + *m++);
            } else {
                *w++ = 0xff & c;
            }
            i++;
        }
    } else {
        while ((c = *m++) != '\0') {
            if (iskan1(c, serverIF[buf->server].lang)
                    && iskan2(*m, serverIF[buf->server].lang)) {
                *w++ = (c << 8) + *m++;
            } else {
                *w++ = 0xff & c;
            }
            i++;
        }
    }
    *w = '\0';
    return i;
}

/*
 * _Xsj3cmPStowPSn()
 *  Multi bytes character(2nd arg.) to wide character(1st arg.)
 * conversion routine. (Maximum n(3rd arg.) bytes)
 */
int
_Xsj3cmPStowPSn(buf, w, m, n)
    Xsj3cBuf                    buf;
    register wchar             *w;
    register unsigned char     *m;
    register int                n;
{
    register unsigned char      c;
    register int                i = 0;

    while ((c = *m++) != '\0' && n--) {
        if (iskan1(c, serverIF[buf->server].lang)
                && iskan2(*m, serverIF[buf->server].lang)) {
            *w++ = (c << 8) + *m++;
            n--;
        } else {
            *w++ = 0xff & c;
        }
        i++;
    }
    *w = '\0';
    return i;
}

/*
 * _Xsj3cmPStowOUTn()
 *  Multi bytes character(2nd arg.) to wide character(1st arg.)
 * conversion routine. (Maximum n(3rd arg.) bytes)
 */
int
_Xsj3cmPStowOUTn(buf, w, m, n)
    Xsj3cBuf                    buf;
    register wchar             *w;
    register unsigned char     *m;
    register int                n;
{
    register unsigned char      c;
    register int                i = 0;
    register wchar              (*conv)();

    if (conv = CodeConvFunc[serverIF[buf->server].lang][out_lang]) {
        while ((c = *m++) != '\0' && n--) {
            if (iskan1(c, serverIF[buf->server].lang)
                    && iskan2(*m, serverIF[buf->server].lang)) {
                *w++ = conv((c << 8) + *m++);
                n--;
            } else {
                *w++ = 0xff & c;
            }
            i++;
        }
    } else {
        while ((c = *m++) != '\0' && n--) {
            if (iskan1(c, serverIF[buf->server].lang)
                    && iskan2(*m, serverIF[buf->server].lang)) {
                *w++ = (c << 8) + *m++;
                n--;
            } else {
                *w++ = 0xff & c;
            }
            i++;
        }
    }
    *w = '\0';
    return i;
}

/*
 * _Xsj3cwPStomPS()
 *  Convert wide character(2nd arg.) to multi bytes characters(1st arg.).
 * (Only for Shift-JIS code)
 */
void
_Xsj3cwPStomPS(buf, m, w)
    Xsj3cBuf                    buf;
    register unsigned char     *m;
    register wchar             *w;
{
    register unsigned char      c1,   c2;

    while (*w != '\0') {
        c1 = *w >> 8;
        c2 = *w & 0xff;
        if (iskan1(c1, serverIF[buf->server].lang)
                    && iskan2(c2, serverIF[buf->server].lang)) {
            *m++ = c1;
            *m++ = c2;
        } else {
            *m++ = c2;
        }
        w++;
    }
    *m = '\0';
}

/*
 * _Xsj3cwOUTtomPS()
 *  Convert wide character(2nd arg.) to multi bytes characters(1st arg.).
 */
void
_Xsj3cwOUTtomPS(buf, m, w)
    Xsj3cBuf                    buf;
    register unsigned char     *m;
    register wchar             *w;
{
    register wchar              (*conv)();
    register wchar              s;

    if (conv = CodeConvFunc[out_lang][serverIF[buf->server].lang]) {
        while (*w != '\0') {
            if (*w & 0xff00) {
                s = conv(*w);
                *m++ = s >> 8;
                *m++ = s & 0xff;
            } else {
                *m++ = (*w & 0xff);
            }
            w++;
        }
    } else {
        while (*w != '\0') {
            if (*w & 0xff00) {
                *m++ = *w >> 8;
                *m++ = *w & 0xff;
            } else {
                *m++ = *w & 0xff;
            }
            w++;
        }
    }
    *m = '\0';
}

/*
 * _Xsj3cWcpy()
 *  Copy wide characters(2nd arg.) to wide characters(1st arg.)
 */
int
_Xsj3cWcpy(w1, w2)
    register wchar             *w1,    *w2;
{
    register int                i = 0;

    while (*w1++ = *w2++) i++;
    return i;
}

/*
 * _Xsj3cWcat()
 *  Appends a copy of  wide character(2nd arg.) to
 * the end of wide character(1st arg.).
 */
void
_Xsj3cWcat(w1, w2)
    register wchar             *w1,    *w2;
{
    while (*w1++);
    w1--;
    while (*w1++ = *w2++);
}

/*
 * _Xsj3cWlen()
 *  Returns the number of characters in 1st arg.
 */
int
_Xsj3cWlen(w)
    register wchar             *w;
{
    register int                i = 0;

    while (*w++)
        i++;
    return i;
}

/*
 * _Xsj3cItoa()
 *  Converts digit number to string type and returns it.
 */
char *
_Xsj3cItoa(num)
    register int                num;
{
    register int                i,  bnum;
    static char                 ch[24];

    i = 20;
    ch[i]= '\0';
    while (num > 9) {
        bnum = num;
        num /= 10;
        ch[--i] = bnum - num * 10 + '0';
    }
    ch[--i] = num + '0';
    return (&ch[i]);
}

/*
 * _Xsj3cXtoa()
 *  Converts hex number to string type and returns it.
 */
char *
_Xsj3cXtoa(num)
    register int                num;
{
    register int                i,  bnum,   anum;
    static char                 ch[20];

    i = 18;
    ch[i]= '\0';
    while (num) {
        bnum = num;
        num >>= 4;
        if ((anum = (bnum - (num << 4))) > 9)
            ch[--i] = anum + 'W';
        else
            ch[--i] = anum + '0';
    }
    ch[--i] = 'x';
    ch[--i] = '0';
    return (&ch[i]);
}

/*
 * _Xsj3cInsertChar()
 *  Convert multi bytes characters(2nd arg.) to wide characters and 
 * insert them (n characters) to yomi buffer after current position.
 * (Only for Shift-JIS code)
 */
void
_Xsj3cInsertChar(buf, seg, m, n)
    Xsj3cBuf                    buf;
    register Xsj3cSeg           seg;
    register unsigned char     *m;
    register int                n;
{
    register unsigned char      c;
    register int                i = seg->num;

    while (i >= seg->cur) {
        seg->yomi[i + n] = seg->yomi[i];
        i--;
    }
    
    i = n;
    while ((c = *m++) != '\0'&& i--) {
        if (iskan1(c, serverIF[buf->server].lang)
                && iskan2(*m, serverIF[buf->server].lang)) {
            seg->yomi[seg->cur++] = (c << 8) + *m++;
        } else {
            seg->yomi[seg->cur++] = 0xff & c;
        }
    }
    seg->num += n;
}

/*
 * _Xsj3cInsertWchar()
 * insert wide characters to yomi buffer after current position.
 * (Only for Shift-JIS code)
 */
void
_Xsj3cInsertWchar(seg, w, n)
    register Xsj3cSeg           seg;
    register wchar             *w;
    register int                n;
{
    register int                i = seg->num;

    while (i >= seg->cur) {
        seg->yomi[i + n] = seg->yomi[i];
        i--;
    }
    
    i = n;
    while (*w != '\0' && i--) {
        seg->yomi[seg->cur++] = *w++;
    }
    seg->num += n;
}

/*
 * _Xsj3cExtractChar()
 *  Extract n characters before current postsion from yomi buffer
 * and convert them to multi bytes character(1st arg.).
 * (Only for Shift-JIS code)
 */
void
_Xsj3cExtractChar(buf, seg, m, n)
    Xsj3cBuf                    buf;
    register Xsj3cSeg           seg;
    register unsigned char     *m;
    register int                n;
{
    register int                i = seg->cur - n,    j = n;
    register unsigned char      c1,   c2;

    while ((seg->yomi[i]) != '\0' && j--) {
        c1 = seg->yomi[i] >> 8;
        c2 = seg->yomi[i] & 0xff;
        if (iskan1(c1, serverIF[buf->server].lang)
                && iskan2(c2, serverIF[buf->server].lang)) {
            *m++ = c1;
            *m++ = c2;
        } else {
            *m++ = c2;
        }
        i++;
    }
    *m = '\0';
    seg->cur -= n;
    j = seg->cur;
    while (i < seg->num) {
        seg->yomi[j++] = seg->yomi[i++];
    }
    seg->yomi[j] = '\0';
    seg->num -= n;
}

/*
 * _Xsj3cStoreYomi()
 *  Convert code of internal yomi buffer from Shift-JIS to EUC
 * and put it to display yomi buffer.
 */
void
_Xsj3cStoreYomi(buf, seg, pos)
    Xsj3cBuf                    buf;
    register Xsj3cSeg           seg;
    register int                pos;
{
    register int                i;
    register wchar              (*conv)();

    if (conv = CodeConvFunc[serverIF[buf->server].lang][out_lang]) {
        for (i = pos; i < seg->num; i++) {
            if (seg->yomi[i] & 0xff00)
                seg->disp[i] = conv(seg->yomi[i]);
            else
                seg->disp[i] = seg->yomi[i];
        }
    } else {
        for (i = pos; i < seg->num; i++)
            seg->disp[i] = seg->yomi[i];
    }
    seg->disp[i] = '\0';
    seg->dnum = seg->num;
}

/*
 * _Xsj3cStoreKanji()
 *  Store converted strings to converted display buffers.
 * (src-buffer and dest-buffer)
 */
int
_Xsj3cStoreKanji(buf, bun, cur, num, change)
    Xsj3cBuf                buf;
    SJ3_BUNSETU            *bun;
    int                     cur,    num,    change;
{
    register int            i,  j;

    if (num > 1) {
        for (i = buf->segnum - 1; i > cur; i--) {
            j = i + num - 1;
            buf->input[j] = buf->input[i];
        }
        for (i = cur + 1; i < cur + num; i++)
            buf->input[i] = NULL;
    }
    for (i = cur; i < cur + num; i++, bun++) {
        if (!buf->input[i]) 
            buf->input[i] = (Xsj3cSeg)Xsj3cCreateSegment(buf);
        else 
            Xsj3cResizeSegment(buf->input[i], KANABUFSIZ);
        buf->input[i]->num = _Xsj3cmPStowPSn(buf, buf->input[i]->yomi,
                bun->srcstr, bun->srclen);
        if (bun->deststr && bun->destlen) {
            buf->input[i]->dnum = _Xsj3cmPStowOUTn(buf, buf->input[i]->disp,
                    bun->deststr, bun->destlen);
        } else {
            _Xsj3cStoreYomi(buf, buf->input[i], 0);
        }
        buf->input[i]->status = SEG_CONVED;
        buf->input[i]->cur = 0;
        buf->input[i]->change = change;
        buf->input[i]->edit = SEG_NOEDIT;
        buf->input[i]->dcid = bun->dcid;
        buf->input[i]->cursegmode = _Xsj3cCheckMode(buf, buf->input[i]);
    }
    if (buf->gakusyuu && buf->convmode == InputModeMask) {
        serverIF[buf->server].func[FUNC_LOCK]();
        locked[buf->server]++;
    } else if (!buf->gakusyuu) {
        for (i = cur; i < num; i++) 
            bzero(&(buf->input[i]->dcid), sizeof(buf->input[i]->dcid));
    }
}

/*
 * _Xsj3cCheckMode()
 *  Check character mode of the yomi buffer of segment.
 */
Xsj3csMode
_Xsj3cCheckMode(buf, seg)
    Xsj3cBuf                    buf;
    register Xsj3cSeg           seg;
{
    register unsigned char      c1, c2;

    c1 = seg->yomi[0] >> 8;
    c2 = seg->yomi[0] & 0xff;
    if (c2 != '\0') {
        if (iskan1(c1, serverIF[buf->server].lang)
                && iskan2(c2, serverIF[buf->server].lang)) {
            if (iskata(seg->yomi[0], serverIF[buf->server].lang))
                return (MODE_ZKATA);
            else if (iszalpha(seg->yomi[0], serverIF[buf->server].lang))
                return (MODE_ZALPHA);
            else
                return (MODE_HIRA);
        } else {
            if (iskana(c2))
                return (MODE_HKATA);
            else
                return (MODE_HALPHA);
        }
    } else {
        return (MODE_HIRA);
    }
}

/*
 * _Xsj3cFlushDcid()
 *  Flush dictionary-id.
 */
void
_Xsj3cFlushDcid(buf)
    Xsj3cBuf        buf;
{
    register int    i;
    unsigned char   mbs[2][KANJIBUFSIZ];

    if (buf->gakusyuu) {
        for (i = 0; i < buf->segnum; i++) {
            if (buf->input[i]->change) {
                if (i < buf->segnum - 1) {
                    _Xsj3cwPStomPS(buf, mbs[0], buf->input[i]->yomi);
                    _Xsj3cwPStomPS(buf, mbs[1], buf->input[i + 1]->yomi);
                    if ((serverIF[buf->server].func[FUNC_STUDY2]
            (mbs[0], mbs[1], &buf->input[i + 1]->dcid)) < 0) {
                        Xsj3cWarning("sj3serv is down. reconnect please");
                    }
                }
                if (i) {
                    if (i == buf->segnum - 1)
                        _Xsj3cwPStomPS(buf, mbs[0], buf->input[i]->yomi);
                    _Xsj3cwPStomPS(buf, mbs[1], buf->input[i - 1]->yomi);
                    if ((serverIF[buf->server].func[FUNC_STUDY2]
            (mbs[1], mbs[0], &buf->input[i]->dcid)) < 0) {
                        Xsj3cWarning("sj3serv is down. reconnect please");
                    }
                } else if (buf->segnum == 1) {
                    _Xsj3cwPStomPS(buf, mbs[0], buf->input[0]->yomi);
                    if ((serverIF[buf->server].func[FUNC_STUDY2]
            (mbs[0], mbs[0], &buf->input[0]->dcid)) < 0) {
                        Xsj3cWarning("sj3serv is down. reconnect please");
                    }
                }
                buf->input[i]->change = OFF;
            }
        }
        buf->convedsegnum = 0;
        if (locked[buf->server] > 0) {
            if (!(--locked[buf->server]))
            serverIF[buf->server].func[FUNC_UNLOCK]();
        }
    }
}

/*
 * _Xsj3cClearDcid()
 *  Clear dictionary-id.
 */
void
_Xsj3cClearDcid(buf)
    Xsj3cBuf        buf;
{
    register int    i;

    for (i = 0; i < buf->segnum; i++) {
        buf->input[i]->change = OFF;
        bzero(&(buf->input[i]->dcid), sizeof(buf->input[i]->dcid));
    }
    if (locked[buf->server] > 0) {
        if (!(--locked[buf->server]))
        serverIF[buf->server].func[FUNC_UNLOCK]();
    }
}
