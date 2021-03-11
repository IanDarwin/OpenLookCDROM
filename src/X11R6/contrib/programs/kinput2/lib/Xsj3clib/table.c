#ifndef lint
static char *rcsid = "$Header: table.c,v 2.3 93/09/21 09:42:45 nao Exp $";
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
#include <ctype.h>
#include "common.h"
#include "sj3ctype.h"
#include "util.h"

extern Xsj3cCVServerIF      serverIF[SERVER_NUM];
extern Xsj3cSymbol          _Xsj3cSymbolInit();
extern Xsj3cHinsi           _Xsj3cHinsiInit();

Xsj3cRKTable               *_Xsj3cRKInit();
Xsj3cHKTable               *_Xsj3cHKInit();
Xsj3cZHTable               *_Xsj3cZHInit();
unsigned char              *_Xsj3cSetPlosive();
unsigned char              *_Xsj3cSetDouble();
void                        Xsj3cInitializeTables();

int                         _Xsj3cmINtowOUT();
int                         _Xsj3cReadAscii();
wchar                      *_Xsj3cStoreWchar();

static Xsj3cRKTable        *_Xsj3cAllocRKTable();
static Xsj3cHKTable        *_Xsj3cAllocHKTable();
static Xsj3cZHTable        *_Xsj3cAllocZHTable();

static unsigned char       *_Xsj3cStoreChar();

static int                  _Xsj3cmINtomPS();

static int                  _Xsj3cReadRK();
static int                  _Xsj3cReadHK();
static int                  _Xsj3cReadZH();

static unsigned char       *chnowp = NULL,     *chmaxp = NULL;
static wchar               *wnowp = NULL,      *wmaxp = NULL;

static Xsj3cRKTable        *rktable[2] = {NULL, NULL};
static Xsj3cHKTable        *hktable[2] = {NULL, NULL};
static Xsj3cZHTable        *zhtable[2] = {NULL, NULL};
static unsigned char       *plosive[2] = {NULL, NULL};
static unsigned char       *rkdouble[2] = {NULL, NULL};

static Xsj3cRKTable        *rknowtp = NULL, *rkmaxtp = NULL;
static Xsj3cHKTable        *hknowtp = NULL, *hkmaxtp = NULL;
static Xsj3cZHTable        *zhnowtp = NULL, *zhmaxtp = NULL;

static unsigned char *
_Xsj3cStoreChar(ch, len)
    unsigned char  *ch;
    int             len;
{
    register unsigned char  *chp;

    if (chnowp == NULL || chnowp + len > chmaxp) {
        chp = (unsigned char *)malloc(BUFSIZ);
        if (chp == NULL)
            return (NULL);
        chnowp = chp;
        chmaxp = chnowp + (BUFSIZ/sizeof(unsigned char));
        strcpy (chp, ch);
        chnowp += len;
        return (chp);
    } else {
        chp = chnowp;
        strcpy (chp, ch);
        chnowp += len;
        return (chp);
    }
}

wchar *
_Xsj3cStoreWchar(wch, len)
    wchar          *wch;
    int             len;
{
    register wchar  *wp;

    if (wnowp == NULL || wnowp + len > wmaxp) {
        wp = (wchar *)malloc(BUFSIZ);
        if (wp == NULL)
            return (NULL);
        wnowp = wp;
        wmaxp = wnowp + (BUFSIZ/sizeof(wchar));
        (void)_Xsj3cWcpy (wp, wch);
        wnowp += len;
        return (wp);
    } else {
        wp = wnowp;
        (void)_Xsj3cWcpy (wp, wch);
        wnowp += len;
        return (wp);
    }
}

static int
_Xsj3cmINtomPS(buf, file, src, dest)
    Xsj3cBuf                buf;
    char                   *file;
    register unsigned char *src,   *dest;
{
    register unsigned char  c;
    register wchar          s,     (*conv)();
    register int            i = 0,  kanji = OFF, kana = OFF;

    conv = CodeConvFunc[in_lang][serverIF[buf->server].lang];
    while ((*src != '\t' && *src != '\n'
            && *src != ' ' && *src != '#')||(kanji || kana)) {
        c = *src++;
        switch (c) {
        case ESC:
            c = *src++;
            if (c == '$') {
                c = *src++;
                i++;
                if (c == 'B' || c == '@') {
                    kanji++;
                    i++;
                } else
                    goto badcode;
            } else if (c == '(') {
                c = *src++;
                i++;
                if (c == 'J' || c == 'B' || c == 'H') {
                    kanji = OFF;
                    i++;
                } else if (c == '$' && *src == 'B') {
                    src++;
                    kanji++;
                    i++;
                } else 
                    goto badcode;
            } else if (c == '&' && !strcmp("@\033$B", src)) {
                src += 4;
                i += 5;
                kanji++;
            } else
                goto badcode;
            break;
        case SI:
            kana = OFF;
            break;
        case SO:
            kana++;
            break;
        case '\\':
            if (!kanji & !kana) {
                *dest++ = *src++;
                i++;
                break;
            }
        default:
            if (kanji) {
                s = (c << 8) + (*src++ & 0x7f);
                s = conv(s);
                *dest++ = (s >> 8) & 0xff;
                *dest++ = s & 0xff;
                i++;
            } else if (iskan1(c, in_lang) && iskan2(*src, in_lang)) {
                if (conv) {
                    s = (c << 8) + (*src++ & 0xff);
                    s = conv(s);
                    *dest++ = (s >> 8) & 0xff;
                    *dest++ = s & 0xff;
                } else {
                    *dest++ = c;
                    *dest++ = *src++;
                }
                i++;
            } else if (iskana(c)) {
                if (in_lang == JP_SJIS || in_lang == JP_JIS8)
                    *dest++ = c;
                else if (in_lang == JP_EUC && iseuckana(c) && iskana2(*src)) {
                    *dest++ = *src++;
                    i++;
                } else 
                    goto badcode;
            } else if (iseuckana(c) && iskana2(*src)) {
                if (in_lang == JP_EUC)
                    *dest++ = *src++;
                else 
                    goto badcode;
                i++;
            } else if (kana) {
                *dest++ = (c | MSB);
            } else if (isascii(c)) {
                *dest++ = c;
            } else {
badcode:
                Xsj3cError("There is bad code %s in %s",_Xsj3cXtoa(c),file);
            }
            break;
        }
        i++;
    }
    *dest = '\0';
    return (i);
}

int
_Xsj3cReadAscii(file, src, dest)
    char                   *file;
    register unsigned char *src,   *dest;
{
    register int            i = 0,  kana = OFF;

    while ((*src != '\t' && *src != ' '
            && *src != '\0' && *src != '\n' && *src != '#')|| kana) {
        if (kana) {
            *dest++ = (*src++ | MSB);
        } if (*src == '\\') {
            src++;
            i++;
            *dest++ = *src++;
        } else if (isascii(*src)) {
            *dest++ = *src++;
        } else if (isdakuten(*src)) {
            *dest++ = *src++;
        } else if (iseuckana(*src) && isdakuten(*(src + 1))) {
            src++;
            i++;
            *dest++ = *src++;
        } else if (*src == SO) {
            src++;
            kana++;
        } else if (*src == SI) {
            src++;
            kana = OFF;
        } else {
            int             n = *src;
            Xsj3cError("There is bad code %s in %s",_Xsj3cXtoa(n),file);
        }
        i++;
    }
    *dest = '\0';
    return (i);
}

int
_Xsj3cmINtowOUT(file, src, dest, len)
    char                   *file;
    register unsigned char *src;
    register wchar         *dest;
    register int           *len;
{
    register unsigned char  c;
    register int            i = 0,  kanji = OFF, kana = OFF;
    register wchar          (*conv)();

    *len = 0;
    conv = CodeConvFunc[in_lang][out_lang];
    while ((*src != '\t' && *src != ' ' && *src != '\0'
            && *src != '\n' && *src != '#')||(kanji || kana)) {
        c = *src++;
        switch (c) {
        case ESC:
            c = *src++;
            if (c == '$') {
                c = *src++;
                i++;
                if (c == 'B' || c == '@') {
                    kanji++;
                    i++;
                } else
                    goto badcode;
            } else if (c == '(') {
                c = *src++;
                i++;
                if (c == 'J' || c == 'B' || c == 'H') {
                    kanji = OFF;
                    i++;
                } else if (c == '$' && *src == 'B') {
                    src++;
                    kanji++;
                    i++;
                } else
                    goto badcode;
            } else if (c == '&' && !strcmp("@\033$B", src)) {
                src += 4;
                i += 5;
                kanji++;
            } else
                goto badcode;
            break;
        case SI:
            kana = OFF;
            break;
        case SO:
            kana++;
            break;
        case '\\':
            if (!kanji && !kana) {
                *dest++ = *src++;
                i++;
                break;
            }
        default:
            if (kanji || (iskan1(c, in_lang) && iskan2(*src, in_lang))) {
                if (conv) 
                    *dest++ = conv((c << 8) + (*src++ & 0xff));
                else
                    *dest++ = (c << 8) + (*src++ & 0xff);
                i++;
            } else if (iskana(c)) {
                if (in_lang == JP_SJIS || in_lang == JP_JIS8)
                    *dest++ = c;
                else if (in_lang == JP_EUC && iseuckana(c) && iskana2(*src)) {
                    *dest++ = *src++;
                    i++;
                } else 
                    goto badcode;
            } else if (iseuckana(c) && iskana2(*src)) {
                if (in_lang == JP_EUC)
                    *dest++ = *src++;
                else 
                    goto badcode;
                i++;
            } else if (kana) {
                *dest++ = (c | MSB);
            } else if (isascii(c)) {
                *dest++ = c;
            } else {
badcode:
                Xsj3cError("There is bad code %s in %s",_Xsj3cXtoa(c),file);
            }
            (*len)++;
            break;
        }
        i++;
    }
    *dest = '\0';
    return (i);
}

/*
 * Xsj3cInitializeTables()
 *  Read definition files and initialize conversion tables.
 */
void
Xsj3cInitializeTables(buf, home, sjrk, sjhk, sjzh, sjsb)
    Xsj3cBuf            buf;
    char               *home;
    char               *sjrk,   *sjhk,  *sjzh,  *sjsb;
{
    buf->rktable = _Xsj3cRKInit(buf, sjrk, home);
    buf->hktable = _Xsj3cHKInit(buf, sjhk, home);
    buf->zhtable = _Xsj3cZHInit(buf, sjzh, home);
    buf->symbol = _Xsj3cSymbolInit(sjsb, home);
    buf->plosive = _Xsj3cSetPlosive(buf);
    buf->rkdouble = _Xsj3cSetDouble(buf);
    buf->hinsi = _Xsj3cHinsiInit(buf);
}

/*
 * _Xsj3cRKInit()
 *  Decide sjrk file to read, then read it and make table for roman-kana
 * conversion table.
 */
Xsj3cRKTable *
_Xsj3cRKInit(buf, sjrk, home)
    Xsj3cBuf            buf;
    char               *sjrk;
    char               *home;
{
    extern char        *getenv();
    register char      *p;
    char                rkfile[BUFSIZ];
    int                 value,  error;

    if (rktable[serverIF[buf->server].lang])
        return (rktable[serverIF[buf->server].lang]);

    if (sjrk) {
        if ((value = _Xsj3cReadRK(buf, sjrk, &error)) > 0 )
            Xsj3cError("can't open sjrk file %s", sjrk);
        else if (value < 0)
            Xsj3cError("read failed line %s sjrk file %s",
                    _Xsj3cItoa(error), sjrk);
    } else {
        rkfile[0] = '\0';
        if ((p = getenv("SJRK")) && *p != '\0') {
            if (*p != '/') {
                if (home)
                    strcpy(rkfile, home);
                strcat(rkfile, "/");
            }
            strcat(rkfile, p);
        } else if (home) {
            strcpy(rkfile, home);
            strcat(rkfile, "/.sjrk");
        } else {
            strcpy(rkfile, SJ3DEFPATH);
            strcat(rkfile, DEF_SJRK_FILE);
        }
        if ((value = _Xsj3cReadRK(buf, rkfile, &error)) > 0 ) {
            strcpy(rkfile, SJ3DEFPATH);
            strcat(rkfile, DEF_SJRK_FILE);
            if ((value = _Xsj3cReadRK(buf, rkfile, &error)) > 0 ) {
                Xsj3cError("can't open sjrk file %s", rkfile);
            } else if (value < 0) {
                Xsj3cError("read failed line %s sjrk file %s",
                        _Xsj3cItoa(error), rkfile);
            }
        } else if (value < 0) {
            Xsj3cError("read failed line %s sjrk file %s",
                    _Xsj3cItoa(error), rkfile);
        }
    }
    return (rktable[serverIF[buf->server].lang]);
}

#define RKTBMAX (BUFSIZ/sizeof(Xsj3cRKTable))

static Xsj3cRKTable *
_Xsj3cAllocRKTable()
{
    register Xsj3cRKTable  *rktp;

    if (rknowtp == NULL || rknowtp > rkmaxtp) {
        rktp = (Xsj3cRKTable *)malloc(sizeof(Xsj3cRKTable) * RKTBMAX);
        if (rktp == NULL)
            return (NULL);
        rknowtp = rktp;
        rkmaxtp = rknowtp + RKTBMAX - 1;
        rknowtp++;
    } else {
        rktp = rknowtp;
        rknowtp++;
    }
    rktp->roma = NULL;
    rktp->yomi = NULL;
    rktp->str = NULL;
    rktp->rlen = 0;
    rktp->ylen = 0;
    rktp->next = NULL;
    return (rktp);
}

/*
 *  _Xsj3cReadRK()
 *   Read sjrk file like a format sj3's roman-kana conversion file,
 *  and make conversion table.
 *   But this routine distinguishes upper-case and lower-case.
 *  It's a big difference.
 */
static int
_Xsj3cReadRK(buf, file, error)
    Xsj3cBuf                buf;
    register char          *file;
    register int           *error;
{
    register FILE          *fp;
    unsigned char           line[256];
    unsigned char          *p;
    unsigned char           roma[RBUFSIZ],  yomi[YBUFSIZ],  str[RBUFSIZ];
    register int            begin = 0,  rlen,   ylen;
    register Xsj3cRKTable  *rktp,  *rktq,  *rktr;

    if ((fp = fopen(file, "r")) == NULL)
        return (OPEN_FAILED);
    
    *error = 1;
    rktp = rktable[serverIF[buf->server].lang] = _Xsj3cAllocRKTable();
    while (fgets(line, sizeof(line), fp) != NULL) {
        p = line;
        while (*p != '\n' && *p != '#') {
            p += _Xsj3cReadAscii(file, p, roma);
            CHECK_END(p);
            SKIP(p);
            p += _Xsj3cmINtomPS(buf, file, p, yomi);
            SKIP(p);
            p += _Xsj3cReadAscii(file, p, str);
            if (roma[0] == '\0' || yomi[0] == '\0')
                break;
            if (begin++) {
                rktr = _Xsj3cAllocRKTable();
                if (!rktr) {
                    Xsj3cWarning("can't allocate roman-kana conversion table");
                    return (ALLOC_FAILED);
                }
                rktp = rktq->next = rktr;
            }
            rktp->roma = _Xsj3cStoreChar(roma, (rlen  = strlen(roma)) + 1);
            rktp->yomi = _Xsj3cStoreChar(yomi, (ylen  = strlen(yomi)) + 1);
            rktp->str = _Xsj3cStoreChar(str, strlen(str) + 1);
            if (!rktp->roma || !rktp->yomi || !rktp->str) {
                Xsj3cWarning("can't allocate roman-kana conversion table");
                return(ALLOC_FAILED);
            }
            rktp->rlen = rlen;
            rktp->ylen = ylen;
            rktq = rktp;
        }
        (*error)++;
    }
    rktp->next = NULL;
    fclose(fp);
    return (OK);
}

/*
 * _Xsj3cHKInit()
 *  Decide sjhk file to read, then read it and make hiragana-katakana
 * conversion table.
 */
Xsj3cHKTable *
_Xsj3cHKInit(buf, sjhk, home)
    Xsj3cBuf            buf;
    char               *sjhk;
    char               *home;
{
    extern char        *getenv();
    register char      *p;
    char                hkfile[BUFSIZ];
    int                 value,  error;

    if (hktable[serverIF[buf->server].lang])
        return (hktable[serverIF[buf->server].lang]);

    if (sjhk) {
        if ((value = _Xsj3cReadHK(buf, sjhk, &error)) > 0 )
            Xsj3cError("can't open sjhk file %s", sjhk);
        else if (value < 0)
            Xsj3cError("read failed line %s sjhk file %s",
                    _Xsj3cItoa(error), sjhk);
    } else {
        hkfile[0] = '\0';
        if ((p = getenv("SJHK")) && *p != '\0') {
            if (*p != '/') {
                if (home)
                    strcpy(hkfile, home);
                strcat(hkfile, "/");
            }
            strcat(hkfile, p);
        } else if (home) {
            strcpy(hkfile, home);
            strcat(hkfile, "/.sjhk");
        } else  {
            strcpy(hkfile, SJ3DEFPATH);
            strcat(hkfile, DEF_SJHK_FILE);
        }
        if ((value = _Xsj3cReadHK(buf, hkfile, &error)) > 0 ) {
            strcpy(hkfile, SJ3DEFPATH);
            strcat(hkfile, DEF_SJHK_FILE);
            if ((value = _Xsj3cReadHK(buf, hkfile, &error)) > 0 ) {
                Xsj3cError("can't open sjhk file %s", hkfile);
            } else if (value < 0) {
                Xsj3cError("read failed line %s sjhk file %s",
                        _Xsj3cItoa(error), hkfile);
            }
        } else if (value < 0) {
            Xsj3cError("read failed line %s sjhk file %s",
                    _Xsj3cItoa(error), hkfile);
        }
    }
    return (hktable[serverIF[buf->server].lang]);
}

#define HKTBMAX (BUFSIZ/sizeof(Xsj3cHKTable))

static Xsj3cHKTable *
_Xsj3cAllocHKTable()
{
    register Xsj3cHKTable  *hktp;

    if (hknowtp == NULL || hknowtp > hkmaxtp) {
        hktp = (Xsj3cHKTable *)malloc(sizeof(Xsj3cHKTable) * HKTBMAX);
        if (hktp == NULL)
            return (NULL);
        hknowtp = hktp;
        hkmaxtp = hknowtp + HKTBMAX - 1;
        hknowtp++;
    } else {
        hktp = hknowtp;
        hknowtp++;
    }
    hktp->hira = NULL;
    hktp->zkata = NULL;
    hktp->hkata = NULL;
    hktp->halpha = NULL;
    hktp->hlen = 0;
    hktp->next = NULL;
    return (hktp);
}

static int
_Xsj3cReadHK(buf, file, error)
    Xsj3cBuf                buf;
    register char          *file;
    register int           *error;
{
    register FILE          *fp;
    unsigned char           line[256];
    unsigned char          *p;
    unsigned char           zhira[RBUFSIZ],  zkata[YBUFSIZ];
    unsigned char           hkata[RBUFSIZ], halpha[RBUFSIZ];
    register int            begin = 0,      hkata_len,      halpha_len;
    register Xsj3cHKTable  *hktp,  *hktq,  *hktr;

    if ((fp = fopen(file, "r")) == NULL)
        return (OPEN_FAILED);
    
    *error = 1;
    hktp = hktable[serverIF[buf->server].lang] = _Xsj3cAllocHKTable();
    while (fgets(line, sizeof(line), fp) != NULL) {
        p = line;
        while (*p != '\n' && *p != '#') {
            p += _Xsj3cmINtomPS(buf, file, p, zhira);
            CHECK_END(p);
            SKIP(p);
            p += _Xsj3cmINtomPS(buf, file, p, zkata);
            CHECK_END(p);
            SKIP(p);
            p += _Xsj3cmINtomPS(buf, file, p, hkata);
            CHECK_END(p);
            SKIP(p);
            p += _Xsj3cReadAscii(file, p, halpha);
            if (zhira[0] == '\0' || zkata[0] == '\0'
                    || hkata[0] == '\0' || halpha[0] == '\0')
                break;
            if (begin++) {
                hktr = _Xsj3cAllocHKTable();
                if (!hktr) {
            Xsj3cWarning("can't allocate hiragana-katakana conversion table");
                    return (ALLOC_FAILED);
                }
                hktp = hktq->next = hktr;
            }
            hktp->hira = _Xsj3cStoreChar(zhira, strlen(zhira) + 1);
            hktp->zkata = _Xsj3cStoreChar(zkata, strlen(zkata) + 1);
            hktp->hkata = _Xsj3cStoreChar(hkata, 
                    (hkata_len = strlen(hkata)) + 1);
            hktp->halpha = _Xsj3cStoreChar(halpha, 
                    (halpha_len = strlen(halpha)) + 1);
            if (!hktp->hira || !hktp->zkata || !hktp->hkata || !hktp->halpha) {
            Xsj3cWarning("can't allocate hiragana-katakana conversion table");
                return(ALLOC_FAILED);
            }
            hktp->hlen = hkata_len;
            if (hkata_len != halpha_len)
                return (READ_FAILED);
            hktq = hktp;
        }
        (*error)++;
    }
    hktp->next = NULL;
    fclose(fp);
    return (OK);
}

/*
 * _Xsj3cZHInit()
 *  Decide sjzh file to read, then read it and make hankaku-zenkaku
 * conversion table.
 */
Xsj3cZHTable *
_Xsj3cZHInit(buf, sjzh, home)
    Xsj3cBuf            buf;
    char               *sjzh;
    char               *home;
{
    extern char        *getenv();
    register char      *p;
    char                zhfile[BUFSIZ];
    int                 value,  error;

    if (zhtable[serverIF[buf->server].lang])
        return (zhtable[serverIF[buf->server].lang]);

    if (sjzh) {
        if ((value = _Xsj3cReadZH(buf, sjzh, &error)) > 0 )
            Xsj3cError("can't open sjzh file %s", sjzh);
        else if (value < 0)
            Xsj3cError("read failed line %s sjzk file %s",
                    _Xsj3cItoa(error), sjzh);
    } else {
        zhfile[0] = '\0';
        if ((p = getenv("SJZH")) && *p != '\0') {
            if (*p != '/') {
                if (home)
                    strcpy(zhfile, home);
                strcat(zhfile, "/");
            }
            strcat(zhfile, p);
        } else if (home) {
            strcpy(zhfile, home);
            strcat(zhfile, "/.sjzh");
        } else {
            strcpy(zhfile, SJ3DEFPATH);
            strcat(zhfile, DEF_SJZH_FILE);
        }
        if ((value = _Xsj3cReadZH(buf, zhfile, &error)) > 0 ) {
            strcpy(zhfile, SJ3DEFPATH);
            strcat(zhfile, DEF_SJZH_FILE);
            if ((value = _Xsj3cReadZH(buf, zhfile, &error)) > 0 ) {
                Xsj3cError("can't open sjzh file %s", zhfile);
            } else if (value < 0) {
                Xsj3cError("read failed line %s sjzk file %s",
                        _Xsj3cItoa(error), zhfile);
            }
        } else if (value < 0) {
            Xsj3cError("read failed line %s sjzk file %s",
                    _Xsj3cItoa(error), zhfile);
        }
    }
    return (zhtable[serverIF[buf->server].lang]);
}

#define ZHTBMAX (BUFSIZ/sizeof(Xsj3cZHTable))

static Xsj3cZHTable *
_Xsj3cAllocZHTable()
{
    register Xsj3cZHTable  *zhtp;

    if (zhnowtp == NULL || zhnowtp > zhmaxtp) {
        zhtp = (Xsj3cZHTable *)malloc(sizeof(Xsj3cZHTable) * ZHTBMAX);
        if (zhtp == NULL)
            return (NULL);
        zhnowtp = zhtp;
        zhmaxtp = zhnowtp + ZHTBMAX - 1;
        zhnowtp++;
    } else {
        zhtp = zhnowtp;
        zhnowtp++;
    }
    zhtp->zkana = NULL;
    zhtp->hkata = NULL;
    zhtp->halpha = NULL;
    zhtp->zalpha = NULL;
    zhtp->next = NULL;
    return (zhtp);
}

static int
_Xsj3cReadZH(buf, file, error)
    Xsj3cBuf        buf;
    char           *file;
    register int   *error;
{
    register FILE          *fp;
    unsigned char           line[256];
    unsigned char          *p;
    unsigned char           hanalpha[RBUFSIZ],    zenalpha[YBUFSIZ];
    unsigned char           zkana[RBUFSIZ],     hkata[RBUFSIZ];
    register int            begin = 0;
    register Xsj3cZHTable  *zhtp,  *zhtq,  *zhtr;

    if ((fp = fopen(file, "r")) == NULL)
        return (OPEN_FAILED);
    
    *error = 1;
    zhtp = zhtable[serverIF[buf->server].lang] = _Xsj3cAllocZHTable();
    while (fgets(line, sizeof(line), fp) != NULL) {
        p = line;
        while (*p != '\n' && *p != '#') {
            p += _Xsj3cReadAscii(file, p, hanalpha);
            CHECK_END(p);
            SKIP(p);
            p += _Xsj3cmINtomPS(buf, file, p, zenalpha);
            CHECK_END(p);
            SKIP(p);
            p += _Xsj3cmINtomPS(buf, file, p, zkana);
            CHECK_END(p);
            SKIP(p);
            p += _Xsj3cmINtomPS(buf, file, p, hkata);
            if (hanalpha[0] == '\0' || zenalpha[0] == '\0'
                    || zkana[0] == '\0' || hkata[0] == '\0')
                break;
            if (begin++) {
                zhtr = _Xsj3cAllocZHTable();
                if (!zhtr) {
                    Xsj3cWarning("can't allocate zen/hankaku conversion table");
                    return (ALLOC_FAILED);
                }
                zhtp = zhtq->next = zhtr;
            }
            zhtp->halpha = _Xsj3cStoreChar(hanalpha, strlen(hanalpha) + 1);
            zhtp->zalpha = _Xsj3cStoreChar(zenalpha, strlen(zenalpha) + 1);
            zhtp->zkana = _Xsj3cStoreChar(zkana, strlen(zkana) + 1);
            zhtp->hkata = _Xsj3cStoreChar(hkata, strlen(hkata) + 1);
            if (!zhtp->halpha || !zhtp->zalpha
                    || !zhtp->zkana || !zhtp->hkata) {
                Xsj3cWarning("can't allocate zen/han-kaku conversion table");
                return(ALLOC_FAILED);
            }
            zhtq = zhtp;
        }
        (*error)++;
    }
    zhtp->next = NULL;
    fclose(fp);
    return (OK);
}

/*
 * _Xsj3cSetPlosive()
 * Set roman-kana plosive conversion data.
 */
unsigned char *
_Xsj3cSetPlosive(buf)
    Xsj3cBuf                buf;
{
    register Xsj3cRKTable  *rktp;
    register int            i;
    register wchar          s;
    unsigned char           tmp[KANABUFSIZ];
    
    if (plosive[serverIF[buf->server].lang])
        return(plosive[serverIF[buf->server].lang]);
    if (!rktable[serverIF[buf->server].lang])
        Xsj3cError("Null roman-kana conversion table");
    i = 0;
    for (rktp = rktable[serverIF[buf->server].lang];
            rktp->next != NULL; rktp = rktp->next) {
        if (*rktp->str != '\0') {
            s = (*rktp->yomi << 8) + *(rktp->yomi + 1);
            if (isplosive(s, serverIF[buf->server].lang)) {
                tmp[i] = *rktp->str;
                i++;
            }
        }
    }
    tmp[i] = '\0';
    if ((plosive[serverIF[buf->server].lang]
            = _Xsj3cStoreChar(tmp, strlen(tmp) + 1)) == NULL)
        Xsj3cError("can't allocate for roma-kana plosive conversion table");
    return(plosive[serverIF[buf->server].lang]);
}

/*
 * _Xsj3cSetDouble()
 * Set roman-kana double conversion data.
 */
unsigned char *
_Xsj3cSetDouble(buf)
    Xsj3cBuf                buf;
{
    register Xsj3cRKTable  *rktp,   *rktp2;
    register int            i;
    unsigned char           tmp[KANABUFSIZ];
    
    if (rkdouble[serverIF[buf->server].lang])
        return(rkdouble[serverIF[buf->server].lang]);
    if (!rktable[serverIF[buf->server].lang])
        Xsj3cError("Null roman-kana conversion table");
    i = 0;
    for (rktp = rktable[serverIF[buf->server].lang];
            rktp->next != NULL; rktp = rktp->next) {
        if (rktp->rlen == 1) {
            if (isvowel(*rktp->roma))
                continue;
            tmp[i] = *rktp->roma;
            for (rktp2 = rktable[serverIF[buf->server].lang];
                    rktp2->next != NULL; rktp2 = rktp2->next) {
                if (rktp2->rlen == 2 && *(rktp2->roma + 1) == tmp[i]
                        && *(rktp2->roma) == tmp[i]) {
                    if (!strcmp(rktp->yomi, rktp2->yomi)) {
                        i++;
                        break;
                    }
                }
            }
        }
    }
    tmp[i] = '\0';
    if ((rkdouble[serverIF[buf->server].lang]
            = _Xsj3cStoreChar(tmp, strlen(tmp) + 1)) == NULL)
        Xsj3cError("can't allocate for roma-kana double conversion table");
    return(rkdouble[serverIF[buf->server].lang]);
}
