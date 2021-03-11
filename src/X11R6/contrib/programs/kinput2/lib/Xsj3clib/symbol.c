#ifndef lint
static char *rcsid = "$Header: symbol.c,v 2.0 92/02/13 18:33:43 nao Exp $";
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
#include "common.h"
#include "util.h"
#include "func.h"
#include "table.h"

Xsj3cSymbol                 _Xsj3cSymbolInit();
int                         Xsj3cGetSymbolNum();
Xsj3cSymbol                 Xsj3cGetSymbols();
wchar                      *Xsj3cGetSymbol();
int                         Xsj3cSetSymbol();
void                        Xsj3cEndSymbol();

static int                  _Xsj3cReadSymbol();

static Xsj3cSymbol          symbollist = NULL;
static int                  symbolnum = 0;

/*
 * _Xsj3cSymbolInit()
 *  Decide sjsb file to read, then read it and make table for symbles.
 */
Xsj3cSymbol
_Xsj3cSymbolInit(sjsb, home)
    char               *sjsb;
    char               *home;
{
    extern char        *getenv();
    register char      *p;
    char                sbfile[BUFSIZ];
    int                 value;

    if (symbollist) {
        return(symbollist);
    }
    if (sjsb) {
        if ((value = _Xsj3cReadSymbol(sjsb)) > 0)
            Xsj3cError("can't open sjsb file %s", sjsb);
        else if ( value < 0 )
            Xsj3cError("read failed sjsb file %s", sjsb);
    } else {
        sbfile[0] = '\0';
        if ((p = getenv("SJSB")) && *p != '\0') {
            if (*p != '/') {
                if (home)
                    strcpy(sbfile, home);
                strcat(sbfile, "/");
            }
            strcat(sbfile, p);
        } else if (home) {
            strcpy(sbfile, home);
            strcat(sbfile, "/.sjsb");
        } else  {
            strcpy(sbfile, SJ3DEFPATH);
            strcat(sbfile, DEF_SJSB_FILE);
        }
        if ((value = _Xsj3cReadSymbol(sbfile)) > 0) {
            strcpy(sbfile, SJ3DEFPATH);
            strcat(sbfile, DEF_SJSB_FILE);
            if ((value = _Xsj3cReadSymbol(sbfile)) > 0) {
                Xsj3cError("can't open sjsb file %s", sbfile);
            } else if ( value < 0 ) {
                Xsj3cError("read failed sjsb file %s", sbfile);
            }
        } else if ( value < 0 ) {
            Xsj3cError("read failed sjsb file %s", sbfile);
        }
    }
    return(symbollist);
}

/*
 *  _Xsj3cReadSymbol()
 *   Read sjsb file that is symbol table file.
 *  Make symbol table and return it.
 */
static int
_Xsj3cReadSymbol(file)
    char                   *file;
{
    FILE                   *fp;
    unsigned char           line[256];
    unsigned char          *p;
    int                     len;
    wchar                   data[RBUFSIZ];
    register int            i,  j;

    if ((fp = fopen(file, "r")) == NULL) {
        return (OPEN_FAILED);
    }

    i = 0;
    while (fgets(line, sizeof(line), fp) != NULL) {
        if (line[0] != '#' && line[0] != '\n')
            i++;
    }
    
    symbolnum = i;
    if ((symbollist = (Xsj3cSymbol)calloc(i, sizeof(Xsj3cSymbolRec))) == NULL) {
        return (ALLOC_FAILED);
    }

    rewind(fp);
    
    j = 0;
    while (fgets(line, sizeof(line), fp) != NULL && j < i) {
        p = line;
        while (*p != '\n' && *p != '#') {
            p += _Xsj3cmINtowOUT(file, p, data, &len);
            SKIP(p);
            if (data[0] == '\0')
                break;
            symbollist[j].data = _Xsj3cStoreWchar(data, len + 1);
            symbollist[j].len = len;
            if (!symbollist[j].data)
                return (ALLOC_FAILED);
            j++;
        }
    }
    fclose(fp);
    return (OK);
}

/*
 * Xsj3cGetSymbolNum()
 *  Set the appointed (by 2nd argument) symbol to 3rd argument.
 */
int
Xsj3cGetSymbolNum(buf, cur)
    Xsj3cBuf            buf;
    int                *cur;
{
    if (!buf->symbol) {
        buf->symbol = _Xsj3cSymbolInit(NULL, NULL);
    }
    *cur = buf->cursymbol;
    return (symbolnum);
}

/*
 * Xsj3cGetSymbols()
 *  Set the appointed (by 2nd argument) symbol to 3rd argument.
 */
Xsj3cSymbol
Xsj3cGetSymbols(buf)
    Xsj3cBuf            buf;
{
    if (!buf->symbol) {
        buf->symbol = _Xsj3cSymbolInit(NULL, NULL);
    }
    return (buf->symbol);
}

/*
 * Xsj3cGetSymbol()
 *  Set the appointed (by 2nd argument) symbol to 3rd argument.
 */
wchar *
Xsj3cGetSymbol(buf, n, len)
    Xsj3cBuf            buf;
    int                 n;
    int                *len;
{
    if (!buf->symbol) {
        buf->symbol = _Xsj3cSymbolInit(NULL, NULL);
    }
    *len = buf->symbol[n].len;
    return (buf->symbol[n].data);
}

/*
 * Xsj3cSetSymbol()
 *  Set the selected symbol to the buffers.
 */
int
Xsj3cSetSymbol(buf, sel_symbol, changed, flush)
    Xsj3cBuf            buf;
    int                 sel_symbol;
    int                *changed;
    int                *flush;
{
    Xsj3cSeg            seg;
    int                 change_pos;
    register int        i;
    wchar               symbol[RBUFSIZ];

    if (buf->segnum && buf->input[buf->curseg]
            && (buf->input[buf->curseg]->edit & SEG_NOEDIT)) {
        /* 現文節が変換済みの場合   */
        if (buf->candidate) 
            Xsj3cEndCandidate(buf, ON);
        /* FlushInConverion on の時は確定する */
        switch (buf->flushiconv) {
        case ON:
            for (i = 1; i < buf->backsegnum + 1; i++) {
                Xsj3cFreeSegment(buf->backup[i]);
                buf->backup[i] = NULL;
            }
            if (!buf->backup) {
                if ((buf->backup = (Xsj3cSeg *)calloc(BUNBUFSIZ,
                        sizeof(Xsj3cSeg))) == NULL) {
                    Xsj3cError("Cannot allocate for backup buffers");
                }
            }
            if (seg = buf->backup[0]) {
                Xsj3cClearSegment(buf, seg);
            } else
                seg = buf->backup[0] = (Xsj3cSeg)Xsj3cCreateSegment(buf);
            buf->backsegnum = 1;
            *flush = ON;
            break;
        case EDIT:
            if (!(seg = buf->input[buf->curseg]))
                seg = buf->input[buf->curseg]
                        = (Xsj3cSeg)Xsj3cCreateSegment(buf);
            *seg->str = '\0';
            seg->sp = seg->str;
            *seg->oldstr = '\0';
            seg->oldlen = 0;
            seg->n_roma = 0;
            seg->n_kana = -1;
            if (buf->curseg == buf->segnum)
                buf->segnum++;
            else {
                _Xsj3cUnConvSeg(buf, ONE, buf->muhencurlast);
                _Xsj3cStoreYomi(buf, seg, 0);
            }
            *flush = OFF;
            break;
        case OFF:
            if (seg = buf->input[buf->segnum]) {
                Xsj3cClearSegment(buf, seg);
            } else {
                seg = buf->input[buf->segnum]
                        = (Xsj3cSeg)Xsj3cCreateSegment(buf);
            }
            buf->curseg = buf->segnum;
            buf->segnum++;
            *flush = OFF;
            break;
        case NONE:
        default:
            *flush = OFF;
            *changed = OFF;
            return 0;
        }
    } else {
        seg = buf->input[buf->curseg];
        if (buf->curseg == buf->segnum) {
            buf->segnum++;
        }
        if (!seg) {
            seg = buf->input[buf->curseg] = (Xsj3cSeg)Xsj3cCreateSegment(buf);
        } else {
            *seg->str = '\0';
            seg->sp = seg->str;
            seg->n_roma = 0;
            seg->n_kana = -1;
            *seg->oldstr = '\0';
            seg->oldlen = 0;
        }
    }
    seg->edit = SEG_EDIT;
    change_pos = seg->cur;
    _Xsj3cwOUTtowPS(buf, symbol, buf->symbol[sel_symbol].data);
    _Xsj3cInsertWchar(seg, symbol, buf->symbol[sel_symbol].len);
    _Xsj3cStoreYomi(buf, seg, change_pos);
    buf->cursymbol = sel_symbol;
    *changed = ON;

    return 0;
}

/*
 * Xsj3cEndSymbol()
 *  End symbol input mode(SelectMode) and back to InputMode.
 */
void
Xsj3cEndSymbol(buf)
    Xsj3cBuf            buf;
{
    buf->convmode = InputModeMask;
}
