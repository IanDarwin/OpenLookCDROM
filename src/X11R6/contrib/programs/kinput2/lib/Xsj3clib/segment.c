#ifndef lint
static char *rcsid = "$Header: segment.c,v 2.0 92/02/13 18:33:33 nao Exp $";
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
#include "util.h"

Xsj3cSeg                    Xsj3cCreateSegment();
void                        Xsj3cFreeSegment();
void                        Xsj3cResizeSegment();
void                        Xsj3cClearSegment();

int                         Xsj3cGetSegNum();
int                         Xsj3cGetPosition();
wchar                      *Xsj3cGetModeStr();
wchar                      *Xsj3cGetSeg();
wchar                      *Xsj3cGetConvertedStr();
int                         Xsj3cGetConvertedLength();

/*
 * Xsj3cCreateSegment()
 *  Allocate Xsj3cSeg type structure and initialize all flags and buffers.
 */
Xsj3cSeg
Xsj3cCreateSegment(buf)
    Xsj3cBuf    buf;
{
    Xsj3cSeg    seg;

    if (!(seg = (Xsj3cSeg)malloc(sizeof(Xsj3cSegRec)))) {
        return (Xsj3cSeg)NULL;
    }
    bzero (seg, sizeof(Xsj3cSegRec));
    if (!(seg->str = (unsigned char *)malloc(RBUFSIZ))) {
        return (Xsj3cSeg)NULL;
    }
    *seg->str = '\0';
    seg->sp = seg->str;
    if (!(seg->oldstr = (unsigned char *)malloc(RBUFSIZ))) {
        return (Xsj3cSeg)NULL;
    }
    *seg->oldstr = '\0';
    seg->size = 0;
    seg->yomi = NULL;
    seg->disp = NULL;
    Xsj3cResizeSegment(seg, KANABUFSIZ);
    seg->n_roma = 0;
    seg->n_kana = -1;
    seg->value = 0;
    seg->oldlen = 0;
    seg->num = 0;
    seg->cur = 0;
    seg->status = SEG_NOCONV;
    seg->cursegmode = buf->inputmode;
    seg->change = OFF;
    seg->edit = SEG_NOEDIT;
    bzero(&seg->dcid, sizeof(seg->dcid));
    return(seg);
}

/*
 * Xsj3cFreeSegment()
 *  Free Xsj3cSeg type structure.
 */
void
Xsj3cFreeSegment(seg)
    Xsj3cSeg    seg;
{
    if (!seg)
        return;
    if (seg->str)
        free(seg->str);
    seg->str = NULL;
    if (seg->oldstr)
        free(seg->oldstr);
    seg->oldstr = NULL;
    if (seg->yomi) {
        free(seg->yomi);
        seg->yomi = NULL;
    }
    if (seg->disp) {
        free(seg->disp);
        seg->disp = NULL;
    }
    seg->size = 0;
    free(seg);
}

/*
 * Xsj3cResizeSegment()
 *  Allocate segment buffers.
 */
void
Xsj3cResizeSegment(seg, size)
    Xsj3cSeg    seg;
    int         size;
{
    if (seg->size != size) {
        if (seg->yomi) {
            if (!(seg->yomi = (wchar *)realloc(seg->yomi,
                    seg->size * sizeof(wchar)))) {
                Xsj3cError("Failed to reallocate yomi buffer");
            }
        } else {
            if (!(seg->yomi = (wchar *)calloc(KANABUFSIZ, sizeof(wchar)))) {
                Xsj3cError("Failed to allocate yomi buffer");
            }
        }
        if (seg->disp) {
            if (!(seg->disp = (wchar *)realloc(seg->disp,
                    seg->size * sizeof(wchar)))) {
                Xsj3cError("Failed to reallocate display buffer");
            }
        } else {
            if (!(seg->disp = (wchar *)calloc(KANABUFSIZ, sizeof(wchar)))) {
                Xsj3cError("Failed to allocate display buffer");
            }
        }
        seg->size = size;
    }
}

/*
 * Xsj3cClearSegment()
 *  Clear segment.
 */
void
Xsj3cClearSegment(buf, seg)
    Xsj3cBuf    buf;
    Xsj3cSeg    seg;
{
    *seg->str = '\0';
    seg->sp = seg->str;
    *seg->oldstr = '\0';
    seg->oldlen = 0;
    Xsj3cResizeSegment(seg, KANABUFSIZ);
    *seg->yomi = '\0';
    *seg->disp = '\0';
    seg->cur = 0;
    seg->num = 0;
    seg->value = 0;
    seg->n_roma = 0;
    seg->n_kana = -1;
    seg->dnum = 0;
    seg->status = SEG_NOCONV;
    seg->cursegmode = buf->inputmode;
    seg->change = OFF;
    seg->edit = SEG_NOEDIT;
    bzero(&seg->dcid, sizeof(seg->dcid));
}

/*
 * Xsj3cGetModeStr()
 *  Return the strings of current mode guide.
 */
wchar *
Xsj3cGetModeStr(buf, len)
    Xsj3cBuf    buf;
    int        *len;
{
    *len = buf->modelen[buf->dispmode];
    return(buf->modestr[buf->dispmode]);
}

#define CURSOR_VISIBLE      1
#define CURSOR_UNVISIBLE    0

/*
 * Xsj3cGetPosition()
 *  Set current segment number to 2nd argument
 * and cursor position to 3rd argument.
 */
int
Xsj3cGetPosition(buf, curseg, pos)
    Xsj3cBuf    buf;
    int        *curseg;
    int        *pos;
{
    if (buf->curseg < buf->segnum) {
        *curseg = buf->curseg;
        *pos = buf->input[buf->curseg]->cur;
        if (buf->input[buf->curseg]->status & SEG_CONVED)
            return CURSOR_UNVISIBLE;
        else
            return CURSOR_VISIBLE;
    } else if (buf->curseg > 0) {
        *curseg = buf->curseg - 1;
        *pos = buf->input[buf->curseg - 1]->dnum;
        return CURSOR_VISIBLE;
    } else {
        *curseg = 0;
        *pos = 0;
        return CURSOR_UNVISIBLE;
    }
}

/*
 * Xsj3cGetSegNum()
 *  Return the number of segments.
 */
int
Xsj3cGetSegNum(buf)
    Xsj3cBuf  buf;
{
    return(buf->segnum);
}

/*
 * Xsj3cGetSeg()
 *  Return the appointed segment in buffers.
 */
wchar *
Xsj3cGetSeg(buf, n, len, attr)
    Xsj3cBuf    buf;
    int         n;
    int        *len;
    int        *attr;
{
    if (buf->convmode & DictModeMask) {
        if (n >= buf->curseg && n <= buf->curseg + buf->dict->n_dict)
            *attr = SEG_REVERSED;
        else if (buf->convedunderline)
            *attr = SEG_UNDER_LINE;
        else
            *attr = SEG_NORMAL;
    } else {
        if (n == buf->curseg) {
            if (SEG_NOEDIT & buf->input[n]->edit) {
                *attr = SEG_REVERSED;
            } else {
                *attr = SEG_UNDER_LINE;
            }
        } else {
            if (buf->input[n]->status & SEG_NOCONV || buf->convedunderline) {
                *attr = SEG_UNDER_LINE;
            } else {
                *attr = SEG_NORMAL;
            }
        }
    }
    *len = buf->input[n]->dnum;
    return (buf->input[n]->disp);
}

/*
 * Xsj3cGetConvertedStr()
 *  Set converted strings in buffer.
 */
wchar *
Xsj3cGetConvertedStr(buf, data)
    Xsj3cBuf                buf;
    wchar                  *data;
{
    register int            i;

    _Xsj3cFlushDcid(buf);
    if (buf->candidate) 
        Xsj3cEndCandidate(buf, ON);
    *data = '\0';
    for (i = 0; i < buf->segnum ; i++)
        _Xsj3cWcat(data, buf->input[i]->disp);
    return (data);
}

/*
 * Xsj3cGetConvertedLength()
 *  Return length of converted strings.
 */
int
Xsj3cGetConvertedLength(buf)
    Xsj3cBuf                buf;
{
    register int            i,  len;

    for (i = 0, len = 0; i < buf->segnum ; i++)
        len += buf->input[i]->dnum;
    return len;
}
