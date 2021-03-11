#ifndef lint
static char *rcsid = "$Header: /var/home/sm/nsc/nao/src/X11/X11R5/contrib/im/kinput2/lib/Xsj3clib/RCS/candidate.c,v 2.2 1992/03/18 09:44:48 nao Exp nao $";
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

extern Xsj3cCVServerIF      serverIF[SERVER_NUM];

Xsj3cCand                   _Xsj3cCandidateInit();
int                         Xsj3cGetCandidateNum();
Xsj3cCand                   Xsj3cGetCandidates();
wchar                      *Xsj3cGetCandidate();
int                         Xsj3cSetCandidate();
void                        Xsj3cEndCandidate();

/*
 * _Xsj3cCandidateInit()
 *  Initialize candidate list of current segment.
 * No alloca() used.
 */
Xsj3cCand
_Xsj3cCandidateInit(buf)
    Xsj3cBuf    buf;
{
    register int        i,  j,  num,    padnum;
    Xsj3csMode          padmode[2];
    unsigned char      *knjbuf;
    SJ3_DOUON          *mbcand;

    if (buf->candidate) {
        return (buf->candidate);
    }
    i = 0;
    buf->curcand = 0;
    if ((knjbuf = (unsigned char *)
        malloc(buf->input[buf->curseg]->size * sizeof(wchar))) == NULL) {
        Xsj3cWarning("Cannot allocate for candidate yomi buffer");
        return (NULL);
    }
    _Xsj3cwPStomPS(buf, knjbuf, buf->input[buf->curseg]->yomi);
    num = buf->candnum = serverIF[buf->server].func[FUNC_CANDNUM](knjbuf);
    if (num < 0) {
        Xsj3cWarning("sj3serv is down. reconnect please");
        if (buf->convmode & SelectModeMask)
            buf->convmode = ConvedModeMask;
        free(knjbuf);
        return ((Xsj3cCand)NULL);
    } else if (num > 0) {
        mbcand = (SJ3_DOUON *)calloc(num, sizeof(SJ3_DOUON));
        if (!mbcand) {
            Xsj3cWarning("Cannot allocate memory for candidate list");
            if (buf->convmode & SelectModeMask) 
                buf->convmode = ConvedModeMask;
            free(knjbuf);
            return ((Xsj3cCand)NULL);
        }
        num = buf->candnum
                = serverIF[buf->server].func[FUNC_CANDIDATE](knjbuf, mbcand);
        if (num <= 0) {
            if (num < 0) {
                Xsj3cWarning("sj3serv is down. reconnect please.");
            } else  {
                Xsj3cWarning("There is no candidate.");
                goto candpad;
            }
            if (buf->convmode & SelectModeMask) 
                buf->convmode = ConvedModeMask;
            free(mbcand);
            free(knjbuf);
            return ((Xsj3cCand)NULL);
        } else {
            if ((buf->candidate = (Xsj3cCand)
                calloc(num, sizeof(Xsj3cCandRec))) == NULL) {
                Xsj3cWarning("Cannot allocate memory for candidate list");
                if (buf->convmode & SelectModeMask)
                    buf->convmode = ConvedModeMask;
                free(knjbuf);
                free(mbcand);
                buf->candnum = -1;
                return ((Xsj3cCand)NULL);
            }
            while (i < num) {
                buf->candidate[i].len = _Xsj3cmPStowOUT(buf,
                    buf->candidate[i].data, mbcand[i].ddata);
                buf->candidate[i].dcid = mbcand[i].dcid;
                i++;
            }
            if (buf->candnum > 1) {
                serverIF[buf->server].func[FUNC_LOCK]();
                locked[buf->server]++;
            } else {
                bzero(&buf->candidate[0].dcid,
                        sizeof(buf->candidate[0].dcid));
            }
        }
        free(mbcand);
        if (buf->candpadding)
            goto candpad;
        else
            free(knjbuf);
    } else {
candpad:
        if (buf->input[buf->curseg]->num > CANDBUFSIZ - 1) {
            free(knjbuf);
            return (buf->candidate);
        }
        switch(buf->input[buf->curseg]->cursegmode) {
        case MODE_HIRA:
            padnum = 1;
            padmode[0] = MODE_ZKATA;
            break;
        case MODE_ZKATA:
            padnum = 1;
            padmode[0] = MODE_HIRA;
            break;
        default:
            padnum = 2;
            padmode[0] = MODE_HIRA;
            padmode[1] = MODE_ZKATA;
            break;
        }
        if (buf->candnum != 1
                || buf->input[buf->curseg]->cursegmode == MODE_HIRA)
            num += (padnum + 1);
        else 
            num += padnum;
        if (buf->candidate)
            buf->candidate = (Xsj3cCand)realloc(buf->candidate,
                    num * sizeof(Xsj3cCandRec));
        else 
            buf->candidate = (Xsj3cCand)calloc(num, sizeof(Xsj3cCandRec));
        if (!buf->candidate) {
            Xsj3cWarning("Cannot allocate memory for candidate list");
            free(knjbuf);
            buf->candnum = -1;
            return ((Xsj3cCand)NULL);
        }
        if (buf->candnum > 1
                || buf->input[buf->curseg]->cursegmode == MODE_HIRA) {
            _Xsj3cwPStowOUT(buf, buf->candidate[i].data,
                    buf->input[buf->curseg]->yomi);
            buf->candidate[i].len = buf->input[buf->curseg]->num;
            bzero(&buf->candidate[i].dcid, sizeof(buf->candidate[i].dcid));
            i++;
        } else if (!buf->candnum) {
            _Xsj3cWcpy(buf->candidate[i].data, buf->input[buf->curseg]->disp);
            buf->candidate[i].len = buf->input[buf->curseg]->dnum;
            bzero(&buf->candidate[i].dcid, sizeof(buf->candidate[i].dcid));
            i++;
        }
        for (j = 0; j < padnum; i++, j++) {
            Xsj3cModeConv(buf, knjbuf, padmode[j],
                    buf->input[buf->curseg]->size);
            buf->candidate[i].len
                    = _Xsj3cmPStowOUT(buf, buf->candidate[i].data, knjbuf);
            bzero(&buf->candidate[i].dcid,
                    sizeof(buf->candidate[i].dcid));
        }
        free(knjbuf);
        buf->candnum = num;
    }
    buf->candseg = buf->curseg;
    return (buf->candidate);
}

/*
 * Xsj3cGetCandidateNum()
 * If list is already initialized, return candidate number,
 * else initialize list and return candidate number
 * when it succeeded in initializing list.
 * If sj3serv is down or failed to allocate memory, return -1.
 */
int
Xsj3cGetCandidateNum(buf, cur)
    Xsj3cBuf            buf;
    int                *cur;
{
    if (!buf->candidate) {
        buf->candidate = _Xsj3cCandidateInit(buf);
    }
    *cur = buf->curcand;
    return (buf->candnum);
}

/*
 * Xsj3cGetCandidates()
 *  Return candidate data.
 */
Xsj3cCand
Xsj3cGetCandidates(buf)
    Xsj3cBuf            buf;
{
    if (!buf->candidate) {
        buf->candidate = _Xsj3cCandidateInit(buf);
    }
    return (buf->candidate);
}

/*
 * Xsj3cGetCandidate()
 *  Return the appointed (by 2nd argument) candidate.
 */
wchar *
Xsj3cGetCandidate(buf, n, len)
    Xsj3cBuf            buf;
    int                 n;
    int                *len;
{
    if (!buf->candidate) {
        buf->candidate = _Xsj3cCandidateInit(buf);
    }
    *len = buf->candidate[n].len;
    return (buf->candidate[n].data);
}

/*
 * Xsj3cSetCandidate()
 *  Set the selected candidate strings to the buffers.
 */
int
Xsj3cSetCandidate(buf, sel_candidate, changed, flush)
    Xsj3cBuf    buf;
    int         sel_candidate;
    int        *changed;
    int        *flush;
{
    register wchar      *p, *q;
    register i,         same = 1;

    *flush = OFF;
    *changed = OFF;
    if (sel_candidate >= buf->candnum)
        return -1;
    p = buf->candidate[sel_candidate].data;
    q = buf->input[buf->curseg]->disp;
    i = buf->input[buf->curseg]->dnum;
    while (i--) {
        if (*p++ != *q++) {
            same = 0;
            break;
        }
    }
    if ((!same || buf->curcand != sel_candidate) &&
        buf->candseg < buf->segnum && buf->input[buf->candseg]) {
        buf->input[buf->candseg]->dnum
                = buf->candidate[sel_candidate].len;
        _Xsj3cWcpy(buf->input[buf->candseg]->disp,
                buf->candidate[sel_candidate].data);
        *changed = ON;
        if (buf->gakusyuu)
            buf->input[buf->candseg]->change = ON;
        buf->curcand = sel_candidate;
    } else {
        *changed = OFF;
    }
    return 0;
}

/*
 * Xsj3cEndCandidate()
 *  SelectMode: End candidate select mode(SelectMode) and back to ConvedMode.
 *  ConvedModeMask: Free buffer of candidates and unlock sj3serv.
 */
void
Xsj3cEndCandidate(buf, sync)
    Xsj3cBuf    buf;
    int         sync;
{
    if (!((buf->convmode & SelectModeMask)
            && buf->selectstatus == SELECT_CAND)) {
        if (buf->candnum > 1) {
            if (sync) {
                if ((serverIF[buf->server].func[FUNC_STUDY]
                        (&buf->candidate[buf->curcand].dcid)) < 0) {
                    Xsj3cWarning("sj3serv is down. reconnect please");
                }
            }
            if (locked[buf->server] > 0) {
                if (!(--locked[buf->server]))
                    serverIF[buf->server].func[FUNC_UNLOCK]();
            }
        }
        free (buf->candidate);
        buf->candidate = NULL;
        buf->curcand = 0;
        buf->candnum = 0;
    } else {
        if ((buf->flushsconv == OFF) && buf->input[buf->segnum] &&
            buf->input[buf->segnum]->num) {
            buf->curseg = buf->segnum;
            buf->segnum++;
            buf->convmode = InputModeMask;
        } else {
            buf->convmode = ConvedModeMask;
        }
    }
}
