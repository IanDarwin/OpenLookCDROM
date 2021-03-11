#ifndef lint
static char *rcsid = "$Header: connect.c,v 2.0 92/02/13 18:33:14 nao Exp $";
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

int                         Xsj3cOpen();
void                        Xsj3cClose();
void                        Xsj3cConnect();

static void                 _Xsj3cOpenError();
static void                 _Xsj3cCloseError();

static int                  connected[SERVER_NUM]= {0};

/*
 * Xsj3cOpen()
 *  Connect to sj3serv(kana-kanji conversion server).
 */
int
Xsj3cOpen(buf, host, user, force, second)
    Xsj3cBuf    buf;
    char       *host;
    char       *user;
    int         force;
    int         second;
{
    extern char        *getenv();
    char               *p;
    char                hostname[SERVER_NUM][32];
    int                 value;
    
    if (connected[buf->server] && !force) {
        return CONNECT_OK;
    }
    if (host) {
        if (user) {
            if ((value = serverIF[buf->server].func[FUNC_OPEN](host, user))
                    != SJ3_NORMAL_END) {
                _Xsj3cOpenError(value);
                if (value == SJ3_SERVER_DEAD || value == SJ3_CONNECT_ERROR)
                    return CONNECT_FAIL;
            }
        } else {
            Xsj3cError("can't connect sj3serv by null user");
            return CONNECT_FAIL;
        }
    } else {
        if (user) {
            hostname[buf->server][0] = '\0';
            if (buf->server == SERVER_SJ3) {
                if (!second) {
                    if ((p = getenv("SJ3SERV")) && *p != '\0') {
                        strcpy(hostname[buf->server], p);
                    } else if (buf->sj3serv) {
                        strcpy(hostname[buf->server], buf->sj3serv);
                    }
                } else {
                    if ((p = getenv("SJ3SERV2")) && *p != '\0') {
                        strcpy(hostname[buf->server], p);
                    } else if (buf->sj3serv2) {
                        strcpy(hostname[buf->server], buf->sj3serv2);
                    }
                }
            }
            if ((value = serverIF[buf->server].func[FUNC_OPEN](hostname, user))
                    != SJ3_NORMAL_END) {
                _Xsj3cOpenError(value);
                if (value == SJ3_SERVER_DEAD || value == SJ3_CONNECT_ERROR)
                    return CONNECT_FAIL;
            }
        } else {
            Xsj3cError("can't connect sj3serv by null user");
            return CONNECT_FAIL;
        }
    }
    locked[buf->server] = 0;
    connected[buf->server] = 1;
    return CONNECT_OK;
}

/*
 * Xsj3cConnect()
 *  Disconnect to sj3serv and reonnect to sj3serv.
 */
void
Xsj3cConnect(buf, host, host2, user)
    Xsj3cBuf    buf;
    char       *host;
    char       *host2;
    char       *user;
{
    serverIF[buf->server].func[FUNC_UNLOCK]();
    Xsj3cClose(buf, ON);
    if ((Xsj3cOpen(buf, host, user, OFF, OFF)) != CONNECT_OK) {
        Xsj3cWarning("Failed to connect first server. try to second server");
        if ((Xsj3cOpen(buf, host2, user, OFF, ON)) != CONNECT_OK) {
            Xsj3cError("Failed to connect seconON server.");
        }
    }
}

/*
 * Xsj3cClose()
 *  Disconnect to sj3serv.
 */
void
Xsj3cClose(buf, force)
    Xsj3cBuf    buf;
    int         force;
{
    int         value;

    if (connected[buf->server]) {
        if ((value = serverIF[buf->server].func[FUNC_CLOSE]())
                != SJ3_NORMAL_END && !force) {
            _Xsj3cCloseError(value);
        }
        connected[buf->server] = 0;
    }
}

/*
 * _Xsj3cOpenError()
 *  Print error messages for connecting to sj3serv.
 */
static void
_Xsj3cOpenError(error)
    int error;
{
    switch (error) {
        case SJ3_SERVER_DEAD:
            Xsj3cWarning("sj3_open: server died in connecting");
            break;
        case SJ3_CONNECT_ERROR:
            Xsj3cWarning("sj3_open: connected to server error");
            break;
        case SJ3_ALREADY_CONNECTED:
            Xsj3cWarning("sj3_open: already connected to server");
            break;
        case SJ3_CANNOT_OPEN_MDICT:
            Xsj3cWarning("sj3_open: can't open main dictionaries");
            break;
        case SJ3_CANNOT_OPEN_UDICT:
            Xsj3cWarning("sj3_open: can't open user dictionaries");
            break;
        case SJ3_CANNOT_OPEN_STUDY:
            Xsj3cWarning("sj3_open: can't open files for study");
            break;
        case SJ3_CANNOT_MAKE_UDIR:
            Xsj3cWarning("sj3_open: can't make directries for user");
            break;
        case SJ3_CANNOT_MAKE_UDICT:
            Xsj3cWarning("sj3_open: can't make user dictionaries");
            break;
        case SJ3_CANNOT_MAKE_STUDY:
            Xsj3cWarning("sj3_open: can't make files for study");
            break;
        default:
            Xsj3cWarning("sj3_open: unknown error");
            break;
    }
}

/*
 * _Xsj3cCloseError()
 *  Print error messages for disconnecting to sj3serv.
 */
static void
_Xsj3cCloseError(error)
    int error;
{
    switch (error) {
        case SJ3_SERVER_DEAD:
            Xsj3cWarning("sj3_close: server died in disconnecting");
            break;
        case SJ3_DISCONNECT_ERROR:
            Xsj3cWarning("sj3_close: server internal error");
            break;
        case SJ3_NOT_CONNECTED:
            Xsj3cWarning("sj3_close: already disconnected to server");
            break;
        case SJ3_NOT_OPENED_MDICT:
            Xsj3cWarning("sj3_close: main dictionaries are not opend");
            break;
        case SJ3_NOT_OPENED_UDICT:
            Xsj3cWarning("sj3_close: user dictionaries are not opend");
            break;
        case SJ3_NOT_OPENED_STUDY:
            Xsj3cWarning("sj3_close: files for study are not opend");
            break;
        case SJ3_CLOSE_MDICT_ERROR:
            Xsj3cWarning("sj3_close: can't close main dictionaries");
            break;
        case SJ3_CLOSE_UDICT_ERROR:
            Xsj3cWarning("sj3_close: can't close user dictionaries");
            break;
        case SJ3_CLOSE_STUDY_ERROR:
            Xsj3cWarning("sj3_close: can't close files for study");
            break;
        default:
            Xsj3cWarning("sj3_close: unknown error");
            break;
    }
}

