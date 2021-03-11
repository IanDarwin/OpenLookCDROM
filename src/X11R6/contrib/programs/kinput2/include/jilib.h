/* $Id: jilib.h,v 5.0 1991/10/01 07:58:04 ishisone Rel $ */

/*
 *	jilib.h -- jilib 用ヘッダファイル (Wnn Version4 対応版)
 *		version 5.0
 *		ishisone@sra.co.jp
 */

/*
 * Copyright (c) 1989  Software Research Associates, Inc.
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of Software Research Associates not be
 * used in advertising or publicity pertaining to distribution of the
 * software without specific, written prior permission.  Software Research
 * Associates makes no representations about the suitability of this software
 * for any purpose.  It is provided "as is" without express or implied
 * warranty.
 *
 * Author:  Makoto Ishisone, Software Research Associates, Inc., Japan
 */

#ifndef _jilib_h
#define _jilib_h

#include	"jslib.h"

/* errormessage コールバックで使われる、エラーの種類を示す定数 */
#define TYPE_WARNING	0
#define TYPE_ERROR	1

/* confirm コールバックで使われる、ファイルの種類を示す定数 */
#define TYPE_DIC	1
#define TYPE_HINDO	2

#ifdef __STDC__
extern WNN_JSERVER_ID *jiOpenServer(char *servername, int timeout);
extern int jiCloseServer(WNN_JSERVER_ID *server);
extern WNN_ENV *jiCreateEnv(WNN_JSERVER_ID *server, char *envname,
			    int override, char *wnnrcfile,
			    void (*errmsgfunc)(), int (*confirmfunc)(),
			    caddr_t client_data);
extern int jiDeleteEnv(WNN_ENV *env);
#else
extern WNN_JSERVER_ID *jiOpenServer();
extern int jiCloseServer();
extern WNN_ENV *jiCreateEnv();
extern int jiDeleteEnv();
#endif

#endif
