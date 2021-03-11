/* $Header: Sj3P.h,v 2.0 92/02/09 17:56:19 nao Exp $ */
/*
 * Copyright (c) 1990  Software Research Associates, Inc.
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

#ifndef _Sj3P_h
#define _Sj3P_h

#include "InputConvP.h"

#include "Sj3.h"
#include "Xsj3clib.h"

typedef struct {
    int foo;
} Sj3ClassPart;

typedef struct _Sj3ClassRec {
    ObjectClassPart     object_class;
    InputConvClassPart  inputConv_class;
    Sj3ClassPart        sj3_class;
} Sj3ClassRec;

typedef enum {
    normal_state,
    candidate_state, 
    symbol_state,
    hinsi_state 
} Sj3State;

typedef struct {

    /* resources */
    String              sj3serv;
    String              sj3serv2;
    String              sj3user;
    String              rcfile;
    String              rkfile;
    String              hkfile;
    String              zhfile;
    String              sbfile;

    /* private data */
    Xsj3cBuf            sj3buf;
    Sj3State            state;
    ICString           *candlist;
    int                 candlistsize;
    int                 numcand;
    int                 curcand;
    ICString           *symbollist;
    int                 cursymbol;
    ICString           *hinsilist;
    int                 curhinsi; 
    Boolean             selectionending;
} Sj3Part;

typedef struct _Sj3Rec {
    ObjectPart          object;
    InputConvPart       inputConv;
    Sj3Part             sj3;
} Sj3Rec;

#endif
