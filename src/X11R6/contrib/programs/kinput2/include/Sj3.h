/* $Header: /var/home/sm/nsc/nao/src/X11/X11R5/contrib/im/kinput2/include/RCS/Sj3.h,v 2.1 1992/06/09 03:55:37 nao Exp $ */
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

#ifndef _Sj3_h
#define _Sj3_h

#include "InputConv.h"

/*
  Sj3 new resources:

  name          class       type        default     access
  ----------------------------------------------------------------------------
  sj3serv       Sj3serv     String      *1          CG
  sj3serv2      Sj3serv2    String      *2          CG
  sj3user       Sj3user     String      *3          CG
  rkfile        Rkfile      String      *4          CG
  sbfile        Sbfile      String      *5          CG
  rcfile        Rcfile      String      *6          CG
  hkfile        Hkfile      String      *7          CG
  zhfile        Zhfile      String      *8          CG

  note: *1) if not specified, use value of an environment variable "SJ3SERV"
                if not specified an environment "SJ3SERV", read from sjrc file
        *2) if not specified, use value of an environment variable "SJ3SERV2"
                if not specified an environment "SJ3SERV2", read from sjrc file
        *3) if not specified, use value of login name
        *4) if not specified, use value of an environment variable "SJRK"
        *5) if not specified, use value of an environment variable "SJSB"
        *6) if not specified, use value of an environment variable "SJRC"
        *7) if not specified, use value of an environment variable "SJHK"
        *8) if not specified, use value of an environment variable "SJZH"
*/

#define XtNsj3serv      "sj3serv"
#define XtCSj3serv      "Sj3serv"
#define XtNsj3serv2     "sj3serv2"
#define XtCSj3serv2     "Sj3serv2"
#define XtNsj3user      "sj3user"
#define XtCSj3user      "Sj3user"
#define XtNrcfile       "rcfile"
#define XtCRcfile       "Rcfile"
#define XtNsbfile       "sbfile"
#define XtCSbfile       "Sbfile"
#define XtNrkfile       "rkfile"
#define XtCRkfile       "Rkfile"
#define XtNhkfile       "hkfile"
#define XtCHkfile       "Hkfile"
#define XtNzhfile       "zhfile"
#define XtCZhfile       "Zhfile"

typedef struct _Sj3ClassRec *Sj3ObjectClass;
typedef struct _Sj3Rec      *Sj3Object;

extern WidgetClass  sj3ObjectClass;

#endif /* _Sj3_h */

