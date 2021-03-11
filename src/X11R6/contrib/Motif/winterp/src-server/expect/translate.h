/* -*-C-*-
*******************************************************************************
*
* File:         translate.h
* RCS:          $Header: /users/npm/src/winterp/src-server/expect/RCS/translate.h,v 2.4 1994/06/06 15:43:06 npm Exp $
* Description:  preface globals that appear in the expect library with "exp_"
*		so we don't conflict with the user.  This saves me having
*		to use exp_XXX throughout the expect program itself, which
*		was written well before the library when I didn't have to worry
*		about name conflicts.
*
* Author:       Don Libes, NIST
* Created:      2/6/90
* Modified:     Sun Jun  5 03:44:38 1994 (Niels Mayer) npm@indeed
* Language:     C
* Package:      N/A
* Status:       X11r6 contrib release
*
* libexpect, by Don Libes, National Institute of Standards and Technology
* WINTERP 2.0 Copyright (C) 1994, Enterprise Integration Technologies Corp. and Niels Mayer.
* WINTERP 1.15-1.99, Copyright (c) 1993, Niels P. Mayer.
* WINTERP 1.0-1.14, Copyright (c) 1989-1992 Hewlett-Packard Co. and Niels Mayer.
* 
* Permission to use, copy, modify, distribute, and sell this software and its
* documentation for any purpose is hereby granted without fee, provided that
* the above copyright notice appear in all copies and that both that
* copyright notice and this permission notice appear in supporting
* documentation, and that the name of National Institute of Standards and
* Technology, Don Libes, Enterprise Integration Technologies, 
* Hewlett-Packard Company, or Niels Mayer not be used in advertising or
* publicity pertaining to distribution of the software without specific,
* written prior permission. National Institute of Standards and Technology,
* Don Libes, Enterprise Integration Technologies, Hewlett-Packard Company,
* and Niels Mayer makes no representations about the suitability of this 
* software for any purpose.  It is provided "as is" without express or
* implied warranty.
* 
* NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY, DON LIBES, ENTERPRISE
* INTEGRATION TECHNOLOGIES, HEWLETT-PACKARD COMPANY AND NIELS MAYER
* DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL IMPLIED
* WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL THE NATIONAL
* INSTITUTE OF STANDARDS AND TECHNOLOGY, DON LIBES, ENTERPRISE INTEGRATION
* TECHNOLOGIES, HEWLETT-PACKARD COMPANY OR NIELS MAYER BE LIABLE
* FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER
* RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF
* CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
* CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
*
*******************************************************************************
*/

/* translate.h - preface globals that appear in the expect library
with "exp_" so we don't conflict with the user.  This saves me having
to use exp_XXX throughout the expect program itself, which was written
well before the library when I didn't have to worry about name conflicts.

Written by: Don Libes, NIST, 12/3/90

Design and implementation of this program was paid for by U.S. tax
dollars.  Therefore it is public domain.  However, the author and NIST
would appreciate credit if this program or parts of it are used.
*/

#define debuglog	exp_debuglog
#define is_debugging	exp_is_debugging
#define logfile		exp_logfile
#define debugfile	exp_debugfile
#define loguser		exp_loguser
#define logfile_all	exp_logfile_all

#define init_pty	exp_init_pty
#define getptymaster	exp_getptymaster
#define getptyslave	exp_getptyslave
