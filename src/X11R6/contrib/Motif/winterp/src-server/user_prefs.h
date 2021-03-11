/* -*-C-*-
********************************************************************************
*
* File:         user_prefs.h
* RCS:          $Header: /users/npm/src/winterp/src-server/RCS/user_prefs.h,v 2.5 1994/06/06 15:41:03 npm Exp $
* Description:  type definition and global datastructure for user preferences
* Author:       Niels Mayer
* Created:      Thu Aug 11 00:06:04 1988
* Modified:     Sun Jun  5 14:30:29 1994 (Niels Mayer) npm@indeed
* Language:     C
* Package:      N/A
* Status:       X11r6 contrib release
*
* Copyright (C) 1994, Enterprise Integration Technologies Corp. and Niels Mayer.
* WINTERP 1.15-1.99, Copyright (c) 1993, Niels P. Mayer.
* WINTERP 1.0-1.14, Copyright (c) 1989-1992 Hewlett-Packard Co. and Niels Mayer.
* 
* Permission to use, copy, modify, distribute, and sell this software and its
* documentation for any purpose is hereby granted without fee, provided that
* the above copyright notice appear in all copies and that both that
* copyright notice and this permission notice appear in supporting
* documentation, and that the name of Enterprise Integration Technologies,
* Hewlett-Packard Company, or Niels Mayer not be used in advertising or
* publicity pertaining to distribution of the software without specific,
* written prior permission. Enterprise Integration Technologies, Hewlett-Packard
* Company, and Niels Mayer makes no representations about the suitability of
* this software for any purpose.  It is provided "as is" without express or
* implied warranty.
* 
* ENTERPRISE INTEGRATION TECHNOLOGIES, HEWLETT-PACKARD COMPANY AND NIELS MAYER
* DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL IMPLIED
* WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL ENTERPRISE
* INTEGRATION TECHNOLOGIES, HEWLETT-PACKARD COMPANY OR NIELS MAYER BE LIABLE
* FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER
* RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF
* CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
* CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
*
********************************************************************************
*/

/*
*------------------------------------------------------------------------------
* See ./winterp/COPYRIGHT for information on contacting the authors.
* Please e-mail comments, modifications, questions, improvements and
* bugfixes to the WINTERP mailing list winterp@netcom.com. Please send 
* mailing list subscribe/unsubscribe notices to winterp-request@netcom.com .
* Post XLISP-specific questions/information to the USENET newsgroup
* comp.lang.lisp.x.
*------------------------------------------------------------------------------
*/


#include "../src-server/config.h"

typedef struct {
  String	lisp_init_file;
  String	lisp_transcript_file;
  String	lisp_lib_dir;		/* for back-compatibility only ... superceded by lisp_lib_path */
  String	lisp_load_path;
  Boolean	enable_init_msgs;
  String	init_message_string;
  int		service_port;
  String	service_name;
  String	unix_socket_filepath;
  Boolean	enable_AF_INET_server;
  Boolean	enable_AF_UNIX_server;
  Boolean	enable_STDIN_server;
  Boolean	enable_XtError_break;
  Boolean	enable_XtWarning_break;
  Boolean	enable_XError_break;

#ifdef SAVERESTORE
  String	lisp_restore_file;
  Boolean	enable_lisp_restore;
#endif /* SAVERESTORE */

} USER_PREFS_DATA, *USER_PREFS_DATA_PTR;

extern USER_PREFS_DATA user_prefs;
