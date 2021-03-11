/* -*-C-*-
********************************************************************************
*
* File:         config.h
* RCS:          $Header: /users/npm/src/winterp/src-server/RCS/config.h,v 2.10 1994/06/06 15:40:35 npm Exp $
* Description:  Default filepaths, ID's etc.
* Author:       Niels Mayer
* Created:      Wed Aug 31 14:09:08 1988
* Modified:     Sun Jun  5 15:20:31 1994 (Niels Mayer) npm@indeed
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

/*
 * define WINTERP_WANT_INET_SERVER (or compile w/ -DWINTERP_WANT_INET_SERVER)
 * if you want WINTERP to allow a INET domain winterp-lisp eval server .
 * Note that the INET domain server is a security hole, allowing anybody on
 * the network to access your machine through WINTERP.
 * That's why it's disabled by default. (see Makefile.{hpux9,sun4,...}
 *
 * Note that WINTERP may be compiled using WINTERP_WANT_UNIX_SERVER,
 * WINTERP_WANT_INET_SERVER, and WINTERP_WANT_STDIN_SERVER.
 * WINTERP can run with all three inputs enabled.
 */
/* #define WINTERP_WANT_INET_SERVER */

/*
 * define WINTERP_WANT_UNIX_SERVER (or compile w/ -DWINTERP_WANT_UNIX_SERVER)
 * if you want WINTERP's Unix Domain Server capabilities. Most of the
 * Makefile.{hpux9,sun4,...} have this feature enabled. However, 
 * you may wish to do this for creating stand-alone applications
 * that do not need a server input. Alternately, your Unix implementation
 * may not support Unix Domain Sockets...
 *
 * Note that WINTERP may be compiled using WINTERP_WANT_UNIX_SERVER,
 * WINTERP_WANT_INET_SERVER, and WINTERP_WANT_STDIN_SERVER.
 * WINTERP can run with all three inputs enabled.
 */
/* #define WINTERP_WANT_UNIX_SERVER */

/*
 * define WINTERP_WANT_STDIN_SERVER (or compile w/ -DWINTERP_WANT_STDIN_SERVER)
 * if you want WINTERP's read/eval/print to work off of STDIN. Most of the
 * Makefile.{hpux9,sun4,...} have this feature enabled. However, 
 * you may wish to disable this for creating stand-alone applications
 * that do not need to allow lisp input from the terminal. You'll also want
 * to disable this compilation feature if your Unix implementation does
 * not support XtAppAddInput() from STDIN.
 *
 * Note that WINTERP may be compiled using WINTERP_WANT_UNIX_SERVER,
 * WINTERP_WANT_INET_SERVER, and WINTERP_WANT_STDIN_SERVER.
 * WINTERP can run with all three inputs enabled.
 */
/* #define WINTERP_WANT_STDIN_SERVER */

/*
 * Special bogus hack used by INET and Unix domain client/server
 * ./winterp.c, ../src-client/wl.c, and ../src-client/wl-tcpip.c.
 */
#define WINTERP_CLIENT_SPECIAL_EOF_CHAR		'\277' /* this won't work if people start sending 8 bit s-expressions between client/server... */
#define WINTERP_CLIENT_SPECIAL_EOF_STRING	"\277" /* this won't work if people start sending 8 bit s-expressions between client/server... */

/*
 * INET Domain client/server defaults used by both ./winterp.c
 * and ../src-client/wl-tcpip.c.
 */
#define DEFAULT_INET_SERVICE_HOSTADDR		"127.0.0.1" /* 'wl-tcpip' uses loopback address by default */
#define DEFAULT_INET_HOSTADDR_ENVVAR		"WINTERP_INET_HOSTNAME"	/* 'wl-tcpip' looks for this environment var holding server hostname */
#define DEFAULT_INET_SERVICE_PORT		23751 /* used by both 'winterp' and 'wl-tcpip' -- default WINTERP INET server port number */
#define DEFAULT_INET_PORT_ENVVAR		"WINTERP_INET_PORTNUM" /* used by both 'winterp' and 'wl-tcpip' -- environment var holding port number */
#define DEFAULT_INET_SERVICE_NAME		"widget_interp"	/* used by both 'winterp' and 'wl-tcpip' -- service name if portnum specified as 0 */
#define DEFAULT_ENABLE_INET_SERVER		FALSE /* by default, don't enable INET server because it is a security hole. */

/*
 * Unix Domain client/server defaults used by both ./winterp.c
 * and ../src-client/wl.c
 */
#define DEFAULT_ENABLE_UNIX_SERVER		FALSE /* by default, don't enable unix domain server */
#define DEFAULT_UNIX_SOCKET_FILEPATH		"/tmp/.winterp_serv" /* used by both 'winterp' and 'wl' -- the default unix domain socket */
#define DEFAULT_UNIX_SOCKET_FILEPATH_ENVVAR	"WINTERP_UNIX_SOCKET_FILEPATH" /* used by both 'winterp' and 'wl' -- environment var for unix socket file */

/*
 * STDIN server defaults
 */
#define DEFAULT_ENABLE_STDIN_SERVER		TRUE

/*
 * Defaults used by winterp.c
 */
#define DEFAULT_LISP_INIT_FILE			NULL /* note code which explicitly checks for default user_prefs.lisp_transcript_file==NULL */
#define DEFAULT_LISP_LIB_DIR			""
#define DEFAULT_LISP_LOAD_PATH			""
#define DEFAULT_LISP_TRANSCRIPT_FILE		NULL /* note code which explicitly checks for default user_prefs.lisp_transcript_file==NULL */
#define DEFAULT_INIT_MESSAGE_STRING		NULL /* note code which explicitly checks for default user_prefs.init_message_string==NULL */
#define DEFAULT_ENABLE_INIT_MSGS		TRUE
#define DEFAULT_ENABLE_XT_ERROR_BREAK		TRUE
#define DEFAULT_ENABLE_XT_WARNING_BREAK		FALSE
#define DEFAULT_ENABLE_X_ERROR_BREAK		TRUE
#ifdef SAVERESTORE
#define DEFAULT_LISP_RESTORE_FILE		"winterp.wks"
#define DEFAULT_ENABLE_LISP_RESTORE		TRUE
#endif /* SAVERESTORE */
