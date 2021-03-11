/* -*-C-*-
********************************************************************************
*
* File:         config.h
* RCS:          $Header: config.h,v 1.2 91/03/17 04:14:58 mayer Exp $
* Description:  Default filepaths, ID's etc.
* Author:       Niels Mayer, HPLabs
* Created:      Wed Aug 31 14:09:08 1988
* Modified:     Sat Oct  5 23:13:19 1991 (Niels Mayer) mayer@hplnpm
* Language:     C
* Package:      N/A
* Status:       X11r5 contrib tape release
*
* WINTERP Copyright 1989, 1990, 1991 Hewlett-Packard Company (by Niels Mayer).
* XLISP version 2.1, Copyright (c) 1989, by David Betz.
*
* Permission to use, copy, modify, distribute, and sell this software and its
* documentation for any purpose is hereby granted without fee, provided that
* the above copyright notice appear in all copies and that both that
* copyright notice and this permission notice appear in supporting
* documentation, and that the name of Hewlett-Packard and David Betz not be
* used in advertising or publicity pertaining to distribution of the software
* without specific, written prior permission.  Hewlett-Packard and David Betz
* make no representations about the suitability of this software for any
* purpose. It is provided "as is" without express or implied warranty.
*
* HEWLETT-PACKARD AND DAVID BETZ DISCLAIM ALL WARRANTIES WITH REGARD TO THIS
* SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS,
* IN NO EVENT SHALL HEWLETT-PACKARD NOR DAVID BETZ BE LIABLE FOR ANY SPECIAL,
* INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
* LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE
* OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
* PERFORMANCE OF THIS SOFTWARE.
*
* See ./winterp/COPYRIGHT for information on contacting the authors.
* 
* Please send modifications, improvements and bugfixes to mayer@hplabs.hp.com
* Post XLISP-specific questions/information to the newsgroup comp.lang.lisp.x
*
********************************************************************************
*/

/*
 * define WINTERP_WANT_INET_SERVER (or compile w/ -DWINTERP_WANT_INET_SERVER)
 * if you want WINTERP to allow a INET domain winterp-lisp eval server .
 * Note that the INET domain server is a security hole, allowing anybody on
 * the network to access your machine through WINTERP.
 * That's why it's disabled by default.
 *
 * Note that WINTERP may be compiled using both WINTERP_WANT_UNIX_SERVER
 * and WINTERP_WANT_INET_SERVER. WINTERP can run with both servers enabled.
 */
#define WINTERP_WANT_INET_SERVER

/*
 * Comment this out if you don't want WINTERP's Unix Domain Server capabilities.
 * You may wish to do this for creating stand-alone applications
 * that do not need a server input. ALternately, your Unix implementation
 * may not support Unix Domain Sockets...
 *
 * Note that WINTERP may be compiled using both WINTERP_WANT_UNIX_SERVER
 * and WINTERP_WANT_INET_SERVER. WINTERP can run with both servers enabled.
 */
/* #define WINTERP_WANT_UNIX_SERVER */

/*
 * INET Domain client/server defaults used by both ./winterp.c
 * and ../src-client/wl-tcpip.c.
 */
#define DEFAULT_INET_SERVICE_HOSTADDR		"127.0.0.1" /* 'wl-tcpip' uses loopback address by default */
#define DEFAULT_INET_HOSTADDR_ENVVAR		"WINTERP_INET_HOSTNAME"	/* 'wl-tcpip' looks for this environment var holding server hostname */
#define DEFAULT_INET_SERVICE_PORT		3914 /* used by both 'winterp' and 'wl-tcpip' -- default WINTERP INET server port number */
#define DEFAULT_INET_PORT_ENVVAR		"WINTERP_INET_PORTNUM" /* used by both 'winterp' and 'wl-tcpip' -- environment var holding port number */
#define DEFAULT_INET_SERVICE_NAME		"widget_interp"	/* used by both 'winterp' and 'wl-tcpip' -- service name if portnum specified as 0 */
#define DEFAULT_ENABLE_INET_SERVER		TRUE /* by default, don't enable INET server because it is a security hole. */

/*
 * Unix Domain client/server defaults used by both ./winterp.c
 * and ../src-client/wl.c
 */
#define DEFAULT_ENABLE_UNIX_SERVER		FALSE /* by default, enable unix domain server */
#define DEFAULT_UNIX_SOCKET_FILEPATH		"/tmp/.winterp_serv" /* used by both 'winterp' and 'wl' -- the default unix domain socket */
#define DEFAULT_UNIX_SOCKET_FILEPATH_ENVVAR	"WINTERP_UNIX_SOCKET_FILEPATH" /* used by both 'winterp' and 'wl' -- environment var for unix socket file */

/*
 * Defaults used by winterp.c
 */
#define DEFAULT_LISP_INIT_FILE			"initialize.lsp"
#define DEFAULT_LISP_TRANSCRIPT_FILE		NULL
#define DEFAULT_LISP_LIB_DIR			"./"
#define DEFAULT_ENABLE_INIT_MSGS		TRUE
#define DEFAULT_ENABLE_XT_ERROR_BREAK		TRUE
#define DEFAULT_ENABLE_XT_WARNING_BREAK		TRUE
#define DEFAULT_ENABLE_X_ERROR_BREAK		TRUE
