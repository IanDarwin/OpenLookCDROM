/* servlib.h -- basic TCP/IP stream socket server routines
 *
 *	(C) Copyright 1991 by Carnegie Mellon University
 *
 *                      All Rights Reserved
 *
 * Permission to use, copy, modify, and distribute this software and its 
 * documentation for any purpose and without fee is hereby granted, 
 * provided that the above copyright notice appear in all copies and that
 * both that copyright notice and this permission notice appear in 
 * supporting documentation, and that the name of CMU not be
 * used in advertising or publicity pertaining to distribution of the
 * software without specific, written prior permission.  
 * 
 * CMU DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
 * CMU BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
 * ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
 * WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
 * ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
 * SOFTWARE.
 *
 * Author: Chris Newman
 * Start Date: 9/4/91
 */

#ifdef __STDC__
/* initialize the server with a given port (0 for dynamic port)
 *  int *port	pointer to the port number
 * returns -1 on failure, 0 on success
 */
int serv_init(int *);

/* accept a connection to the server (non-blocking)
 * returns -1 on failure, fd on success
 */
int serv_accept(void);

/* close a connection
 *  int fd	the fd returned by serv_accept()
 */
void serv_close(int);

/* shutdown the server
 */
void serv_shutdown(void);
#else /* __STDC__ */
int serv_init(), serv_accept();
void serv_close(), serv_shutdown();
#endif /* __STDC__ */
