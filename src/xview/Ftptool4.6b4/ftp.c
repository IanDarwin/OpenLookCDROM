
#pragma ident   "@(#)ftp.c 1.7     93/08/19"

/*
 * Copyright (c) 1985, 1989 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#ifndef lint
static char sccsid[] = "@(#)ftp.c	5.38 (Berkeley) 4/22/91";
#endif /* not lint */

#include "ftptool.h"
#include "ftp_var.h"

#ifdef USE_PROTOTYPES
int ftp_hookup(char *host, short port)
#else
int ftp_hookup(host, port)
char	*host;
short	port;
#endif
{
	register struct hostent *hp = 0;
	int s, len;
	static char hostnamebuf[MAXHOSTNAMELEN + 1];
	char	*ftperr;

	code = 0;
	bzero((char *)&hisctladdr, sizeof (hisctladdr));
	hisctladdr.sin_addr.s_addr = inet_addr(host);
	if (hisctladdr.sin_addr.s_addr != -1) {
		hisctladdr.sin_family = AF_INET;
		(void) strncpy(hostnamebuf, host, sizeof (hostnamebuf));
	} else {
		hp = gethostbyname(host);
		if (hp == NULL) {
			if (try_proxy)
				footer_message("%s unknown. Trying proxy.",
				    host);
			else
				footer_message("%s: unknown host", host);
			code = -1;
			return (1);
		}
		hisctladdr.sin_family = hp->h_addrtype;
		bcopy(hp->h_addr_list[0],
		    (caddr_t)&hisctladdr.sin_addr, hp->h_length);
		(void) strncpy(hostnamebuf, hp->h_name, sizeof (hostnamebuf));
	}
	s = socket(hisctladdr.sin_family, SOCK_STREAM, 0);
	if (s < 0) {
		perror("ftptool: socket");
		code = -1;
		return (0);
	}
	hisctladdr.sin_port = port;
	while (connect(s, (struct sockaddr *)&hisctladdr,
	    sizeof (hisctladdr)) < 0) {
		if (hp && hp->h_addr_list[1]) {
			extern char *inet_ntoa();

			sprintf(scratch, "connect to address %s: %s",
			    inet_ntoa(hisctladdr.sin_addr), strerror(errno));
			log_message(scratch);
			hp->h_addr_list++;
			bcopy(hp->h_addr_list[0],
			    (caddr_t)&hisctladdr.sin_addr, hp->h_length);
			sprintf(scratch, "Trying %s...",
			    inet_ntoa(hisctladdr.sin_addr));
			log_message(scratch);
			footer_message(scratch);
			(void) close(s);
			s = socket(hisctladdr.sin_family, SOCK_STREAM, 0);
			if (s < 0) {
				perror("ftptool: socket");
				code = -1;
				return (0);
			}
			continue;
		}
		code = -1;
		if ((errno == ENETUNREACH) || (errno == EHOSTUNREACH)) {
			if (try_proxy)
				footer_message("%s unreachable. Trying proxy.",
				    host);
			(void) close(s);
			return (1);
		}
		sprintf(scratch, "connect: %s: %s", host, strerror(errno));
		footer_message(scratch);
		goto bad;
	}
	len = sizeof (myctladdr);
	if (getsockname(s, (struct sockaddr *)&myctladdr, &len) < 0) {
		perror("ftptool: getsockname");
		code = -1;
		goto bad;
	}
#ifdef SYSV386
	hp = gethostbyname(myhostname);
	myctladdr.sin_family = hp->h_addrtype;
	bcopy(hp->h_addr_list[0], (caddr_t)&myctladdr.sin_addr, hp->h_length);
#endif
	responsefp = fdopen(s, "r");
	commandfp = fdopen(s, "w");
	if (responsefp == NULL || commandfp == NULL) {
		fprintf(stderr, "ftptool: fdopen failed.\n");
		close_files();
		code = -1;
		goto bad;
	}
	if (verbose) {
		sprintf(scratch, "Connected to %s.\n", hostnamebuf);
		log_message(scratch);
	}
	if (getreply(0) > 2) {	/* read startup message from server */
		close_files();
		code = -1;
		ftperr = ftp_error(' ', "Service not available.");
		footer_message(ftperr);
		goto bad;
	}

	/*
	 * Could get:
	 * Connected to sun-barr.ebay.sun.com.
	 * The Internet FTP relay is down for system maintenance.
	 * Please try again later this weekend.
	 * Sorry for any inconvenience.
	 * Network Operations
	 * 421 Service not available, remote server has closed connection
	 */
	if (!strncmp(response_line, "The Internet", 12)) {
		footer_message("The proxy FTP relay is down. Try again later.");
		close_files();
		code = -1;
		goto bad;
	}
#ifdef SO_OOBINLINE
	{
	int on = 1;

	if (setsockopt(s, SOL_SOCKET, SO_OOBINLINE, (char *)&on,
	    sizeof (on)) < 0 && debug) {
			perror("ftptool: setsockopt");
		}
	}
#endif /* SO_OOBINLINE */

	return (2);
bad:
	(void) close(s);
	return (0);
}

#ifdef USE_PROTOTYPES
int ftp_login(char *user, char *pass, char *acct)
#else
int ftp_login(user, pass, acct)
char	*user;
char	*pass;
char	*acct;
#endif
{
	int n;
	char	*ftperr;
	int	aflag;

	n = command("USER %s", user);
	/*
	 * We may have just consumed some startup messages from a
	 * server that spews them at connection, but we only grabbed
	 * the sun-barr one.
	 */
	if (code == 220) {
		cpend = 1;
		n = getreply(0);
	} else if (code == 0) {
		/* for nic.ddn.mil */
		while (code == 0 || code == 220) {
			cpend = 1;
			n = getreply(0);
		}
	}
	if (code == 500) {
		/* sun-barr.ebay doesn't recognize host */
		/* 500 yavin: unknown host */
		/* 500 connect: connection timed out */
		ftperr = ftp_error(' ', "Connect failed.");
		footer_message(ftperr);
		quit_ftp();
	} else if (code == 530) {
		/* XXX login unknown */
		/* login failed */
		ftperr = ftp_error(' ', "Connect failed. Login unknown.");
		footer_message(ftperr);
		quit_ftp();
	} else if (code == 421) {
		footer_message("Service not available.");
		quit_ftp();
	}

	/* Contact line is in the Sorry line */
	/* 421 Service not available  (for Iftp ) */
	if (!strncmp(response_line, "Sorry", 5)) {
		ftperr = "Connect failed. This host is directly reachable.";
		footer_message(ftperr);
		quit_ftp();
		return (0);
	}
	if (n == CONTINUE) {
		code = 0;
		n = command("PASS %s", pass);
		if (n == ERROR || code == 421) {
			if (code == 421)
				ftperr = &response_line[4];
			else
				ftperr = ftp_error(' ', "Connect failed.");
			footer_message(ftperr);
			quit_ftp();
			return (0);
		}
	}
	aflag = 0;
	if (n == CONTINUE) {
		/* Account needed */
		aflag++;
		if (acct != NULL)
			n = command("ACCT %s", acct);
		else
			n = command("ACCT %s", "anonymous");
	}
	if (n != COMPLETE) {
		return (0);
	}
	if (!aflag && acct != NULL && *acct != '\0')
		(void) command("ACCT %s", acct);
	return (1);
}

#ifdef USE_PROTOTYPES
int command(char *fmt, ...)
#else
/*VARARGS*/
int command(va_alist)
va_dcl
#endif
{
#ifndef USE_PROTOTYPES
	char *fmt;
#endif
	va_list ap;
	int r;

	notify_do_dispatch();
	abrtflag = 0;
	if (commandfp == NULL) {
		/*
		perror ("No control connection for command");
		 */
		code = 421;
		return (0);
	}
#ifdef USE_PROTOTYPES
	va_start(ap, fmt);
#else
	va_start(ap);
	fmt = (char *)va_arg(ap, char *);
#endif
	vfprintf(commandfp, fmt, ap);
	va_end(ap);
	fprintf(commandfp, "\r\n");
	(void) fflush(commandfp);
	cpend = 1;
	r = getreply(!strcmp(fmt, "QUIT"));
	return (r);
}

#ifdef USE_PROTOTYPES
int command_dataconn(FILE **a_file, char *lmode, char *fmt, ...)
#else
/*VARARGS2*/
int command_dataconn(a_file, lmode, va_alist)
FILE	**a_file;
char	*lmode;
va_dcl
#endif
{
#ifndef USE_PROTOTYPES
	char *fmt;
#endif
	va_list ap;
	int r;

	notify_do_dispatch();
	abrtflag = 0;
	if (commandfp == NULL) {
		/*
		perror ("No control connection for command");
		*/
		code = 421;
		return (0);
	}
#ifdef USE_PROTOTYPES
	va_start(ap, fmt);
#else
	va_start(ap);
	fmt = (char *)va_arg(ap, char *);
#endif
	vfprintf(commandfp, fmt, ap);
	va_end(ap);
	fprintf(commandfp, "\r\n");
	(void) fflush(commandfp);
#ifdef SYSV386
	*a_file = dataconn(lmode);
#endif
	cpend = 1;
	r = getreply(!strcmp(fmt, "QUIT"));
#ifndef SYSV386
	if (r == PRELIM)
		*a_file = dataconn(lmode);
#endif
	return (r);
}

#include <ctype.h>

#ifdef USE_PROTOTYPES
int getreply(int expecteof)
#else
int getreply(expecteof)
int expecteof;
#endif
{
	register int c, n;
	register int dig;
	register char *cp;
	int originalcode = 0, continuation = 0;
	int pflag = 0;

	for (;;) {
		dig = n = code = 0;
		cp = response_line;
		notify_do_dispatch();
		while ((c = getc(responsefp)) != '\n') {
			if (c == IAC) {	/* handle telnet commands */
				switch (c = getc(responsefp)) {
				case WILL:
				case WONT:
					c = getc(responsefp);
					fprintf(commandfp,
					    "%c%c%c", IAC, DONT, c);
					(void) fflush(commandfp);
					break;
				case DO:
				case DONT:
					c = getc(responsefp);
					fprintf(commandfp,
					    "%c%c%c", IAC, WONT, c);
					(void) fflush(commandfp);
					break;
				default:
					break;
				}
				continue;
			}
			dig++;
			if (c == EOF) {
				if (expecteof) {
					code = 221;
					notify_no_dispatch();
					return (0);
				}
				lostpeer();
				if (verbose) {
					log_message("421 Service not available, remote server has closed connection\n");
				}
				code = 421;
				notify_no_dispatch();
				return (4);
			}
			if (c != '\r' && (verbose > 0 ||
			    (verbose > -1 && n == '5' && dig > 4))) {
				/*
				(void) putchar(c);
				 */
				log_char(c);
			}
			if (dig < 4 && isdigit(c))
				code = code * 10 + (c - '0');
			if (!pflag && code == 227)
				pflag = 1;
			if (dig > 4 && pflag == 1 && isdigit(c))
				pflag = 2;
/*
			if (pflag == 2) {
				if (c != '\r' && c != ')')
					*pt++ = c;
				else {
					*pt = '\0';
					pflag = 3;
				}
			}
*/
			if (dig == 4 && c == '-') {
				if (continuation)
					code = 0;
				continuation++;
			}
			if (n == 0)
				n = c;
			if (cp < &response_line[sizeof (response_line) - 1])
				*cp++ = c;
		}
		if (verbose > 0 || (verbose > -1 && n == '5')) {
			/*
			(void) putchar(c);
			 */
			log_char(c);
			(void) fflush (stdout);
		}
		if (continuation && code != originalcode) {
			if (originalcode == 0)
				originalcode = code;
			continue;
		}
		*cp = '\0';
		if (n != '1')
			cpend = 0;
		if (code == 421 || originalcode == 421)
			lostpeer();
		notify_no_dispatch();
		return (n - '0');
	}
}

#ifdef USE_PROTOTYPES
int empty(fd_set *mask, int sec)
#else
int empty(mask, sec)
fd_set *mask;
int sec;
#endif
{
	struct timeval t;

	t.tv_sec = (long) sec;
	t.tv_usec = 0;
	return (select(32, mask, (fd_set *)NULL, (fd_set *)NULL, &t));
}

#define	HASHBYTES 1024

#ifdef USE_PROTOTYPES
int sendrequest(char *cmd, char *local, char *remote, int size)
#else
int sendrequest(cmd, local, remote, size)
char	*cmd, *local, *remote;
int		size;
#endif
{
	struct stat st;
	struct timeval start, stop;
	register int c, bytes;
	long d;
	FILE *fin = NULL, *dout = 0, *popen();
	int (*closefunc)(), pclose(), fclose();
	char *lmode, buf[BUFSIZ], *bufp;
	char	*ftperr;
	int		errormsg = 0;
	int	gettimeofday();

	closefunc = NULL;
	lmode = "w";
	update_status_label("Sending", remote, size);
	fin = fopen(local, "r");
	if (fin == NULL) {
		local_footer_message("Open: %s: %s.", local, strerror(errno));
		code = -1;
		return (0);
	}
	closefunc = fclose;
	if (fstat(fileno(fin), &st) < 0 ||
		(st.st_mode&S_IFMT) != S_IFREG) {
		local_footer_message("%s: not a plain file.", local);
		fclose(fin);
		fin = NULL;
		code = -1;
		return (1);
	}
	if (initconn()) {
		code = -1;
		if (closefunc != NULL)
			(*closefunc)(fin);
		return (0);
	}
	if (remote) {
#ifdef SYSV386
		if (command_dataconn(&dout, lmode, "%s %s", cmd, remote)
		    != PRELIM) {
#else
		if (command("%s %s", cmd, remote) != PRELIM) {
#endif
			if (closefunc != NULL)
				(*closefunc)(fin);
			/* Permission denied */

			sprintf(scratch, "Put %s failed.", remote);
			ftperr = ftp_error(' ', scratch);
			local_footer_message(ftperr);

			return (1);
		}
	} else
#ifdef SYSV386
		if (command_dataconn(&dout, lmode, "%s", cmd) != PRELIM) {
#else
		if (command("%s", cmd) != PRELIM) {
#endif
			if (closefunc != NULL)
				(*closefunc)(fin);
			fprintf(stderr, "command != PRELIM\n");
			return (1);
		}
#ifndef SYSV386
	dout = dataconn(lmode);
#endif
	if (dout == NULL)
		goto abort;
	(void) gettimeofday(&start, (struct timezone *)0);
	switch (curtype) {

	case TYPE_I:
	case TYPE_L:
		errno = d = 0;
		notify_do_dispatch();
		while ((c = read(fileno(fin), buf, sizeof (buf))) > 0) {
			if (abort_transfer) {
				notify_no_dispatch();
				goto abort;
			}
			for (bufp = buf; c > 0; c -= d, bufp += d)
				if ((d = write(fileno(dout), bufp, c)) <= 0) {
					break;
				} else {
					/* change image */
					update_status_gauge(d);
				}
			if (abort_transfer) {
				notify_no_dispatch();
				goto abort;
			}
		}
		notify_no_dispatch();
		if (c < 0) {
			footer_message("Read %s: %s.", local, strerror(errno));
			errormsg = 1;
		}
		if (d < 0) {
			local_footer_message(
			    "Write failed (remote file system full?).");
			errormsg = 1;
			goto abort;
		}
		break;

	case TYPE_A:
		notify_do_dispatch();
		bytes = 0;
		while ((c = getc(fin)) != EOF) {
			if (abort_transfer) {
				notify_no_dispatch();
				goto abort;
			}
			if (c == '\n') {
				if (ferror(dout))
					break;
				(void) putc('\r', dout);
				bytes++;
			}
			(void) putc(c, dout);
			bytes++;
			if (bytes >= 1024) {
				/* change image */
				update_status_gauge(bytes);
				bytes = 0;
			}
			if (abort_transfer) {
				notify_no_dispatch();
				goto abort;
			}
		}
		notify_no_dispatch();
		if (ferror(fin)) {
			errormsg = 1;
			local_footer_message("%s: %s.", local, strerror(errno));
		}
		if (ferror(dout)) {
			if (errno != EPIPE)
				perror("netout");
		}
		break;
	}
	(void) gettimeofday(&stop, (struct timezone *)0);
	if (closefunc != NULL)
		(*closefunc)(fin);
	(void) fclose(dout);
	dout = NULL;
	(void) getreply(0);
	if (!errormsg)
		local_footer_message("Send of %s complete.", remote);
	return (0);
abort:
	(void) gettimeofday(&stop, (struct timezone *)0);
	if (!cpend) {
		code = -1;
		return (0);
	}
	if (data >= 0) {
		(void) close(data);
		data = -1;
	}
	if (dout) {
		(void) fclose(dout);
		dout = NULL;
	}
	(void) getreply(0);
	code = -1;
	if (closefunc != NULL && fin != NULL)
		(*closefunc)(fin);
	if (!errormsg)
		local_footer_message("Send of %s aborted.", remote);
	return (2);
}

#ifdef USE_PROTOTYPES
int recvrequest(char *cmd, char *local, char *remote,
	char *lmode, int size)
#else
int recvrequest(cmd, local, remote, lmode, size)
char	*cmd, *local, *remote, *lmode;
int		size;
#endif
{
	FILE *fout = NULL, *din = 0, *popen();
	int (*closefunc)(), pclose(), fclose();
	int is_retr, tcrflag, bare_lfs = 0;
	register int bytes;
	char *gunique();
	static int bufsize;
	static char *buf;
	register int c;
	long d;
	struct timeval start, stop;
	struct stat st;
	off_t lseek();
	char	*ftperr;
	int		errormsg = 0;
	int	gettimeofday();

	update_status_label("Receiving", remote, size);
	is_retr = strcmp(cmd, "RETR") == 0;
	closefunc = NULL;
	tcrflag = !crflag && is_retr;
	if (strcmp(local, "-") && *local != '|') {
		if (access(local, 2) < 0) {
			char *dir = rindex(local, '/');

			if (errno != ENOENT && errno != EACCES) {
				footer_message("Access: %s: %s.", local,
				    strerror(errno));
				code = -1;
				return (0);
			}
			if (dir != NULL)
				*dir = 0;
			d = access(dir ? local : ".", 2);
			if (dir != NULL)
				*dir = '/';
			if (d < 0) {
				footer_message("Access: %s: %s.", local,
				    strerror(errno));
				code = -1;
				return (0);
			}
			if (!unique_local_names && errno == EACCES &&
			    chmod(local, 0600) < 0) {
				footer_message("Chmod: %s: %s.", local,
				    strerror(errno));
				code = -1;
				return (1);
			}
			if (unique_local_names && errno == EACCES &&
			    (local = gunique(local)) == NULL) {
				code = -1;
				return (1);
			}
		} else if (unique_local_names &&
		    (local = gunique(local)) == NULL) {
			code = -1;
			return (1);
		}
	}
	if (initconn()) {
		code = -1;
		return (0);
	}
	if (remote) {
#ifdef SYSV386
		if (command_dataconn(&din, "r", "%s %s", cmd, remote)
		    != PRELIM){
#else
		if (command("%s %s", cmd, remote) != PRELIM) {
#endif
			/* Not a plain file */
			/* Permission denied */
			/* No such file or directory */

			sprintf(scratch, "Get %s failed.", remote);
			ftperr = ftp_error(' ', scratch);
			footer_message(ftperr);
			return (1);
		}
	} else {
#ifdef SYSV386
		if (command_dataconn(&din, "r", "%s", cmd) != PRELIM) {
#else
		if (command("%s", cmd) != PRELIM) {
#endif
			footer_message("command != PRELIM");
			/*
			fprintf(stderr, "command != PRELIM\n");
			 */
			return (1);
		}
	}
#ifndef SYSV386
	din = dataconn("r");
#endif
	if (din == NULL)
		goto abort;
	if (strcmp(local, "-") == 0)
		fout = stdout;
	else if (*local == '|') {
		fout = popen(local + 1, "w");
		if (fout == NULL) {
			perror(local+1);
			goto abort;
		}
		closefunc = pclose;
	} else {
		fout = fopen(local, lmode);
		if (fout == NULL) {
			footer_message("Open: %s: %s.", local, strerror(errno));
			errormsg = 1;
			goto abort;
		}
		closefunc = fclose;
	}
	if (fstat(fileno(fout), &st) < 0 || st.st_blksize == 0)
		st.st_blksize = BUFSIZ;
	if (st.st_blksize > bufsize) {
		if (buf)
			(void) free(buf);
		buf = malloc((unsigned)st.st_blksize);
		if (buf == NULL) {
			perror("malloc");
			bufsize = 0;
			goto abort;
		}
		bufsize = st.st_blksize;
	}
	(void) gettimeofday(&start, (struct timezone *)0);
	switch (curtype) {

	case TYPE_I:
	case TYPE_L:
		errno = d = 0;
		notify_do_dispatch();
		while ((c = read(fileno(din), buf, bufsize)) > 0) {
			if (abort_transfer) {
				notify_no_dispatch();
				goto abort;
			}
			if ((d = write(fileno(fout), buf, c)) <= 0)
				break;
			/* change image */
			update_status_gauge(d);
			if (d != c)
				break;
			if (abort_transfer) {
				notify_no_dispatch();
				goto abort;
			}
		}
		notify_no_dispatch();
		if (c < 0) {
			if (errno != EPIPE)
				perror("netin");
		}
		if (d < c) {
			errormsg = 1;
			if (d < 0) {
				footer_message("Write failed: %s",
				    strerror(errno));
				goto abort;
			} else {
				footer_message("Short write: %s",
				    strerror(errno));
			}
		}
		break;

	case TYPE_A:
		notify_do_dispatch();
		bytes = 0;
		while ((c = getc(din)) != EOF) {
			if (abort_transfer) {
				notify_no_dispatch();
				goto abort;
			}
			if (c == '\n')
				bare_lfs++;
			while (c == '\r') {
				bytes++;
				if ((c = getc(din)) != '\n' || tcrflag) {
					if (ferror(fout))
						goto break2;
					(void) putc('\r', fout);
				}
			}
			(void) putc(c, fout);
			bytes++;
			if (bytes >= 1024) {
				/* change image */
				update_status_gauge(bytes);
				bytes = 0;
			}
			if (abort_transfer) {
				notify_no_dispatch();
				goto abort;
			}
	contin2:	/* null */;
		}
break2:
		notify_no_dispatch();
		if (bare_lfs) {
			fprintf(stderr,
			    "WARNING! %d bare linefeeds received in ASCII mode\n",
			    bare_lfs);
			fprintf(stderr,
			    "File may not have transferred correctly.\n");
		}
		if (ferror(din)) {
			if (errno != EPIPE)
				perror("netin");
		}
		if (ferror(fout)) {
			errormsg = 1;
			footer_message("%s: %s.", local, strerror(errno));
		}
		break;
	}
	if (closefunc != NULL)
		(*closefunc)(fout);
	(void) gettimeofday(&stop, (struct timezone *)0);
	(void) fclose(din);
	din = NULL;
	(void) getreply(0);
	if (!errormsg)
		footer_message("Receive of %s complete.", remote);
	return (0);
abort:

	/* abort using RFC959 recommended IP,SYNC sequence  */

	(void) gettimeofday(&stop, (struct timezone *)0);
	if (!cpend) {
		code = -1;
		return (0);
	}

	abort_remote(din);
	code = -1;
	if (data >= 0) {
		(void) close(data);
		data = -1;
	}
	if (closefunc != NULL && fout != NULL)
		(*closefunc)(fout);
	if (din) {
		(void) fclose(din);
		din = NULL;
	}
	if (!errormsg)
		footer_message("Receive of %s aborted.", remote);
	return (2);
}

/*
 * Need to start a listen on the data channel before we send the command,
 * otherwise the server's connect may fail.
 */
#ifdef USE_PROTOTYPES
int initconn(void)
#else
int initconn()
#endif
{
	register char *p, *a;
	int result, len, tmpno = 0;
	int on = 1;
#ifdef SYSV386
	ushort	data_port;
#endif

noport:
	data_addr = myctladdr;
	if (sendport)
		data_addr.sin_port = 0;	/* let system pick one */
	if (data != -1) {
		(void) close(data);
		data = -1;
	}
	data = socket(AF_INET, SOCK_STREAM, 0);
	if (data < 0) {
		perror("ftptool: socket");
		if (tmpno)
			sendport = 1;
		return (1);
	}
	if (!sendport)
		if (setsockopt(data, SOL_SOCKET, SO_REUSEADDR, (char *)&on,
		    sizeof (on)) < 0) {
			perror("ftptool: setsockopt (reuse address)");
			goto bad;
		}
	if (bind(data, (struct sockaddr *)&data_addr, sizeof (data_addr)) < 0) {
		perror("ftptool: bind");
		goto bad;
	}
	len = sizeof (data_addr);
	if (getsockname(data, (struct sockaddr *)&data_addr, &len) < 0) {
		perror("ftptool: getsockname");
		goto bad;
	}
	if (listen(data, 1) < 0)
		perror("ftptool: listen");
	if (sendport) {
		a = (char *)&data_addr.sin_addr;
#ifdef SYSV386
	data_port = htons(data_addr.sin_port);
	p = (char *)&data_port;
	p[0] += p[1];		/* Switches variables, without a temp var */
	p[1] = p[0] - p[1];
	p[0] -= p[1];
#else
		p = (char *)&data_addr.sin_port;
#endif
#define	UC(b)	(((int)b)&0xff)
		result = command("PORT %d,%d,%d,%d,%d,%d",
		    UC(a[0]), UC(a[1]), UC(a[2]), UC(a[3]),
		    UC(p[0]), UC(p[1]));
		if (result == ERROR && sendport == -1) {
			sendport = 0;
			tmpno = 1;
			goto noport;
		}
		return (result != COMPLETE);
	}
	if (tmpno)
		sendport = 1;
	return (0);
bad:
	(void) close(data), data = -1;
	if (tmpno)
		sendport = 1;
	return (1);
}

#ifdef USE_PROTOTYPES
FILE *dataconn(char *lmode)
#else
FILE *dataconn(lmode)
char *lmode;
#endif
{
	struct sockaddr_in from;
	int s, fromlen = sizeof (from);

restart:
	s = accept(data, (struct sockaddr *) &from, &fromlen);
	if (s < 0) {
		if (errno == EINTR)
			goto restart;
		perror("ftptool: accept");
		(void) close(data);
		data = -1;
		return (NULL);
	}
	(void) close(data);
	data = s;
	return (fdopen(data, lmode));
}

#ifdef USE_PROTOTYPES
char *gunique(char *local)
#else
char *gunique(local)
char	*local;
#endif
{
	static char new[MAXPATHLEN + 1];
	static char first[MAXPATHLEN + 1], last[MAXNAMLEN + 1];
	char *slash;
	int d, count = 0;
	extern char *newname; /* from view_remote_file */

	if ((strlen(local) + 3) > (size_t)MAXPATHLEN) {
		sprintf(scratch, "Unique name for %s too long.", local);
		footer_message(scratch);
		log_message(scratch);
		log_char('\n');
		return (NULL);
	}
	slash = rindex(local, '/');
	if (slash) {
		*slash = '\0';
		strcpy(first, local);
		strcpy(last, slash + 1);
		*slash = '/';
	} else {
		*first = '\0';
		strcpy(last, local);
	}
	d = 0;
	while (!d) {
		if (++count == 100) {
			sprintf(scratch,
			    "Cannot find unique name for %s.", local);
			footer_message(scratch);
			log_message(scratch);
			log_char('\n');
			return (NULL);
		}
		if (slash)
			sprintf(new, "%s/%02d.%s", first, count, last);
		else
			sprintf(new, "%02d.%s", count, last);
		if ((d = access(new, 0)) < 0)
			break;
	}

	newname = new;
	footer_message("Unique name %s generated.", new);
	return (new);
}

#ifdef USE_PROTOTYPES
void abort_remote(FILE *din)
#else
void abort_remote(din)
FILE *din;
#endif
{
	char buf[BUFSIZ];
	int nfnd;
	fd_set mask;
	int	rval;

	/*
	 * send IAC in urgent mode instead of DM because 4.3BSD places oob mark
	 * after urgent byte rather than before as is protocol now
	 */
	sprintf(buf, "%c%c%c", IAC, IP, IAC);
restart:
	rval = send(fileno(commandfp), buf, 3, MSG_OOB);
	if (rval == -1 && errno == EINTR)
		goto restart;
	if (rval != 3)
		perror("abort_remote1");
	fprintf(commandfp, "%cABOR\r\n", DM);
	(void) fflush(commandfp);
	FD_ZERO(&mask);
	FD_SET(fileno(responsefp), &mask);
	if (din) {
		FD_SET(fileno(din), &mask);
	}
	if ((nfnd = empty(&mask, 10)) <= 0) {
		if (nfnd < 0) {
			perror("abort_remote2");
		}
		/*
		if (ptabflg)
			code = -1;
		*/
		lostpeer();
	}
	if (din && FD_ISSET(fileno(din), &mask)) {
		while (read(fileno(din), buf, BUFSIZ) > 0)
			/* LOOP */;
	}
	if (getreply(0) == ERROR && code == 552) {
		/* 552 needed for nic style abort */
		(void) getreply(0);
	}
	(void) getreply(0);
}

#ifdef USE_PROTOTYPES
void lostpeer(void)
#else
void lostpeer()
#endif
{

	if (connected) {
		if (commandfp != NULL) {
			(void) shutdown(fileno(commandfp), 1+1);
			(void) fclose(commandfp);
			commandfp = NULL;
		}
		if (data >= 0) {
			(void) shutdown(data, 1+1);
			(void) close(data);
			data = -1;
		}
		connected = 0;
	}
}

#ifdef USE_PROTOTYPES
FILE *open_remote_ls(int nlst)
#else
FILE *open_remote_ls(nlst)
int	nlst;
#endif
{
	char	*ftperr;
	char	*cmd;
	FILE *din = 0;
	char *gunique();
	off_t lseek();

	if (nlst)
		cmd = "NLST"; /* dir */
	else
		cmd = "LIST"; /* ls */

	settype(ASCII);
	if (initconn()) {
		code = -1;
		return (NULL);
	}
#ifdef SYSV386
	if (command_dataconn(&din, "r", "%s", cmd) != PRELIM) {
#else
	if (command("%s", cmd) != PRELIM) {
#endif
		if (code == 530) {
			/* 530 You must define working directory with CWD */
			ftperr = ftp_error(' ',
			    "cd somewhere first or invalid directory");
			footer_message(ftperr);
		} else if (code == 550) {
			/* 550 No files found. */
			ftperr = ftp_error(' ', "No files found.");
			footer_message(ftperr);
		} else {
			footer_message("Unknown error %d.", code);
		}
		return (NULL);
	}
#ifndef SYSV386
	din = dataconn("r");
#endif
	if (din == NULL)
		return (NULL);
	return (din);
}

#ifdef USE_PROTOTYPES
char *next_remote_line(FILE *din)
#else
char *next_remote_line(din)
FILE	*din;
#endif
{
	char	*str = response_line;
	char	*cptr = str;
	int		c;

	notify_do_dispatch();
	while ((c = getc(din)) != '\n' && c != EOF && c != '\0') {
		if (c == '\r')
			continue;
		*cptr++ = (char)c;
	}
	*cptr = '\0';
	notify_no_dispatch();
	if (c == EOF)
		return (NULL);
	return (str);
}

#ifdef USE_PROTOTYPES
void close_remote_ls(FILE *din)
#else
void close_remote_ls(din)
FILE	*din;
#endif
{
	if (ferror(din))
		perror("netin");
	(void) fclose(din);
	(void) getreply(0);
	return;
}

struct	types {
	char	*t_name;
	char	*t_mode;
	int	t_type;
	char	*t_arg;
} types[] = {
	{ "binary",	"I",	TYPE_I,	0 },
	{ "ascii",	"A",	TYPE_A,	0 },
	{ "tenex",	"L",	TYPE_L,	"8" },
/*
	{ "image",	"I",	TYPE_I,	0 },
	{ "ebcdic",	"E",	TYPE_E,	0 },
*/
};

/*
 * Set transfer type.
 */
#ifdef USE_PROTOTYPES
void settype(int type)
#else
void settype(type)
int	type;
#endif
{
	register struct types *p;
	int comret;

	if (type > (sizeof (types)/sizeof (types[0]))) {
		fprintf(stderr, "%d: unknown mode\n", type);
		code = -1;
		return;
	}
	/* make sure values in window match table! */
	p = &types[type];

	if ((p->t_arg != NULL) && (*(p->t_arg) != '\0'))
		comret = command ("TYPE %s %s", p->t_mode, p->t_arg);
	else
		comret = command("TYPE %s", p->t_mode);
	if (comret == COMPLETE) {
		curtype = p->t_type;
	}
}

#ifdef USE_PROTOTYPES
char *parse_hostname(const char *host, int *port)
#else
char *parse_hostname(host, port)
char *host;
int	*port;
#endif
{
	static char ftphost[MAXHOSTNAMELEN + 1];
	char *tmp;
	struct servent *servent;

	/* strip leading whitespace */
	while (*host != '\0' && isspace(*host))
		host++;

	if (*host == '\0')
		return (NULL);

	(void) strcpy(ftphost, host);

	tmp = strtok(ftphost, " \t");
	/* now ftphost is terminated. */

	tmp = strtok(NULL, " \t");
	if (tmp == NULL) /* no port */
		return (ftphost);

	if (isdigit(*tmp)) {
		*port = htons(atoi(tmp));
	} else {
		servent = getservbyname(tmp, "tcp");
		if (servent == NULL) {
			sprintf(scratch, "%s service unknown. Using %d.",
				tmp, *port);
			footer_message(scratch);
			log_message(scratch);
		} else {
			*port = servent->s_port;
		}
	}
	return (ftphost);
}
