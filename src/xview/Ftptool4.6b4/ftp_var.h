
#pragma ident   "@(#)ftp_var.h 1.4     93/05/25"

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

#define	strerror(e) sys_errlist[e]
extern char *sys_errlist[];

#ifdef USE_PROTOTYPES

int ftp_hookup(char *host, short port);
int ftp_login(char *user, char *pass, char *acct);
int command(char *fmt, ...);
int command_dataconn(FILE **a_file, char *lmode, char *fmt, ...);
int getreply(int expecteof);
int empty(fd_set *mask, int sec);
int sendrequest(char *cmd, char *local, char *remote, int size);
int recvrequest(char *cmd, char *local, char *remote,
	char *lmode, int size);
int initconn(void);
FILE *dataconn(char *lmode);
char *gunique(char *local);
void abort_remote(FILE *din);
void lostpeer(void);
FILE *open_remote_ls(int nlst);
char *next_remote_line(FILE *din);
void close_remote_ls(FILE *din);
void settype(int type);
char *parse_hostname(const char *host, int *port);

#else

int ftp_hookup();
int ftp_login();
int command();
int command_dataconn();
int getreply();
int empty();
int sendrequest();
int recvrequest();
int initconn();
FILE *dataconn();
char *gunique();
void abort_remote();
void lostpeer();
FILE *open_remote_ls();
char *next_remote_line();
void close_remote_ls();
void settype();
char *parse_hostname();

#endif
