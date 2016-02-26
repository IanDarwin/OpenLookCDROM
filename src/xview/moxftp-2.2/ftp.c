/*
 * Copyright (c) 1992 The Regents of the University of Texas System.
 * Copyright (c) 1993 The Regents of the University of Texas System.
 * Copyright (c) 1994 The Regents of the University of Texas System.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted provided
 * that the above copyright notice and this paragraph are duplicated in all
 * such forms and that any documentation, advertising materials,  and other
 * materials  related to such  distribution  and use  acknowledge  that the
 * software  was  developed  by the  University of Texas.  The  name of the
 * University may not be  used to endorse or promote  products derived from
 * this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */


#ifndef lint
static char copyright[] =
"@(#) Copyright (c) 1993 The Regents of the University of Texas System.\n\
 All rights reserved.\n";
static char rcsid[] =
"$Id: ftp.c,v 1.2 1994/05/26 22:03:35 jones Exp $\n";
static char rcspath[] =
"$Source: /mintaka/u0/xx/ss/jones/jones.old/mftp.src/moxftp-2.2/RCS/ftp.c,v $\n";
#endif

/* $Log: ftp.c,v $
 * Revision 1.2  1994/05/26  22:03:35  jones
 * More meaning full connection close message.
 *
 * Revision 1.1  1994/03/14  18:55:53  jones
 * Initial revision
 *
 */

#include "machine.h"
#include "defs.h"
#include "proto/ftp.h"

int  LAST_RESPONSE = 0;
char last_response[1000];
struct _netrc netrc[MAX_NETRC];
int NUM_NETRC = 0;

/*
 * See rfc959.
 */
struct _ftp_response ftp_response[] = {
   { ""   ,  00004, "No ftp response"},   	/* 0 Error abort */
   { ""   ,  00001, "Dummy"},			/* 1 No error    */
   { ""   ,  00401, "Dummy 2"},			/* 2 No error Error OK */
   { "777",  01004, "Error restart"},		/* 3 Error restart */
   { "776",  02004, "Error reconnect"},		/* 4 Error reconnect */
   { "110",  00002, "Restart marker reply."},
   { "125",  00002, "REPLY_PERLIMINARY"},
   { "120",  00002, "Service ready in nnn minutes."},
   { "125",  00002, "Data connection already open; transfer starting."},
   { "150",  00002, "File status okay; about to open data connection."},
   { "200",  00001, "Command okay."},
   { "202",  00001, "Command not implemented, superfluous at this site."},
   { "211",  00001, "System status, or system help reply."},
   { "212",  00001, "Directory status."},
   { "213",  00001, "File status."},
   { "214",  00001, "Help message."},
   { "215",  00001, "NAME system type."},
   { "220",  00001, "Service ready for new user."},
   { "221",  00001, "Service closing control connection."},
   { "225",  00001, "Data connection open; no transfer in progress."},
   { "226",  00001, "Closing data connection."},
   { "227",  00001, "Entering Passive Mode (h1,h2,h3,h4,p1,p2)."},
   { "230",  00001, "User logged in, proceed."},
   { "250",  00001, "Requested file action okay, completed."},
   { "251",  00001, "PATHNAME created."}, /* suppect but some do return it */
   { "257",  00001, "PATHNAME created."},
   { "331",  00002, "User name okay, need password."},
   { "332",  00022, "Need account for login."},
   { "350",  00002, "Requested file action pending further information."},
   { "421",  00014, "Service not available, closing control connection."},
   { "425",  01044, "Can't open data connection."},
   { "426",  00004, "Connection closed; transfer aborted."},
   { "450",  00004, "Requested file action not taken."},
   { "451",  00004, "Requested action aborted: local error in processing."},
   { "452",  00004, "Requested action not taken."},
   { "500",  00004, "Syntax error, command unrecognized."},
   { "501",  00004, "Syntax error in parameters or arguments."},
   { "502",  00004, "Command not implemented."},
   { "503",  00004, "Bad sequence of commands."},
   { "504",  00004, "Command not implemented for that parameter."},
   { "530",  00014, "Not logged in."},
   { "532",  00024, "Need account for storing files."},
   { "550",  00004, "Requested action not taken."},
   { "551",  00004, "Requested action aborted: page type unknown."},
   { "552",  00004, "Requested file action aborted."},
   { "553",  00004, "Requested action not taken."}
};

#define	NUM_RESPONSE (sizeof(ftp_response)/sizeof(struct _ftp_response))

#define DEFAULT    0
#define LOGIN      1
#define PASSWD     2
#define ACCOUNT    3
#define MACH       4
#define REMOTE_DIR 5
#define LOCAL_DIR  6
#define NOTE       7

static struct toktab {
        char *tokstr;
        int value;
} toktab[]= {
        "default",      DEFAULT,
        "login",        LOGIN,
        "password",     PASSWD,
        "passwd",       PASSWD,
        "account",      ACCOUNT,
        "machine",      MACH,
        "remote_dir",   REMOTE_DIR,
        "local_dir",    LOCAL_DIR,
        "note",         NOTE,
        0,              0
};

#define MAX_T 100
static int current = 0;
static int max = 0;
static char *token[MAX_T];

void
Devine_Last_Response(output, msg)
struct _output *output;
char *msg;
{
    struct _output *next = output;
    int i;

    while(next) {
	for (i=0; i<next->lines; i++) {
	    char *cp = next->output[i];

	    if (bsdstrstr(cp, "too many anonymous users") !=  NULL) {
		strcpy(last_response, "Too many anonymous users");
		return;
	    }
	}
	next = next->next;
    }
    strcpy(last_response, msg);
}

void
Set_Note(line)
char *line;
{
    char host[1000], note[1000];
    int n,i;

    n = sscanf(line, "%s %[^$]", host, note);
    if (n != 2) return;
    for (i=0; i< NUM_NETRC; i++) {
	if (strcmp(host, netrc[i].host) == 0) {
	    netrc[i].note = XtNewString(note);
	    return;
	}
    }
}

int  
ftp_status(output)
struct _output *output;
{
    int i,j;
    int last_good = 0;
    struct _output *next = output;
    char *s;
  
    if (!output) return 0;

    last_response[0] = '\0';
    while(next) {
	for (j=0; j<next->lines; j++) {
	    char *cp = next->output[j];
	    if (bsdstrstr(cp, "bytes received") !=  NULL) continue;
	    if (bsdstrstr(cp, "bytes sent") !=  NULL) continue;
	    for (i=0; i<  NUM_RESPONSE; i++) {
            	if (strncmp(cp, ftp_response[i].code, 3) == 0) break;
	    }
	    if (i != NUM_RESPONSE) {
		strcpy(last_response, next->output[j]);
		s = INDEX(last_response, '\n');
		if (s) *s = '\0';
		if (Debug>3)
		   fprintf(stderr,"xxxx last_response;%s\n",last_response);
		LAST_RESPONSE = i;
	        if(ftp_response[i].status & FTP_ERROR) {
		    if (ftp_response[i].status & FTP_NEED_LOGIN) {
			Set_Status("Connection closed by peer");
			Devine_Last_Response(output,
					     "Connection closed by peer");
			Set_Reconnect(0);
			return i;
		    }
		    if (ftp_response[i].status & FTP_NEED_RESTART) {
			Set_Reconnect(1);
		    }
		    return LAST_RESPONSE;
		}
		last_good = i;
	    }
	}
	next = next->next;
    }
    LAST_RESPONSE = last_good;
    return last_good;
}

static char *
get_note()
{
    char *line;
    
    if (max && current < max) {
	line = concat(NULL, token[current]);
	current++;
	while(current < max) {
	    line = concat(line, " ");
	    line = concat(line, token[current]);
	    current++;
	}
    } else {
	line = concat(NULL, " ");
    }
    return line;
}

static char *
next(file)
FILE *file;
{
    char line[2000];
    char *cp;
    char *next_t;
    int  i;
	

agein:
    if (max && current < max) {
        current++;
	return token[current-1];
    }
    if (max) {
	for (i=0; i < max; i++) {
	    XtFree(token[i]);
	}
    }
    current = 0;
    max = 0;
    if (fgets(line, sizeof(line), file) == NULL) return NULL;
    cp = line;
    next_t = XtMalloc(strlen(cp)+1);
    while(cp = Next_Token(cp, next_t)) {
	token[max] = next_t;
	max++;
	if (max ==  MAX_T) break;
        next_t = XtMalloc(strlen(cp)+1);
    }
    XtFree(next_t);
    goto agein;
}

void
read_netrc(input, name, host, type)
FILE *input;
char *name;
char *host;
int  type;
{
    char *t;
    char *next();
    char *get_note();
    char *cp;
    int  n,i;

    while(t = next(input)) {
        if (NUM_NETRC == MAX_NETRC) break;
	for (i=0; toktab[i].tokstr; i++) {
	     if(strcmp(toktab[i].tokstr, t) == 0) break;
	}
        if (!toktab[i].tokstr) continue;
	/*
	 * Ignore  REMOTE_DIR, LOCAL_DIR, and NOTE in .netrc.
         */
	if (!type && toktab[i].value >= REMOTE_DIR) continue;
	switch(toktab[i].value) {
	    case DEFAULT:
		break;
	    case LOGIN:
		t = next(input);
		if (!t) break;
	        netrc[NUM_NETRC].user = XtNewString(t);
		break;
	    case MACH:
		t = next(input);
		if (!t) break;
		if (netrc[NUM_NETRC].host) {
		    NUM_NETRC++;
                    if (NUM_NETRC == MAX_NETRC) break;
		}
	        netrc[NUM_NETRC].host = XtNewString(t);
		break;
	    case PASSWD:
		t = next(input);
		if (!t) break;
		if (!name || !bsdstrstr(t,"USER")) {
	        	netrc[NUM_NETRC].pass = XtNewString(t);
		} else {
			char *cp = NULL;
			cp = concat(cp, name);
			cp = concat(cp, "@");
			cp = concat(cp, host);
	        	netrc[NUM_NETRC].pass = cp;
		}
		break;
	    case REMOTE_DIR:
		t = next(input);
		if (!t) break;
	        netrc[NUM_NETRC].remote_dir = XtNewString(t);
		break;
	    case LOCAL_DIR:
		t = next(input);
		if (!t) break;
	        netrc[NUM_NETRC].local_dir = XtNewString(t);
		break;
	    case NOTE:
		t = get_note();
		if (!t) break;
	        netrc[NUM_NETRC].note = XtNewString(t);
		XtFree(t);
		break;
	    case ACCOUNT:
		break;
	}
    }
    while(t = next(input));
    if (NUM_NETRC < MAX_NETRC && netrc[NUM_NETRC].host) NUM_NETRC++;
}

void
response(text)
char *text;
{
   char *err;
   char *cp;

   if (last_response[0]) {
	while ((cp = INDEX(last_response,'\n')) != NULL) *cp = '\0';
	err = concat(NULL, text);
   	err = concat(err , "- Last Response is:");
   	err = concat(err , last_response);
   	Set_Status(err);
   	XtFree(err);
   } else {
	Set_Status(text) ;
   }
}
