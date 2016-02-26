/*
 * Copyright (c) 1992 The Regents of the University of Texas System.
 * Copyright (c) 1993 The Regents of the University of Texas System.
 * Copyright (c) 1994 The Regents of the University of Texas System.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted provided
 * that the above copyright notice and this paragraph are duplicated in all
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
"$Id: initialize.c,v 1.1 1994/03/14 18:55:53 jones Exp $\n";
static char rcspath[] =
"$Source: /mintaka/u0/xx/ss/jones/jones.old/mftp.src/moxftp-2.0/RCS/initialize.c,v $\n";
#endif

/* $Log: initialize.c,v $
 * Revision 1.1  1994/03/14  18:55:53  jones
 * Initial revision
 *
 */

#include "machine.h"
#include <sys/param.h>
#include <sys/types.h>
#if defined(MAXPATHLEN)&&!defined(PATH_MAX)
#define PATH_MAX MAXPATHLEN
#endif
#include "defs.h"
#include "proto/initialize.h"

#include <sys/socket.h>
#include <netdb.h>
#include <unistd.h>
#include <pwd.h>


char *localhost = NULL;
char *local_host_name  = NULL;
char *username  = NULL;
char *default_local_dir = NULL;
extern int NUM_NETRC;

void
Initialize()
{
    char name[1000];
    struct hostent *hp;
    char pathname[PATH_MAX];
    char line[2000];
    FILE *input = NULL;
    struct passwd *pw;
    char *temp = NULL;


#if defined(XXX)
#if !defined(CRAY)
    temp = XtMalloc(500000);
    XtFree(temp);
#endif
#endif

    gethostname(name, sizeof(name));

#if defined(XOWFILESEARCHPATH)
    if (!getenv("XFILESEARCHPATH")) {
	setenv("XFILESEARCHPATH", XOWFILESEARCHPATH);
    }
#endif

#if defined(OPENWINDOWHOME)
    if (!getenv("OPENWINHOME")) {
	setenv("OPENWINHOME", OPENWINDOWHOME);
    }
#endif

#if defined(DEBUG)
    if (access(APP_RES, R_OK) != 0) setenv("XAPPLRESDIR",".");
#endif

    hp = gethostbyname(name);
    if (hp) { 
        local_host_name = XtNewString(hp->h_name);
    } else {
	local_host_name = XtNewString(name);
    }
    localhost = "\n\n\n\n";
#if defined(USE_GETWD)
    local_dir = concat(NULL, getwd(pathname));
#else
    local_dir = concat(NULL, getcwd(pathname, sizeof(pathname)));
#endif
    if (local_dir[strlen(local_dir) - 1] != '/') {
	local_dir = concat(local_dir, "/");
    }
    default_local_dir = XtNewString(local_dir);

    pw =  getpwuid(getuid());
    if (pw) {
	username = XtNewString(pw->pw_name);
        sprintf(line,"%s/%s", pw->pw_dir, ".moxftprc");
        input = fopen(line, "r");
        if (input) {
	    read_netrc(input, NULL, NULL, 1);
	    fclose(input);
	} else { 
            sprintf(line,"%s/%s", pw->pw_dir, ".netrc");
            input = fopen(line, "r");
	    if (input) {
	        read_netrc(input, NULL, NULL, 0);
	        fclose(input);
	    }
#if defined (NET_RC)
	    else {
                input = fopen(NET_RC, "r");
	        if (input) {
	            read_netrc(input, pw->pw_name, local_host_name, 1);
	    	    fclose(input);
	        }
	    }
#endif
	}
    }

    if (!NUM_NETRC) {
	if (pw) {
	    char *cp = NULL;

            cp = concat(cp, pw->pw_name);
            cp = concat(cp, "@");
            cp = concat(cp, local_host_name);
            netrc[NUM_NETRC].pass = cp;
	} else {
            netrc[NUM_NETRC].pass = XtNewString("guest");
	}
	netrc[NUM_NETRC].user = XtNewString("anonymous");
	netrc[NUM_NETRC].host = XtNewString("ftp.chpc.utexas.edu");
	netrc[NUM_NETRC].note = XtNewString("Home of xftp/mftp/oftp");
	NUM_NETRC++;
    }

    Init_Dotxftp();
    if(!NUM_NETRC) return;
    qsort((char *)netrc, NUM_NETRC, sizeof(struct _netrc), sort_by_name);
}

static int
sort_by_name(a, b)
char *a;
char *b;
{
     struct _netrc *x = (struct _netrc *)a;
     struct _netrc *y = (struct _netrc *)b;
     int value = 0;

     value = strcmp(x->host, y->host);
     return value;
}
