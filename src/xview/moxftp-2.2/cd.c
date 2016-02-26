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
"$Id: cd.c,v 1.1 1994/03/14 18:55:53 jones Exp $\n";
static char rcspath[] =
"$Source: /mintaka/u0/xx/ss/jones/jones.old/mftp.src/mftpnew/RCS/cd.c,v $\n";
#endif

/* $Log: cd.c,v $
 * Revision 1.1  1994/03/14  18:55:53  jones
 * Initial revision
 *
 */

#include "defs.h"
#include "proto/cd.h"

char *last_cd;
struct _dirs *previous_dir = NULL;


static void 
finish_cd(output, tag, data)
struct _output *output;
int  tag;
DATA data;
{
    int code;

    code =  ftp_status(output);

    if (ftp_response[code].status & FTP_ERROR) {
	response("cd failed");
    } else {
        Set_Status(" ");
    }
    do_callbacks(code, (CB *)data);
}

static void
status(command, tag)
char *command;
int   tag;
{
    char *mess = NULL;
    
    mess = concat(mess, "Changing to remote directory ");
    mess = concat(mess, last_cd);
    Set_Status(mess);
    XtFree(mess);
}

void 
Start_Cd(echo, paramater, flag, callback, data, cb)
int    echo;
char  *paramater;
int    flag;
void (*callback)();
DATA   data;
CB    *cb;
{
    char *cp = NULL;
    char *cd = NULL;
    CB *cbp;

    previous_dir = Find_Dir(remote_dir, hostname, 0);

    if (flag) {
	cd = Compute_Cd(paramater);
    } else {
	cd = XtNewString(paramater);
    }
    if (last_cd) {
	XtFree(last_cd);
    }
    last_cd = XtNewString(cd);
    cp = concat(cp, "CWD ");
    cp = concat(cp, cd);

    cbp = link_callback(NULL, callback, data, cb);
    write_ftp(cp, status,  finish_cd, NULL, 0, echo, (DATA)cbp);
    XtFree(cp);
    XtFree(cd);
}
