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
"$Id: pwd.c,v 1.1 1994/03/14 18:55:53 jones Exp $\n";
static char rcspath[] =
"$Source: /mintaka/u0/xx/ss/jones/jones.old/mftp.src/mftpnew/RCS/pwd.c,v $\n";
#endif

/* $Log: pwd.c,v $
 * Revision 1.1  1994/03/14  18:55:53  jones
 * Initial revision
 *
 */

#include "machine.h"
#include "defs.h"
#include "proto/pwd.h"

void 
Start_Pwd(echo, redraw,  callback, data, cb)
int    echo;
int    redraw;
void (*callback)();
DATA   data;
CB    *cb;
{
    extern char *last_cd;
    CB *cbp;

    cbp = link_callback(NULL, callback, data, cb);
    if (!s_tran || !s_tran->no_pwd) {
        write_ftp("PWD", status,  finish_pwd, NULL, redraw, echo, (DATA)cbp);
    } else {
	if (last_cd) {
            Set_Remote(last_cd);
            Display_Files(remote_dir, hostname, 0);
	    XtFree(last_cd);
	    last_cd = NULL;
        }
        do_callbacks(1, (CB*)cbp);
    }
}

static void 
finish_pwd(output, tag, data)
struct _output *output;
int  tag;
DATA data;
{
    int  code;
    char *start, *end;
    char *message;
    struct _dirs *dir;
    extern struct _dirs *Lastdir;

    code =  ftp_status(output);
    message =  XtNewString("I have no clue");
    if (ftp_response[code].status & FTP_ERROR || 
	(strncmp(ftp_response[code].code , "257", 3) != 0 &&
	 strncmp(ftp_response[code].code , "251", 3) != 0) ) {
	Set_Status("Could not get current working directory\n");
    }  else {
        start = INDEX(last_response, '\"');
        if (start) {
	    start++;
            end  = INDEX(start, '\"');
	    if (end && end != start) {
		*end = '\0';
		message = XtNewString(start);
	    }
	}
        Set_Remote(message);
    	dir = Find_Dir(remote_dir, hostname, 1);
        if (Lastdir && Lastdir->link_up)  {
            dir->up = Lastdir;
	    Lastdir->link_up = 0;
        } 
        if (tag) Display_Files(remote_dir, hostname, 0);
        Set_Status(" ");
    }
    XtFree(message);
    do_callbacks(code, (CB*)data);
}

static void
status(command, tag)
char *command;
int   tag;
{
    Set_Status("Checking the name of the remote directory");
}
