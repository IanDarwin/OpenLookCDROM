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
"$Id: mkdir.c,v 1.1 1994/03/14 18:55:53 jones Exp $\n";
static char rcspath[] =
"$Source: /mintaka/u0/xx/ss/jones/jones.old/mftp.src/mftpnew/RCS/mkdir.c,v $\n";
#endif

/* $Log: mkdir.c,v $
 * Revision 1.1  1994/03/14  18:55:53  jones
 * Initial revision
 *
 */

#include "defs.h"
#include "proto/mkdir.h"

void 
Start_Mkdir(echo, file,  callback, data, cb)
int    echo;
char  *file; 
void (*callback)();
DATA   data;
CB    *cb;
{
    CB  *cbp;
    char *cp = NULL;

    cp = concat(cp, "MKD ");
    cp = concat(cp, file);
 
    cbp = link_callback(NULL, callback, data, cb);
    write_ftp(cp, status,  finish_mkdir, NULL, 0, echo, (DATA)cbp);
    XtFree(cp);
}

static void 
finish_mkdir(output, tag, data)
struct _output *output;
int  tag;
DATA data;
{
    int  i;
    char *start, *end;
    char *message;
    char temp[1000];

    i =  ftp_status(output);

    if (ftp_response[i].status & FTP_ERROR) {
	if(strncmp(ftp_response[i].code, "550", 3) == 0) {
	    i = 2;
	    Set_Status(" ");
	} else {
	    response("mkdir failed");
	}
    } else {
        Set_Status(" ");
    }
    do_callbacks(i, (CB*)data);
}

static void
status(command, tag)
char *command;
int   tag;
{
    Set_Status("Making a directory");
}
