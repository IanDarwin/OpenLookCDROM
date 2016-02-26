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
"$Id: type.c,v 1.1 1994/03/14 18:55:53 jones Exp $\n";
static char rcspath[] =
"$Source: /mintaka/u0/xx/ss/jones/jones.old/mftp.src/mftpnew/RCS/type.c,v $\n";
#endif

/* $Log: type.c,v $
 * Revision 1.1  1994/03/14  18:55:53  jones
 * Initial revision
 *
 */

#include "defs.h"
#include "proto/type.h"

static char *type_name = NULL;
static int   set_default = 0;

void 
Start_Type(echo, t, flag, callback, data, cb)
int    echo;
char  *t; 
int    flag;
void (*callback)();
DATA   data;
CB    *cb;
{
    char *cp = NULL;
    char *T;
    CB *cbp;

    cp = concat(cp, "TYPE ");
    if (type_name) XtFree(type_name);
    if (*t == 'i') {
	type_name = XtNewString("binary");
	T = "I";
    } else if (*t == 'b') {
	type_name = XtNewString("binary");
	T = "I";
    } else if (*t == 'a') {
	type_name = XtNewString("ascii");
	T = "A";
    } else if (*t == 't') {
	T = "T";
	type_name = XtNewString("tenex");
    } else  {
	T = "A";
	type_name = XtNewString("ascii");
    }

    cp = concat(cp, T);
    set_default = flag;

    cbp = link_callback(NULL, callback, data, cb);
    write_ftp(cp, status,  finish_type, NULL, 0, echo, (DATA)cbp);
    XtFree(cp);
}

static void 
finish_type(output, tag, data)
struct _output *output;
int  tag;
DATA data;
{
    int  code;
    extern default_transfer_mode;

    code =  ftp_status(output);

    if (ftp_response[code].status & FTP_ERROR)  {
	Set_Status("Type command failed");
        set_default =  0;
    } else {
        if (*type_name == 'b' ) {
	    transfer_mode = MODE_T_BINARY;
        } else if (*type_name == 'a') {
	    transfer_mode = MODE_T_ASCII;
        } else if (*type_name == 'i') {
	    transfer_mode = MODE_T_BINARY;
        } else if (*type_name == 't') {
	    transfer_mode = MODE_T_TENEX;
        } else {
	    transfer_mode = 0;
	}
    }
    if (set_default) default_transfer_mode = transfer_mode;
    set_default = 0;
    if (type_name) XtFree(type_name);
    type_name = NULL;
    Set_Info();
    Set_Status(" ");
    do_callbacks(1, (CB*)data);
}

static void
status(command, tag)
char *command;
int   tag;
{
    char *cp = NULL;

    cp = concat(cp, "Setting mod to ");
    cp = concat(cp, type_name);
    Set_Status(cp);
    XtFree(cp);
}
