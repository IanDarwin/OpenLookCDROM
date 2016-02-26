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
"$Id: disconnect.c,v 1.2 1994/03/22 23:21:04 jones Exp $\n";
static char rcspath[] =
"$Source: /mintaka/u0/xx/ss/jones/jones.old/mftp.src/moxftp-2.0/RCS/disconnect.c,v $\n";
#endif

/* $Log: disconnect.c,v $
 * Revision 1.2  1994/03/22  23:21:04  jones
 * Don't reset remote directory if retry set.
 *
 * Revision 1.1  1994/03/14  18:55:53  jones
 * Initial revision
 *
 */

#include "defs.h"
#include "proto/disconnect.h"


void
Set_Logged_Out()
{
    extern default_transfer_mode;
    extern char *system_name;
    extern char *system_type;
    extern int retry;

    if(system_name) {
        XtFree(system_name);
        system_name = NULL;
    }
    if(system_type) {
        XtFree(system_type);
        system_type = NULL;
    }
    remote_type = 0;
    connected = 0;
    s_tran = NULL;
    s_tran_files = NULL;
    if (!retry) Set_Remote(".");
    Set_Info();
    Clear_Dirs();
    Clear_Noop(NOOP_CLEAR_ALL);
    Update_Files_Opts(0);
    set_icon(ICON_DISCONNECT);
    Set_Wn_Name(NULL);
    Init_Login();
}

static void
finish_disconnect(output, tag, data)
struct _output *output;
int  tag;
DATA data;
{
    int code;
    extern char *system_name;
    extern char *system_type;

    code =  ftp_status(output);
    Set_Logged_Out();
    Close_Ftp();
    do_callbacks(code, (CB*)data);
}

void 
Start_Discconect(echo, callback, data, cb)
int    echo;
void (*callback)();
DATA   data;
CB    *cb;
{
    CB *cbp;
 
    cbp = link_callback(NULL, callback, data, cb);
    write_ftp("COLSE", status, finish_disconnect, NULL, 0, echo, (DATA)cbp);
}

static void
status(command, tag)
char *command;
int   tag;
{
    Set_Status("Closing connection.....");
}
