/*
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
"$Id: reconnect.c,v 1.1 1994/03/14 18:55:53 jones Exp $\n";
static char rcspath[] =
"$Source: /mintaka/u0/xx/ss/jones/jones.old/mftp.src/mftpnew/RCS/reconnect.c,v $\n";
#endif

/* $Log: reconnect.c,v $
 * Revision 1.1  1994/03/14  18:55:53  jones
 * Initial revision
 *
 */

#include "defs.h"
#include "proto/reconnect.h"

extern char *gateway;
extern int use_gateway;
extern Widget Top;

/*
 * Globals.
 */
int no_recconect_list = 0;

static void
finish_cd_cb(data, code, cb)
DATA data;
int code;
CB  *cb;
{
    if (ftp_response[code].status & FTP_ERROR) {
        response("Reconnect failed - cd");
	Set_Status_Error("cwd - ftp error: ",  last_response);
	do_callbacks(code, (CB *)cb);
        Set_Reconnect(1);
    } else {	
        Start_Pwd(0, 0, finish_pwd_cb, NULL, cb);
    }
}

static void
finish_pass(output, tag, data)
struct _output *output;
int tag;
DATA data;
{
    int  code;

    code =  ftp_status(output);
    if (ftp_response[code].status & FTP_ERROR) {
        response("Reconnect failed - password");
	Set_Status_Error("PASS - ftp error: ",  last_response);
        Set_Reconnect(1);
    } else {	
	logged_in++;
	if (remote_dir &&
            !(strlen(remote_dir) == 1 && *remote_dir == '.')) {
             Start_Cd(0, remote_dir, 0, finish_cd_cb, NULL, (CB *)data);
	     return;
	} else {
             Start_Pwd(0, 0, finish_pwd_cb, NULL, (CB *)data);
	     return;
	}
    }
    do_callbacks(code, (CB *)data);
}

static void
finish_pwd_cb(data, code, cb)
DATA data;
int code;
CB  *cb;
{
    struct _dirs *dir = NULL;

    if (ftp_response[code].status & FTP_ERROR) {
        response("Reconnect failed - pwd");
	Set_Status_Error("pwd - ftp error: ",  last_response);
	do_callbacks(code, cb);
        Set_Reconnect(1);
    } else {	
        dir = Find_Dir(remote_dir, hostname, 0);
	if (dir && (noautodir|no_recconect_list)) {
	    no_recconect_list = 0;
            Start_Type(0, "image", 1, finish_type_cb,  NULL, cb);
	} else {
	    no_recconect_list = 0;
   	    Start_Remote_Dir(0, 1, finish_remote_cb, NULL, cb);
	}
    }
}

static void
finish_reconnect(output, tag, data)
struct _output *output;
int tag;
DATA   data;
{
    int  code;

    code =  ftp_status(output);
    if (ftp_response[code].status & FTP_ERROR) {
        response("Reconnect failed");
        Set_Reconnect(1);
	do_callbacks(code, (CB *)data);
    } else {
	connected = 1;
        Set_Status("Connected");
        set_icon(ICON_CONNECT);
        WcSetInsensitiveCB(Top, "*reconnect", NULL);
	start_user(0, (CB *)data);
    }
}

static void
finish_remote_cb(data, code, cb)
DATA data;
int code;
CB  *cb;
{
    if (ftp_response[code].status & FTP_ERROR) {
        response("Reconnect failed - remote dir");
	Set_Status_Error("remote dir - ftp error: ",  last_response);
	do_callbacks(code, cb);
        Set_Reconnect(1);
    } else {	
        Start_Type(0, "image", 1, finish_type_cb,  NULL, cb);
    }
}

static void
finish_type_cb(data, code, cb)
DATA data;
int code;
CB  *cb;
{
    if (ftp_response[code].status & FTP_ERROR) {
        response("Reconnect failed - type");
        Set_Status_Error("type - ftp error: ",  last_response);
	do_callbacks(code, cb);
        Set_Reconnect(1);
    } else {
	Clear_Noop(NOOP_CONNECT|NOOP_NOTCONN);
    }
    do_callbacks(code, cb);
}

static void
finish_user(output, tag, data)
struct _output *output;
int tag;
DATA data;
{
    int  code;

    code =  ftp_status(output);
    if (ftp_response[code].status & FTP_ERROR) {
        response("Reconnect failed - user");
	Set_Status_Error("USER - ftp error: ",  last_response);
        Set_Reconnect(1);
	do_callbacks(code, (CB *)data);
    } else {
        start_pass(0,  (CB *)data);
    }
}

static void
start_pass(echo, cbp)
int    echo;
CB    *cbp;
{
    char *pass = NULL;
    char *hide = NULL;
    char *cp1 = NULL;
    char *cp = NULL;

    pass = concat(pass, "PASS ");
    hide = XtNewString(pass);
    pass = concat(pass, password);
    cp1 = cp  = concat(cp, password);
    while(*cp) { *cp = 'X'; cp++;}
    hide = concat(hide, cp1);
    XtFree(cp1);
    write_ftp(pass, NULL, finish_pass, hide,  0, 0, (DATA)cbp);
}

void
Start_Reconnect(cbp)
CB    *cbp; 
{
     char *host;

     if (use_gateway && gateway && gateway[0]) {
         host =  gateway;
     } else {
         host =  hostname;
     }
     Set_Noop(NOOP_CONNECT);
     Connect_Ftp(host, status, finish_reconnect, NULL, 0, 0, (DATA)cbp);
     set_icon(ICON_CONNECTING);
}

static void
start_user(echo, cbp)
int    echo;
CB    *cbp; 
{
    char *user = NULL;

    user = concat(user, "USER ");
    user = concat(user, login);
    if (use_gateway && gateway && gateway[0]) {
        user = concat(user, "@");
        user = concat(user, hostname);
    }

    write_ftp(user, status,  finish_user, NULL, 0, echo, (DATA)cbp);
    XtFree(user);
}

static void
status(command, tag)
char *command;
int   tag;
{
     char *cp = NULL;

     cp = concat(cp, "Reconnecting to host ");
     cp = concat(cp, hostname);
     Set_Status(cp);
     XtFree(cp);
}
