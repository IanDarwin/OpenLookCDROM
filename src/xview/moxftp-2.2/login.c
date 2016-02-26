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
"$Id: login.c,v 1.1 1994/03/14 18:55:53 jones Exp jones $\n";
static char rcspath[] =
"$Source: /mintaka/u0/xx/ss/jones/jones.old/mftp.src/moxftp-2.2/RCS/login.c,v $\n";
#endif

/* $Log: login.c,v $
 * Revision 1.1  1994/03/14  18:55:53  jones
 * Initial revision
 *
 */

#include "defs.h"
#include "proto/login.h"

extern char *gateway;
extern int use_gateway;
int    logged_in = 0;

static void 
finish_init(output, tag, data)
struct _output *output;
int  tag;
DATA data;
{
    int code;

    code =  ftp_status(output);
    do_callbacks(code, (CB *)data);
}

static void
finish_syst_cb(data, code, cb)
DATA  data;
int   code;
CB   *cb;
{
    start_set_type(0, cb);
}


static void 
finish_pass(output, tag, data)
struct _output *output;
int  tag;
DATA data;
{
    int code;

    code =  ftp_status(output);
    if (ftp_response[code].status & FTP_ERROR) {
        Set_Status_Error("Login failed - ftp error: ",  last_response);
	login_error(1, code, (CB *)data);
    } else {
        set_icon(ICON_CONNECT);
        Set_Wn_Name(hostname);
	logged_in++;
	if(!s_tran || !s_tran->notype) {
	    Start_Syst(0, finish_syst_cb, NULL, (CB *)data);
	    return;
	}
        start_set_type(0, (CB *)data);
    }
}

static void
finish_cd_cb(data, code, cb)
DATA data;
int code;
CB  *cb;
{
    if (ftp_response[code].status & FTP_ERROR) {
	Set_Status_Error("cwd - ftp error: ", last_response);
        do_callbacks(code, (CB *)cb);
    } else {
        Start_Pwd(0, 0, finish_pwd_cb, NULL, cb);
    }
}

static void
finish_pwd_cb(data, code, cb)
DATA data;
int code;
CB  *cb;
{
    if (ftp_response[code].status & FTP_ERROR) {
	Set_Status_Error("cwd - ftp error: ", last_response);
	login_error(0, code, cb);
    } else {
        Start_Remote_Dir(0, 1, finish_remote_dir_cb, NULL, cb);
    }
}

static void
finish_local_dir_cb(data, code, cb)
DATA data;
int  code;
CB   *cb;
{
    if (ftp_response[code].status & FTP_ERROR) {
	Set_Status_Error("remote dir - ftp error: ", last_response);
	login_error(0, code,  cb);
    } else {
	Set_Dir_Type(REMOTE);
	do_callbacks(1, cb);
	Do_Prompt(NULL, 1);
	Clear_Noop(NOOP_CONNECT|NOOP_NOTCONN);
    }
}

static void
finish_remote_dir_cb(data, code, cb)
DATA data;
int  code;
CB   *cb;
{
    if (ftp_response[code].status & FTP_ERROR) {
	Set_Status_Error("remote dir - ftp error: ", last_response);
	login_error(0, code, cb);
    } else {
        Start_Local_Dir(0, 0, finish_local_dir_cb, NULL, cb);
    }
}

static void
finish_login_failed_cb(data, code, cb)
DATA data;
int  code;
CB   *cb;
{

}

static void
login_error(flag, code, cb)
int flag;
int code;
CB  *cb;
{

    if (flag) Start_Discconect(FTP_ECHO, finish_login_failed_cb, NULL, NULL);
    do_callbacks(code, cb);
}

static void
finish_set_type_cb(data, code, cb)
DATA data;
int  code;
CB   *cb;
{
    if (remote_dir &&
	!(strlen(remote_dir) == 1 && *remote_dir == '.')) {
	Start_Cd(0, remote_dir, 0, finish_cd_cb, NULL, cb);
    } else {
        Start_Pwd(0, 1, finish_pwd_cb, NULL, cb);
    }
}


static void 
finish_user(output, tag,  data)
struct _output *output;
int  tag;
DATA data;
{
    int code;

    code =  ftp_status(output);

    if (ftp_response[code].status & FTP_ERROR) {
        Set_Status_Error("Login failed - ftp error: ",  last_response);
	login_error(1, code, (CB *)data);
    } else { 
	Stop_Retry();
	Clear_List();
	Init_Close();
	start_pass(0, (CB *)data);
    }
}

static void
status(command, tag)
char *command;
int   tag;
{
    Set_Status("Logging in ....");
}

void 
Start_Login(echo, callback, data, cb)
int    echo;
void (*callback)();
DATA   data;
CB    *cb;
{
    char *user = NULL;
    CB  *cbp;

    user = concat(user, "USER ");
    user = concat(user, login);
    if (use_gateway && gateway && gateway[0]) {
	user = concat(user, "@");
	user = concat(user, hostname);
    }

    cbp = link_callback(NULL, callback, data, cb);
    write_ftp(user, status,  finish_user, NULL, 0, echo,  (DATA)cbp);
    XtFree(user);
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


static void
start_set_type(echo, cbp)
int    echo;
CB    *cbp;
{ 
    if (remote_type == REMOTE_SYSTEM_UNIX) {
	Start_Type(0, "image", 1, finish_set_type_cb, 0, cbp);
    } else {
	Start_Type(0, "ascii", 1, finish_set_type_cb, 0, cbp);
   }
}
