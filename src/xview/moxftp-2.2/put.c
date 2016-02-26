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
"$Id: put.c,v 1.1 1994/03/14 18:55:53 jones Exp $\n";
static char rcspath[] =
"$Source: /mintaka/u0/xx/ss/jones/jones.old/mftp.src/mftpnew/RCS/put.c,v $\n";
#endif

/* $Log: put.c,v $
 * Revision 1.1  1994/03/14  18:55:53  jones
 * Initial revision
 *
 */

#include "machine.h"
#include "defs.h"
#include <sys/stat.h>

static char *remote = NULL;
static char *local   = NULL;
static int hash_flag =  -1;
static int size = 0;

extern XtAppContext App_context;
static XtIntervalId timeout;

static void 
finish_put(output, tag,  data)
struct _output *output;
int  tag;
DATA data;
{
    int code;
    char *msg = NULL;

    if(timeout || hash_flag > 0) {
	if(timeout) XtRemoveTimeOut(timeout);
	timeout = 0;
        Update_Percent(100, size);
        Update_Percent(0, 0);
    }
    hash_flag =  -1;
    Unset_Busy();
    Clear_Noop(NOOP_PUT);
    code =  ftp_status(output);

    if (ftp_response[code].status & FTP_ERROR) {
        Set_Status_Error("put failed - ftp error:",  last_response);
    } else { 
        msg = concat(msg, "Finished transfer of local file ");
	msg = concat(msg,  local);
	Set_Status_Short(msg);
	XtFree(msg);
    }
    XtFree(local);
    XtFree(remote);
    local  = NULL;
    remote = NULL;
    do_callbacks(code, (CB *)data);
}

static void
status(command, tag)
char *command;
int   tag;
{
    char *status = NULL;

    Animate(Top);
    status = concat(status, "Put local file ");
    status = concat(status, local);
    if (local && *local && strcmp(local, remote) != 0) {
    	status = concat(status, " to remote file ");
    	status = concat(status, remote);
    }
    Set_Status(status);
    XtFree(status);
  
}

static int  bytes = 0;

static void 
percent(client_data, id)
caddr_t client_data;
XtIntervalId *id;
{
    struct stat statbuff;
    int p;
    char *path = NULL;
    
    path = concat(path, local_dir);
    path = concat(path, local);
    if(stat(path, &statbuff) == 0) { 
        p = (100*statbuff.st_size)/bytes;
        size = statbuff.st_size;
        Update_Percent(p, size);
        timeout =  XtAppAddTimeOut(App_context, (unsigned long)(1*1000), 
			           (XtTimerCallbackProc)percent,
                                   (XtPointer)NULL);
    } 
    XtFree(path);
}

void 
Start_Put(echo, remote_file, local_file, size, callback, data, cb)
int    echo;
char  *local_file;
char  *remote_file;
int   size;
void (*callback)();
DATA  data;
CB    *cb;
{
    CB  *cbp;
    char *command = NULL;

    command = concat(command,"STOR ");

    local = XtNewString(local_file);

    if (remote_file && *remote_file) {
         remote = XtNewString(remote_file);
    } else {
         remote = XtNewString(local_file);
    }

    if (INDEX(remote,' ') != NULL) {
        command = concat(command, "\"");
        command = concat(command, remote);
        command = concat(command, "\"");
    } else {
        command = concat(command, remote);
    }

    cbp = link_callback(NULL, callback, data, cb);
    Start_Listen(command, status, finish_put, 0, echo,
                  0, cbp, local, 1);

    XtFree(command);
    if (size) bytes = size;
    else      bytes = 0;
    hash_flag = 0;
    Set_Noop(NOOP_PUT);
    Set_Busy(BUSY_PUT);
}

