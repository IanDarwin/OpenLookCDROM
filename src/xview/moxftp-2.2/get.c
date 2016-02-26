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
"$Id: get.c,v 1.1 1994/03/14 18:55:53 jones Exp $\n";
static char rcspath[] =
"$Source: /mintaka/u0/xx/ss/jones/jones.old/mftp.src/moxftp-2.0/RCS/get.c,v $\n";
#endif

/* $Log: get.c,v $
 * Revision 1.1  1994/03/14  18:55:53  jones
 * Initial revision
 *
 */

#include "machine.h"
#include "defs.h"

static char *remote = NULL;
static char *local   = NULL;
static int rate = 0;
static int size = 0;
static int bytes = 0;

extern XtAppContext App_context;
static XtIntervalId timeout;
static char *view_file_name = NULL;
static void percent();

static void 
finish_get(output, tag, data)
struct _output *output;
int   tag;
DATA  data;
{
    int code;
    char *msg = NULL;

    if(timeout) {
	if (timeout) XtRemoveTimeOut(timeout);
	timeout = 0;
        Update_Percent(100, size);
        Update_Percent(0, 0);
    }
    Unset_Busy();
    Clear_Noop(NOOP_GET);
    code =  ftp_status(output);

    if (ftp_response[code].status & FTP_ERROR) {
 	Set_Status_Error("get failed - ftp error:",  last_response);
    } else { 
        msg = concat(msg, "Finished transfer of remote file ");
        msg = concat(msg,  remote);
        Set_Status_Short(msg);
        XtFree(msg);

        Set_Status(" ");
    }
    XtFree(local);
    XtFree(remote);
    local  = NULL;
    remote = NULL;
    do_callbacks(code, (CB*)data);
}

static void
status(command, tag)
char *command;
int   tag;
{
    char *cp = command;
    char *junk = NULL;
    char *status = NULL;

    Animate(Top);
    status = concat(status, "Fetching remote file ");
    status = concat(status, remote);
    if (local && *local && (strcmp(local, remote) != 0)) {
    	status = concat(status, " to local file ");
        status = concat(status, local);
    }
    Set_Status(status);
    XtFree(junk);
    XtFree(status);
    if (bytes) {
        timeout =  XtAppAddTimeOut(App_context, (unsigned long)1000,
                                   (XtTimerCallbackProc)percent,
                                   (XtPointer)NULL);
    }
  
}


static void 
percent(client_data, id)
caddr_t client_data;
XtIntervalId *id;
{
    int p;
    
    size = Get_Chars();
    p = (100*size)/bytes;
    Update_Percent(p, size);
    timeout =  XtAppAddTimeOut(App_context, (unsigned long)1000, 
		               (XtTimerCallbackProc)percent, 
			       (XtPointer)NULL);
}
   
void
get_peek_150(code)
char *code;
{
    char *cp;
    int  atoi();
    
    if (!remote || timeout) return;
    if (strncmp(code, "150 O", sizeof("150 O") - 1) != 0) return;
    if (!(cp = bsdstrstr(code,"byte"))) return;
    cp = RINDEX(code, '(');
    if (!cp) return;
    cp++;
    bytes = atoi(cp);
    if (bytes == 0) return;
    timeout =  XtAppAddTimeOut(App_context, (unsigned long)1000, 
			           (XtTimerCallbackProc)percent, 
				   (XtPointer)NULL);
}


void 
Start_Get(echo, remote_file, local_file, size, callback, data, cb)
int    echo;
char  *local_file;
char  *remote_file;
int    size;
void (*callback)();
DATA   data;
CB    *cb;
{
    char *command = NULL;
    CB *cbp;

    command = concat(command,"RETR ");
    if (INDEX(remote_file,' ') != NULL) {
	command = concat(command, "\"");
        command = concat(command, remote_file);
	command = concat(command, "\"");
    } else {
    	command = concat(command, remote_file);
    }
    remote = XtNewString(remote_file);

    if (local_file && *local_file) {
         local = XtNewString(local_file);
    } else {
         local = XtNewString(remote_file);
    }

    if (size) bytes = size;
    else      bytes = 0;

    cbp = link_callback(NULL, callback, data, cb);
    Start_Listen(command, status, finish_get, 0, echo,
                 0, cbp, local, 0);
    XtFree(command);
    Set_Busy(BUSY_GET);
    Set_Noop(NOOP_GET);
}



static void 
finish_view(output, data, cb)
struct _output *output;
DATA  data;
CB    *cb;
{
    int code;
    char *msg = NULL;

    if(timeout) {
	if (timeout) XtRemoveTimeOut(timeout);
	timeout = 0;
        Update_Percent(100, size);
        Update_Percent(0, 0);
    }
    Unset_Busy();
    Clear_Noop(NOOP_GET);
    code =  ftp_status(output);

    if (ftp_response[code].status & FTP_ERROR) {
 	Set_Status_Error("view failed - ftp error:",  last_response);
    } else { 
        msg = concat(msg, "Finished transfer of remote file ");
        msg = concat(msg,  remote);
        Set_Status_Short(msg);
        XtFree(msg);
        Set_Status(" ");
        View_File(local, view_file_name);
    }
    XtFree(local);
    XtFree(remote);
    if(view_file_name) XtFree(view_file_name);
    local  = NULL;
    remote = NULL;
    view_file_name = NULL;
    do_callbacks(code, cb);
}

static int new = 0;

void 
Start_View(echo, remote_file, local_file, size, callback, data, cb)
int    echo;
char  *local_file;
char  *remote_file;
int    size;
void (*callback)();
DATA   data;
CB     *cb;
{
    char *command = NULL;
    CB   *cbp;

    local = XtMalloc(sizeof("/tmp/view%d%d%s") + sizeof(local_file) + 10 + 
		     10+1);
    sprintf(local, "/tmp/view%d%d%s",getpid(), new, local_file);
    view_file_name = XtNewString(local_file);
    new++;
    command = concat(command,"RETR ");
    if (INDEX(remote_file,' ') != NULL) {
        command = concat(command, "\"");
        command = concat(command, remote_file);
        command = concat(command, "\"");
    } else {
        command = concat(command, remote_file);
    }
    remote = XtNewString(remote_file);

    if (size) bytes = size;
    else      bytes = 0;
    cbp = link_callback(NULL, callback, data, cb);
    Start_Listen(command, status, finish_view, 0, echo, 0, cbp, local, 0);
    XtFree(command);
    Set_Busy(BUSY_GET);
    Set_Noop(NOOP_GET);
}


