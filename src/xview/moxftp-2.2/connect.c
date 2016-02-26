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
"$Id: connect.c,v 1.1 1994/03/14 18:55:53 jones Exp $\n";
static char rcspath[] =
"$Source: /mintaka/u0/xx/ss/jones/jones.old/mftp.src/mftpnew/RCS/connect.c,v $\n";
#endif

/* $Log: connect.c,v $
 * Revision 1.1  1994/03/14  18:55:53  jones
 * Initial revision
 *
 */

#include "machine.h"
#include "defs.h"
#include "proto/connect.h"

char  *hostname    = NULL;
char  *login       = NULL;
char  *password    = NULL;
char  *remote_dir  = NULL;
char  *local_dir   = NULL;
char  *ftp_type    = NULL;
int   default_transfer_mode = MODE_T_ASCII;
int   connected  = 0;
int   guessed = 0;
extern int ibm_rt;
extern int appolo;
extern int no_dot_dot;
extern char *gateway;
extern int use_gateway;

static void
finish_connect(output, tag, data)
struct _output *output;
int tag;
DATA data;
{
    int  code;

    code =  ftp_status(output);
    if (ftp_response[code].status & FTP_ERROR) {
        response("Connection failed");
	Set_Logged_Out();
    	Close_Ftp();
    } else {
 	connected = 1;
	ftp_type = XtNewString("Ascii");
	guess_system_type(output);
        Set_Status("Connected");
	Start_Login(FTP_ECHO|FTP_PROMPT_NO, NULL, NULL, NULL);
    }
    do_callbacks(code, (struct _callback *)data);
}

static void
guess_system_type(output)
struct _output *output;
{
    int i,j;
    extern char *system_name;
    extern char *system_type;
    struct _output *next = output;
    char *last_response = NULL;

    ibm_rt = 0;
    appolo = 0;
    no_dot_dot = 0;
    /*
     * Always assume that we are dealing with a unix box.
     */
    guessed = 1;
    system_name = XtNewString("Unix I guess");
    system_type = XtNewString("Unix I guess");
    remote_type = REMOTE_SYSTEM_UNIX;

    while(next) {
        for (j=0; j<next->lines; j++) {
            char *cp = next->output[j];

            if (strncmp(cp, "220", 3) == 0) {
		last_response = cp;
		break;
	    }
	}
	if (last_response) break;
	next = next->next;
    }

    if (last_response == NULL) return;
    for (i=0; i<NO_TRAN;i++) {
	if (translate[i].guess[0]) {
	    for (j=0; j<N_GUESS; j++) {
       		if (translate[i].guess[j] && 
		    bsdstrstr(last_response, translate[i].guess[j]) != NULL) {
    		    guessed++;
		    if(translate[i].unix) {
			if(translate[i].ibm_rt) ibm_rt++;
			else 		ibm_rt = 0;
			if(translate[i].apollo) no_dot_dot++;
			else 		        no_dot_dot = 0;
			if (system_name) XtFree(system_name);
			if (system_type) XtFree(system_type);
	                system_name = XtNewString(translate[i].system);
	            	system_type = XtNewString(translate[i].system);
	    		remote_type = REMOTE_SYSTEM_UNIX;
		    } else {
			ibm_rt = 0;
			no_dot_dot = 0;
			if (system_name) XtFree(system_name);
			if (system_type) XtFree(system_type);
	            	system_name = XtNewString(translate[i].system);
	            	system_type = XtNewString(translate[i].system);
	    		remote_type = REMOTE_SYSTEM_OTHER;
	    		s_tran = &translate[i];
		        Set_Other_Tables();
		    }
	    	    Set_Type_Name();
	    	    return;
		}
	    }
	}
    }
}

static void
status(command, tag)
char *command;
int   tag;
{
     char *cp = NULL;

     cp = concat(cp, "Connecting to host ");
     cp = concat(cp, hostname);
     Set_Status(cp);
     XtFree(cp);
}

void
Start_Connect(echo, callback, data, cb)
int    echo;
void (*callback)();
DATA   data;
CB    *cb;
{
     char *host;
     extern int aborting;
     struct _callback *cbp;

     aborting = 0;

     if (use_gateway && gateway && gateway[0]) {
         host =  gateway;;
     } else {
         host =  hostname;;
     }
     Set_Noop(NOOP_CONNECT);
     cbp = link_callback(NULL, callback, data, cb);
     Connect_Ftp(host, status, finish_connect, NULL, 0, echo, (DATA)cbp);
     set_icon(ICON_CONNECTING);
}
