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
"$Id: syst.c,v 1.1 1994/03/14 18:55:53 jones Exp $\n";
static char rcspath[] =
"$Source: /mintaka/u0/xx/ss/jones/jones.old/mftp.src/mftpnew/RCS/syst.c,v $\n";
#endif

/* $Log: syst.c,v $
 * Revision 1.1  1994/03/14  18:55:53  jones
 * Initial revision
 *
 */

#include "machine.h"
#include "defs.h"
#include "proto/syst.h"

char *system_name = NULL;
char *system_type = NULL;
int   remote_type = 0;
int   ibm_rt      = 0;
int   no_dot_dot  = 0;
int   appolo      = 0;
int   transfer_mode = MODE_T_ASCII;
extern int   guessed;

void
Set_Other_Tables()
{
    int i;

    if (!s_tran->use_other)  {
        s_tran_files = s_tran;
	return;
    }
    for (i=0; i<NO_TRAN;i++) {
	if (!translate[i].system) continue;
	if (strcmp(s_tran->use_other, translate[i].system) == 0) {
	    s_tran_files = &translate[i];
	    return;
	}
    }
}

void
Set_Type_Name()
{
    Set_Info();
}

void
Start_Syst(echo, callback, data, cb)
int echo;
void (*callback)();
DATA  data;
CB   *cb;
{
    CB *cbp;
   

    cbp = link_callback(NULL, callback, data, cb);

    write_ftp("SYST", status,  finish_syst, NULL, 0, echo, (DATA)cbp);
}

static void 
finish_syst(output, tag, data)
struct _output *output;
int  tag;
DATA data;
{
    int code, i, j, n;
    char *sys_name, *cp, *end;

    Set_Status(" ");
    code =  ftp_status(output);

    if ((ftp_response[code].status & FTP_ERROR) ||
        strncmp(ftp_response[code].code, "215", 3) != 0 ||
        (int)strlen(last_response) < 5 ) {
	if (!remote_type) {
	    system_type = XtNewString("System Type Unknown");
	    remote_type = REMOTE_SYSTEM_UNKNOWN;
	}
        Set_Info();
    } else {
	/*
         * Some ftpd's are really broken and don't know who they are, sigh.
 	 */
  	if ((strncmp("UNKNOWN", (char *)&last_response[4], 
		     sizeof("UNKNOWN")-1) == 0) &&
	    guessed) {
            do_callbacks(1, (CB*)data);
	    return;
	}

        system_name = XtNewString((char*)&last_response[4]);
        for (i=0; i < (int)strlen((char*)&last_response[4]); i++) {
	     if ( last_response[4+i] == '\n') break;
             system_name[i] = last_response[4+i];
        }
        system_name[i] =  '\0';
        cp = XtNewString(last_response+4);
	if ((end = INDEX(cp, ' ')) != NULL) {
	    *end = '\0';
	} else if ((end = INDEX(cp, '\n')) != NULL) {
	    *end = '\0';
	}
        system_type = XtNewString("System Type Unknown");
   	remote_type = REMOTE_SYSTEM_UNKNOWN;
        for (i=0; i<NO_TRAN;i++) {
	    int found = 0;
	    for(j=0; j < N_TYPE; j++) {
                if (translate[i].type[j] &&
    	    	    strcmp(cp, translate[i].type[j]) == 0) {
	            XtFree(system_type);
		    guessed = 0;
		    if (translate[i].unix) {
			if(translate[i].ibm_rt) ibm_rt++;
			if(translate[i].apollo) no_dot_dot++;
                    	remote_type = REMOTE_SYSTEM_UNIX;
	            	system_name = XtNewString(translate[i].system);
	            	system_type = XtNewString(translate[i].system);
		    } else {
		     	remote_type = REMOTE_SYSTEM_OTHER;
	  	    	system_name = XtNewString(translate[i].system);
		    	system_type = XtNewString(translate[i].system);
                        s_tran = &translate[i];
		        Set_Other_Tables();
		    }
		    found++;
		    break;
	        }
	    }
	    if (found) break;
        }
        Set_Type_Name();
        XtFree(cp);
    }
    do_callbacks(1, (CB*)data);
}

static void
status(command, tag)
char *command;
int   tag;
{
    Set_Status("Probing for system type");
}
