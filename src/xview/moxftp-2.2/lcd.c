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
"$Id: lcd.c,v 1.1 1994/03/14 18:55:53 jones Exp $\n";
static char rcspath[] =
"$Source: /mintaka/u0/xx/ss/jones/jones.old/mftp.src/mftpnew/RCS/lcd.c,v $\n";
#endif

/* $Log: lcd.c,v $
 * Revision 1.1  1994/03/14  18:55:53  jones
 * Initial revision
 *
 */

#include "machine.h"
#include "defs.h"

void 
Start_Lcd(echo, paramater, callback, data, cb)
int    echo;
char  *paramater;
void (*callback)();
DATA   data;
CB     *cb;
{
    char *cp = NULL;
    char  pathname[1000];
    char *local, *path;
    CB    *cbp;
    struct _dirs *dir;

    
    cbp = link_callback(NULL, callback, data, cb);
    if (chdir(paramater) < 0) {
	cp = concat(cp, "lcd to ");
	cp = concat(cp, paramater);
	cp = concat(cp, " failed");
        Set_Status(cp);
	XtFree(cp);
	do_callbacks(0, cbp);
    } else {
#if defined(USE_GETWD)
    	local = (char *)getwd(pathname);
#else
        local = (char *)getcwd(pathname, sizeof(pathname));
#endif
	path = XtNewString(local);
        if (path[strlen(path)-1] != '/') {
	    path = concat(path, "/");
        }
    	Set_Local(path);
        local = concat(NULL,  "Local dir is now: ");
        local= concat(cp,  path);
        Set_Status(local);
        dir = Find_Dir(local_dir, localhost, 1);
        Update_Stat_Time(dir);
	if(!dir || dir->update || dir->n < 0) Update_Local_Dir();
        Set_Status(" ");
	XtFree(path);
	XtFree(local);
	do_callbacks(1, cbp);
    }
}
