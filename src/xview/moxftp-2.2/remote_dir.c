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
"$Id: remote_dir.c,v 1.1 1994/03/14 18:55:53 jones Exp $\n";
static char rcspath[] =
"$Source: /mintaka/u0/xx/ss/jones/jones.old/mftp.src/mftpnew/RCS/remote_dir.c,v $\n";
#endif

/* $Log: remote_dir.c,v $
 * Revision 1.1  1994/03/14  18:55:53  jones
 * Initial revision
 *
 */
#include "machine.h"
#include "defs.h"
#include "proto/remote_dir.h"

static XtIntervalId timeout;

extern int  ibm_rt;
extern int  appolo;
extern int  guessed;
extern char *system_name;
extern char *system_type;

#define MAXSTRING 2000


void 
Start_Remote_Dir(echo, flag, callback, data, cb)
int    echo;
int    flag;
void (*callback)();
DATA   data;
CB    *cb;
{
    struct _dirs *dir = NULL;
    char *s;
    CB *cbp;
 
    dir = Find_Dir(remote_dir, hostname, 1);
    Update_Remote_Time(dir);

    cbp = link_callback(NULL, callback, data, cb);
    if (flag || dir == NULL || (dir && dir->n < 0) || (dir && dir->update)) {
	Push_Files(dir);
        if ((guessed == 1) || (appolo == 2)) {
	    s = XtNewString("LIST -l");
	} else {
	    s = XtNewString("LIST");
	}
	Start_Listen(s, status, finish_remote_dir, 0, echo, 
		     flag, cbp, NULL, 0);
	XtFree(s);
        Set_Busy(BUSY_DIR);
        Set_Noop(NOOP_DIR);
        return;
    } else {
	do_callbacks(1, cbp);
    }
}

void
Update_Remote_Time(dir)
struct _dirs *dir;
{
    time_t now;
    time_t w;
    extern int mult_file;

    /*
     * Some one else is worring direcotry updates.
     */
    if (mult_file) return;

    if (dir == NULL) return;
    time(&now);
    if (dir->time != 0) {
	w = now - dir->time;
	w = w/(10*60);
	if (w) {
	    dir->update++;
	    dir->time = now;
	}
    } else {
	dir->time = now;
    }
}

static void 
finish_remote_dir(output, tag,  data)
struct _output *output;
int  tag;
DATA data;
{
    int code;
    struct _dirs *dir;

    if(timeout) {
        if (timeout) XtRemoveTimeOut(timeout);
        timeout = 0;
        Update_Percent(0, 0);
    }

    Unset_Busy();
    Update_Percent(0, 0);
    Clear_Noop(NOOP_DIR);
    code =  ftp_status(output);
    if (ftp_response[code].status & FTP_ERROR) {
	char *cp = NULL;
	cp = concat(cp, "Remote dir failed: ");
	cp = concat(cp, last_response);
	Set_Status(cp);
        dir = Find_Dir(remote_dir, hostname, 0);
	Pop_Files(dir);
        do_callbacks(code, (CB *)data);
	return;
    }

    switch (remote_type) {
      case REMOTE_SYSTEM_UNIX:
	parse_unix(output);
	break;
      case  REMOTE_SYSTEM_OTHER:
        parse_other(output);
	break;
      default:
	fprintf(stderr,"Unknown remote type %d\n",remote_type);
    }

    dir = Find_Dir(remote_dir, hostname, 0);
    Restore_Dir_Old(dir);
    if (dir) dir->update = 0;
    if(Remote_local == REMOTE) Display_Files(remote_dir, hostname,  0);
    Set_Status(" ");
    do_callbacks(code, (CB *)data);
}

static void
init_output(output)
struct _output *output;
{
    if (output) {
	output->current = 0;
	output->temp = output;
    }
}

static char *
next_output(output, peek)
struct _output *output;
int peek;
{
    struct _output *next;
    int i;

    if (!output) return NULL;
    next = output->temp;
    while(next) {
        i = next->current;
        if (next->current < next->lines ) {
	    if (!peek) next->current++;
            return (next->output[i]);
        }
        next = next->next;
	if (!peek) output->temp = next;
	if (next == NULL) return NULL;
	if (peek) {
            if (0 < next->lines ) {
            	return (next->output[0]);
            } else {
	 	return NULL;
	    }
	}
	
        next->current = 0;
    }
    return NULL;
}

static void
parse_other(output)
struct _output *output;
{
    struct _dirs *dir;
    int files;
     
    if (output == NULL) return;

    /*
     * Compute number of files.
     */
    files = scan_other(output, NULL);
    if (!files) return;
    /*
     * Set up dirtory.
     */
    if (remote_dir == NULL) return;
    dir = Find_Dir(remote_dir, hostname, 1);
    dir->files = (struct _files *)XtMalloc(files*sizeof(struct _files));
    ZERO((char *)dir->files, files*sizeof(struct _files));
    dir->n     = files;
    dir->type  = REMOTE_SYSTEM_OTHER;
    /*
     * Scan files.
     */
    files = scan_other(output, dir);
}

static void
parse_unix(output)
struct _output *output;
{
    struct _dirs *dir;
    int files;
     
    if (output == NULL) return;

    /*
     * Compute number of files.
     */
    files = scan_unix(output, NULL);
    if (!files) {
        dir = Find_Dir(remote_dir, hostname, 1);
	dir->n = 0;
	return;
    };
    /*
     * Set up dirtory.
     */
    dir = Find_Dir(remote_dir, hostname, 1);
    dir->files = (struct _files *)XtMalloc(files*sizeof(struct _files));
    ZERO((char *)dir->files, files*sizeof(struct _files));
    dir->n     = files;
    dir->type  = REMOTE_SYSTEM_UNIX;
    /*
     * Scan files.
     */
    files = scan_unix(output, dir);
    if (files > dir->n) fprintf(stderr,"Too many files %d %d\n",files, dir->n);
}

static void
percent(client_data, id)
caddr_t client_data;
XtIntervalId *id;
{
    int size;
   
    size = Get_Chars();
    if (size) Update_Percent(0, size);
    timeout =  XtAppAddTimeOut(App_context, (unsigned long)1000,
                               (XtTimerCallbackProc)percent,
                               (XtPointer)NULL);
}

static int
scan_other(output, dir)
struct _output *output;
struct _dirs *dir;
{
    char *cp, *nl;
    int  n = 0;
    int  files = 0;
    char *line;

    init_output(output);
    while(cp = next_output(output,0)) {
        n = strlen(cp);
	if (n >= MAXSTRING) continue;
	line = concat(NULL, cp);
	if (s_tran->indent && (cp = next_output(output, 1)) && *cp == ' ') {
	    n = strlen(cp);
	    if (((int)strlen(line)+n) >= MAXSTRING) continue;
 	    cp = next_output(output, 0);
	    /*
	     * Squache new line.
	     */
	    line[(int)strlen(line) -1] = '\0';
	    line = concat(line, cp);
	}
        if(Tran_Scan(line, dir, files)) continue;
	XtFree(line);
	files++;
	if (dir && files > dir->n) {
	    fprintf(stderr, "Busted %d %d\n", files, dir->n);
	}
    }
    return files;
}

static int
scan_unix(output, dir)
struct _output *output;
struct _dirs *dir;
{
    char *cp, *nl;
    int  n = 0;
    int  mode;
    int  files = 0;
    char type[MAXSTRING+1];
    char owner[MAXSTRING+1];
    char group[MAXSTRING+1];
    int  size;
    int  links;
    char line[MAXSTRING+1];
    char month[MAXSTRING+1];
    char day[MAXSTRING+1];
    char time_year[MAXSTRING+1];
    char file[MAXSTRING+1];
    char linkname[MAXSTRING+1];
    char junk[MAXSTRING+1];
    char junk1[MAXSTRING+1];
    int  pass = 0;

    init_output(output);
    while(cp = next_output(output,0)) {
        if(pass > 20) {
            pass = 0;
            Update_Screen();
        }
        pass++;
        n = strlen(cp);
	if (n >= MAXSTRING) continue;
        if ((guessed == 1) && (appolo == 0)) {
	    if (cp[10] == '+') {
		appolo = 1;
	        n = sscanf(cp, "%11s %[^\n]", type , line);
                n = sscanf(line, "%s %s %s %s %s %s %s %s %s %s",
		                 owner, group, junk1, month, day, time_year, 
		       	       file, junk, linkname);
	        if(n == 5) appolo = 2;
                if (system_name) XtFree(system_name);
                if (system_type) XtFree(system_type);
                system_name = XtNewString("Appolo");
                system_type = XtNewString("Appolo");
		Set_Type_Name();
	        guessed = 3;
	    }
	}
	if (!appolo) {
	    n = sscanf(cp, "%10s%d %[^\n]", type, &links, line);
	    if (n != 3) continue;
	} else {
	    if (appolo == 1) {
	        n = sscanf(cp, "%11s%d %[^\n]",
		     type , &links, line);
	        if (n != 3) continue;
	    } if (appolo == 2) {
	        n = sscanf(cp, "%11s %[^\n]",
		     type, line);
		links = 0;
	        if (n != 2) continue;
	    }
	}
        if ((guessed < 3) && (ibm_rt == 0)) {
            n = sscanf(line, "%s %s %s %s %s %s %s %s %s %s",
		       owner, group, junk1, month, day, time_year, 
		       file, junk, linkname);
	    if (verify_mode(type) > 0) {
	        if (n == 6) ibm_rt++;
		if (n == 8 && strcmp(file, "->") == 0) ibm_rt++; 
	    }
	    if (ibm_rt) {
		if (guessed) {
                    if (system_name) XtFree(system_name);
                    if (system_type) XtFree(system_type);
                    system_name = XtNewString("IBM RT");
                    system_type = XtNewString("IBM RT");
		    Set_Type_Name();
		}
	    }
	    guessed = 3;
	}
	if (ibm_rt) {
            n = sscanf(line, "%s %d %s %s %s %s %s %s %s",
		       owner, &size, month, day, time_year, 
		       file, junk, linkname);
	    group[0] = '\0';
            if (n == 6) {
		linkname[0] = '\0';
	    } else {
	        if (type[0] != 'l' && type[0] != 'L') continue;
	        if (strcmp(junk, "->") != 0) continue;
	    }
	    if (n < 6) {
	        fprintf(stderr, "Could not parse ibm rt file entry %s\n", cp);
	        continue;
	    }
	} else if (appolo == 2) {
            n = sscanf(line, "%d %s %s %s %s %s %s %s",
		       &size, month, day, time_year, 
		       file, junk, linkname);
	    group[0] = '\0';
	    owner[0] = '\0';
            if (n == 5) {
		linkname[0] = '\0';
	    } else {
	        if (type[0] != 'l' && type[0] != 'L') continue;
	        if (strcmp(junk, "->") != 0) continue;
	    }
	    if (n < 5) {
	        fprintf(stderr, "Could not parse appolo file entry %s\n", cp);
	        continue;
	    }
	} else {
            n = sscanf(line, "%s %s %d %s %s %s %s %s %s",
		       owner, group, &size, month, day, time_year, 
		       file, junk, linkname);
            if (n == 7) {
		linkname[0] = '\0';
	    } else {
	        if (!((type[0] == 'l' || type[0] == 'L') &&
	             (strcmp(junk, "->") == 0))) {
                    n = sscanf(line, "%s %s %d %s %s %s %[^\n]",
		       owner, group, &size, month, day, time_year, 
		       file);
		    if (n != 7)  {
			continue;
		    }
		    linkname[0] = '\0';
		}
	    }
	    if (n < 6) continue;
	}

	if ((mode = verify_mode(type)) < 0) {
		fprintf(stderr, "Don't mode mode %s %s\n",type, file);
	        continue;
	}

	if (dir) {
	    dir->files[files].owner     = XtNewString(owner);
	    dir->files[files].group     = XtNewString(group);
	    dir->files[files].link      = links;
	    dir->files[files].size      = size;
	    dir->files[files].month     = XtNewString(month);
	    dir->files[files].day       = XtNewString(day);
	    dir->files[files].time_year = XtNewString(time_year);
	    dir->files[files].name      = XtNewString(file);
	    dir->files[files].linkname  = XtNewString(linkname);
	    dir->files[files].modes     = XtNewString(type);
	    dir->files[files].mode      = mode;
	    dir->files[files].local     = dir->files[files].name;
	    dir->files[files].remote    = dir->files[files].name;    
	    dir->files[files].time      = get_unix_time(day, time_year, month);
	    if ((mode & MODE_TYPE_DIR) || (mode & MODE_TYPE_LINK))
	      dir->files[files].dir_name  = dir->files[files].name;
	}
	files++;
	if (dir && files > dir->n) {
	    fprintf(stderr, "Busted %d %d\n", files, dir->n);
	}
    }
    return files;
}

static void
status(command, tag)
char *command;
int   tag;
{
    char *cp = NULL;

    Animate(Top);
    percent(NULL, NULL);
    cp = concat(cp,"Getting a directory list of ");
    cp = concat(cp, remote_dir);
    Set_Status(cp);
    XtFree(cp);
}

static int 
verify_mode(mode)
char *mode;
{
    int type = 0;

    if ((int)strlen(mode) < 10) return -1;

         if (mode[0] == 'd') type |= MODE_TYPE_DIR;
    else if (mode[0] == 'D') type |= MODE_TYPE_DIR;
    else if (mode[0] == '-') type |= MODE_TYPE_FILE;
    else if (mode[0] == 'F') type |= MODE_TYPE_FILE;
    else if (mode[0] == 'm') type |= MODE_TYPE_OFFLINE|MODE_TYPE_FILE;
    else if (mode[0] == 'l') type |= MODE_TYPE_LINK;
    else if (mode[0] == 'L') type |= MODE_TYPE_LINK;
    else if (mode[0] == 'c') type |= MODE_TYPE_CHAR;
    else if (mode[0] == 'C') type |= MODE_TYPE_CHAR;
    else if (mode[0] == 's') type |= MODE_TYPE_SOCK;
    else if (mode[0] == 'S') type |= MODE_TYPE_SOCK;
    else if (mode[0] == 'b') type |= MODE_TYPE_BLOCK;
    else if (mode[0] == 'B') type |= MODE_TYPE_BLOCK;

         if (mode[1] == 'r') type |= MODE_OWNER_READ;

         if (mode[2] == 'w') type |= MODE_OWNER_WRITE;
    else if (mode[2] == 's') type |= MODE_OWNER_WRITE;
    else if (mode[2] == 'S') type |= MODE_OWNER_WRITE;

         if (mode[3] == 'x') type |= MODE_OWNER_EXECUTE;

         if (mode[4] == 'r') type |= MODE_GROUP_READ;

         if (mode[5] == 'w') type |= MODE_GROUP_WRITE;
    else if (mode[5] == 's') type |= MODE_GROUP_WRITE;
    else if (mode[5] == 'S') type |= MODE_GROUP_WRITE;
         if (mode[6] == 'x') type |= MODE_GROUP_EXECUTE;

         if (mode[7] == 'r') type |= MODE_OTHER_READ;

         if (mode[8] == 'w') type |= MODE_OTHER_WRITE;
    else if (mode[8] == 's') type |= MODE_OTHER_WRITE;
    else if (mode[8] == 'S') type |= MODE_OTHER_WRITE;

         if (mode[9] == 'x') type |= MODE_OTHER_EXECUTE;
    else if (mode[9] == 't') type |= MODE_OTHER_EXECUTE;

    return type;
}
