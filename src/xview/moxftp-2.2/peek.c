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
"$Id: peek.c,v 1.1 1994/03/14 18:55:53 jones Exp $\n";
static char rcspath[] =
"$Source: /mintaka/u0/xx/ss/jones/jones.old/mftp.src/mftpnew/RCS/peek.c,v $\n";
#endif

/* $Log: peek.c,v $
 * Revision 1.1  1994/03/14  18:55:53  jones
 * Initial revision
 *
 */
#include "machine.h"
#include "defs.h"
#include <ctype.h>
#include "proto/peek.h"

extern LAST_RESPONSE;

#define SKIP_BLANKS(cp) {if (cp) while (isspace(*cp)) cp++; }
#define REMOVE_TRAILING_BLANKS(cp) \
        {\
            if (cp) { \
                char *c; \
                c = cp + (int)strlen(cp) - 1; \
                while(isspace(*c) && c > cp) c--; \
                c++; \
                *c = '\0'; \
            }\
        }


char *h_ascii[] = {
"Set default transfer mode to ascii\n",
"SYNTAX:  ascii\n",
NULL,
};

char *h_binary[] = {
"Set default transfer mode to binary\n",
"SYNTAX:  binary\n",
NULL,
};


char *h_cd[] = {
"Change remote current working directory\n",
"SYNTAX:  cd <directory name>\n",
NULL,
};

char *h_image[] = {
"Set default transfer mode to binary\n",
"SYNTAX:  image\n",
NULL,
};

char *h_lcd[] = {
"Change local working directory\n",
"SYNTAX:  lcd <directory name>\n",
NULL,
};

char *h_ls[] = {
"List contents of remote directory\n",
"SYNTAX:  ls <paramsters>\n",
NULL,
};

char *h_get[] = {
"Get remote file\n",
"SYNTAX:   get <remote file>\n",
"           or \n",
"SYNTAX:   get <remote file> <local file>\n",
NULL,
};

char *h_regget[] = {
"Get multiple files based on regular expression\n",
"SYNTAX:   regget <regular expression>\n",
NULL,
};

char *h_put[] = {
"Put local file\n",
"SYNTAX:   put <local file>\n",
"           or \n",
"SYNTAX:   put <local file> <remote file>\n",
NULL,
};

char *h_regput[] = {
"Put multiple files based on regular expression\n",
"SYNTAX:   regput <regular expression>\n",
NULL,
};

char *h_reg[] = {
"Select files based on regular exprsions\n",
"SYNTAX:   reg <regular expression>\n",
NULL,
};

char *h_help[] = {
"Print help text\n",
"SYNTAX:   help <command>\n",
NULL,
};

char *h_dir[] = {
"List contents of remote directory\n",
"SYNTAX:  dir\n",
NULL,
};

char *h_pwd[] = {
"Print working directory on remote machine\n",
NULL,
};

char *h_lpwd[] = {
"Print working directory on local machine\n",
NULL,
};

char *h_lmkdir[] = {
"Make a local direoctry\n",
"SYNTAX:   lmkidr <local directory name>\n",
NULL,
};

char *h_mkdir[] = {
"Make a remote  directory\n",
"SYNTAX:  lmkidr <remote directory name>\n",
NULL,
};


char *h_tenex[] = {
"Set default transfer mode to tenex\n",
"SYNTAX:  tenex\n",
NULL,
};

char *h_delete[] = {
NULL,
};

char *h_quote[] = {
"Send an arbitrary ftp command\n",
"SYNTAX:  quote <command>\n",
NULL,
};

char *h_site[] = {
"Send site-specific ommand\n",
"SYNTAX:  site <command>\n",
"You might want to try - site help\n",
"The may produce a list of site coimmands\n",
NULL,
};

char *h_rh[] = {
"Get remote help form server\n",
"SYNTAX:   remotehelp\n",
"             or \n",
"SYNTAX:   remotehelp <remote ftp command>\n",
NULL,
};

struct _peek_list {
	char  *name;
	int  (*next)();
	int   value;
        void (*start)();
	void (*finish)();
	void (*callback)();
	char **help;
} peek_list[] = {
{ "ascii"  , params,  0, peek_ascii  , NULL       , cb_prompt,     h_ascii},
{ "binary" , params,  0, peek_binary , NULL       , cb_prompt,     h_binary},
{ "dir"    , NULL  ,  0, peek_dir    , peek_finish, set_remote_cb, h_dir},
{ "cd"     , params,  1, peek_cd     , NULL       , cb_pwd,        h_cd},
{ "help"   , NULL  ,  0, peek_help   , NULL       , NULL,          h_help},
{ "image"  , params,  0, peek_image  , NULL       , cb_prompt,     h_image},
{ "get"    , params, -1, peek_get    , NULL       , cb_prompt,     h_get},
{ "reg"    , params,  1, peek_reg    , NULL       , cb_prompt,     h_reg},
{ "regget" , NULL  ,  0, peek_regget , NULL       , cb_prompt,     h_regget},
{ "regput" , NULL  ,  0, peek_regput , NULL       , cb_prompt,     h_regput},
{ "lcd"    , NULL  ,  0, peek_lcd    , NULL       , cb_ldir,       h_lcd},
{ "ls"	   , NULL  ,  0, peek_ls     , finish_ls  , cb_prompt,     h_ls},
{ "lmkdir" , params,  1, peek_lmkdir , NULL	  , set_local_cb,  h_lmkdir},
{ "lpwd"   , NULL  ,  0, peek_lpwd   , NULL       , set_local_cb,  h_lpwd},
{ "mkdir"  , params,  1, peek_mkdir  , NULL	  , set_remote_cb, h_mkdir},
{ "put"    , params, -1, peek_put    , NULL       , cb_prompt,     h_put},
{ "pwd"    , NULL  ,  0, peek_pwd    , NULL       , set_remote_cb, h_pwd},
{ "quote",   NULL  ,  0, peek_quote  , peek_finish, cb_prompt,     h_quote},
{ "remotehelp",
	     NULL  ,  0, peek_rhelp  , peek_finish, cb_prompt,     h_rh},
{ "site"   , NULL  ,  0, peek_site   , peek_finish, cb_prompt,     h_site},
{ "tenex"  , params,  0, peek_tenex  , NULL       , cb_prompt,     h_tenex},
};

#define PEEK_NUM (sizeof(peek_list)/sizeof(struct _peek_list))

#define LS_COMMAND "NLST"

void
Lower(string)
char *string;
{
   char *cp = string;

   if (!cp) return;
   while(*cp) {
	if (isupper(*cp)) *cp = tolower(*cp);
	cp++;
   }
}

char *
Next_Token(string, token)
char *string;
char *token;
{
   char *start = string;
   char *end;
   char *cp;
   int  n;

   token[0] = '\0';
   
   SKIP_BLANKS(start)
   if (!start || !*start)  return NULL;
  
   end = start;
   while (*end) if(!isspace(*end))  end++; else break;
   n = end - start;
   strncpy(token, start, n);
   token[n] = '\0';
   cp = token;
   return end;
}

void
Peek(string)
char *string;
{
   char *cp = string;
   char *end;
   char token[10000];
   char s[1000];
   int  i;
   
   if (!connected) {
        Add_Status_Text("Not logged in!\n");
        Add_Status_Text("Command ignored\n");
	Do_Prompt(NULL, 1);
	return;
   }
   end = INDEX(cp, '\n');
   if (end) *end = '\0';
   cp = Next_Token(cp, token);
   Lower(token);
   if (token[0]) {
	for(i=0; i<(sizeof(peek_list)/sizeof(struct _peek_list)); i++) 
	    if (strncmp(peek_list[i].name, token, strlen(peek_list[i].name)) 
		== 0) break;
	if (i != sizeof(peek_list)/sizeof(struct _peek_list)) {
	    if (peek_list[i].next) {	
		if(peek_list[i].next(token, cp, peek_list[i].value)) {
    		    Do_Prompt(NULL, 1);
		    return;
		}
	    }
            peek_list[i].start(FTP_ECHO|FTP_PROMPT_NO|FTP_COMMAND_NO, token,
			       cp, peek_list[i].finish, 0,
		               peek_list[i].callback, 0); 
        } else {
   	  Add_Status_Text("Invalid command - ");
   	  Add_Status_Text(token);
   	  Add_Status_Text("\n");
	  Do_Prompt(NULL, 1);
	  return;
	}
   }  else {
	Do_Prompt(NULL, 1);
	return;
   }
   return;
}

static void
cb_dir(data, i, cb)
DATA data;
int i;
CB   *cb;
{
    int stuff = (int)data;

    if (ftp_response[i].status & FTP_ERROR) {
	Do_Prompt(NULL, 1) ;
	return;
    }
    Start_Remote_Dir(0, 0, cb_set_dir_type,  (DATA)REMOTE, NULL);
}

static void
cb_ldir(data, i, cb)
DATA data;
int i;
CB   *cb;
{
    int stuff = (int)data;

    if (ftp_response[i].status & FTP_ERROR) {
	Do_Prompt(NULL, 1) ;
	return;
    }
    Start_Local_Dir(0, 0, cb_set_dir_type,  (DATA)LOCAL, NULL);
}

static void 
cb_prompt(data, i, cb)
DATA data;
int i;
CB   *cb;
{
    Do_Prompt(NULL, 1) ;
}

static void
cb_pwd(data, i, cb)
DATA data;
int i;
CB   *cb;
{
    int stuff = (int)data;

    if (ftp_response[i].status & FTP_ERROR) {
	Do_Prompt(NULL, 1) ;
	return;
    }
    Start_Pwd(0, 1, cb_dir, (DATA)0, NULL);
}

static void
cb_set_dir_type(data, i, cb)
DATA data;
int i;
CB   *cb;
{
    int stuff = (int)data;

    if (!(ftp_response[i].status & FTP_ERROR)) {
        Set_Dir_Type(stuff);
    }
    Do_Prompt(NULL, 1) ;
}

static void
cb_set_mode(data, i, cb)
DATA data;
int i;
CB  *cb;
{
    int stuff = (int)data;
    extern char *ftp_type;
    extern int default_transfer_mode;

    if (!(ftp_response[i].status & FTP_ERROR)) {
	if (ftp_type) XtFree(ftp_type);
	if (stuff == 0) {
	    default_transfer_mode = MODE_T_BINARY;
	} else {
	    default_transfer_mode = MODE_T_ASCII;
        }
	Set_Info();
    }
    Do_Prompt(NULL, 1) ;
}

static void
do_help_command(command)
char *command;
{
    int i;
    char **cp;

    for (i=0; i<PEEK_NUM;i++) {
	if (strcmp(command, peek_list[i].name) == 0) {
	    cp = peek_list[i].help;
           Add_Status_Text("Help for command: ");
           Add_Status_Text(command);
           Add_Status_Text("\n");
	    while (*cp != NULL) {
		Add_Status_Text(*cp);
		cp++;
	    }
	    return;
	}
    }
    Add_Status_Text("No help availabele on ");
    Add_Status_Text(command);
    Add_Status_Text("\n");
}

static void
do_help_list()
{
    char  *buff;
    char  format[15];
    int i, n, l, m;

    n = 0;
    for (i=0; i<PEEK_NUM;i++) {
	l = strlen(peek_list[i].name);
	if (l > n) n = l;
    }
    m = 80/(n+1);

    sprintf(format, "%%-%d.%ds ",n, n);
    buff = XtMalloc(sizeof(format) + n + 2);
    Add_Status_Text("Command may be abbreviated.  Cmmands are:\n");
    for (i=0; i<PEEK_NUM;i++) {
	if (i%m == 0) Add_Status_Text("\n");
	sprintf(buff, format, peek_list[i].name);
	Add_Status_Text(buff);
    }
    Add_Status_Text("\n");
    XtFree(buff);
}

static void 
finish_ls(output, tag, data)
struct _output *output;
int  tag;
DATA data;
{
    struct _output *next;
    int i;
    int code;

    code =  ftp_status(output);
    next = output;

    while(next) {
        for (i=0; i<next->lines; i++) {
            Add_Status_Text(next->output[i]);
        }
        next = next->next;
    }
    do_callbacks(code, (CB *)data);
}

static int
params(command, string, n)
char *command;
char *string;
int   n;
{
    char *cp = string;
    char token[10000];
    int  i = 0;

    while(1) {
 	cp = Next_Token(cp, token);
	if (!*token) break;
	i++;
    }
    if (n < 0) {
	if ( i >= -n) return 0;
	if (-n == 1)  {
            sprintf(token, "ftp %s requires at leat one paramater\n",command);
	} else {
            sprintf(token, "ftp %s requires at least %d paramaters\n",
		    command,-n);
	}
    } else {
	if (n == i) return 0;
        if (n == 1) {
            sprintf(token, "ftp %s command takes one paramater\n",command);
        } else {
            sprintf(token, "ftp %s command takes %d paramaters\n",command,n);
        }
    }
    Add_Status_Text(token);
    return 1;
}

static void
peek_ascii(echo, command, params, finish, tag, callback, stuff)
int    echo;
char  *command;
char  *params;
void (*finish)();
int    tag;
void (*callback)();
int    stuff;
{
    Start_Type(echo, "a", 1, callback, (DATA)stuff, NULL);

}

static void
peek_binary(echo, command, params, finish, tag, callback, stuff)
int    echo;
char  *command;
char  *params;
void (*finish)();
int    tag;
void (*callback)();
int    stuff;
{
    Start_Type(echo, "b", 1, callback, (DATA)stuff, NULL);
}

static void
peek_cd(echo, command, params, finish, tag, callback, stuff)
int    echo;
char  *command;
char  *params;
void (*finish)();
int    tag;
void (*callback)();
int    stuff;
{
    char *dirname;
    struct _dirs *dir;
    int i;

    dirname  = XtMalloc(strlen(params));
    (void)Next_Token(params, dirname);

    /*
     * symbolic link hack.
     */
    if (strcmp(dirname, "..") == 0) {
	dir = Find_Dir(remote_dir, hostname, 0);
	if (dir && dir->up) {
	     Start_Cd(echo, dir->up->pwd_name, 0, callback, (DATA)stuff, NULL);
	     dir->up = NULL;
	} else {
	     Start_Cd(echo, dirname, 0, callback, (DATA)stuff, NULL);
	}
    } else {
	dir = Find_Dir(remote_dir, hostname, 0);
        dir->link_up = 0;
	for (i=0; i<dir->n; i++) {
 	    if (strcmp(dirname, dir->files[i].name) == 0 &&
	        dir->files[i].mode & MODE_TYPE_LINK) {
                dir->link_up++;
	    }
        }
        Start_Cd(echo, dirname, 0, callback, (DATA)stuff, NULL);
    }
    XtFree(dirname);
}

static void
peek_dir(echo, command, params, finish, tag,  callback, stuff)
int    echo;
char  *command;
char  *params;
void (*finish)();
int    tag;
void (*callback)();
int    stuff;
{
    Start_Remote_Dir(echo, 1, callback, (DATA)0, NULL);
}

static void
peek_finish(output, tag, data)
struct _output *output;
int  tag;
DATA data;
{
    int code;

    code =  ftp_status(output);
    do_callbacks(code, (CB *)data);
}

static void
peek_get(echo, command, params, finish, tag, callback, stuff)
int    echo;
char  *command;
char  *params;
void (*finish)();
int    tag;
void (*callback)();
int    stuff;
{
    char *remote_file = NULL;
    char *local_file  = NULL;
    char *cp = params;


    local_file  = XtMalloc(strlen(cp));
    remote_file = XtMalloc(strlen(cp));
   
    cp = Next_Token(cp, remote_file);
    cp = Next_Token(cp, local_file);
 
    Start_Get(echo, remote_file, local_file, 0, callback, (DATA)stuff, NULL);
    XtFree(remote_file);
    XtFree(local_file);
}

static void
peek_help(echo, command, params, finish, tag, callback, stuff)
int    echo;
char  *command;
char  *params;
void (*finish)();
int    tag;
void (*callback)();
int    stuff;
{
    char *what;

    what = XtMalloc(strlen(params));
    (void)Next_Token(params, what);
    if (what[0] == '\0') {
	do_help_list();
    } else {
	do_help_command(what);
    }
    XtFree(what);
    Do_Prompt(NULL, 1);
}

static void
peek_image(echo, command, params, finish, tag, callback, stuff)
int    echo;
char  *command;
char  *params;
void (*finish)();
int    tag;
void (*callback)();
int    stuff;
{
    Start_Type(echo, "b", 1, callback, (DATA)stuff, NULL);
}

static void
peek_lcd(echo, command, params, finish, tag, callback, stuff)
int    echo;
char  *command;
char  *params;
void (*finish)();
int    tag;
void (*callback)();
int    stuff;
{
    SKIP_BLANKS(params);
    REMOVE_TRAILING_BLANKS(params);
    Start_Lcd(echo, params, callback, (DATA)stuff, NULL);
}

static void
peek_lmkdir(echo, command, params, finish, tag, callback, stuff)
int    echo;
char  *command;
char  *params;
void (*finish)();
int    tag;
void (*callback)();
int    stuff;
{
    char *dir;
    CB *cbp;
    char *cp = NULL;
    int code = 1;

    cbp = link_callback(NULL, callback, (DATA)1, NULL);

    dir = XtMalloc(strlen(params));
    (void)Next_Token(params, dir);

    if (mkdir(dir, 0770) == -1) {
	cp = concat(cp, "Could not create local direcotry: ");
	cp = concat(cp, dir);
 	Add_Status_Text(cp);
        Add_Status_Text("\n");
	code = 0;
	XtFree(dir);
    }
    XtFree(dir);
    do_callbacks(code, cbp);
}

static void
peek_lpwd(echo, command, params, finish, tag, callback, stuff)
int    echo;
char  *command;
char  *params;
void (*finish)();
int    tag;
void (*callback)();
int    stuff;
{
    char  pathname[1000];
    char  *path;
    char  *cp = NULL;
    CB *cbp;

#if defined(USE_GETWD)
    path = (char *)getwd(pathname);
#else
    path = (char *)getcwd(pathname, sizeof(pathname));
#endif
    cp = concat(cp, "Local directory: ");
    cp = concat(cp, path);
    Add_Status_Text(cp);
    Add_Status_Text("\n");
    XtFree(cp);

    cbp = link_callback(NULL, callback, (DATA)0, NULL);
    do_callbacks(1, cbp);
}

static void
peek_ls(echo, command, params, finish, tag, callback, stuff)
int    echo;
char  *command;
char  *params;
void (*finish)();
int    tag;
void (*callback)();
int    stuff;
{
    char *cp = NULL;
    CB   *cbp;

    SKIP_BLANKS(params)
    REMOVE_TRAILING_BLANKS(params);
    cp = concat(cp, LS_COMMAND);
    if (params && *params != '\0') {
        cp = concat(cp, " ");
        cp = concat(cp, params);
    }
    cbp = link_callback(NULL, callback, (DATA)stuff, NULL);
    Start_Listen(cp, NULL, finish, 0, echo, 0, cbp, NULL, 0);
    XtFree(cp);
}

static void
peek_mkdir(echo, command, params, finish, tag, callback, stuff)
int    echo;
char  *command;
char  *params;
void (*finish)();
int    tag;
void (*callback)();
int    stuff;
{
    char *dir;

    dir = XtMalloc(strlen(params));
    (void)Next_Token(params, dir);
    Start_Mkdir(echo, dir,  callback, (DATA)1, NULL);
    XtFree(dir);
}

static void
peek_put(echo, command, params, finish, tag, callback, stuff)
int    echo;
char  *command;
char  *params;
void (*finish)();
int    tag;
void (*callback)();
int    stuff;
{
    char *remote_file = NULL;
    char *local_file  = NULL;
    char *cp = params;


    local_file  = XtMalloc(strlen(cp));
    remote_file = XtMalloc(strlen(cp));
   
    cp = Next_Token(cp, local_file);
    cp = Next_Token(cp, remote_file);
 
    Start_Put(echo, remote_file, local_file, 0, callback, (DATA)stuff, NULL);
    XtFree(remote_file);
    XtFree(local_file);
}

static void
peek_pwd(echo, command, params, finish, tag, callback, stuff)
int    echo;
char  *command;
char  *params;
void (*finish)();
int    tag;
void (*callback)();
int    stuff;
{
    Start_Pwd(echo, NULL,  callback, (DATA)0, NULL);
}

static void
peek_quote(echo, command, params, finish, tag, callback, stuff)
int    echo;
char  *command;
char  *params;
void (*finish)();
int    tag;
void (*callback)();
int    stuff;
{
    CB *cbp;
     
    SKIP_BLANKS(params);
    if (params == NULL || params[0] == ' ' ) return;
    cbp = link_callback(NULL, callback, (DATA)stuff, NULL);
    write_ftp(params, NULL,  finish, NULL, 0, echo, (DATA)cbp);
}

static void
peek_reg(echo, command, params, finish, tag, callback, stuff)
int    echo;
char  *command;
char  *params;
void (*finish)();
int    tag;
void (*callback)();
int    stuff;
{
    CB *cbp;

    SKIP_BLANKS(params)
    REMOVE_TRAILING_BLANKS(params);
    Clear_Files_Marks();
    Glob_Files(params, 1);
    cbp = link_callback(NULL, callback, (DATA)stuff, NULL);
    do_callbacks(1, cbp);
}

static void
peek_regget(echo, command, params, finish, tag, callback, stuff)
int    echo;
char  *command;
char  *params;
void (*finish)();
int    tag;
void (*callback)();
int    stuff;
{
    CB *cbp;

    SKIP_BLANKS(params)
    REMOVE_TRAILING_BLANKS(params);
    Clear_Files_Marks();
    Glob_Files(params, 1);
    cbp = link_callback(NULL, callback, (DATA)stuff, NULL);
    Do_List(Start_Get, cbp);
}

static void
peek_regput(echo, command, params, finish, tag, callback, stuff)
int    echo;
char  *command;
char  *params;
void (*finish)();
int    tag;
void (*callback)();
int    stuff;
{
    CB *cbp;

    SKIP_BLANKS(params)
    REMOVE_TRAILING_BLANKS(params);
    Clear_Files_Marks();
    Glob_Files(params, 1);
    cbp = link_callback(NULL, callback, (DATA)stuff, NULL);
    Do_List(Start_Put, cbp);
}

static void
peek_rhelp(echo, command, params, finish, tag, callback, stuff)
int    echo;
char  *command;
char  *params;
void (*finish)();
int    tag;
void (*callback)();
int    stuff;
{
    char *help = NULL;
    CB *cbp;

    cbp = link_callback(NULL, callback, (DATA)stuff, NULL);
    help = concat(help, "HELP");
    help = concat(help, params);
    write_ftp(help, NULL, finish, NULL, 0, echo, (DATA)cbp);
    XtFree(help);
}

static void
peek_site(echo, command, params, finish, tag, callback, stuff)
int    echo;
char  *command;
char  *params;
void (*finish)();
int    tag;
void (*callback)();
int    stuff;
{
    CB *cbp;
    char *site = NULL;

    SKIP_BLANKS(params);
    REMOVE_TRAILING_BLANKS(params);
    site = concat(site, "SITE");
    if (params && params[0] != '\0') {
        site = concat(site, " ");
        site = concat(site, params);
    }
    cbp = link_callback(NULL, callback, (DATA)stuff, NULL);
    write_ftp(site, NULL,  finish, NULL, 0, echo, (DATA)cbp);
}

static void
peek_tenex(echo, command, params, finish, tag, callback, stuff)
int    echo;
char  *command;
char  *params;
void (*finish)();
int    tag;
void (*callback)();
int    stuff;
{
    Start_Type(echo, "t", 1, callback, (DATA)stuff, NULL);
}

static void
set_local_cb(data, code, cb)
DATA data;
int code;
CB  *cb;
{
    struct _dirs *dir;

    if (!(ftp_response[code].status & FTP_ERROR)) {
	if ((int)data) {
            dir = Find_Dir(local_dir, localhost, 0);
	    if (dir) dir->update++;
	}
	Set_Dir_Type(LOCAL);
    }
    Do_Prompt(NULL, 1) ;

}

static void
set_remote_cb(data, code, cb)
DATA data;
int code;
CB  *cb;
{
    struct _dirs *dir;
  
    if (!(ftp_response[code].status & FTP_ERROR) &&
	!(ftp_response[code].status & FTP_ERROR_OK)) {
	if ((int)data) {
	    dir = Find_Dir(remote_dir, hostname, 0);
	    dir->update++;
	}
  	Set_Dir_Type(REMOTE);
    }
    Do_Prompt(NULL, 1) ;

}
