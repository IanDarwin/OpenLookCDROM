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
"$Id: translate.c,v 1.1 1994/03/14 18:55:53 jones Exp $\n";
static char rcspath[] =
"$Source: /mintaka/u0/xx/ss/jones/jones.old/mftp.src/mftpnew/RCS/translate.c,v $\n";
#endif

/* $Log: translate.c,v $
 * Revision 1.1  1994/03/14  18:55:53  jones
 * Initial revision
 *
 */


#include "machine.h"
#include "defs.h"
#include <ctype.h>
#include <pwd.h>
#include "view_progs.h"

int        NO_TRAN = 0;
int        TRAN    = 0;

struct _translate translate[NUM_TRAN];

struct _translate *s_tran = NULL;
struct _translate *s_tran_files = NULL;
/*
 * Default viewer table.
 */
struct _viewers viewers[] = {
	{NONE       ,TEXT_PROG},
	{TEXT       ,TEXT_PROG},
	{PICTURE    ,PICTURE_PROG},
	{PS         ,PS_PROG},
	{AUDIO	    ,AUDIO_PROG},
	{TAR        ,TAR_PROG},
};

FILE *input;

#if defined(XAW)
char *name[] =  {	
    ".xftp",
    ".mftp",
    ".oftp",
    NULL,
};
#endif
#if defined(MOTIF)
char *name[] =  {	
    ".mftp",
    ".xftp",
    ".oftp",
    NULL,
};
#endif
#if defined(OPENWINDOW)
char *name[] =  {	
    ".oftp",
    ".xftp",
    ".mftp",
    NULL,
};
#endif



#define START_MASK   010000
#define STATE_NULL   000001
#define STATE_TRAN   000002
#define STATE_USER   000004
#define START_USER  (STATE_USER|START_MASK)
#define START_TRAN  (STATE_TRAN|START_MASK)
#define STATE_U_T   (STATE_TRAN|STATE_USER)

/*
 * The nonuix system hack.
 * This table has grown I have found more nonunix system.
 * I have tried to generalize it so that any directorive 
 * can be applied to any system.
 */
#define SET_NAME             0
#define SET_GUESS            1
#define SET_TYPE             2
#define SET_FILE             3
#define SET_SIZE             4
#define SET_DATE             5
#define SET_TIME             6
#define SET_OWNER            7
#define SET_PROT             8
#define SET_DIR              9
#define SET_UNIX            10
#define SET_BACK            11
#define SET_P1              12
#define SET_P2              13
#define SET_ISDIR           14
#define SET_REMOTE          15
#define SET_LOCAL           16
#define SET_UP              17
#define SET_GUESS_CWD       18
#define SET_LONG            19
#define SET_MEDIUM          20
#define SET_INDENT          21
#define SET_REWRITE         22
#define SET_INIT            23
#define SET_ISUNIX          24
#define SET_NOTYPE          25
#define SET_REMOTE_EXAMPLES 26
#define SET_UNIX_EXAMPLES   27
#define SET_NOTE            28
#define SET_USE_OTHER       29
#define SET_CONVERT_SIZE    30
#define SET_MODE	    31
#define SET_DIR_NAME        32
#define SET_NO_PWD          33
#define SET_UP_EXP	    34
#define SET_CD_EXP	    35
#define SET_SYSTEM_NOTE     36
#define SET_IS_RT           37
#define SET_IS_APOLLO	    38
#define SET_VIEWER	    39
#define SET_END             40

struct _parse {
    char  *directive;
    int    state;
    int    type;
} parse[] =  {
  { "translation", START_TRAN, SET_NAME  },
  { "trans",       START_USER, SET_NAME  },
  { "guess_cwd",   STATE_TRAN, SET_GUESS_CWD},
  { "guess",	   STATE_TRAN, SET_GUESS },
  { "name",        STATE_TRAN, SET_FILE  },
  { "size",        STATE_TRAN, SET_SIZE  },
  { "date",        STATE_TRAN, SET_DATE  },
  { "time",        STATE_TRAN, SET_TIME  },
  { "owner",	   STATE_TRAN, SET_OWNER },
  { "prot",	   STATE_TRAN, SET_PROT  },
  { "p1",	   STATE_TRAN, SET_P1    },
  { "p2",	   STATE_TRAN, SET_P2    },
  { "up_exp",	   STATE_TRAN, SET_UP_EXP},
  { "up",	   STATE_TRAN, SET_UP    },
  { "unix", 	   STATE_U_T,  SET_UNIX  },
  { "back", 	   STATE_U_T,  SET_BACK  },
  { "type", 	   STATE_TRAN, SET_TYPE  },
  { "end",	   STATE_U_T,  SET_END   },
  { "isdir",	   STATE_TRAN, SET_ISDIR },
  { "remote",	   STATE_TRAN, SET_REMOTE},
  { "rewrite",	   STATE_TRAN, SET_REWRITE},
  { "local",	   STATE_TRAN, SET_LOCAL},
  { "indent",	   STATE_TRAN, SET_INDENT},
  { "listing_l",   STATE_TRAN, SET_LONG},
  { "listing_m",   STATE_TRAN, SET_MEDIUM},
  { "init",        STATE_TRAN, SET_INIT},
  { "isunix",      STATE_TRAN, SET_ISUNIX},
  { "isibmrt",	   STATE_TRAN, SET_IS_RT},
  { "isapollo",	   STATE_TRAN, SET_IS_APOLLO},
  { "notype",      STATE_TRAN, SET_NOTYPE},
  { "examples_u",  STATE_U_T,  SET_UNIX_EXAMPLES}, 
  { "examples_r",  STATE_U_T,  SET_REMOTE_EXAMPLES},
  { "note",        STATE_NULL, SET_NOTE},
  { "use_other",   STATE_TRAN, SET_USE_OTHER},
  { "convert_size",STATE_TRAN, SET_CONVERT_SIZE},
  { "mode",        STATE_TRAN, SET_MODE},
  { "no_pwd",      STATE_TRAN, SET_NO_PWD},
  { "dir_name",    STATE_TRAN, SET_DIR_NAME},
  { "dir",	   STATE_TRAN, SET_DIR   },
  { "cd_exp",	   STATE_TRAN, SET_CD_EXP},
  { "system_note", STATE_TRAN, SET_SYSTEM_NOTE},
  { "viewer",      STATE_NULL, SET_VIEWER}
};

#define MAXPARSE (sizeof(parse)/sizeof(struct _parse))

struct _names {
	char *name;
	int   value;
} names[] = {
{"name" , FILE_NAME },
{"size" , FILE_SIZE },
{"date" , FILE_DATE },
{"time" , FILE_TIME },
{"owner", FILE_OWNER},
{"prot" , FILE_PROT },
{"group", FILE_GROUP},
{"p1"   , FILE_P1   },
{"p2"   , FILE_P2   },
};

#define MAXNAMES (sizeof(names)/sizeof(struct _names))

#include "proto/translate.h"

static struct _translate *t;
int    map_index;
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



char   *example = NULL;

void
create_e (e, regexpp, source, MAP, mode)
char **e;
regexp **regexpp;
char   **source;
int    MAP;
int    *mode;
{
    char   *s;
    char   *x,*y;
    char   format1[100];
    char   format2[100];
    int i,j,k,n,l1,l2;



    l1 = 0;
    for (i = 0; i<N_E; i++) {
	if (e[i] == NULL) break;
        if ((n = strlen(e[i])) > l1) l1 = n;
    }

    for (i = 0; i<N_E; i++) {
	if (e[i] == NULL) break;
        if ((n = strlen(e[i])) > l2) l2 = n;
    }
    sprintf(format1, "%%-%ds -> ", l1);
    sprintf(format2, "%%-%ds ", l1);
    for (i = 0; i<N_E; i++) {
        char *x;

	if (e[i] == NULL) break;
        x = XtNewString(e[i]);
        if(MAP == UPPER) upper(x);
        if(MAP == LOWER) Lower(x);
        for (j=0; j<N_T; j++) {
            if(regexpp[j] &&
 		regexec(regexpp[j], x)) {
		y = (char *)XtMalloc(strlen(source[j])+100);
		regsub(regexpp[j], source[j], y);
	        s = (char *)XtMalloc(strlen(y)+strlen(format1)+5);
		sprintf(s, format1, e[i]);
		example = concat(example, s);
		XtFree(s);
	        s = (char *)XtMalloc(strlen(y)+strlen(format1)+5);
		sprintf(s, format2, y);
		example = concat(example, s);
		XtFree(y);
		XtFree(s);
		if (mode[j]) {
		    if (mode[j]&MODE_TYPE_ASCII) {
		       example = concat(example, "(Transfer mode ASCII)");
		    } else if (mode[j]&MODE_TYPE_BINARY) {
		       example = concat(example, "(Transfer mode BINARY)");
		    }
                }
		example = concat(example, "\n");
		break;
	    }
	}
	XtFree(x);
    }
}

static tran_index = 0;

char *
Compute_Cd(dir)
char *dir;
{
    char *cp, *cp1;

    if (s_tran && s_tran->cd_exp && regexec(s_tran->cd_regexp, remote_dir)) {
        cp = (char *)XtMalloc(2000);
        regsub(s_tran->cd_regexp, s_tran->cd_source, cp);
        cp1 = (char *)XtMalloc(2000);
        sprintf(cp1, cp, dir);
        XtFree(cp);
        return cp1;
    } else {
        cp = XtNewString(dir);
        return cp;
    }
}

char *
Create_Translation_Examples()
{
    char **e;
    regexp **regexpp;
    char   **source;
    int    *mode;

    if (example) example = NULL;

    if (!s_tran_files ||
       ((!s_tran_files->unix_examples || !s_tran_files->file_to_unix_regexp) &&
       (!s_tran_files->remote_examples ||!s_tran_files->file_to_other_regexp))) 
        return XtNewString("No translation examples");

    e = s_tran_files->remote_examples;
    regexpp = s_tran_files->file_to_unix_regexp;
    source = s_tran_files->file_to_unix_source;
    mode   = s_tran_files->file_to_unix_mode;
    if (e && e[0] && regexpp && regexpp[0]) {
	example = concat(example, 
		         "Remote to unix file tranlations examples\n\n");
	create_e(e, regexpp, source, s_tran_files->local, mode);
	example = concat(example, "\n");
    }

    e = s_tran_files->unix_examples;
    regexpp = s_tran_files->file_to_other_regexp;
    source = s_tran_files->file_to_other_source;
    mode   = s_tran_files->file_to_other_mode;
    if (e && e[0] && regexpp && regexpp[0]) {
	example = concat(example, 
		         "Unix to remote file tranlations examples\n\n");
	create_e(e, regexpp, source, s_tran_files->remote, mode);
    }
    return example;

}

void
Init_Dotxftp()
{

    char *cp;
    int  i = 0;
    int  found = 0;
    struct passwd *pw;
    char *line;

    pw =  getpwuid(getuid());
    
    if (pw) {
        line = (char *)XtMalloc((int)strlen(pw->pw_dir)+10);
        while(name[i] && pw && !found)  {
            sprintf(line,"%s/%s", pw->pw_dir, name[i]);
            input = fopen(line,"r");
            if (input != NULL) {
		read_init(get_next, 0);
		fclose(input);
		input = NULL;
		found++;
	    	break;
            }	
            i++;
	}
 	XtFree(line);
    }
    input = fopen("dotxftp","r");
    if (input != NULL) {
        read_init(get_next, 0);
        fclose(input);
    }	
    read_init(get_next_tran, 1);
    merge_use_other();
}

void
Init_Tranlation_List(w)
Widget w;
{
    int i;
    char *list = NULL;
    int max = 0;
    int n;
    char format[100];
    char line[1000];
    int  work_in_progress = 0;
    XrmDatabase dbase;
  
    max = strlen("System Name");
    for (i=0; i< NO_TRAN; i++) {
	if ((n = (int)strlen(translate[i].system)) > max) max = n;
    }
    sprintf(format, "%%-%ds    %%s\n", max);
    list = concat(list, "Operating Systems\n");
    list = concat(list, Ftpname);
    list = concat(list, " understands the following operating systems:\n");
    list = concat(list, "\n");
    for (i=0; i< NO_TRAN; i++) {
	sprintf(line, format, translate[i].system, translate[i].system_note);
        list = concat(list, line);
        if (INDEX(line, '*') != NULL) work_in_progress++; 
    }
    if (work_in_progress) {
	list = concat(list, "\n");
	list = concat(list, "\
A * by an entry means that the tranlations table for this system is\n\
under construction.\n\
");
    }
    dbase = XtDatabase(XtDisplay(w));
    XrmPutStringResource(&dbase, "*system_list.help_text", list);
    XtFree(list);
}

char *
Tran_Remote_File(xx, mode)
char *xx;
int  *mode;
{
    int j;
    char *remote, *y;

    if (!s_tran_files) {
	if(remote_type == REMOTE_SYSTEM_UNIX) {
            remote = XtNewString(xx);
	    return remote;
	}
	return NULL;
    }

    remote = XtNewString(xx);
    if(s_tran_files->remote == UPPER) upper(remote);
    if(s_tran_files->remote == LOWER) Lower(remote);
    for (j=0; j<N_T; j++) {
    	if(s_tran_files->file_to_other_regexp[j] &&
	   regexec(s_tran_files->file_to_other_regexp[j], remote)) {
	    y = (char *)XtMalloc(2000);
	    regsub(s_tran_files->file_to_other_regexp[j], 
		   s_tran_files->file_to_other_source[j], y);
	    if (!*y) {
	        XtFree(y);
	        XtFree(remote);
		return NULL;
	    }
            remote = XtNewString(y);
	    *mode |= s_tran_files->file_to_other_mode[j];
	    XtFree(y);
	    return remote;
	}
    }
    return NULL;
}

int
Tran_Scan(line, dir, files)
char         *line;
struct _dirs *dir;
int           files;
{
    int i, j;
    struct _translate *tran;
    int	   mode = 0;
    char   *file_name = NULL;
    char   *file_name_unix = NULL;
    char   *file_dir = NULL;
    char   *file_remote = NULL;
    char   *file_size = NULL;
    char   *file_date = NULL;
    char   *file_time = NULL;
    char   *file_owner = NULL;
    char   *file_prot = NULL;
    char   *file_group = NULL;
    char   *file_type = NULL;
    char   *file_p1 = NULL;
    char   *file_p2 = NULL;
    char   *x, *y;

    tran = s_tran_files;
    if (!tran->regexp) compile_regexp(tran);
    tran = s_tran;
    if (!tran->regexp) compile_regexp(tran);
    if (!line) return 1;
    if (!*line) return 1;
    /*
     * Some machines, "A20", don't understand pwd!
     */
    if (!dir && tran->dir_name_regexp) {
	if (regexec(tran->dir_name_regexp, line)) {
	    if (tran->dir_name_regexp->startp[1]) {
	      	if(remote_dir) XtFree(remote_dir);
		remote_dir = getstring(tran->dir_name_regexp, 1);
	    }
	}
    }
    if(!regexec(tran->regexp, line)) return 1;
    if (!dir) return 0;

    for (i=1; i<NSUBEXP; i++) {
	if (tran->regexp->startp[i]) {
	    x = getstring(tran->regexp,i);
	    switch(tran->map[i]) {
		case FILE_NAME: 
		    file_name = XtNewString(x);
		    if(tran->local == UPPER) upper(x);
		    if(tran->local == LOWER) Lower(x);
		    for (j=0; j<N_T; j++) {
		    	if(s_tran_files->file_to_unix_regexp[j] &&
			   regexec(s_tran_files->file_to_unix_regexp[j], x)) {
			     y = (char *)XtMalloc(2000);
			     regsub(s_tran_files->file_to_unix_regexp[j], 
			            s_tran_files->file_to_unix_source[j], y);
			     file_name_unix = XtNewString(y);
			     mode = s_tran_files->file_to_unix_mode[j];
			     XtFree(y);
			     break;
		        }
		    }
                    if(tran->remote_to_get_rewrite_exp[0]) {
			for(j=0; j<N_T; j++) {
			    if(tran->remote_to_get_rewrite_exp[j] &&
			       regexec(tran->remote_to_get_rewrite_regexp[j], 
				       file_name)) {
			        y = (char *)XtMalloc(2000);
				regsub(tran->remote_to_get_rewrite_regexp[j], 
			               tran->remote_to_get_rewrite_source[j], 
				       y);
				XtFree(file_name);
				file_name = XtNewString(y);
			        XtFree(y);
			        break;
			    }
			}
		    }
		    /*
		     * Its a sad story.
		     */
		    file_remote = XtNewString(file_name);
                    if(s_tran_files->remote == UPPER) upper(file_remote);
                    if(s_tran_files->remote == LOWER) Lower(file_remote);
		    break;
		case FILE_SIZE:
		    file_size = XtNewString(x);
		    break;
		case FILE_DATE:
		    file_date = XtNewString(x);
		    break;
		case FILE_TIME:
		    file_time = XtNewString(x);
		    break;
		case FILE_OWNER:
		    file_owner = XtNewString(x);
		    break;
		case FILE_PROT: 
		    file_prot = XtNewString(x);
		    break;
		case FILE_GROUP:
		    file_group = XtNewString(x);
		    break;
		case FILE_P1:
		    file_p1 = XtNewString(x);
		    break;
		case FILE_P2:
		    file_p2 = XtNewString(x);
		    break;
	    }
/*
	    XtFree(x);
*/
	}
    }
    if(tran->is_dir_regexp) {
	switch(tran->is_dir_field) {
	    case FILE_NAME: 
		x = file_name;
		break;
	    case FILE_SIZE:
		x = file_size;
		break;
	    case FILE_TIME:
		x = file_time;
		break;
	    case FILE_DATE:
		x = file_date;
		break;
	    case FILE_OWNER:
		x = file_owner;
		break;
	    case FILE_PROT: 
		x = file_prot;
		break;
	    case FILE_GROUP:
		x = file_group;
		break;
	    case FILE_P1:
		x = file_p1;
		break;
	    case FILE_P2:
		x = file_p1;
		break;
	}
	if (x) {
	    if(regexec(tran->is_dir_regexp, x) && file_name) {
        	for (j=0; j<N_T; j++) {
   		    if(tran->dir_regexp[j] && 
	   	       regexec(tran->dir_regexp[j], file_name)) {
		    	y = (char *)XtMalloc(2000);
		    	regsub(tran->dir_regexp[j], 
				   tran->dir_source[j], y);
			file_dir = XtNewString(y);
			XtFree(y);
		    	break;
		    }
		}
	    }
	}
    }
    if (dir) {
    	dir->files[files].g_owner    = file_owner;
        dir->files[files].g_group    = file_group;
        dir->files[files].g_link_int = 0;
        dir->files[files].g_size     = file_size;
	dir->files[files].size       = set_size(file_size);
        dir->files[files].g_date     = file_date;
        dir->files[files].g_time     = file_time;
        dir->files[files].name       = file_name;
	dir->files[files].remote     = file_remote;
        dir->files[files].g_prot     = file_prot;
        dir->files[files].g_type     = file_type;
	dir->files[files].local      = file_name_unix;
	dir->files[files].g_p1 	     = file_p1;
	dir->files[files].g_p2 	     = file_p2;
        dir->files[files].mode       = mode; 
	dir->files[files].time	     = get_remote_time(file_date, 
						       file_time);
	if (file_dir) {
            dir->files[files].mode |= MODE_TYPE_DIR;
	    dir->files[files].dir_name = file_dir;
	} else {
                dir->files[files].mode |= MODE_TYPE_FILE;
	}
    }
    return 0;
}

static int
check(map, line)
int map;
char *line;
{
    int i;

    for (i=0; i<map_index; i++) {
	if (t->map[i] == map) {
	    fprintf(stderr,"Duplicate tranlation entry %s",line);
	    return 1;
	}
    }
    t->map[map_index] = map;
    map_index++;
    return 0;
}

static void
compile_regexp(tran)
struct _translate *tran;
{
    int i;

    tran->regexp = regcomp(tran->exp);
    if (tran->dir_name_exp) tran->dir_name_regexp = regcomp(tran->dir_name_exp);
    if (tran->up_exp) tran->up_regexp = regcomp(tran->up_exp);
    if (tran->cd_exp) tran->cd_regexp = regcomp(tran->cd_exp);
    if (tran->is_dir) tran->is_dir_regexp = regcomp(tran->is_dir);
    if (tran->convert_size_exp) tran->convert_size_regexp =
			regcomp(tran->convert_size_exp);
    for(i=0; i<N_T; i++) {
        if (!tran->dir_exp[i]) break;
	tran->dir_regexp[i] = regcomp(tran->dir_exp[i]);
    }
    for(i=0; i<N_T; i++) {
	if(!tran->file_to_unix_exp[i]) break;
	tran->file_to_unix_regexp[i] = regcomp(tran->file_to_unix_exp[i]);
    }
    for(i=0; i<N_T; i++) {
	if(!tran->file_to_other_exp[i]) break;
	tran->file_to_other_regexp[i] = 
	  regcomp(tran->file_to_other_exp[i]);
    }
    for(i=0; i<N_T; i++) {
	if(!tran->remote_to_get_rewrite_exp[i]) break;
	tran->remote_to_get_rewrite_regexp[i] = 
	  regcomp(tran->remote_to_get_rewrite_exp[i]);
    }
    for(i=0; i<N_T; i++) {
	if(!tran->mode_exp[i]) break;
	tran->mode_regexp[i] = regcomp(tran->mode_exp[i]);
    }

}

static char *
get_next(s, n)
char *s;
int   n;
{
    char *xx;
    
    xx = fgets(s, n, input);
    return xx;
}

static char *
get_next_tran(s, n)
char *s;
int   n;
{
    extern char *tran_table[];
    
    if(!tran_table[tran_index]) return NULL;
    if ((int)strlen(tran_table[tran_index]) > (n-1)) {
	fprintf(stderr,"Internal tranlation has a string that is too long!\n");
	exit(1);
    }
    strcpy(s, tran_table[tran_index]);
    tran_index++;
    return s;
}

static char *
getstring(r, i)
regexp *r;
int    i;
{
    int n;
    char *string;

    
    n = r->endp[i] - r->startp[i];
    if (n == 0) return NULL;
    string = (char *)XtMalloc(n+1);
    strncpy(string, r->startp[i], n);
    string[n] = '\0';
    return string;
}

static void
merge_t(t1, t2)
struct _translate *t1; 
struct _translate *t2; 
{
    int i, j;

    for (i=0; i<N_T; i++) {
	if (!t1->file_to_unix_exp[i]) break;
    } 

    if (i<N_T) {
	for (j=0; (i+j) < N_T; j++) {
	    if (!t2->file_to_unix_exp[j]) break;
	    t1->file_to_unix_exp[i+j] = t2->file_to_unix_exp[j];
	    t1->file_to_unix_source[i+j] = t2->file_to_unix_source[j];
	    t1->file_to_unix_mode[i+j]  = t2->file_to_unix_mode[j];
	}
    }

    for (i=0; i<N_T; i++) {
	if (!t1->file_to_other_exp[i]) break;
    } 

    if (i<N_T) {
	for (j=0; (i+j) < N_T; j++) {
	    if (!t2->file_to_unix_exp[j]) break;
	    t1->file_to_other_exp[i+j] = t2->file_to_other_exp[j];
	    t1->file_to_other_source[i+j] = t2->file_to_other_source[j];
	    t1->file_to_other_mode[i+j] = t2->file_to_other_mode[j];
	}
    }

    for (i=0; i<N_E; i++) {
	if (!t1->remote_examples[i]) break;
    } 

    if (i<N_E) {
        for (j=0; (i+j)<N_E; j++) {
            if (!t2->remote_examples[j]) break;
            t1->remote_examples[i+j] = t2->remote_examples[j];
	}
    }

    for (i=0; i<N_E; i++) {
	if (!t1->unix_examples[i]) break;
    } 

    if (i<N_E) {
        for (j=0; (i+j)<N_E; j++) {
            if (!t2->unix_examples[j]) break;
            t1->unix_examples[i+j] = t2->unix_examples[j];
	}
    }
}

static void
merge_use_other()
{
    int i, j;
    for (i=0; i< NO_TRAN; i++) {
	if (translate[i].use_other) {
	    for (j=0; j<NO_TRAN; j++) {
	        if (strcmp(translate[i].use_other, translate[j].system) == 0) {
			merge_t(&translate[i] ,&translate[j]);
			XtFree(translate[i].use_other);
			translate[i].use_other = NULL;
			break;
		}
	    }
	}
    }
}

static void
read_init(get_next, ignore_dup)
char *(*get_next)();
int  ignore_dup;
{
    char line[2000];
    char *cp;
    int state, i,j, entry;
    int  dup = 0;


    state = STATE_NULL;
    while(get_next(line, sizeof(line)) != NULL) {
        int s;

        if (line[0] == '#') continue;
	if (line[0] == '!') continue;
	for (i=0; i<MAXPARSE; i++) {
            if (strncmp(line, parse[i].directive, 
			strlen(parse[i].directive)) == 0) break;
	}
        if (i == MAXPARSE) {
	    fprintf(stderr, "unknown directive %s\n", line);
	    fflush(stderr);
	    continue;
	}
	s = parse[i].state & (~START_MASK);
	if (parse[i].state & START_MASK) {
	    if (state & STATE_NULL) {
		state = s;
	    } else {
		fprintf(stderr, "invalide directive %s\n",
		        parse[i].directive);
		continue;
	    }
  	}
	cp = &line[strlen(parse[i].directive)];
	SKIP_BLANKS(cp);
	REMOVE_TRAILING_BLANKS(cp);
        entry = i;
        if (dup && !(parse[i].state & STATE_TRAN))  dup = 0;
	if (state & s) { 
	    switch(parse[i].type) {
	      case SET_NAME:
    	        for (i=0; i< NO_TRAN; i++) {
        	    if (strcmp(translate[i].system,cp) == 0) {
			if (translate[i].options & TRAN_OPTION_USER &&
			    !(parse[entry].state & STATE_USER)) break;
			dup++;
			if (!ignore_dup) {
			    fprintf(stderr,"duplicate system table %s\n",cp);
			} 
		      	state = STATE_NULL;
		  	while(get_next(line, sizeof(line)) != NULL) {
			    if (strncmp(line, "end", sizeof("end")-1) == 0)
			        break;
		        }
			break;
		    }
    		}
		if (dup) {
		    dup = 0;
		    break;
	        }
		t = &translate[i];
		if (i == NO_TRAN) {
		    NO_TRAN++;
		    if (NO_TRAN >= NUM_TRAN) {
			fprintf(stderr, "To many translations\n");
			exit(1);
		    }
		}
		if (parse[entry].state & STATE_USER)  
			t->options |= TRAN_OPTION_USER;
		map_index = 1;
		t->system = XtNewString(cp);
		break;
	      case SET_GUESS:
	        for (j=0; j<N_GUESS; j++) {
		    if (!t->guess[j]) {
		        t->guess[j] = XtNewString(cp);
			break;
		    }
 		}
		break;
	      case SET_TYPE:
	        for (j=0; j<N_TYPE; j++) {
		    if (!t->type[j]) {
		        t->type[j] = XtNewString(cp);
			break;
		    }
 		}
		break;
	      case SET_FILE:
		if (dup) continue;
		if(check(FILE_NAME, line)) break;
		if (t->exp) t->exp = concat(t->exp, "[ 	]+"); 
		t->exp = concat(t->exp, cp);
		break;
	      case SET_SIZE:
		if(check(FILE_SIZE, line)) break;
		if (t->exp) t->exp = concat(t->exp, "[ 	]+"); 
		t->exp = concat(t->exp, cp);
		break;
	      case SET_DATE:
		if(check(FILE_DATE, line)) break;
		if (t->exp) t->exp = concat(t->exp, "[ 	]+"); 
		t->exp = concat(t->exp, cp);
		break;
	      case SET_TIME:
		if(check(FILE_TIME, line)) break;
		if (t->exp) t->exp = concat(t->exp, "[ 	]+"); 
		t->exp = concat(t->exp, cp);
		break;
	      case SET_P1:
		if(check(FILE_P1, line)) break;
		if (t->exp) t->exp = concat(t->exp, "[ 	]+"); 
		t->exp = concat(t->exp, cp);
		break;
	      case SET_P2:
		if(check(FILE_P2, line)) break;
		if (t->exp) t->exp = concat(t->exp, "[ 	]+"); 
		t->exp = concat(t->exp, cp);
		break;
	      case SET_ISDIR:
		for (j=0; j<MAXNAMES; j++) {
		    if (strncmp(names[j].name, cp, 
				strlen(names[j].name)) == 0) {
			t->is_dir_field = names[j].value;
			cp = cp + strlen(names[j].name);
			SKIP_BLANKS(cp);
			REMOVE_TRAILING_BLANKS(cp);
			t->is_dir = XtNewString(cp);
			break;
		    }
		}
		if (j == MAXNAMES) {
		    fprintf(stderr,"I don't know fieled %s\n", cp);
		}
		break;
	      case SET_OWNER:
		if(check(FILE_OWNER, line)) break;
		if (t->exp) t->exp = concat(t->exp, "[ 	]+"); 
		t->exp = concat(t->exp, cp);
		break;
	      case SET_PROT:
		if(check(FILE_PROT, line)) break;
		if (t->exp) t->exp = concat(t->exp, "[ 	]+"); 
		t->exp = concat(t->exp, cp);
		break;
	      case SET_DIR:
		set_exp(t->dir_exp, t->dir_source, 
			cp, get_next, parse[i].directive , NULL);
		break;
	      case SET_UNIX:
		set_exp(t->file_to_unix_exp, t->file_to_unix_source, 
			cp, get_next, parse[i].directive, t->file_to_unix_mode);
		break;
	      case SET_BACK:
		set_exp(t->file_to_other_exp, t->file_to_other_source, 
			cp, get_next, parse[i].directive, t->file_to_other_mode);
		break;
	      case SET_END:
		if (dup) {
		    dup = 0;
		    state = STATE_NULL;
		    break;;  /* Need to realese all that storage sigh */
		}
		state = STATE_NULL;
		break;
	      case SET_REMOTE:
		if (strncmp("upper", cp, strlen("upper")) == 0) {
		    t->remote = UPPER;
		} else if (strncmp("lower", cp, strlen("lower")) == 0) {
		    t->remote = LOWER;
		}
		break;
	      case SET_LOCAL:
		if (strncmp("upper", cp, strlen("upper")) == 0) {
		    t->local = UPPER;
		} else if (strncmp("lower", cp, strlen("lower")) == 0) {
		    t->local = LOWER;
		}
		break;
	      case SET_UP:
		t->up = XtNewString(cp);
		break;
	      case SET_GUESS_CWD:
		t->guess_cwd = XtNewString(cp);
		break;
	      case SET_LONG:
		t->format_l = set_listing(t->listing_l, cp);
		break;
	      case SET_MEDIUM:
		t->format_m = set_listing(t->listing_m, cp);
		break;
	      case SET_INDENT:
		t->indent++;
		break;
	      case SET_INIT:
		t->init = XtNewString(cp);
		break;
	      case SET_REWRITE:
		set_exp(t->remote_to_get_rewrite_exp, 
			t->remote_to_get_rewrite_source, 
			cp, get_next, parse[i].directive, NULL);
		break;
	      case SET_ISUNIX:
		t->unix++;
		break;
  	      case SET_IS_RT:
		t->unix++;
		t->ibm_rt++;
		break;
	      case SET_IS_APOLLO:
		t->unix++;
		t->apollo++;
		break;
	      case SET_NOTYPE:
		t->notype++;
		break;
	      case SET_REMOTE_EXAMPLES:
	        for (j=0; j<N_E; j++) {
		    if (!t->remote_examples[j]) {
		        t->remote_examples[j] = XtNewString(cp);
			if((j+1) < N_E) t->remote_examples[j+1] = NULL;
			break;
		    }
 		}
	        break;
	      case SET_UNIX_EXAMPLES:
	        for (j=0; j<N_E; j++) {
		    if (!t->unix_examples[j]) {
		        t->unix_examples[j] = XtNewString(cp);
			if((j+1) < N_E) t->unix_examples[j+1] = NULL;
			break;
		    }
 		}
		break;
	      case SET_USE_OTHER:
		t->use_other = XtNewString(cp);
		break;
	      case SET_NOTE:
		Set_Note(cp);
		break;
	      case SET_CONVERT_SIZE:
		t->convert_size_exp = XtNewString(cp);
    		if (get_next(line, sizeof(line)) == NULL) {
		    fprintf(stderr,"Hit end of file early\n");
		    fflush(stderr);
		    return;
		}
		cp = line;
	        SKIP_BLANKS(cp);
	        REMOVE_TRAILING_BLANKS(cp);
		if (cp && *cp) {
        	    t->convert_size_source = XtNewString(cp);
		}
		break;
	      case SET_MODE:
		break;
	      case SET_DIR_NAME:
		t->dir_name_exp = XtNewString(cp);
		break;
	      case SET_NO_PWD:
		t->no_pwd++;
		break;
	      case SET_CD_EXP:
		t->cd_exp = XtNewString(cp);
    		if (get_next(line, sizeof(line)) == NULL) {
		    fprintf(stderr,"Hit end of file early\n");
		    fflush(stderr);
		    return;
		}
		cp = line;
	        SKIP_BLANKS(cp);
	        REMOVE_TRAILING_BLANKS(cp);
		if (cp && *cp) {
        	    t->cd_source = XtNewString(cp);
		}
		break;
	      case SET_UP_EXP:
		t->up_exp = XtNewString(cp);
    		if (get_next(line, sizeof(line)) == NULL) {
		    fprintf(stderr,"Hit end of file early\n");
		    fflush(stderr);
		    return;
		}
		cp = line;
	        SKIP_BLANKS(cp);
	        REMOVE_TRAILING_BLANKS(cp);
		if (cp && *cp) {
        	    t->up_source = XtNewString(cp);
		}
		break;
	      case SET_SYSTEM_NOTE:
		t->system_note = XtNewString(cp);
		break;
	      case SET_VIEWER:
	        SKIP_BLANKS(cp);
		if (strncmp(cp, "text ", sizeof("text ")-1) == 0) {
		     cp = cp + sizeof("text ") - 1;
	             SKIP_BLANKS(cp);
	             REMOVE_TRAILING_BLANKS(cp);
		     viewers[TEXT].command = XtNewString(cp);
		} else if (strncmp(cp, "picture ", sizeof("picture ")-1) == 0) {
		     cp = cp + sizeof("picture ") - 1;
	             SKIP_BLANKS(cp);
	             REMOVE_TRAILING_BLANKS(cp);
		     viewers[PICTURE].command = XtNewString(cp);
		} else if (strncmp(cp, "ps ", sizeof("ps ")-1) == 0) {
		     cp = cp + sizeof("ps ") - 1	;
	             SKIP_BLANKS(cp);
	             REMOVE_TRAILING_BLANKS(cp);
		     viewers[PS].command = XtNewString(cp);
		} else if (strncmp(cp, "audio ", sizeof("audio ")-1) == 0) {
		     cp = cp + sizeof("audio ") - 1;
	             SKIP_BLANKS(cp);
	             REMOVE_TRAILING_BLANKS(cp);
		     viewers[AUDIO].command = XtNewString(cp);
		} else if (strncmp(cp, "tar ", sizeof("tar ")-1) == 0) {
		     cp = cp + sizeof("tar ") - 1;
	             SKIP_BLANKS(cp);
	             REMOVE_TRAILING_BLANKS(cp);
		     viewers[PICTURE].command = XtNewString(cp);
		}
		break;
	    }
	} else {
	    if (dup && parse[i].type & STATE_TRAN) continue;
	    fprintf(stderr,"Command used in wrong state\n");
	    dup = 0;
	}
    }
}

static void
set_exp(exps, source, exp, get_next, directive, mode)
char **exps;
char **source;
char *exp;
char *(*get_next)();
char *directive;
int  *mode;
{
    char line[2000];
    char *cp, *c;
    int i;
   
    for (i=0; i<N_T; i++) {
	if (exps[i]) continue;
        break;
    }
    if (i==N_T) {
	fprintf(stderr, "Too many %s directives\n",directive);
	return;
    }
    strcpy(line, "x");
    if (!get_next( line, sizeof(line)) || !isspace(*line)) {
	fprintf(stderr, "Directive %s must have continuation line %s\n",
		         directive,line);
	return;
    }
    exps[i] = XtNewString(exp);
    cp = line;
    SKIP_BLANKS(cp)
    REMOVE_TRAILING_BLANKS(cp)
    if (mode) {
	char *c = RINDEX(cp, ' ');
	if (c) {
	    c++;
	    if (strncmp(c, "ascii", sizeof("ascii") -1) == 0) {
		mode[i] = MODE_TYPE_ASCII;
	        c--;
	        *c = '\0';
                REMOVE_TRAILING_BLANKS(cp)
	    } else if (strncmp(c, "binary", sizeof("binary")-1) == 0) {
		mode[i] = MODE_TYPE_BINARY;
	        c--;
	        *c = '\0';
                REMOVE_TRAILING_BLANKS(cp)
	    } else if (strncmp(c, "tenex", sizeof("tenex")-1) == 0) {
		mode[i] = MODE_TYPE_TENEX;
	        c--;
	        *c = '\0';
                REMOVE_TRAILING_BLANKS(cp)
	    }
	}
    }
    source[i] = XtNewString(cp);
}

static char *
set_listing(map, list)
int *map;
char *list;
{
    char *cp = list;
    char *end = NULL;
    char *format = NULL;
    int  j, i;

    i = 0;
    while (*cp) {
        for (j=0; j<MAXNAMES; j++) {
	    if (strncmp(names[j].name, cp, strlen(names[j].name)) == 0) {
	        map[i] = names[j].value;
		i++;
	        cp = cp + strlen(names[j].name);
	        if (*cp == '(') {
		    cp++;
		    if (end = INDEX(cp, ')')) {
		    	int n;
		    	char *temp;

		    	n = end - cp;
		    	temp = (char *)malloc(n+1);
		    	strncpy(temp, cp, n);
		    	temp[n] = '\0';
		    	format = concat(format, "%");
		    	format = concat(format, temp);
		    	format = concat(format, "s ");
		    	cp += n + 1;
	            } else {
			return NULL;
		    }
	        } else {
		    format = concat(format, "%s ");
		}
	        SKIP_BLANKS(cp);
		break;
            } 
        }
        if(j==MAXNAMES) return NULL;
    }
    return format;
}

static int
set_mode(mode)
char *mode;
{
    if (strcmp(mode, "ascii") == 0) {
	return MODE_TYPE_ASCII;
    } else if (strcmp(mode, "binary") == 0) {
	return MODE_TYPE_BINARY;
    } else {
        return 0;
    }
}

static int
set_size(size)
char *size;
{
    int s[NSUBEXP];
    char *x;
    char *start, *end, p[100];
    int i, n;
    int result, p1, p2;
    char op;
  
    if (!s_tran) return 0;
    if (!s_tran->convert_size_regexp || !s_tran->convert_size_source) return 0;
    if(!regexec(s_tran->convert_size_regexp, size)) return 0;;
    for (i=1; i<NSUBEXP; i++) {
	if (s_tran->convert_size_regexp->startp[i]) {
	    x = getstring(s_tran->convert_size_regexp,i);
	    n = i;
	    end = INDEX(x, ',');
	    start = x;
	    s[i] = 0;
	    if (end) {
		while (end) {
		    int l;
		    l =  end - start;
		    if ((!s[i] && l <= 3) || l == 3) {
			strncpy(p, start, l);
		        p[l] = '\0';
			s[i] = s[i]*1000 + atoi(p);
		    } else {
			break;
		    }
		    start += l;
		    if (!*start) break;
		    start++;
		    if (!*start) break;
		    end = INDEX(start, ',');
		    if (!end) {
			if((int)strlen(start) >= 3) end = start + 3;
		    }
		}
	    } else {
		s[i] = atoi(x);
	    }
	    XtFree(x);
	} else {
	    break;
	}
    }
    start = s_tran->convert_size_source;
    p1 = p2 = -1;
    op = '&';
    while(start && *start) {
        if (*start == '\\')  {
	    start++;
	    i = *start - '0';
	    if (i >= NSUBEXP) return 0;
	    if (p1 < 0) {
		p1 = s[i];
	    } else if (p2 < 0) {
		p2 = s[i];
		switch (op) {
		     case '*':
			p1 = p1*p2;
			break;
		     case '/':
			p1 = p1/p2;
		        break;
		     case '+':
			p1 = p1+p2;
			break;
		     case '-':
			p1 = p1-p2;
			break;
		     case '&':
			return 0;
		  
		}
		p2 = -1;
	        op = '&';
	    } else {
		return 0;
	    }
	    start++;
	} else if (*start == '*') {
	    op = '*';
	    start++;
	} else if (*start == '/') {
	    op = '/';
	    start++;
	} else if (*start == '+') {
	    op = '+';
	    start++;
	} else if (*start == '-') {
	    op = '-';
	    start++;
	} else if (isdigit(*start)) {
	    i = 0; 
	    while (isdigit(*start)) { i = i*10 + *start - '0'; start++;}
	    if (p1 < 0) {
		p1 = i;
	    } else if (p2 < 0) {
		p2 = i;
		switch (op) {
		     case '*':
			p1 = p1*p2;
			break;
		     case '/':
			p1 = p1/p2;
		        break;
		     case '+':
			p1 = p1+p2;
			break;
		     case '-':
			p1 = p1-p2;
			break;
		     case '&':
			return 0;
		}
	        op = '&';
		p2 = -1;
   	    }
	} else {
	} 
    }
    if (p1 < 0) return 0; 
#if defined(DEBUG)
    fprintf(stderr,"size %d string %s\n", p1, size);
#endif
    return p1;
}

static void
upper(string)
char *string;
{
   char *cp = string;

   if (!cp) return;
   while(*cp) {
        if (islower(*cp)) *cp = toupper(*cp);
        cp++;
   }
}
