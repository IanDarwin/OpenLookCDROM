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
"$Id: dir_subs.c,v 1.2 1994/05/20 20:12:49 jones Exp $\n";
static char rcspath[] =
"$Source: /mintaka/u0/xx/ss/jones/jones.old/mftp.src/moxftp-2.0/RCS/dir_subs.c,v $\n";
#endif

/* $Log: dir_subs.c,v $
 * Revision 1.2  1994/05/20  20:12:49  jones
 * Fixe retainpos to work only only on currently displayed directory.
 *
 * Revision 1.1  1994/03/14  18:55:53  jones
 * Initial revision
 *
 */

#include "machine.h"
#include "defs.h"
#include "proto/dir_subs.h"

struct _dirs 	*first_dir = NULL;
struct _dirs 	*Lastdir = NULL;
int     	List_type = LIST_SHORT;
int     	Remote_local = REMOTE;
int     	List_sort = SORT_BY_NAME;
int     	List_reverse = 0;
int	       	List_by_type = 0;
static int     	ignore_errors = 0;
extern int 	no_dot_dot;
extern int  	ibm_rt;
extern int  	appolo;
extern int      aborting;
extern XtAppContext  App_context;
int		mult_file = 0;

static struct {
    int    mode;
    int    index;
    char   *title_many;
    char   *title_one;
} file_types[] = {
 {MODE_TYPE_BLOCK    , 0, "block devices", "block device"},
 {MODE_TYPE_CHAR     , 1, "char devices" , "char device"},
 {MODE_TYPE_LINK     , 2, "links"        , "link"},
 {MODE_TYPE_SOCK     , 3, "sockits"      , "sockit"},
 {MODE_TYPE_DIR      , 4, "directories"  , "directory"},
 {MODE_TYPE_FILE     , 5, "files"        , "file"},
 {MODE_TYPE_OFFLINE  , 6, "offline files", "offline file"},
 {MODE_TYPE_SELECT   , 7, "selected"     , "selected"},
 {0                  , 8, "items"        , "item"}
};
		
static int max_tran_name = 0;
static int max_tran_tran = 0;
static char tran_format1[100];
static char tran_format2[100];

#define DO_FILES   0
#define DO_DIRS    1

struct _dir_stack {
    struct _dirs *dir;
    struct _dirs *local;
    int    restart;
    int    i;
    int    up;
    int    state;
    struct _dir_stack *next;
};

struct _dir_stack *dir_stack         = NULL;
void             (*dir_stack_func)() = NULL;
CB   		  *dir_stack_cb      = NULL;



void
Clean_Up(code, cb)
int code;
CB *cb;
{
    struct _dir_stack *next, *prev;
    int i;

    do_callbacks(code, cb);

    if (dir_stack_cb) do_callbacks(code, dir_stack_cb);
    dir_stack_cb = NULL;
    
    next = dir_stack;
    while(next) {
	if (next->next == NULL) break;
	for(i=0; i<next->dir->n; i++) {
	    next->dir->files[i].mode &= ~MODE_TYPE_DO;
        }
	prev = next;
	next = next->next;
	XtFree((char *)prev);
    }
    dir_stack = next;
    if (dir_stack) {
	if (strcmp(dir_stack->local->pwd_name, local_dir) != 0) {
       	    Start_Lcd(0, dir_stack->local->pwd_name, cb_clean_up_lcd, 
		      NULL, NULL);
	    return;
	} else if(strcmp(dir_stack->dir->pwd_name, remote_dir) != 0) {
	    Start_Cd(0, dir_stack->dir->pwd_name, 0, cb_clean_up_cd, 
		     NULL, NULL);
	    return;
	}
    }
    clean_up_finish();
}

void
Clear_Dirs()
{
    struct _dirs *next = first_dir;
    struct _dirs *prev;
    
    Lastdir = NULL;

    while(next) {
	Clear_Files(next);
	prev = next;
	next = next->next;
#define R(x) if (prev->x) XtFree(prev->x)
	R(pwd_name);
	R(host);
#undef R
        XtFree((char *)prev);
    }
    first_dir = NULL;
    Clear_List();
    Lastdir = 0;
}

void
Clear_Files(dir)
struct _dirs *dir;
{
    int i;

    if (!dir) return;

    if (dir->n < 0) return;

    if (Lastdir && (Lastdir == dir))  {
	Clear_List();
    }
    Free_Files(dir);
    dir->update = 0;
    dir->sort_type = -1;
    for(i=0; i<LIST_MAX; i++) dir->position[i] = -1;
}

void
Clear_List()
{
    retain_position(Lastdir);
    Lastdir = NULL;
    Add_File_Text(" ", 0, 0);
    Finish_File_Text(0, -1);
    Set_Item_Title(" ");
}

void
Count_Files(dir, flag)
struct _dirs *dir;
int flag;
{
    int i, j;
    char num[20];
    char *title = NULL;
    int old_fcounts[9];
    int slected = 0;


    if (!dir->fcount[8] || flag) {
	for (i=0; i<9; i++) {
	    old_fcounts[i] = dir->fcount[i];
    	    dir->fcount[i] = 0;
	}
	for (i=0; i<dir->n; i++) {
    	    dir->fcount[8]++;
	    if (dir->files[i].mode & file_types[7].mode) dir->fcount[7]++;
	    if (dir->files[i].mode & file_types[6].mode) dir->fcount[6]++;
	    for (j=0; j<6; j++) {
		if (dir->files[i].mode & file_types[j].mode) {
    	    	    dir->fcount[j]++;
		}
	    }
    	}
    }

    Update_Files_Opts(dir->fcount[7]);

    if (flag && flag > 0) {
        for (i=0; i<9; i++) {
	    if(old_fcounts[i] != dir->fcount[i]) break;
        }
        if (i==9) return;
    }

    for (i=8; i>=0; i--) {
	if (dir->fcount[i]) {
	    sprintf(num,"%d ",dir->fcount[i]);
   	    if (title) {
		title = concat(title, ", ");
	    }
	    title = concat(title, num);
	    if (dir->fcount[i] == 1) {
	         title = concat(title, file_types[i].title_one);
	    } else {
	         title = concat(title, file_types[i].title_many);
	    }
	}
    }
    Set_Item_Title(title);
    XtFree(title);
    Single_File_Actions();
}

void
Display_Files(dirname, host, redraw)
char *dirname;
char *host;
int  redraw;
/*
 * Unix Dir routines.
 */
{
    struct _dirs *dir;

    dir = Find_Dir(dirname, host, 0);
    if (dir == NULL || dir->n <0) {
	Clear_List();
        return;
    }
    if (!redraw && Lastdir == dir) return;
    Draw_Files(dir);
}

void
Do_File(func, file)
void (*func)();
int file;
{
    int i;

    if (!Lastdir || file < 0 || file >= Lastdir->n)  {
    	Update_Mark(-1); 
        return;
    }
    Set_Noop(NOOP_ACTION);
    for(i=0; i<Lastdir->n; i++) Lastdir->files[i].mode &= ~MODE_TYPE_DO;
    Lastdir->files[file].mode |= MODE_TYPE_DO;
    dir_stack_func = func;
    dir_stack_cb = NULL;
    ignore_errors = ignorerrs;
    mult_file = 1;
    do_dir(Lastdir);
}

void
Do_List(func, cb)
void (*func)();
CB   *cb;
{
    int i, do_count = 0;


    if (!Lastdir) {
	if (cb) do_callbacks(1, cb);
	return;
    }
    for (i=0; i<Lastdir->n; i++) {
	if (Lastdir->files[i].mode & MODE_TYPE_SELECT) {
	    Lastdir->files[i].mode |= MODE_TYPE_DO;
	    Lastdir->files[i].mode &= ~MODE_TYPE_SELECT;
	    do_count++;
	} else {
	    Lastdir->files[i].mode &= ~MODE_TYPE_DO;
	}
    }
    if (!do_count) {
	if (cb) do_callbacks(1, cb);
	return;
    }
    Set_Noop(NOOP_ACTION);
    dir_stack_func = func;
    dir_stack_cb = cb;
    ignore_errors = ignorerrs;
    mult_file = 1;
    do_dir(Lastdir);
}

void
Draw_Files(d)
struct _dirs *d;
{
    struct _dirs *dir;
    int i;
    int c = 0;
     
    Reset_Search();
    dir = d;
    if (dir == NULL) dir = Lastdir;
    if (dir == NULL) return;


    retain_position(Lastdir);
    if (Lastdir && Lastdir->link_up) Lastdir->link_up = 0;

    Lastdir = dir;

    if  (Lastdir->n == 0) {
	Clear_List();
        Lastdir = dir;
	return;
    }

    Set_Xaw_List(List_type);	

    Count_Files(dir, -1);
    c = Sort_Files(dir, List_sort, List_reverse, List_by_type);

    c |= (List_type != dir->list_type);
    fix_position(dir, List_type);
    Lastdir->update_file = NULL;

    switch(List_type) {
	default:
	case LIST_SHORT:
	     switch(dir->type) {
		case REMOTE_SYSTEM_UNIX:
		case REMOTE_SYSTEM_LOCAL:
	           list_short_unix_dir(dir);
		   break;
		default:
	           list_short_dir(dir);
	     }
	     break;
	case LIST_MEDIUM:
	     switch (dir->type) {
	  	case REMOTE_SYSTEM_UNIX:
		    list_medium_unix_dir(dir);
		    break;
		case REMOTE_SYSTEM_LOCAL:
		    list_medium_unix_dir(dir);
		    break;
		case REMOTE_SYSTEM_OTHER:
		    if(s_tran) {
		        list_long_dir(dir, LIST_MEDIUM, s_tran->listing_m,
				      s_tran->format_m);
		    } else {
	            	list_short_dir(dir);
		    }
		    break;
		default:
	            list_short_dir(dir);
	     }
	     break;
        case LIST_LONG:
	     switch (dir->type) {
	  	case REMOTE_SYSTEM_UNIX:
		    list_long_unix_dir(dir);
		    break;
		case REMOTE_SYSTEM_LOCAL:
		    list_long_unix_dir(dir);
		    break;
		case REMOTE_SYSTEM_OTHER:
		    if(s_tran) {
		        list_long_dir(dir, LIST_LONG, s_tran->listing_l, 
				      s_tran->format_l);
		    } else {
	            	list_short_dir(dir);
		    }
		    break;
		default:
	       	    list_short_dir(dir);
	     }
	     break;
        case LIST_TRANSLATIONS:
	     list_translations(dir);
	     break;
    }
    if (dir->highlight > -1) {
        Finish_File_Text(dir->position[dir->list_type], dir->highlight);
    } else {
        Finish_File_Text(dir->position[dir->list_type], -1);
    }
}

struct _dirs *
Find_Dir(dirname, host, create)
char *dirname;
char *host;
int  create;
{
    struct _dirs *next = first_dir;
    struct _dirs *dir, *prev;
    int i;

    if (next != NULL) {
	while(next != NULL) {
	    prev = next;
      	    if(strcmp(next->pwd_name, dirname) == 0 &&
	       strcmp(next->host    , host   ) == 0    ) return next;
	    next = next->next;
        }
	if (!create) return NULL;
	dir = prev->next = (struct _dirs *)XtMalloc(sizeof(struct _dirs));
        ZERO((char *)dir, sizeof(struct _dirs));
    } else {
	if (!create) return NULL;
	first_dir = dir = (struct _dirs *)XtMalloc(sizeof(struct _dirs));
        ZERO((char *)dir, sizeof(struct _dirs));
    }
    dir->n         =  -1;
    dir->pwd_name  = XtNewString(dirname);
    dir->host      = XtNewString(host);
    dir->list_type = 0;
    dir->type      = 0;
    dir->highlight = -1;
    dir->sort_type = -1;
    for(i=0; i<LIST_MAX; i++) dir->position[i] = -1;
    return dir;
}

void
Free_Files(dir)
struct _dirs *dir;
{
    int i;

    if (!dir) return;
    if (dir->n < 0 || (dir->files == NULL)) return;
#define R(x) if (dir->files[i].x) XtFree(dir->files[i].x)
#define C(x) dir->files[i].x
    for(i=0; i<dir->n; i++) {
        R(display);
        if (C(name) != C(dir_name)) R(dir_name);
        if (C(name) != C(remote  )) R(remote  );
        if (C(name) != C(local   )) R(local  );
	R(name);
 	R(linkname);
  	R(owner);
	R(modes);
	R(group);
	R(month);
	R(day);
	R(time_year);
	R(g_owner);
	R(g_group);
	R(g_prot);
	R(g_time);
	R(g_type);
	R(g_size);
	R(g_dir);
	R(g_p1);
	R(g_p2);
	R(g_link);
    }
#undef R
#undef C
    XtFree((char*)dir->files);
    dir->files = NULL;
    dir->n = -1;
}

void
Pop_Files(dir)
struct _dirs *dir;
{
    struct _dirs *odir;

/*
 * Needs work.
 */
    if (dir || dir->odir) {
	odir = dir->odir;
	*dir = *dir->odir;
	XtFree((char *)odir);
        dir->odir = NULL;
    }
}

void
Push_Files(dir)
struct _dirs *dir;
{


    if (dir == NULL) return;

    if (Lastdir && Lastdir == dir) retain_position(dir);

    if (dir->odir) {
        Clear_Files(dir->odir);
	XtFree((char *)dir->odir);
	dir->odir = NULL;
    }

    dir->odir = (struct _dirs *)XtMalloc(sizeof(struct _dirs));
    *dir->odir = *dir;
    dir->odir->odir = NULL;
    dir->odir->next = NULL;
    dir->n    =  -1;
    dir->files = NULL;
}

void
Restart_List(cb, flag)
CB *cb;
int flag;
{
    ignore_errors =  flag|ignorerrs;

    if (dir_stack) {
        if (dir_stack->restart) {
	    Set_Status("Could not restart multi file operation");
	    Clean_Up(0, cb);
	    return;
	}
        dir_stack->restart++;
	if (strcmp(dir_stack->local->pwd_name, local_dir) != 0) {
            Start_Lcd(0, dir_stack->local->pwd_name, cb_restart_lcd, NULL, cb);
	    return;
	} else if(strcmp(dir_stack->dir->pwd_name, remote_dir) != 0) {
	    Start_Cd(0, dir_stack->dir->pwd_name, 0, cb_restart_cd, NULL, cb);
	    return;
	}
        dir_stack->restart = 0;
    }
    do_next(0, 1, (DATA)cb);
}

void
Restore_Dir_Old(dir)
struct _dirs *dir;
{
    struct _dirs *ndir;
    struct _dirs *odir;
    char   *pos[LIST_MAX];
    char  *highname = NULL;
    int i, j, k, n;

    if (dir == NULL || dir->odir == NULL) return;
    if (dir->odir->n < 0 || dir->n < 0) {
        Clear_Files(dir->odir);
        XtFree((char *)dir->odir);
        dir->odir = NULL;
	return;
    }

    ndir = dir;
    odir = dir->odir;
    for (i=0; i<LIST_MAX; i++) pos[i] = NULL;


    if (odir->highlight >= 0 ) highname = odir->files[odir->highlight].name;
    for(i=0; i<LIST_MAX; i++) {
	if (odir->position[i] >= 0) {
		pos[i] = odir->files[odir->position[i]].name;
	
	}
    }

    /*
     * Compare old and new directory list.
     */
    ndir->sort_type = -1;
    (void)Sort_Files(ndir, SORT_BY_NAME, 0, 0);
    odir->sort_type = -1;
    (void)Sort_Files(odir, SORT_BY_NAME, 0, 0);

    for (i=0; i<odir->n; i++) {
	odir->files[i].temp = 0;;
	if (highname && strcmp(highname, odir->files[i].name) == 0)
	    odir->files[i].temp = 1;
        for(j=0; j<LIST_MAX; j++) 
	    if (pos[j] && strcmp(pos[j], odir->files[i].name) == 0)
	        odir->files[i].temp = 1;
    }

    i = 0;
    j = 0;

    while (i < ndir->n &&  j < odir->n) {
        n = strcmp(ndir->files[i].name, odir->files[j].name);
	if (n == 0) {
	    /* 
	     * Retain selection.
	     */
	    ndir->files[i].mode &= ~(MODE_TYPE_ASCII|
			             MODE_TYPE_BINARY|MODE_TYPE_TENEX);
	    ndir->files[i].mode |= odir->files[j].mode&
			(MODE_TYPE_SELECT|MODE_TYPE_ASCII|
			 MODE_TYPE_BINARY|MODE_TYPE_TENEX);
	    /*
	     * Retain do flags.
	     */
	    if (odir->files[j].mode & MODE_TYPE_DO) {
		if (odir->files[j].mode & 
		    (MODE_TYPE_FILE|MODE_TYPE_DIR|MODE_TYPE_LINK)) {
		    ndir->files[i].mode |= odir->files[j].mode& MODE_TYPE_DO;
		}
	    }
	    if (odir->files[j].temp != 0) {
		if (highname && strcmp(highname, ndir->files[i].name) == 0) 
			ndir->highlight = i;
        	for(k=0; k<LIST_MAX; k++) {
		    if (pos[k] && strcmp(pos[k], ndir->files[i].name) == 0) {
			ndir->position[k] = i;
		    }
		}
	    }
	    i++;
	    j++;
	    if (i >= ndir->n || j >= odir->n)  break; 
	} else if (n < 0) {
	    i++;
	} else {
	    j++;
	}
    }

    /*
     * Put list in right order.
     */
    (void)Sort_Files(ndir, List_sort, List_reverse, List_by_type);

    /* 
     * Insure that we will update screen.
     */
    if (Lastdir && (Lastdir == ndir))  Lastdir = NULL;

    Clear_Files(ndir->odir);
    XtFree((char *)ndir->odir);
    ndir->odir = NULL;
}

void
Set_Dir_Type(type)
int type;
{
    struct _dirs *dir;

    Remote_local = type;
    Set_Remote_Local(type);
    switch (Remote_local)  {
	case LOCAL:
	    dir = Find_Dir(local_dir, localhost, 0);
	    Update_Stat_Time(dir);
	    if (dir && dir->update && !noautodir) {
	        Start_Local_Dir(0, 0, cb_set_dir_type, (DATA)0, NULL);
	    } else {
	        Display_Files(local_dir, localhost, 0);
	    }
	    break;
	case REMOTE:
	    dir = Find_Dir(remote_dir, hostname, 0);
	    Update_Remote_Time(dir);
	    if (dir && dir->update && !noautodir) {
	 	Start_Remote_Dir(0, 0, cb_set_dir_type,  (DATA)0, NULL);
	    } else {
	        Display_Files(remote_dir, hostname, 0);
	    }
	    break;
   }
}

void
Set_List_Type(type)
int type;
{
    List_type = type;
    if(Lastdir) {
	Draw_Files(Lastdir);
    }
}



static void
cb_cd(data, code, cb)
DATA data;
int  code;
CB  *cb;
{
    int 		index_x = (int)data;
    int 	      	j = dir_stack->i;
    int 	      	i;
    char 	      	*up = NULL;
    char 	      	*error = NULL;
    struct _dirs      	*other;

    if (ftp_response[code].status & FTP_ERROR) {
    }
    if (j < 0) return;

    if (index_x && ftp_response[code].status & FTP_ERROR) {
	Set_Status(last_response);
	if (code >= 0) {
	    if (index_x == 2 && Remote_local == REMOTE) {
	        error = concat(error, "Could not cd to remote directory: ");
	        error = concat(error, dir_stack->dir->files[j].dir_name);
	        Start_Lcd(0, "..", cb_cont, (DATA)error, cb);
	        return;
	    }
	    if (index_x == 2 && Remote_local == LOCAL) {
	        error = concat(error, "Could not lcd to local directory: ");
	        error = concat(error, dir_stack->dir->files[j].dir_name);
	        switch(remote_type) {
	            case REMOTE_SYSTEM_UNIX:
		   	Start_Cd(0, "..", 0, cb_cont, (DATA)error, cb);
		    	return;
 		    case REMOTE_SYSTEM_OTHER:
			up = Compute_Up();
			if (up) {
                   	    Start_Cd(0, up , 0, cb_cont, (DATA)error, cb);
		   	    XtFree(up);
                   	    return;
			}
	    	}
	    }
	}
	Clean_Up(code, cb);
	return;
    }

    switch (index_x) {
	case 0:
	    switch(Remote_local) {
		case REMOTE:
	    	    break;
		case LOCAL:
            	    Start_Cd(0, dir_stack->dir->files[j].dir_name, 
			     1, cb_cd, (DATA)1, NULL);
		    break;
	    }
	    return;
	case 1:
	    switch(Remote_local) {
		case REMOTE:
            	    Start_Cd(0, dir_stack->dir->files[j].dir_name, 1, cb_cd, 
			     (DATA)2, NULL);
	    	    break;
		case LOCAL:
	 	    Start_Lcd(0, dir_stack->dir->files[j].dir_name, cb_cd, 
			      (DATA)2, NULL);
		    break;
	    }
	    return;
	case 2:
	    switch(Remote_local) {
		case REMOTE:
	    	    Start_Pwd(0, 0, cb_cd, (DATA)3, NULL);
	    	    return;
		case LOCAL:
	    	    Start_Pwd(0, 0, cb_cd, (DATA)3, NULL);
	     	    return;
	    }
	case 3:
	    switch(Remote_local) {
		case REMOTE:
	            Start_Remote_Dir(0, 0, cb_cd,  (DATA)4, NULL);
		    break;
		case LOCAL:
		    Start_Local_Dir(0, 0, cb_cd, (DATA)4, NULL);
		    break;
	    }
	    return;
	case 4:
	    switch(Remote_local) {
		case REMOTE:
               	    Display_Files(remote_dir, hostname, 0);
       		    other = Find_Dir(local_dir, localhost, 0);
              	    if (other) other->update = 1;
		    break;
		case LOCAL:
               	    Display_Files(local_dir, localhost, 0);
                    other = Find_Dir(remote_dir, hostname, 0);
                    if (other) other->update = 1;
		    break;
	    }

	    if (Lastdir != NULL) {
	        for (j=0; j < Lastdir->n; j++) {
		    if ((Lastdir->files[j].name[0] == '.' &&
		         Lastdir->files[j].name[1] == '\0') ||
		        (Lastdir->files[j].name[0] == '.' &&
		         Lastdir->files[j].name[1] == '.' &&
		         Lastdir->files[j].name[2] == '\0')) continue;
		    if (Lastdir->files[j].mode&(MODE_TYPE_FILE|MODE_TYPE_DIR)) {
	            	Lastdir->files[j].mode |= MODE_TYPE_DO;
		    }
	        }
	        if (dir_stack->i >=0 ) {
		    dir_stack->dir->files[dir_stack->i].mode &= ~MODE_TYPE_DO;
	        } 
                dir_stack->i = -1;
	        do_dir(Lastdir);
	    } else {
	        if (dir_stack->i >=0 ) {
		    dir_stack->dir->files[dir_stack->i].mode &= ~MODE_TYPE_DO;
	        } 
                dir_stack->i = -1;
		do_next(0, 1, (DATA)cb);
	    }
    }
}

static void
cb_clean_up_cd(data, code, cb)
DATA data;
int  code;
CB  *cb;
{
    Set_Remote(dir_stack->dir->pwd_name);
    if (Remote_local == REMOTE) Set_Dir_Type(Remote_local);
    clean_up_finish();
}

static void
cb_clean_up_lcd(data, code, cb)
DATA data;
int  code;
CB  *cb;
{
    if (ftp_response[code].status & FTP_ERROR) {
        clean_up_finish();
	return;
    }

    Set_Local(dir_stack->local->pwd_name);
    if (Remote_local == LOCAL) Set_Dir_Type(Remote_local);

    if(strcmp(dir_stack->dir->pwd_name, remote_dir) != 0) {
	Start_Cd(0, dir_stack->dir->pwd_name, 0, cb_clean_up_cd, NULL, NULL);
	return;
    }
    clean_up_finish();
}

static void
cb_cont(data, code, cb)
DATA  data;
int   code;
CB   *cb;
{

    if (ftp_response[code].status & FTP_ERROR_RESTART) { 
        dir_stack->i = -1;
	wait_no_connect((caddr_t)cb, NULL);
	return;
    } else {
        if (ftp_response[code].status & FTP_ERROR) {
	    Set_Status(last_response);
	    Set_Status("Giving up");;
	    Clean_Up(code, cb);
	    return;
        }
        if (dir_stack->i >=0 ) {
            dir_stack->dir->files[dir_stack->i].mode &= ~MODE_TYPE_DO;
        } 
        dir_stack->i = -1;
	
        if (!ignore_errors) {
	    Continue_Dialog((char *)data, cb);
            if (data) XtFree((char *)data);
 	    return;
        }
        if (data) XtFree((char *)data);
        do_next(0, 1, (DATA)cb);
    }
}

static void
cb_restart_cd(data, code, cb)
DATA data;
int  code;
CB  *cb;
{
     dir_stack->restart = 0;
     do_next(0, 1, (DATA)cb);
}

static void
cb_restart_lcd(data, code, cb)
DATA data;
int  code;
CB  *cb;
{
    if (ftp_response[code].status & FTP_ERROR) {
	Clean_Up(code, cb);
	return;
    }
    if(strcmp(dir_stack->dir->pwd_name, remote_dir) != 0) {
	Start_Cd(0, dir_stack->dir->pwd_name, 0, cb_restart_cd, NULL, cb);
	return;
    }
    dir_stack->restart = 0;
    do_next(0, 1, (DATA)cb);
}

static void
cb_set_dir_type(data, code, cb)
DATA  data;
int   code;
CB   *cb;
{
    if (ftp_response[code].status & FTP_ERROR) {
    }
    switch(Remote_local) {
	case REMOTE:
       	    Display_Files(remote_dir, hostname, 0);
	    break;
	case LOCAL:
       	    Display_Files(local_dir, localhost, 0);
	    break;
    }
}

static void
cb_up(data, code, cb)
DATA  data;
int   code;
CB   *cb;
{
    int i = (int)data;
    struct _dirs  *dir;
    char  *up;

    if (i && ftp_response[code].status & FTP_ERROR) {
	Set_Status(last_response);
	Clean_Up(code, cb);
	return;
    }

    switch (i) {
	case 0:
    	    switch(Remote_local) {
        	case LOCAL:
               	    Start_Lcd(0, "..", cb_up, (DATA)1, NULL);
		    return;
	        case REMOTE:
		    Update_Local_Dir();
		    switch(remote_type) {
                    	case REMOTE_SYSTEM_UNIX:
                           if (no_dot_dot) {
                  		up = Compute_Up_Apollo();
				Start_Cd(0, up , 0,  cb_up, (DATA)1, NULL);
				XtFree(up);
			    } else {
                             	Start_Cd(0, "..", 0, cb_up, (DATA)1, NULL);
                            }
                    	    return;
               	    	case REMOTE_SYSTEM_OTHER:
			    up = Compute_Up();
			    if (up) {
                    	        Start_Cd(0, up, 0,  cb_up, (DATA)1, NULL);
			     	XtFree(up);
                    	    	return;
			    }
        	    }
 	    }
	    break;
	case 1:
	    switch(Remote_local) {
		case REMOTE:
	    	    Start_Pwd(0, 0, cb_up, (DATA)2, NULL);
		    return;
		case LOCAL:
	    	    Start_Pwd(0, 0, cb_up, (DATA)2, NULL);
		    return;
	    }
	case 2:
	    switch(Remote_local) {
		case REMOTE:
	            Start_Remote_Dir(0, 0, cb_up,  (DATA)3, NULL);
		    return;
		case LOCAL:
		    Start_Local_Dir(0, 0, cb_up, (DATA)3, NULL);
		    return;
	    }
	    break;
	case 3:
	    switch(Remote_local) {
		case REMOTE:
               	    Display_Files(remote_dir, hostname, 0);
		    break;
		case LOCAL:
               	    Display_Files(local_dir, localhost, 0);
	    }
            do_next(0, 1, (DATA)cb);
    }
}

void
clean_up_finish()
{
    int i;
    int flag = 0;

    remove_func_do(clean_up_finish, DO_ABORT|DO_DISCONNECT);
    if (dir_stack) {
	if(aborting && strcmp(dir_stack->dir->pwd_name, remote_dir) != 0) {
	    Set_Remote(dir_stack->dir->pwd_name);
	    if (Remote_local == REMOTE) Set_Dir_Type(Remote_local);
	}
        for(i=0; i<dir_stack->dir->n; i++) {
	    dir_stack->dir->files[i].mode &= ~MODE_TYPE_DO;
        }
        XtFree((char *)dir_stack);
        Draw_Files(NULL);
    }
    dir_stack = NULL;
    mult_file = 0;
    Clear_Noop(NOOP_ACTION);
}

static void
do_dir(dir)
struct _dirs *dir;
{
    struct _dir_stack *top;
    struct _dir_stack *next;

    add_func_do(clean_up_finish, DO_ABORT|DO_DISCONNECT);

    if (dir_stack_func == NULL) {
	Clear_Noop(NOOP_ACTION);
	return;
    }
    top = (struct _dir_stack *)XtMalloc(sizeof(struct _dir_stack));
    ZERO((char *)top, sizeof(struct _dir_stack));
    top->dir = dir;
    top->local = Find_Dir(local_dir, localhost, 0);
    top->restart = 0;
    top->state = DO_FILES;
    top->i = -1;
    if (dir_stack) {
	next = dir_stack;
	dir_stack = top;
	top->next = next;
    } else {
	dir_stack = top;
    }
    do_next(0, 1, NULL);
}

static void
do_next(i, code, data)
int  i;
int  code;
DATA data;
{
    struct _dir_stack *next;
    struct _dirs      *dir, *other;
    char   *cp;
    extern int default_transfer_mode;
    char   *up;
    CB  *cb = (CB *)data;
    int size;

    Update_Mark(-1); 
    
    if (ftp_response[code].status & FTP_ERROR_RESTART) { 
        dir_stack->i = -1;
	wait_no_connect((caddr_t)cb, NULL);
	return;
    } else if (ftp_response[code].status & FTP_ERROR) {
	char *error = NULL;
	int j;

	Set_Status(last_response);
	if (code > 5) {
            if (dir_stack_func != Start_View && !ignore_errors) {
	    	j = dir_stack->i;
	        if (j >= 0 && j < dir_stack->dir->n) {
	            if (dir_stack_func == Start_Put) {
       	 	        error = concat(error, "Could not put file: ");
	                error = concat(error, dir_stack->dir->files[j].local);
	            } else {
                        error = concat(error, "Could not get file: ");
	                error = concat(error, dir_stack->dir->files[j].remote);
	            }
	        } else {
	            error = concat(error, "Error");
	        }
	        Continue_Dialog(error, cb);
	        XtFree(error);
	        return;
	    } 
	} else {
 	    Clean_Up(code, cb);
	    return;
	}
    }

    if (aborting) return;
    if (!dir_stack) return;
	
    switch(dir_stack->state) {
	case DO_FILES:
	    dir = dir_stack->dir;
	    if (dir_stack->i >=0 ) {
		dir->files[dir_stack->i].mode &= ~MODE_TYPE_DO;
	    } 
            dir_stack->i = 0;
	    for(i= dir_stack->i; i< dir->n; i++) {
	        if ((dir->files[i].mode&MODE_TYPE_DO) &&
		    (dir->files[i].mode&(MODE_TYPE_FILE|MODE_TYPE_LINK)) &&
		    dir->files[i].remote && dir->files[i].local &&
		    !(dir->files[i].mode&MODE_TYPE_IGNORE)) {
		    if (dir->files[i].mode & MODE_TYPE_BINARY &&
		      	transfer_mode != MODE_T_BINARY) {
			Start_Type(0, "image", 0, do_next, (DATA)0, cb);
			return;
	 	    } else if (dir->files[i].mode & MODE_TYPE_ASCII &&
		      	       transfer_mode != MODE_T_ASCII) {
			Start_Type(0, "ascii", 0,  do_next, (DATA)0, cb);
			return;
	 	    } else if (dir->files[i].mode & MODE_TYPE_TENEX &&
		      	       transfer_mode != MODE_T_TENEX) {
			Start_Type(0, "tenex", 0, do_next, (DATA)0, cb);
			return;
		    } else if (default_transfer_mode != transfer_mode &&
			       !(dir->files[i].mode & 
			  (MODE_TYPE_BINARY|MODE_TYPE_ASCII|MODE_TYPE_TENEX))) {
			switch(default_transfer_mode) {
				case MODE_T_BINARY:
				    Start_Type(0, "image", 0, do_next, 
					       (DATA)0, cb);
				    return;
				case MODE_T_ASCII:
				    Start_Type(0, "ascii", 0, do_next, 
					       (DATA)0, cb);
				    return;
				case MODE_T_TENEX:
				    Start_Type(0, "tenex", 0, do_next, 
					       (DATA)0, cb);
				    return;
				default:
				    fprintf(stderr,"Unknow type code %d\n",
					default_transfer_mode);
				    break;
			}
		    }
	            switch(Remote_local) {
			case REMOTE:
                    	     other = Find_Dir(local_dir, localhost, 0);
                             if (other) other->update = 1;
			     break;
		        case LOCAL:
                    	     other = Find_Dir(remote_dir, hostname, 0);
                             if (other) other->update = 1;
			     break;

		    }
                    dir_stack->i = i;
		    Highlight(i, 1);
		    if (dir->files[i].mode&MODE_TYPE_LINK) {
		         size = 0;
		    } else {
		        size = dir->files[i].size;
		    }
                    dir_stack_func(0, dir->files[i].remote, 
				   dir->files[i].local,
				   size, do_next, (DATA)0, cb);
		    return;
	         }
	    }
	    dir_stack->i = -1;
	    dir_stack->state = DO_DIRS;
	case DO_DIRS:
	    dir =  dir_stack->dir;
	    if (dir_stack->i >=0 ) {
		dir->files[dir_stack->i].mode &= ~MODE_TYPE_DO;
	    } 
            dir_stack->i = 0;
	    for(i= dir_stack->i; i< dir->n; i++) {
	        if (dir->files[i].mode&MODE_TYPE_DO &&
		    dir->files[i].mode&MODE_TYPE_DIR &&
		    !(dir->files[i].mode&MODE_TYPE_IGNORE)) {
		    dir_stack->i = i;
 	       	    if (Remote_local == REMOTE &&
			dir->files[i].local) {
			char *cp = NULL;

			cp = concatdir(cp, local_dir);
			cp = concat(cp, dir->files[i].local);
			mkdir(cp, 0700);	
			Start_Lcd(0, dir->files[i].local, cb_cd, (DATA)1, cb);
			XtFree(cp);
		        return;
        	    } else if (Remote_local == LOCAL &&
			       dir->files[i].remote) {
			Start_Mkdir(0, dir->files[i].remote, cb_cd, (DATA)0,
				    cb);
		        return;
        	    }
	         }
	    }
    }

    if (dir_stack->next) {
	for(i=0; i<dir_stack->dir->n; i++) {
	    dir_stack->dir->files[i].mode &= ~MODE_TYPE_DO;
        }
        next = dir_stack->next;
        XtFree((char *)dir_stack);
        dir_stack = next;
        switch(Remote_local) {
	  case REMOTE:
	    Start_Lcd(0, "..", cb_up, (DATA)0, cb);
	    return;
	  case LOCAL:
	    switch(remote_type) {
	      case REMOTE_SYSTEM_UNIX:
		Start_Cd(0, "..", 0, cb_up, (DATA)0, cb);
		return;
              case REMOTE_SYSTEM_OTHER:
		up = Compute_Up();
		if (up) {
                   Start_Cd(0, up , 0, cb_up, (DATA)0, cb);
		   XtFree(up);
                   return;
		}
	    }
	}
    }

    for(i=0; i<dir_stack->dir->n; i++) {
        dir_stack->dir->files[i].mode &= ~MODE_TYPE_DO;
    }

    mult_file = 0;
#if defined(XXX)
    if (Lastdir != dir_stack->dir && !dir_stack->up) {
	dir_stack->up++;
        switch(Remote_local) {
	  case REMOTE:
	    Start_Lcd(0, "..", cb_up, (DATA)0, cb);
	    return;
	  case LOCAL:
	    switch(remote_type) {
	      case REMOTE_SYSTEM_UNIX:
		Start_Cd(0, "..", 0, cb_up, (DATA)0, cb);
		return;
              case REMOTE_SYSTEM_OTHER:
		up = Compute_Up();
		if (up) {
                   Start_Cd(0, up , 0, cb_up, (DATA)0, cb);
		   XtFree(up);
                   return;
		}
	    }
	}
    }
#endif

    Draw_Files(NULL);
    XtFree((char *)dir_stack);
    dir_stack = NULL;
    remove_func_do(clean_up_finish, DO_ABORT|DO_DISCONNECT);
    do_callbacks(1, cb);
    if (dir_stack_cb) do_callbacks(1, dir_stack_cb);
    Clear_Noop(NOOP_ACTION);
    Set_Status("Done...");
}

static void
fix_position(dir, new_type)
struct _dirs *dir;
int           new_type;
{
    if (!dir) return;
    if (dir->position[new_type] == -1) {
    	if(dir->highlight != -1) {
	    dir->position[new_type] = dir->highlight;
	    return;
	} 
	if (dir->list_type != LIST_NONE &&
	    dir->position[dir->list_type] >= 0) {
	    dir->position[new_type] = dir->position[dir->list_type];
	    return;
     	}
    }
}

static void
list_long_dir(dir, type, map, format)
struct _dirs *dir;
int     type;
int     *map;
char    *format;
{
    int i,j,n;
    char *plist[N_LISTING];
    char *cp;

    if (dir->files[0].display && dir->list_type != type)  {
        for (i = 0; i<dir->n; i++) {
	    XtFree(dir->files[i].display);
	    dir->files[i].display = NULL;
	}
    }

    dir->list_type = type;

    if (dir->files[0].display) {
        for (i= 0; i<dir->n; i++) {
	    Add_File_Text(dir->files[i].display,
		dir->files[i].mode&MODE_TYPE_SELECT,
		dir->files[i].mode&MODE_TYPE_DIR);
	}
	return;
    } 

    for (i = 0; i<dir->n; i++) {
	n = 0;
        for (j=0; j<N_LISTING; j++) {
	    if(!map[j]) break;
	    switch(map[j]) {
		case FILE_NAME:
		    plist[j] = dir->files[i].name;
		    break;
		case FILE_SIZE:
		    plist[j] = dir->files[i].g_size;
		    break;
		case FILE_DATE:
		    plist[j] = dir->files[i].g_date;
		    break;
		case FILE_TIME:
		    plist[j] = dir->files[i].g_time;
		    break;
		case FILE_OWNER:
		    plist[j] = dir->files[i].g_owner;
		    break;
		case FILE_PROT:  
		    plist[j] = dir->files[i].g_prot;
		    break;
		case FILE_GROUP:  
		    plist[j] = dir->files[i].g_group;
		    break;
		case FILE_P1:
		    plist[j] = dir->files[i].g_p1;
		    break;
		case FILE_P2:
		    plist[j] = dir->files[i].g_p2;
		    break;
		default:
		fprintf(stderr,"Unknown map %d\n",map[j]);
	    }
	    n += strlen(plist[j]);
	}

        n += strlen(format);
        cp = (char *)XtMalloc(n+100);
        sprintf(cp, format,
		plist[0], plist[1], plist[2], plist[3],
		plist[4], plist[5], plist[6], plist[7],
		plist[8], plist[9]);
        dir->files[i].display = XtNewString(cp);
	Add_File_Text(dir->files[i].display,
		dir->files[i].mode&MODE_TYPE_SELECT,
		dir->files[i].mode&MODE_TYPE_DIR);
	XtFree(cp);
    }
}

static void
list_long_unix_dir(dir)
struct _dirs *dir;
{
    int i;

    if (dir->files[0].display && dir->list_type != LIST_LONG)  {
        for (i = 0; i<dir->n; i++) {
	    XtFree(dir->files[i].display);
	    dir->files[i].display = NULL;
	}
    }

    dir->list_type = LIST_LONG;

    if (dir->files[0].display) {
        for (i= 0; i<dir->n; i++) {
	    Add_File_Text(dir->files[i].display,
		dir->files[i].mode&MODE_TYPE_SELECT,
		dir->files[i].mode&MODE_TYPE_DIR);
	}
	return;
    } 

    for (i = 0; i<dir->n; i++) {
	char *cp;
	char  id[2*16+4];
        
        if (Remote_local == REMOTE && ibm_rt) {
	    sprintf(id, "%-8.8s", 
		    dir->files[i].owner);
	} else if (appolo == 2) {
	    id[0] = '\0';
	} else {
	    sprintf(id, "%-8.8s %-8.8s ", 
		    dir->files[i].owner, 
		    dir->files[i].group);
	}

	if (dir->files[i].mode&MODE_TYPE_LINK) {
    	    cp = (char *)XtMalloc(strlen(dir->files[i].name) + 100 +
			          strlen(dir->files[i].linkname));
            sprintf(cp, 
		" %-10.10s %3d %s %10d %3.3s %2.2s %5.5s %s -> %s",
 	        dir->files[i].modes,
	        dir->files[i].link,
		id,
 	        dir->files[i].size,
 	        dir->files[i].month,
 	        dir->files[i].day,
 	        dir->files[i].time_year, dir->files[i].name,
	        dir->files[i].linkname);
	} else {
            cp = (char *)XtMalloc(strlen(dir->files[i].name) + 100);
            sprintf(cp, " %-10.10s %3d %s %10d %3.3s %2.2s %5.5s %s",
 	        dir->files[i].modes,
	        dir->files[i].link,
		id,
 	        dir->files[i].size,
 	        dir->files[i].month,
 	        dir->files[i].day,
 	        dir->files[i].time_year,
 	        dir->files[i].name);
	}
	
        dir->files[i].display = cp;
	Add_File_Text(cp, dir->files[i].mode&MODE_TYPE_SELECT,
			  dir->files[i].mode&MODE_TYPE_DIR);
    }
}

static void
list_medium_unix_dir(dir)
struct _dirs *dir;
{
    int i;

    if (dir->files[0].display && dir->list_type != LIST_MEDIUM)  {
        for (i = 0; i<dir->n; i++) {
	    XtFree(dir->files[i].display);
	    dir->files[i].display = NULL;
	}
    }

    dir->list_type = LIST_MEDIUM;

    if (dir->files[0].display) {
        for (i= 0; i<dir->n; i++) {
	    Add_File_Text(dir->files[i].display, 
		dir->files[i].mode&MODE_TYPE_SELECT,
		dir->files[i].mode&MODE_TYPE_DIR);
	}
	return;
    } 

    for (i = 0; i<dir->n; i++) {
	char *cp;

        cp = (char *)XtMalloc(strlen(dir->files[i].name) + 100);
        sprintf(cp, " %-10.10s %10d %3.3s %2.2s %5.5s %s",
 	        dir->files[i].modes,
 	        dir->files[i].size,
 	        dir->files[i].month,
 	        dir->files[i].day,
 	        dir->files[i].time_year,
 	        dir->files[i].name);
        dir->files[i].display = cp;
	Add_File_Text(cp,
		dir->files[i].mode&MODE_TYPE_SELECT,	
		dir->files[i].mode&MODE_TYPE_DIR);
    }
}

static void
list_short_dir(dir)
struct _dirs *dir;
{
    int i;

    if (dir->files[0].display && dir->list_type != LIST_SHORT)  {
        for (i = 0; i<dir->n; i++) {
	    XtFree(dir->files[i].display);
	    dir->files[i].display = NULL;
	}
    }

    dir->list_type = LIST_SHORT;

    if (dir->files[0].display) {
        for (i = 0; i<dir->n; i++)  {
	    Add_File_Text(dir->files[i].display,
		          dir->files[i].mode&MODE_TYPE_SELECT,
		          dir->files[i].mode&MODE_TYPE_DIR);
	}
	return;
    } 

    for (i = 0; i<dir->n; i++) {
	char *cp = NULL;
	if(dir->files[i].mode&MODE_TYPE_DIR) { 
	    cp = concat(cp, "d ");
        } else if (dir->files[i].mode&MODE_TYPE_LINK) {
	    cp = concat(cp, "l ");
        } else if (dir->files[i].mode&MODE_TYPE_OFFLINE) {
	    cp = concat(cp, "m ");
        } else {
	    cp = concat(cp, "  ");
	}
        cp = concat(cp, dir->files[i].name);
        dir->files[i].display = cp;
	fflush(stderr);
	Add_File_Text(cp,
		      dir->files[i].mode&MODE_TYPE_SELECT,
		      dir->files[i].mode&MODE_TYPE_DIR);
    }
}

static void
list_short_unix_dir(dir)
struct _dirs *dir;
{
    int i;

    if (dir->files[0].display && dir->list_type != LIST_SHORT)  {
        for (i = 0; i<dir->n; i++) {
	    XtFree(dir->files[i].display);
	    dir->files[i].display = NULL;
	}
    }

    dir->list_type = LIST_SHORT;

    if (dir->files[0].display) {
        for (i = 0; i<dir->n; i++)  {
	    Add_File_Text(dir->files[i].display,
			  dir->files[i].mode&MODE_TYPE_SELECT,
			  dir->files[i].mode&MODE_TYPE_DIR);
	}
	return;
    } 

    for (i = 0; i<dir->n; i++) {
	char *cp = NULL;
        cp = concat(cp, dir->files[i].name);
	if(dir->files[i].mode&MODE_TYPE_DIR) { 
	    if (cp &&  cp[strlen(cp)-1] != '/') {
	        cp = concat(cp, "/");
	    }
        } else if (dir->files[i].mode&MODE_TYPE_LINK) {
	    cp = concat(cp, "@");
        } else if (dir->files[i].mode&MODE_TYPE_OFFLINE) {
	    cp = concat(cp, "%");
        } 
        dir->files[i].display = cp;
	Add_File_Text(cp,
		      dir->files[i].mode&MODE_TYPE_SELECT,
		      dir->files[i].mode&MODE_TYPE_DIR);
    }
}

static void
list_translations(dir)
struct _dirs *dir;
{
    int i, k, n;

    dir->update_file = update_translation;
    if (dir->files[0].display && dir->list_type != LIST_TRANSLATIONS)  {
        for (i = 0; i<dir->n; i++) {
	    XtFree(dir->files[i].display);
	    dir->files[i].display = NULL;
	}
    }
    dir->list_type = LIST_TRANSLATIONS;
    max_tran_name = 5;
    for (i = 0; i<dir->n; i++) {
	if ((k = strlen(dir->files[i].name)) > max_tran_name) max_tran_name = k;
    }
    sprintf(tran_format1, "%%-%ds -> ", max_tran_name);
    max_tran_tran = 5;
    for (i = 0; i<dir->n; i++) {
        switch(dir->type) {
	    case REMOTE_SYSTEM_LOCAL:
		if (!dir->files[i].remote) break; 
	        if ((k = strlen(dir->files[i].remote)) >  max_tran_tran) 
				max_tran_tran = k;
	        break;
	    default:
		if (!dir->files[i].local) break; 
	        if ((k = strlen(dir->files[i].local)) >  max_tran_tran) 
				max_tran_tran = k;
		break;
	}
    }
    sprintf(tran_format2, "%%-%ds ",  max_tran_tran);

    for (i = 0; i<dir->n; i++) {
        update_translation(dir,i);
	Add_File_Text(dir->files[i].display, 
		dir->files[i].mode&MODE_TYPE_SELECT, 
		dir->files[i].mode&MODE_TYPE_DIR);
    }
}

static void
retain_position(dir)
struct _dirs *dir;
{
    int n;

    if (!dir) return;
    if (!Lastdir || Lastdir != dir) return;
    if (dir->list_type == LIST_NONE) return;
    n = Get_List_Position();
    if (n >= dir->n) return;
    dir->position[dir->list_type] = n;
}

static void
update_translation(dir, i)
struct _dirs *dir;
int           i;
{
    char *cp1, *cp2;

    cp1 = (char *)XtMalloc(1+max_tran_name+strlen(tran_format1));
    sprintf(cp1, tran_format1, dir->files[i].name);
    switch(dir->type) {
        case REMOTE_SYSTEM_LOCAL:
	    cp2 = XtMalloc(max_tran_tran+1+strlen(tran_format2));
	    if (dir->files[i].remote) {
                sprintf(cp2, tran_format2, dir->files[i].remote);
	    }  else {
                sprintf(cp2, tran_format2, "");
 	    }
	    cp1 = concat(cp1, cp2);
	    break;
        default:
	    cp2 = XtMalloc(max_tran_tran+1+strlen(tran_format2));
	    if (dir->files[i].local) {
                sprintf(cp2, tran_format2, dir->files[i].local);
	    } else {
                sprintf(cp2, tran_format2, "");
	    }
	    cp1 = concat(cp1, cp2);
	    break;
    }
    if (dir->files[i].mode&MODE_TYPE_ASCII) {
	cp1 = concat(cp1, "(Transfer mode ASCII) ");
    } else if (dir->files[i].mode&MODE_TYPE_BINARY) {
	cp1 = concat(cp1, "(Transfer mode BINARY)");
    } else if (dir->files[i].mode&MODE_TYPE_TENEX) {
	cp1 = concat(cp1, "(Transfer mode TENEX)");
    } else {
	cp1 = concat(cp1, "                      ");
    }
    XtFree(cp2);
    if (dir->files[i].display) {
	strcpy(dir->files[i].display, cp1);
	XtFree(cp1);
    } else {
	dir->files[i].display = cp1;
    }
}

static void
wait_no_connect(client_data, id)
caddr_t client_data;
XtIntervalId *id;
{
    
    if (connected) {
	XtAppAddTimeOut(App_context,
               	        (unsigned long)(1*1000),
                        (XtTimerCallbackProc)wait_no_connect,
                        (XtPointer)client_data);
    } else {
	if (ignore_errors) {
	    Dialog_Restart(DIALOG_RESTART_IGNORE, (CB*)client_data);
	} else {
	    Dialog_Restart(DIALOG_RESTART_CONTINUE, (CB*)client_data);
	}
    }
}
