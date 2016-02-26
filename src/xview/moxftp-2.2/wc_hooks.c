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
"$Id: wc_hooks.c,v 1.3 1994/05/20 20:21:21 jones Exp jones $\n";
static char rcspath[] =
"$Source: /mintaka/u0/xx/ss/jones/jones.old/mftp.src/moxftp-2.2/RCS/wc_hooks.c,v $\n";
#endif

/* $Log: wc_hooks.c,v $
 * Revision 1.3  1994/05/20  20:21:21  jones
 * Do not depend on X11R5 NULL value XtGetValues trick.
 *
 * Revision 1.2  1994/03/22  23:21:37  jones
 * Correctly set remote dir form .moxftprc file.
 *
 * Revision 1.1  1994/03/14  18:55:53  jones
 * Initial revision
 *
 */

#include <ctype.h>
#include <stdio.h>

#include "machine.h"
#include "defs.h"
#include "popen.h"
#include "connect_info.h"
#include "wc.h"
#include "view_progs.h"

#include <X11/Xlib.h>
#include <X11/cursorfont.h>
#include <X11/Xos.h>
#include "List.h"
#include "ListSWP.h"
#include "LabelQUICK.h"

/*
 * Local Function Proto types.
 */
#include "proto/wc_hooks.h"

/*
 * Externals.
 */
extern XtAppContext  App_context;
extern Widget	     Top;
extern struct _dirs *Lastdir;
extern char *local_host_name; 
extern char *username; 
extern int  no_dot_dot;
extern char *gateway;
extern int use_gateway;
extern int aborting;

char *GetLabel();
char *GetText();

static int L_w = 0;
static int L_h = 0;

static char **host_text;
static int   *host_select;

extern int Remote_local;
       int retry = 0;			/* Retry flag */
XtIntervalId timeout_retry = 0; 	/* Sort status timeout */
static char *last_status_msg = NULL;

char pass[100];

/*
 * Options.
 */
int noautodir = 0;
int ignorerrs = 0;
static struct _op {
	char *name;
	int  *value;
	char *set;
	char *unset;
} op[] =  {
{"NoAutoDir"   ,  &noautodir, "No Auto Directory Listings", 
			      "Auto Directory Listings"},
{"IgnoreErrors",  &ignorerrs, "Ignore Errors", 
			      "Don't Ignore Errors"}
};


/*
 * Connect structure.
 */
struct _connect_info connect_info[] = {
#define INDEX_HOST      0
  { "host"     , NULL, NULL,  &hostname    },
#define INDEX_LOGIN     1
  { "login"    , NULL, NULL,  &login       },
#define INDEX_PASSWORD  2 
  { "password" , NULL, NULL,  &password    },
#define INDEX_REMOTEDIR 3
  { "remotedir", NULL, "." ,  &remote_dir  },
#define INDEX_LOCAL     4
  { "localdir" , NULL, NULL , &local_dir   },
  { "gateway"  , NULL, NULL,  &gateway}
};

int NO_CONNECT = sizeof(connect_info)/sizeof(struct _connect_info);

       Widget Status_text       = NULL;
static Widget Connect           = NULL;
static Widget Item_w            = NULL;
static Widget List_w	        = NULL;
static Widget Host_w 		= NULL;
static Widget Host_name         = NULL;
static Widget System_name       = NULL;
static Widget Default_mode      = NULL;
static Widget Status_w          = NULL;
static Widget Dir_w	        = NULL;
static Widget Remote_local_w    = NULL;
static Widget Percent           = NULL;
static Widget Listing_w		= NULL;
static GC     Percent_gc        = NULL;
static Widget Tran_w = NULL;
static Widget Shelltran_w = NULL;
static Widget Shellstatus_w = NULL;

static Widget Connect_Shell    = NULL;
static Widget fmenu_clear_all  = NULL;
static Widget fmenu_get_all    = NULL;
static Widget fmenu_put_all    = NULL;
static Widget fmenu_delete_all = NULL;

static Widget smbytype  = NULL;
static Widget smnormal  = NULL;

static MyXawListReturnStruct items;

/*
 * local structures.
 */

#define X_R    0x001	/* remote */
#define X_L    0x002	/* local */

#define X_F    0x004	/* file */
#define X_D    0x008	/* directory */

#define X_M    0x010	/* markced */

#define X_A    0x020	/* ascii */
#define X_B    0x040    /* binary */
#define X_T    0x080    /* tenex */
#define X_DM   0x100    /* default mode */
#define X_TM   (X_A|X_B|X_T|X_DM) 

#define X_O    0x200	/* alwasy active */
#define X_I    0x400
#define X_U    0x800

static struct {
      int type;
      int flags;
      char *name;
} file_actions_types[] = {
        {DO_UP,         X_O,              "up",},
        {DO_DIR,        X_O,              "dir",},
        {DO_CD,         X_R|X_L|X_D,      "cd"},
        {DO_GET,        X_R|X_F|X_D,      "get"}, 
        {DO_VIEW,       X_R|X_F,          "view"},
        {DO_PUT,        X_L|X_F,          "put"},
        {DO_ASCII,      X_R|X_L|X_F|X_A,  "ascii"},
        {DO_BINARY,     X_R|X_L|X_F|X_B,  "binary"},
        {DO_TENEX,      X_R|X_L|X_F|X_T,  "tenex"},
        {DO_DEFAULT,    X_R|X_L|X_F|X_DM, "default"},
	{DO_IGNORE,     X_R|X_L|X_I,	  "ignore"},
	{DO_USE,        X_R|X_L|X_U,	  "use"},
        {DO_clear_all,  X_R|X_L|X_M,      "clear_all"},
        {DO_get_all,    X_R|X_M,          "get_all"},
        {DO_put_all,    X_L|X_M,          "put_all"},
        {DO_delete_all, 0,                "delete_all"},
	{0,		0,                 NULL}
};

#define MAX_ACTIONS 500
static int num_file_actions = 0;
static struct _file_actions {
    int type;
    int flags;
    Widget w;
} file_actions[MAX_ACTIONS];

#if defined(XAW)||defined(OPENWINDOW)
#define XXsensitive  XtNsensitive
#endif
#if defined(MOTIF)
#define XXsensitive  XmNsensitive
#endif

static struct {
     char   *name;
     char   *icon;
     void   (*callback)();
} shell_icons[] = {
 {"*Shellcommand", "comm",   clear_command},
 {"*Shellstatus",  "status", clear_status_shell},
 {"*Shelltran",    "tran",   clear_tran},
 {NULL, 	    NULL,    NULL},
};

/*
 * File list management.
 */
static char **file_text = NULL;
static int  *file_select = NULL;
static int   file_num = 0;
static int   file_n = 0;
static int   Do_action  = 0;

/*
 * Toggle list
 */
struct _toggle {
     char *type;
     char *subtype;
     Widget w;
     struct _toggle *next;
} *toggle = NULL;

/*
 * Percent data.
 */
static int Percent_x = 0;
static Widget Rate_w;
struct timeval t1;
struct timeval t2;
struct timeval t3;

/*
 * Search data.
 */
static int   search_last = -1;
static char *search_string = NULL;
static int   search_exact = -1;

/*
 * Glob data.
 */
static regexp *search_globreg = NULL;
static int   search_host_last = -2;
static char *search_host_string = NULL;
static regexp *search_host_globreg = NULL;


/*
 * Global data.
 */
static char *tran  = NULL;
static XtIntervalId timeout = 0;
static Widget Log_w;
char *status_xx = NULL;
static Widget Filesopts_w = NULL;
struct _view_file {
   	char *file;
	char *name;
	FILE *FILEX;
};

/*
 * Actions
 */
static XtActionsRec Actions[] = {
    {"resize_percent"     , resize_percent_act},
    {"Cd"                 , cd_act},
    {"Do_up"              , do_up_act},
    {"Single_File_Actions", single_file_actions_act},
    {"Listing_type"       , listing_type_act},
    {"remote_local_toggle", remote_local_toggle_act},
    {"Open_file"          , open_file_act},
    {"SetFileAction"      , set_file_action_act},
    {"SetStatusIcon"      , set_status_icon_act},
    {"Login"              , do_connect_act},
    {"SetIcons"           , set_icons_act},
    {"search_next"	  , search_act},
    {"search_host"	  , search_host_act},
    {"Noop"		  , noop_act}
};

/*
 * Functions and subroutines.
 */


void 
Add_File_Text(s, selected, bold)
char *s;
int  selected;
int  bold;
{

    if (!file_text || file_n >= file_num) {
	if (file_text) {
            file_text = 
		(char **)XtRealloc((char *)file_text, 
			           2*file_num*sizeof(char **));
            file_select = 
		(int *)XtRealloc((char *)file_select, 
				 2*file_num*sizeof(int));	
	    file_num *= 2;
	} else {
	    /*
	     * Yes there are system out there with more then 1000
	     * files in a directory. 
	     */
	    file_text = (char **)XtMalloc(3000*sizeof(char **));
 	    file_select = (int*)XtMalloc(3000*sizeof(int));
	    file_num = 3000;
	}
    }

    if (s)  {
	file_text[file_n] = s;
	file_select[file_n]  = 0;
	if (selected) file_select[file_n] |= LIST_SELECT;
	if (bold) file_select[file_n] |= LIST_BOLD;
	file_n++;
 	return;
    }
    file_text[file_n] = NULL;
    file_select[file_n] = 0;
    Changetext(file_text, file_select, file_n, 0, -1);
    file_n = 0;
}

void 
Add_Status_Text(s) 
char *s;
{
    SetWidget(Status_text, "*.ftp", 0);
    if(Status_text != NULL) AppendDialogText(Status_text, s);
}

void 
Archie_Host(host, dir)
char *host;
char *dir;
{
    int j = INDEX_HOST;
    char *pass = NULL;

    if (connect_info[j].p && *connect_info[j].p) XtFree(*connect_info[j].p);
    *connect_info[j].p =  XtNewString(host);
    if (connect_info[j].w) SetText(connect_info[j].w, *connect_info[j].p);

    j = INDEX_REMOTEDIR;
    if (connect_info[j].p && *connect_info[j].p) XtFree(*connect_info[j].p);
    *connect_info[j].p = XtNewString(dir); 
    if (connect_info[j].w) SetText(connect_info[j].w, *connect_info[j].p);

    j = INDEX_LOGIN;
    if (connect_info[j].p && *connect_info[j].p) XtFree(*connect_info[j].p);
    *connect_info[j].p = XtNewString("anonymous");
    if (connect_info[j].w) SetText(connect_info[j].w, *connect_info[j].p);

    j = INDEX_PASSWORD;
    pass = concat(pass, username);
    pass = concat(pass, "@");
    pass = concat(pass, local_host_name);

    if (connect_info[j].p && *connect_info[j].p) XtFree(*connect_info[j].p);
    *connect_info[j].p = set_text_password(connect_info[j].w, pass);
    XtFree(pass);
}

void 
Changetext(list, select, n, pos, h) 
char   **list;
int    *select;
int    n;
int    pos;
int    h;
{

    SetWidget(List_w, "*listsw.list", 0);
    MyXawListChange(List_w, list, 0, 0, 1, select, pos, h);
    Single_File_Actions();
}

void 
Clear_Files_Marks()
{
    struct _dirs *dir;
    int i;

    dir = Lastdir;
    if (!dir) return;

    for (i=0; i<dir->n; i++) {
	if (file_select[i]&LIST_SELECT) {
	    file_select[i] &= ~LIST_SELECT;
	    MyXawListHighlight(List_w, i);
	}
    }
}

char * 
Compute_Up()
{
    char *up = NULL;

    if (s_tran->up) {
        up = XtNewString(s_tran->up);
    } else if (s_tran->up_exp && regexec(s_tran->up_regexp, remote_dir)) {
    	up = (char *)XtMalloc(2000);
        regsub(s_tran->up_regexp, s_tran->up_source, up);
    }
    if (!up && s_tran->up) {
	Set_Status_Short("Don't know how to go up any father");
    } else if (!up) {
	Set_Status_Short(
	   "I don't know how to go up a directory tree for the system");
    }
    return up;
}

char * 
Compute_Up_Apollo()
{
    char *up = NULL;
    char *cp;

    up = XtNewString(remote_dir);
    cp = RINDEX(remote_dir, '/');
    if (cp && cp != &remote_dir[0]) {
	 up[cp - remote_dir] = '\0';
    }
    return up;
}

void 
Finish_File_Text(pos, h)
int pos;
int h;
{
    file_text[file_n] = NULL;
    file_select[file_n] = 0;
    Changetext(file_text, file_select, file_n, pos, h);
    file_n = 0;
}

int 
Get_List_Position()
{
    int  position;

    SetWidget(List_w, "*listsw.list", 0);
    position =  GetCurrentPos(List_w);
    return position;
}

void 
Glob_Files(glob, select)
char *glob;
int   select;
{
    struct _dirs *dir;
    int i;
    regexp *globreg;
    int found = 0;

    dir = Lastdir;
    if (!dir) return;
    if (glob == NULL) return;

    globreg = regcomp(glob);

    if (globreg == NULL) return;

    for (i=0; i<dir->n; i++) {
	if(regexec(globreg,dir->files[i].name) ){
	    if (select) {
		Lastdir->files[i].mode |= MODE_TYPE_SELECT;
	        file_select[i] |= LIST_SELECT;
	    } else {
		Lastdir->files[i].mode &= ~MODE_TYPE_SELECT;
	        file_select[i] &= ~LIST_SELECT;
	    }
	    MyXawListHighlight(List_w, i);
	    found++;
        }
    }
    if (found) Count_Files(dir, 1);
    XtFree((char *)globreg);
}

void 
Highlight(i, c)
int i;
int c;
{
    Widget xx;
    extern Widget Top;

    xx  = WcFullNameToWidget(Top, "*list");
    MyXawListHighlight(xx, i);
    if (c) SetPos(xx, i, 0);
/*
    if (c) MyXawListSetPos(xx);
*/
    Single_File_Actions();
}

void 
Init_Close()
{
    SetWidget(Connect, "*.connect", 0);
    SetLabel(Connect, "Close");
    WcSetSensitiveCB(Connect, "this", NULL);
}

void 
Init_Login()
{
    Clear_Noop(NOOP_CLEAR_ALL);
    Set_Noop(NOOP_NOTCONN);
    SetWidget(Connect, "*.connect", 0);
    WcSetSensitiveCB(Connect, "this", NULL);
    WcSetInsensitiveCB(Top, "*reconnect", NULL);
    logged_in = 0;
    last_response[0] = '\0';
    SetLabel(Connect, "Login");
}

void 
Register()
{
    RCP  ("MyListSW"           , MylistSWWidgetClass);
    RCP  ("myListSWClass"      , MylistSWWidgetClass); 
    RCP  ("LableQUICKClass"    , labelQUICKWidgetClass);
}

void 
Register_App_CallBacks_and_Actions()
{
    XtAppAddActions(App_context, Actions, 
			 sizeof(Actions)/sizeof(XtActionsRec));

    RCALL("Login"   	        , do_connect_cb);
    RCALL("init_connect_info"  	, init_connect_info);
    RCALL("set_connect_info"   	, set_connect_info);
    RCALL("connect_disconnect" 	, connect_disconnect);
    RCALL("SetFileAction"      	, set_file_action_cb);
    RCALL("SetFileAction_menu" 	, set_file_action_menu_cb);
    RCALL("SetHostList"		, set_host_list_cb);
    RCALL("GetConnectInfo"     	, get_connect_info);
    RCALL("Login"   	        , do_connect_cb);
    RCALL("Do_up"              	, do_up_cb);
    RCALL("listing_type"       	, listing_type_cb);
    RCALL("remote_local_toggle"	, remote_local_toggle_cb);
    RCALL("quit"		, quit_cb);
    RCALL("abort"		, abort_cb);
    RCALL("Do_cd"              	, do_cd_cb);
    RCALL("msg"			, msg_cb);
    RCALL("search_next"		, search_cb);
    RCALL("Reconnect"		, reconnect_cb);
    RCALL("set_tran"	        , set_tran_cb);
    RCALL("Clear_List_Entry"   	, clear_list_entry_cb);
    RCALL("list_notify"        	, list_notify);
    RCALL("SelectHost"         	, select_host_cb);
    RCALL("List_Translations"  	, list_translations_cb);
    RCALL("Shelltran"		, shell_tran_cb);
    RCALL("Anonymous"		, anonymous_cb);
    RCALL("SetIcons"		, set_icons_cb);
    RCALL("Register_action"	, register_action_cb);
    RCALL("Single_File_Actions"	, single_file_actions_cb);
    RCALL("toggle"		, toggle_cb);
    RCALL("mark_menu"          	, mark_menu_cb);
    RCALL("raise_window"	, raise_window_cb);
    RCALL("op"			, op_cb);
    RCALL("NoWindowGroup"       , no_window_group_cb);

    RCALL("NoWindowGroup"     , no_window_group_cb);
    Register_help_CallBacks_and_Actions();
#if defined(XAW)
     init_xaw_acctions(App_context);
#endif
#if defined(MOTIF)
     init_motif_acctions(App_context);
#endif
#if defined(OPENWINDOW)
     init_olt_acctions(App_context);
#endif
     Register_Noop_CallBacks_and_Actions();
     Init_Tranlation_List(Top);
     Init_Archie();
}

void 
Reset_Search()
{
    search_last = -1;
    search_exact = -1;
    if (search_string) XtFree(search_string);
    if (search_globreg) XtFree((char *)search_globreg);
    search_globreg = NULL;
    search_string = NULL;;
    Reset_Search_String();
    WcSetInsensitiveCB(Top, "*next", NULL);
}

void 
Restore_Last_Status_Msg()
{
    if (last_status_msg) {
        Set_Status_No_Log(last_status_msg);
    } else {
        Set_Status_No_Log(" ");
    }
}

void 
Search_Files(search, exact)
char *search;
int   exact;
{
    search_last = -1;
    if (search_string) XtFree(search_string);
    if (search_globreg) XtFree((char *)search_globreg);
    search_globreg = NULL;
    search_string = XtNewString(search);
    search_exact = exact;
    if (!exact) search_globreg = regcomp(search_string);
    search_next();
}

void 
Search_Host(search)
char *search;
{
    search_host_last = -1;
    if (search_host_string) XtFree(search_host_string);
    if (search_host_globreg) XtFree((char *)search_host_globreg);
    search_globreg = NULL;
    search_host_string = XtNewString(search);
    search_host_globreg = regcomp(search_host_string);
    search_host_next();
}

void 
Set_Busy(mode)
{
    set_icon(mode);
}

void 
Set_Info()
{
    extern char *system_name;
    extern char *system_type;
    extern char *ftp_type;
    extern int  default_transfer_mode;

    SetWidget(Host_name, "*.host_name", 0);
    if (hostname) SetLabel(Host_name, hostname);
    else          SetLabel(Host_name, "");
   
    SetWidget(System_name, "*.system_name", 0);
    if (system_name) SetLabel(System_name, system_name);
    else             SetLabel(System_name, "");

    if (ftp_type) XtFree(ftp_type);
    if (default_transfer_mode == MODE_T_BINARY) {
            ftp_type = XtNewString("Binary");
    } else if (default_transfer_mode == MODE_T_ASCII) {
            ftp_type = XtNewString("Ascii");
    } else if (default_transfer_mode == MODE_T_TENEX) {
            ftp_type = XtNewString("Tenex");
    } else {
            ftp_type = XtNewString("UNKNOWN");
    }

    SetWidget(Default_mode, "*.default_mode", 0);
    SetLabel(Default_mode, ftp_type);
}

void 
Set_Item_Title(title)
char *title;
{
    SetWidget(Item_w, "*listsw.items", 0);
    SetLabel (Item_w,  title);
}

void 
Set_Local(s)
char *s;
{
    char *t = NULL;

    if (local_dir) {
	XtFree(local_dir);
    }
    local_dir = XtNewString(s);
    if( Remote_local == LOCAL) {
       	SetWidget(Dir_w, "*.dir", 0);
        t =  concat(t, "Local Directory: ");
        t =  concat(t, local_dir);
        SetLabel(Dir_w, t);
        XtFree(t);
    }
}

void 
Set_Reconnect(reconnect)
int reconnect;
{
    Set_Noop(NOOP_NOTCONN);
    if(logged_in) WcSetSensitiveCB(Top, "*reconnect", NULL);
    aborting = 1;
    if (reconnect) {
         do_all(DO_DISCONNECT);
    } else {	
         do_all(DO_RESTART);
    }
    aborting = 0;
    set_icon(ICON_RECONNECT);
}

void 
Set_Remote(s) 
char *s;
{
    char *t = NULL;

    if (remote_dir) {
	XtFree(remote_dir);
    }
    remote_dir = XtNewString(s);
    if( Remote_local == REMOTE) {
       	SetWidget(Dir_w, "*.dir", 0);
        t =  concat(t, "Remote Directory: ");
        t =  concat(t, remote_dir);
        SetLabel(Dir_w, t);
        XtFree(t);
    }
}

void 
Set_Remote_Local(type)
int type;
{
    char *t = NULL;
    
    SetWidget(Remote_local_w, "*.dir_display", 0);
    if (Remote_local == LOCAL) {
	SetLabel(Remote_local_w, "Local ");
       	SetWidget(Dir_w, "*.dir", 0);
        t =  concat(t, "Local Directory: ");
        t =  concat(t, local_dir);
        SetLabel(Dir_w, t);
        XtFree(t);
    } else {
	SetLabel(Remote_local_w, "Remote");
       	SetWidget(Dir_w, "*.dir", 0);
        t =  concat(t, "Remote Directory: ");
        t =  concat(t, remote_dir);
        SetLabel(Dir_w, t);
        XtFree(t);
    }
}

void 
Set_Status(s)
char *s;
{
    char *ss = NULL;

    if (last_status_msg) {
	XtFree(last_status_msg);
  	last_status_msg = NULL;
    }
    Set_Status_No_Log(s);
    last_status_msg = XtNewString(s);
    SetWidget(Status_w, "*.status", 0);
    if (s && s[0] && s[0] != ' ') {
        if (!Shellstatus_w) {
            Shellstatus_w = WcFullNameToWidget(Top, "*Shellstatus");
            if (!Shellstatus_w) {
                WcCreateNamedPopups ( Top, "Shellstatus" );
            }
            Shellstatus_w = WcFullNameToWidget(Top, "*Shellstatus");
            if (!Shellstatus_w) {
                fprintf(stderr,"Could not create Status Popup Shell\n");
                exit(1);
            }
        }
        SetWidget(Log_w, "*.status_text", 0);
	ss = concat(ss, s);
	ss = concat(ss, "\n");
	if (status_xx && strcmp(status_xx, ss) == 0) {
	    XtFree(ss);
	} else {
            AppendStatusText(Log_w, ss);
	    if (status_xx) XtFree(status_xx);
	    status_xx = ss;
	}
    }
}

void 
Set_Status_Error(s1, s2)
char *s1;
char *s2;
{
    char *text = NULL;

    text = concat(text, s1);
    text = concat(text, s2);
    Set_Status(text);
    XtFree(text);
}

void 
Set_Status_No_Log(s)
char *s;
{
    if(timeout) {
        XtRemoveTimeOut(timeout);
        timeout = 0;
    }
    SetWidget(Status_w, "*.status", 0);
    SetLabel(Status_w, s);
}

void 
Set_Status_Short(s)
char *s;
{
    Set_Status(s);
    timeout =  XtAppAddTimeOut(App_context, (unsigned long)60*1000, 
			       (XtTimerCallbackProc)clear_status, 
			       (XtPointer)NULL);
}

void 
Set_Xaw_List(type)
int type;
{
    Arg arg[2];

    SetWidget(List_w, "*listsw.list", 0);
    if (type == LIST_LONG || type == LIST_MEDIUM)  {
        XtSetArg(arg[0], XtNforceColumns, TRUE);            
        XtSetArg(arg[1], XtNdefaultColumns, 1);            
        XtSetValues(List_w, arg, XtNumber(arg));
    } else {
        XtSetArg(arg[0], XtNforceColumns, FALSE);            
        XtSetArg(arg[1], XtNdefaultColumns, 100);            
        XtSetValues(List_w, arg, XtNumber(arg));
    }
}

void 
Single_File_Actions()
{
    MyXawListReturnStruct *item;
    struct _dirs *dir;
    extern struct _dirs *Lastdir;
    int i;
    int selected = 0;
    Arg Sensitive[1];
    Arg Insensitive[1];
    int rl = 0;
    int ft = 0;
    int tt = 0;
    int ma = 0;
    int ig = 0;

    XtSetArg(Sensitive[0],   XXsensitive, TRUE);
    XtSetArg(Insensitive[0], XXsensitive, FALSE);

    if (!connected) {
        for (i=0; i<num_file_actions; i++) {
    	    XtSetValues(file_actions[i].w, Insensitive, 1);
        }
	return;
    }

    dir = Lastdir;
    if (dir && dir->n > 0) {
	for (i = 0;  i< dir->n; i++) {
	     if (dir->files[i].mode&MODE_TYPE_SELECT) {
		ma = X_M;
	     }
	}
    } 

    if (Remote_local == REMOTE) {
        rl |= X_R;
    } else {
        rl |= X_L;
    }

    SetWidget(List_w, "*listsw.list", 0);
    item = MyXawListShowCurrent(List_w);
    items = *item;
    i = item->list_index;
    if (i == XAW_LIST_NONE || !dir || 
	dir->n <= i ||
        (dir->files[i].mode&(MODE_TYPE_FILE|MODE_TYPE_LINK) &&
         (!dir->files[i].remote || !dir->files[i].local))) {
	ft = 0;
    } else if (dir->files[i].mode & MODE_TYPE_DIR) {
	ft = X_D;
	ig = dir->files[i].mode & MODE_TYPE_IGNORE;
    } else if (dir->files[i].mode & (MODE_TYPE_LINK|MODE_TYPE_FILE)) {
	if (dir->files[i].mode & MODE_TYPE_LINK) {
	    ft = X_D|X_F;
 	} else {
	    ft = X_F;
	}
        if (dir->files[i].mode & MODE_TYPE_ASCII) {
	    tt = X_DM|X_B|X_T;
	} else if (dir->files[i].mode & MODE_TYPE_BINARY) {
	    tt = X_DM|X_A|X_T;
	} else if (dir->files[i].mode & MODE_TYPE_TENEX) {
	    tt = X_DM|X_A|X_B;
	} else {
	    tt = X_DM|X_A|X_B|X_T;
	}
	ig = dir->files[i].mode & MODE_TYPE_IGNORE;
    } 

    for (i=0; i<num_file_actions; i++) {
	if (X_O & file_actions[i].flags) {
    	     XtSetValues(file_actions[i].w, Sensitive, 1);
	     continue;
	} else {
             XtSetValues(file_actions[i].w, Insensitive, 1);
	}

	if ((rl&file_actions[i].flags) == 0)  continue;

	if ((ft&X_F) && (ft&file_actions[i].flags)) {
	    if((X_TM&file_actions[i].flags) == 0 ||
	       (tt&file_actions[i].flags)) {
    	        XtSetValues(file_actions[i].w, Sensitive, 1);
	    }
        }
	if ((ft&X_D) && (ft&file_actions[i].flags)) {
    	    XtSetValues(file_actions[i].w, Sensitive, 1);
	}

        if ((ma&X_M) && (file_actions[i].flags&X_M)) {
    	    XtSetValues(file_actions[i].w, Sensitive, 1);
	}

        if ((file_actions[i].flags&X_I) && !ig && (ft&(X_D|X_F))) {
    	    XtSetValues(file_actions[i].w, Sensitive, 1);
	}

        if ((file_actions[i].flags&X_U) && ig && (ft&(X_D|X_F))) {
    	    XtSetValues(file_actions[i].w, Sensitive, 1);
	}
    }
    XtFree((char *)item);
}

void 
Stop_Retry()
{
    retry = 0;
    Reset_Retry_Button();
    if (!connected) Set_Remote(".");
    if(timeout_retry) {
	XBell(XtDisplay(Top), -80);
	XtRemoveTimeOut(timeout_retry);
	timeout_retry = 0;
    }
}

void 
Unset_Busy() 
{
    set_icon(ICON_CONNECT);
}

void 
Update_Files_Opts(x)
int x;
{
    Arg Sensitive[1];
    Arg Insensitive[1];

    XtSetArg(Sensitive[0],   XXsensitive, TRUE);
    XtSetArg(Insensitive[0], XXsensitive, FALSE);

    SetWidget(Filesopts_w , "*filesopts" , 0);

    if (x) {
    	XtSetValues(Filesopts_w, Sensitive, 1);
    } else {
    	XtSetValues(Filesopts_w, Insensitive, 1);
    }
}

void 
Update_Mark(i)
int i;
{
    struct _dirs *dir = Lastdir;

    SetWidget(List_w, "*listsw.list", 0);
/*
    MyXawListUnhighlight(List_w);
*/
    if (dir == NULL || i == XAW_LIST_NONE || i < 0 || i> dir->n) return;
    XStoreBytes(XtDisplay(List_w), dir->files[i].name, 
		strlen(dir->files[i].name));
}

void 
Update_Percent(i, size)
int i;
int size;
{
    double rate = 0;
    char   label[500];
    int n;

    SetWidget(Percent, "*.percent", 1);
    if (Percent_gc == NULL) init_percent();

    if (i >= 0) {
        Percent_x = i;
        draw_percent(Percent);
    }
    SetWidget(Rate_w, "*.rate", 1);
    if (size == 0 && i == 0) {
	t1.tv_usec = 0;
	t1.tv_sec  = 0;
	t2.tv_usec = 0;
	t2.tv_sec  = 0;
	label[0] = '\0';
	SetLabelQUICKQuick(Rate_w, label);
        return;
    } 
    if (t1.tv_sec == 0) {
	gettimeofday(&t1, (struct timezone *)NULL);
	t3 = t1;
    } else {
	gettimeofday(&t2, (struct timezone *)NULL);
        rate = (double)(t2.tv_sec - t3.tv_sec) +
               (double)(t2.tv_usec - t3.tv_usec)/1000000.0;
	
/*
	if (rate < 2.0) return; 
*/
	t3 = t2;
        rate = (double)(t2.tv_sec - t1.tv_sec) +
               (double)(t2.tv_usec - t1.tv_usec)/1000000.0;
	rate = (double)size/(rate*1000000.0);
	if (rate > 1000.0) {
	    rate = rate/1000.0;
	    sprintf(label,"%6.2f GB/SEC", rate);
        } else if (rate < 1.0) {
	    rate = rate*1000;
	    sprintf(label,"%6.2f KB/SEC", rate);
        } else {
	    sprintf(label,"%6.2f MB/SEC", rate);
	}
	SetLabelQUICKQuick(Rate_w,label);
    }
}

void 
View_File(file, name)
char *file;
char *name;
{
    int n;
    char *command = NULL;
    struct _view_file *x;

    n = strlen(file);
    if (n > 2 && strcmp(&file[n-2], ".Z") == 0) {
	command = XtMalloc(strlen(file)+ 
			   strlen("uncompress %s >/dev/null 2>/dev/null") +5);
	sprintf(command, "uncompress %s >/dev/null 2>/dev/null", file);
	x = (struct _view_file *)XtMalloc(sizeof(struct _view_file));
	x->FILEX = lpopen(command, "r");
	file[n-2]  = '\0';
	XtFree(command);
	x->file = XtNewString(file);
	x->name = XtNewString(name);
	XtAppAddInput(App_context, fileno(x->FILEX),
                      (XtPointer)XtInputReadMask,
                      (XtInputCallbackProc)wait_file,
                      (XtPointer)x);
    } else if (n > 3 && strcmp(&file[n-3], ".gz") == 0) {
	command = XtMalloc(strlen(file)+ 
			   strlen("gunzip %s >/dev/null 2>/dev/null") +5);
	sprintf(command, "gunzip %s >/dev/null 2>/dev/null", file);
	x = (struct _view_file *)XtMalloc(sizeof(struct _view_file));
	x->FILEX = lpopen(command, "r");
	file[n-3]  = '\0';
	XtFree(command);
	x->file = XtNewString(file);
	x->name = XtNewString(name);
	XtAppAddInput(App_context, fileno(x->FILEX),
                      (XtPointer)XtInputReadMask,
                      (XtInputCallbackProc)wait_file,
                      (XtPointer)x);
    } else {
        View(file, name);
    }
}

static void 
abort_cb(w, stuff, unused)
Widget  w;
char    *stuff;
caddr_t unused;
{
    if (logged_in) 
        Abort_Ftp();
    else 
        Close_Ftp();
}

static void 
anonymous_cb(w, closure, call_data)
Widget w;
XtPointer closure;
XtPointer call_data;
{
    char *cp = (char *)closure;
    char *pass = NULL;
    int  j = INDEX_PASSWORD;
    int  i = INDEX_LOGIN;
   
    if (!cp) return; 
    if (strcmp(cp, "MAIL") == 0) {
	pass = concat(pass, username);
	pass = concat(pass, "@");
	pass = concat(pass, local_host_name);
    } else if (strcmp(cp, "NAME") == 0) {
	pass = XtNewString(username);
    } else {
	pass = XtNewString(cp);
    }

    if (connect_info[i].p && *connect_info[i].p) XtFree(*connect_info[i].p);
    *connect_info[i].p = XtNewString("anonymous");
    SetText(connect_info[i].w, *connect_info[i].p);

    if (connect_info[j].p && *connect_info[j].p) XtFree(*connect_info[j].p);
    *connect_info[j].p = set_text_password(connect_info[j].w, pass);
    XtFree(pass);
}

static void 
cb_cd(data, code, cb)
DATA data;
int  code;
CB   *cb;
{
    int  stuff = (int)data;
    extern struct _dirs *previous_dir;

    
    if (ftp_response[code].status & FTP_ERROR) {
	do_callbacks(code, cb);
	Clear_Noop(NOOP_DIR);
        if (ftp_response[code].status & FTP_ERROR_RECONN  &&
	    previous_dir) 
		Set_Remote(previous_dir->pwd_name);
	return;
    }
    switch (stuff) {
	case 0:
	    switch(Remote_local) {
		case REMOTE:
	    	    Start_Pwd(0 , 0, cb_cd, (DATA)1, NULL);
	    	    return;
		case LOCAL:
		    break;
	    }
	case 1:
	    switch(Remote_local) {
		case LOCAL:
		    Start_Local_Dir(0, 0, cb_cd, (DATA)2, NULL);
		    break;
		case REMOTE:
	            Start_Remote_Dir(0, 0, cb_cd,  (DATA)2, NULL);
		    break;
	    }
	    return;
	case 2:
	    switch(Remote_local) {
		case LOCAL:
               	    Display_Files(local_dir, localhost, 0);
		    break;
		case REMOTE:
               	    Display_Files(remote_dir, hostname, 0);
		    break;
	    }
    }
    do_callbacks(1, cb);
    Clear_Noop(NOOP_DIR);
}

static void 
cd_act(w, event, params, nparams)
Widget          w;
XEvent         *event;
String         *params;
Cardinal       *nparams;
{
     Do_action = DO_CD;
}

static void 
clean_str(str)
char *str;
{
    char *cp = str;

    if (!cp || !*cp) return;
    while (*cp) {
	if (*cp == '\n') { *cp = '\0'; break;}
	if (*cp == '\r') { *cp = '\0'; break;}
	if (*cp == '') { *cp = '\0'; break;};
	cp++;
    }
}

static void 
clear_cb(w, stuff, unused)
Widget  w;
char    *stuff;
caddr_t unused;
{
    int i;

    if (!Lastdir) return;

    SetWidget(List_w, "*listsw.list", 0);

    for (i=0; i<Lastdir->n; i++) {
	if (Lastdir->files[i].mode &~MODE_TYPE_SELECT) {
        	Lastdir->files[i].mode &= ~MODE_TYPE_SELECT; 
        	file_select[i] &= ~LIST_SELECT;
        	MyXawListHighlight(List_w, i);
	}
    }
    Count_Files(Lastdir, 1);
}

static void  
clear_command(w)
Widget w;
{
#if defined(XAW)||defined(OPENWINDOW)
    XtPopdown(w);
#endif
}

static void 
clear_connect(w, stuff, xxx)
Widget w;
char   *stuff;
char   *xxx;
{
    Clear_Noop(NOOP_CONNECT);
#if defined(XAW)||defined(OPENWINDOW)
    XtPopdown(Connect_Shell);
#endif
}

static void 
clear_list_entry_cb(w, closure, call_data)
Widget w;
XtPointer closure;
XtPointer call_data;
{ 
    Update_Mark(items.list_index);
}

static void 
clear_status(client_data, id)
caddr_t client_data;
XtIntervalId *id;
{
    SetWidget(Status_w, "*.status", 0);
    SetLabel(Status_w, "");
}

static void 
clear_status_shell(w, stuff, xxx)
Widget w;
char   *stuff;
char   *xxx;
{
#if defined(XAW)||defined(OPENWINDOW)
    XtPopdown(w);
#endif
}

static void 
clear_tran(w, stuff, xxx)
Widget w;
char   *stuff;
char   *xxx;
{
#if defined(XAW)||defined(OPENWINDOW)
    XtPopdown(Shelltran_w);
#endif
}

static void 
connect_disconnect(w, stuff, unused)
Widget  w;
char    *stuff;
caddr_t unused;
{
    if (!logged_in) {
	Stop_Retry();
        WcSetInsensitiveCB(w, "this", NULL);
        if (!Connect_Shell) {
            Connect_Shell = WcFullNameToWidget(Top, "*Shellconnect");
            if (!Connect_Shell) {
                WcCreateNamedPopups ( Top, "Shellconnect" );
            }
            Connect_Shell = WcFullNameToWidget(Top, "*Shellconnect");
            if (!Connect_Shell) {
	        fprintf(stderr,"Could not create Connect Popup Shell\n");
	        exit(1);
            }
	}
        set_connect_info();
        XtPopup(Connect_Shell, XtGrabNone);
	Set_Icon(Connect_Shell, "connect");
	WM_DELETE(Connect_Shell, clear_connect, "connect");
    } else {
	extern int aborting;
	Stop_Retry();
        aborting = 0;  /* rude */
	Close_Ftp(); 
    }
}

static void 
create_tran(w)
Widget  w;
{
     char *cp;

     cp = Create_Translation_Examples();
#if defined(OPENWINDOW)
     SetStringSource(w, cp);
#else
     SetText(w, cp);
#endif
     XtFree(cp);
}

static void 
do_cd_cb(w, stuff, unused)
Widget  w;
char    *stuff;
caddr_t unused;
{
    struct _dirs *dir;
    MyXawListReturnStruct *item;
    int i;

    SetWidget(List_w, "*listsw.list", 0);
    item = MyXawListShowCurrent(List_w);
    if (item->list_index == XAW_LIST_NONE) {
	XtFree((char *)item);
	return;
    }
    dir = Lastdir;
    if (!dir) return;
    i = item->list_index;
    if (dir->files[item->list_index].mode & MODE_TYPE_DIR ||
        dir->files[item->list_index].mode & MODE_TYPE_LINK) {
	dir->link_up = 0;
        if (dir->files[item->list_index].mode & MODE_TYPE_LINK) {
	    dir->link_up++;
        }
        if (Remote_local == REMOTE) {
    	    Start_Cd(0, dir->files[item->list_index].dir_name, 1,  cb_cd, 
		     (DATA)0, NULL);
        } else {
    	    Start_Lcd(0, dir->files[item->list_index].dir_name, cb_cd, (DATA)0,
		      NULL);
    	}
    } else {
	char *cp = NULL;
  	cp = concat(cp, dir->files[item->list_index].name);
	cp = concat(cp, " is not a directory ");
	Set_Status_Short(cp);
	XtFree(cp);
    }
    XtFree((char *)item);
}

static void 
do_connect()
{
    int i;
    Arg arg[1];
    char *new;
    char *old;
    String str;

    for (i=0; i < NO_CONNECT; i++) {
        if (strcmp(connect_info[i].name, "password") == 0) {
            new = XtNewString(pass);
        } else {
            new = GetText(connect_info[i].w);
        }
        clean_str(new);
        if (connect_info[i].p && *connect_info[i].p)
            XtFree(*connect_info[i].p);
        *connect_info[i].p = new;
    }

    if (verify_connect_info()) return;

    if(retry) {
        timeout_retry =  XtAppAddTimeOut(App_context, 
			       (unsigned long)(2*60*1000), 
			       (XtTimerCallbackProc)retry_connect,
			       (XtPointer)NULL);
    }

    Clear_Noop(NOOP_CONNECT);
    XtPopdown(Connect_Shell);
    Start_Connect(1, NULL, NULL, NULL);
    
}

static void 
do_connect_act(w, event, params, nparams)
Widget          w;
XEvent         *event;
String         *params;
Cardinal       *nparams;
{  
    do_connect();
}

static void 
do_connect_cb(w, stuff, unused)
Widget  w;
char    *stuff;
caddr_t unused;
{
    do_connect();
}

static void 
do_dir()
{
    switch(Remote_local) {
	case LOCAL:
            Start_Local_Dir(0, 1, NULL, (DATA)0, NULL);
            Display_Files(local_dir, localhost, 0);
	    return;
	case REMOTE:
            Start_Remote_Dir(0, 1, NULL,  (DATA)0, NULL);
	    return;
    }
}

static void 
do_toggle(type, subtype)
char *type;
char *subtype;
{
    struct _toggle *next = toggle;

    while (next) {
	if (strcmp(next->type, type) == 0) {
	    if (strcmp(next->subtype, subtype) == 0) {
		Mark_Menu(next->w);
	    } else {
		Unmark_Menu(next->w);
	    }
	}
	next = next->next;
    }
}

static void 
do_up()
{
    char *up;
    struct _dirs *dir;

    Set_Noop(NOOP_DIR);
    switch(Remote_local) {
	case LOCAL:
	        dir = Find_Dir(local_dir, localhost, 0);
	    	if (dir && dir->up) {
	    	    Start_Lcd(0, dir->up->pwd_name, cb_cd, (DATA)0, NULL);
		    dir->up = NULL;
		} else {
	    	    Start_Lcd(0, "..", cb_cd, (DATA)0, NULL);
		}
	    break;
	case REMOTE:
    	    switch(remote_type) {
		case REMOTE_SYSTEM_UNIX:
	            dir = Find_Dir(remote_dir, hostname, 0);
		    if (dir && dir->up) {
   	    	        Start_Cd(0, dir->up->pwd_name, 0, cb_cd, (DATA)0, NULL);
		    	dir->up = NULL;
		    } else {
		        if (no_dot_dot) {
			     up = Compute_Up_Apollo();
	   		     Start_Cd(0, up , 0,  cb_cd, (DATA)0, NULL);
			     XtFree(up);
			} else {
   	    	    	     Start_Cd(0, "..", 0, cb_cd, (DATA)0, NULL);
			}
		    }
	    	    return;
		case REMOTE_SYSTEM_OTHER:
		    up = Compute_Up();
		    if (up) {
	   		Start_Cd(0, up , 0,  cb_cd, (DATA)0, NULL);
			XtFree(up);
		    }
		    cb_cd((DATA)1, 1, NULL);
		    return;
	    }
    } 
}

static void 
do_up_act(w, event, params, num_params)
Widget w;
XEvent * event;
String * params;
Cardinal * num_params;
{
     do_up();
}

static void 
do_up_cb(w, closure, call_data)
Widget w;
XtPointer closure;
XtPointer call_data;
{
     do_up();
}

static void 
draw_percent(w)
Widget w;
{
    Arg args[2];
    int  ww;
    Dimension width, height;

    if(!XtIsRealized(w)) return;
    XtSetArg(args[0], XtNwidth, &width);
    XtSetArg(args[1], XtNheight, &height);
    XtGetValues(w, args, XtNumber(args));
    if (Percent_x)  {
    	ww = ((int)width*Percent_x)/100;
	if (ww >= (int)width) ww = width;
    	XDrawLine(XtDisplay(w), XtWindow(w), Percent_gc, 0, 0, ww, 0);
	XClearArea(XtDisplay(w), XtWindow(w), ww, 0, width-ww, height, False);
    } else {
	XClearArea(XtDisplay(w), XtWindow(w), 0, 0, width, height, False);
    }
}

static void  
file_action(w, item)
Widget w;
MyXawListReturnStruct *item;
{
    int i;

    for (i=0; i<num_file_actions; i++) {
	if (file_actions[i].w == w)  {
	    Do_action = file_actions[i].type;
	    if (((Do_action == DO_UP) ||
	         (file_actions[i].type == DO_DIR)) && 
		item != NULL &&
		item->list_index == XAW_LIST_NONE) {
		if (Do_action == DO_UP) {
		    do_up();
		} else  if (Do_action == DO_DIR) {
		    do_dir();
		}
		Do_action = 0;
	    } else {
	        list_notify(List_w, NULL, (XtPointer)item);
	    }
	    return;
	}
    }

}

static void 
get_connect_info(w, stuff, unused)
Widget  w;
char    *stuff;
caddr_t unused;
{
    int i;
    Arg arg[1];
    char *new;
    char *old;
    String str;

    for (i=0; i < NO_CONNECT; i++) {
	if (strcmp(connect_info[i].name, "password") == 0) {
	    new = XtNewString(pass);
	} else {
	    new = GetText(connect_info[i].w);
        }
        clean_str(new);
	if (connect_info[i].p && *connect_info[i].p) 
	    XtFree(*connect_info[i].p);
	*connect_info[i].p = new;
    }
}

static void 
init_connect_info(w, stuff, unused)
Widget  w;
char    *stuff;
caddr_t unused;
{
    int i;

    for (i=0; i < NO_CONNECT; i++) {
        if (strcmp(connect_info[i].name, stuff) == 0) {
	    connect_info[i].w = w;
	    return;
	}
    }
    fprintf(stderr,"don't know stuff %s\n",stuff);
    fflush(stderr);

}

static void 
init_percent()
{
    Arg args[2];
    XGCValues gcv;

    XtSetArg(args[0], XtNforeground, &gcv.foreground);
    XtSetArg(args[1], XtNbackground, &gcv.background);
    XtGetValues(Percent, args, XtNumber(args));
  
    gcv.line_width = 5;
    Percent_gc =  XtGetGC(Percent, 
		          GCLineWidth|GCForeground|GCBackground, &gcv);
}

static void 
list_notify(w, closure, call_data)
Widget w;
XtPointer closure;
XtPointer call_data;
{
    struct _dirs *dir;
    extern struct _dirs *Lastdir;
    MyXawListReturnStruct *item = (MyXawListReturnStruct*)call_data;
    int i;
    char *cp = NULL;
    int action = Do_action;

    dir = Lastdir;

    Do_action = 0;
    switch (action) {

	   case DO_UP: 
		do_up();
		return;

	   case DO_DIR:
		do_dir();
		return;

	   case DO_get_all:
    		if(Remote_local == LOCAL) {
        	    Set_Status_Short("Get on a local directory not allowed.");
        	    return;
    		}
    		Do_List(Start_Get, NULL);
		return;

	   case DO_put_all:
    		if(Remote_local == REMOTE) {
        	    Set_Status_Short("Put on a remote directory not allowed.");
        	    return;
    	        }
    		Do_List(Start_Put, NULL);
		return;

	   case DO_clear_all:
		clear_cb(w, NULL, NULL);
		return;
    }

    if (!dir ||
        dir->n < 0 ||
        dir->n <= item->list_index ||
	item->list_index < 0) {
        if ((item->list_index == XAW_LIST_NONE ||
             item->type == TYPE_NOTIFY) && dir) {
	    dir->highlight = -1;
   	    update_selections(dir);
	    Count_Files(dir, 1);
	}
	Single_File_Actions();
	return;
    }


    dir->highlight = item->list_index;
    if (action) {
	switch(action) {
	   case DO_CD:
	    	if (dir->files[item->list_index].mode & MODE_TYPE_DIR ||
	       	    dir->files[item->list_index].mode & MODE_TYPE_LINK) {
         	    dir->link_up = 0;
        	    if (dir->files[item->list_index].mode & MODE_TYPE_LINK) 
            			dir->link_up++;
		    Set_Noop(NOOP_DIR);
	    	    if (Remote_local == REMOTE) {
	    	      	Start_Cd(0, dir->files[item->list_index].dir_name, 1, 
				cb_cd, (DATA)0, NULL);
	            } else {
	    	        Start_Lcd(0, dir->files[item->list_index].dir_name, 
				 cb_cd, (DATA)0, NULL);
	            }
	      	} else {
	            cp = concat(cp, dir->files[item->list_index].name);
	            cp = concat(cp, " is not a directory ");
	            Set_Status(cp);
	            XtFree(cp);
	        }
                break;
	    case DO_GET:
		Do_File(Start_Get, item->list_index);
		Highlight(item->list_index, 0);
		return;
	    case DO_VIEW:
		Do_File(Start_View, item->list_index);
		Highlight(item->list_index, 0);
		return;
	    case DO_PUT:
		Do_File(Start_Put, item->list_index);
		Highlight(item->list_index, 0);
		return;
	    case DO_ASCII:
		dir->files[item->list_index].mode &= 
			~(MODE_TYPE_ASCII|MODE_TYPE_BINARY|MODE_TYPE_TENEX);
		dir->files[item->list_index].mode |= MODE_TYPE_ASCII;
		if (dir->update_file) {
		    dir->update_file(dir, item->list_index);
 		    MyXawListHighlight(w, item->list_index);
		}
	        break;
	    case DO_BINARY:
		dir->files[item->list_index].mode &= 
			~(MODE_TYPE_ASCII|MODE_TYPE_BINARY|MODE_TYPE_TENEX);
		dir->files[item->list_index].mode |=  MODE_TYPE_BINARY;
		if (dir->update_file) {
		    dir->update_file(dir, item->list_index);
 		    MyXawListHighlight(w, item->list_index);
		}
	        break;
	    case DO_TENEX:
		dir->files[item->list_index].mode &=
			~(MODE_TYPE_ASCII|MODE_TYPE_BINARY|MODE_TYPE_TENEX);
		dir->files[item->list_index].mode |=  MODE_TYPE_TENEX;
		if (dir->update_file) {
		    dir->update_file(dir, item->list_index);
 		    MyXawListHighlight(w, item->list_index);
		}
	        break;
	    case DO_DEFAULT:
		dir->files[item->list_index].mode &= 
			~(MODE_TYPE_ASCII|MODE_TYPE_BINARY|MODE_TYPE_TENEX);
		if (dir->update_file) {
		    dir->update_file(dir, item->list_index);
 		    MyXawListHighlight(w, item->list_index);
		}
		break;
	    case DO_IGNORE:
		dir->files[item->list_index].mode |= MODE_TYPE_IGNORE; 
		if (dir->update_file) {
		    dir->update_file(dir, item->list_index);
 		    MyXawListHighlight(w, item->list_index);
		}
		break;
	    case DO_USE:
		dir->files[item->list_index].mode &= ~MODE_TYPE_IGNORE; 
		if (dir->update_file) {
		    dir->update_file(dir, item->list_index);
 		    MyXawListHighlight(w, item->list_index);
		}
		break;
	}
	if (action != DO_CD) Update_Mark(item->list_index);
        Single_File_Actions();
	return;
    }

    update_selections(dir);

    if (dir->files[item->list_index].mode&MODE_TYPE_IGNORE) {
	file_select[item->list_index] &= ~LIST_SELECT;
	MyXawListHighlight(w, item->list_index);
    }

    Count_Files(dir, 1);
}

static void 
list_translations_cb(w, closure, call_data)
Widget w;
XtPointer closure;
XtPointer call_data;
{
    if (!Shelltran_w) {
        Shelltran_w = WcFullNameToWidget(Top, "*Shelltran");
        if (!Shelltran_w) {
	    WcCreateNamedPopups ( Top, "Shelltran" );
 	}
        Shelltran_w = WcFullNameToWidget(Top, "*Shelltran");
	if (!Shelltran_w) {
	    fprintf(stderr,"Could not create Translation Popup Shell\n");
	    return;
	}
    }
    XtPopup(Shelltran_w, XtGrabNone);
    Set_Icon(Shelltran_w, "tran");
    if(Tran_w) create_tran(Tran_w);
}

static void 
listing_type(w, stuff)
Widget  w;
char    *stuff;
{
    extern List_type;
    extern List_by_type;
    extern List_sort;
    extern List_reverse;

    SetWidget(smbytype , "*smbytype", 0);
    SetWidget(smnormal , "*smnormal", 0);

    if (strcmp(stuff, "SORT_BY_NAME") == 0) {
	if (List_sort != SORT_BY_NAME) {
	    List_sort = SORT_BY_NAME;
	    do_toggle("sort_type", stuff);
	    Set_List_Type(List_type);
	}
    } else if (strcmp(stuff, "SORT_BY_SIZE") == 0) {
      	if ( List_sort != SORT_BY_SIZE) {
	    List_sort = SORT_BY_SIZE;
	    do_toggle("sort_type", stuff);
	    Set_List_Type(List_type);
	}
    } else if (strcmp(stuff, "SORT_BY_AGE") == 0) {
      	if (List_sort != SORT_BY_AGE) {
	    List_sort = SORT_BY_AGE;
	    do_toggle("sort_type", stuff);
	    Set_List_Type(List_type);
	}
    } else if (strcmp(stuff, "SORT_BY_TYPE") == 0) {
	if (List_by_type) {
	    List_by_type = 0;
#if defined(XAW)||defined(MOTIF)
	    Unmark_Menu(smbytype);
#else
	    SetLabel(smbytype, "Sort By Type");
#endif
	} else {
	    List_by_type = 1;
#if defined(XAW)||defined(MOTIF)
	    Mark_Menu(smbytype);
#else
	    SetLabel(smbytype, "Don't Sort By Type");
#endif
	}
        Set_List_Type(List_type);
    } else if (strcmp(stuff, "NORMAL") == 0) {
        if (List_reverse != 0) {
            List_reverse = 0;
#if defined(XAW)||defined(MOTIF)
            Mark_Menu(smnormal);
#else
	    SetLabel(smnormal, "Reverse");
#endif
	    Set_List_Type(List_type);
	} else {
            List_reverse = 1;
#if defined(XAW)||defined(MOTIF)
	    Unmark_Menu(smnormal);
#else
	    SetLabel(smnormal, "Normal");
#endif
	    Set_List_Type(List_type);
	}
    } else if (strcmp(stuff, "SHORT")  == 0) {
	if (Listing_w) SetLabel(Listing_w, "Short Listing ");
	if (List_type != LIST_SHORT) Set_List_Type(LIST_SHORT);
	do_toggle("listing", "SHORT");
    } else if (strcmp(stuff, "MEDIUM") == 0) {
	if (Listing_w) SetLabel(Listing_w, "Medium Listing");
	if (List_type != LIST_MEDIUM) Set_List_Type(LIST_MEDIUM);
	do_toggle("listing", "MEDIUM");
    } else if (strcmp(stuff, "LONG") == 0) {
	if (Listing_w) SetLabel(Listing_w, "Long Listing  ");
	if (List_type != LIST_LONG) Set_List_Type(LIST_LONG);
	do_toggle("listing", "LONG");
    } else if (strcmp(stuff, "TRANSLATIONS") == 0) {
	if (Listing_w) SetLabel(Listing_w, "Translations  ");
	if (List_type != LIST_TRANSLATIONS) Set_List_Type(LIST_TRANSLATIONS);
	do_toggle("listing", "TRANSLATIONS");
    } else {
	fprintf(stderr,"Don't know %s\n", stuff);
    }
}

static void 
listing_type_act(w, event, params, nparams)
Widget          w;
XEvent         *event;
String         *params;
Cardinal       *nparams;
{
    if (*nparams != 1) return;
    listing_type(w, params[0]);
}

static void 
listing_type_cb(w, stuff, unused)
Widget  w;
char    *stuff;
caddr_t unused;
{
    listing_type(w, stuff);
}

static void 
mark_menu_cb(w, closure, call_data)
Widget w;
XtPointer closure;
XtPointer call_data;
{
     Mark_Menu(w);
}

static void 
msg_cb(w, closure, call_data) 
Widget w; 
XtPointer closure; 
XtPointer call_data;
{
    if (!closure) return;
    printf("msg: %s\n", (char *)closure);
}

static void
no_window_group_cb(w, closure, call_data)
Widget w;
XtPointer closure;
XtPointer call_data;
{
    XtVaSetValues(w, XtNwindowGroup, XtUnspecifiedWindowGroup,
                     XtNtransient, FALSE,
                     NULL);
}

static void 
noop_act(w, event, params, nparams)
Widget          w;
XEvent         *event;
String         *params;
Cardinal       *nparams;
{
}

static void 
op_cb(w, closure, call_data)
Widget w;
XtPointer closure;
XtPointer call_data;
{
    char *type = (char *)closure;
    int i;

    for (i=0; i<(sizeof(op)/sizeof(struct _op)); i++) {
	if (strcmp(op[i].name, type) == 0) {
	     if (*op[i].value) {
		*op[i].value = 0;
#if defined(OPENWINDOW)
                SetLabel(w, op[i].set);
#else
                Unmark_Menu(w);
#endif
	     } else {
		*op[i].value = 1;
#if defined(OPENWINDOW)
                SetLabel(w, op[i].unset);
#else
                Mark_Menu(w);
#endif
	     }
	     return;
	}
    }
}

static void 
open_file_act(w, event, params, nparams)
Widget          w;
XEvent         *event;
String         *params;
Cardinal       *nparams;
{

    MyXawListReturnStruct *item;
    struct _dirs *dir;
    int i;
    Widget xx;

    SetWidget(List_w, "*listsw.list", 0);
    item = MyXawListShowCurrent(List_w);
    items = *item;
    XtFree((char *)item);

    dir = Lastdir;
    i = items.list_index;
    if (!dir ||
        dir->n < 0 ||
        dir->n <= i ||
	i == XAW_LIST_NONE ||
        i < 0) {
        Do_action = 0;
        return;
    }

    if (dir->files[i].mode & MODE_TYPE_DIR) {
	Do_action = DO_CD;
    } else if (dir->files[i].mode & MODE_TYPE_FILE) {
	if (Remote_local == REMOTE) {
	    Do_action = DO_GET;
	} else {
	    Do_action = DO_PUT;
	}
    }
    if (!Do_action) return;
	
    xx  = WcFullNameToWidget(Top, "*list");
    list_notify(xx, NULL, (XtPointer)&items);
}

static void 
quit_cb(w, stuff, unused)
Widget  w;
char    *stuff;
caddr_t unused;
{
    exit(0);
}

static void 
raise_window(name)
char *name;
{
    Widget w;

    w = WcFullNameToWidget(Top, name);
    if (w == NULL) return;
    XRaiseWindow(XtDisplay(w), XtWindow(w));
}

static void 
raise_window_cb(w, closure, call_data)
Widget w;
XtPointer closure;
XtPointer call_data;
{
    raise_window((char *)closure);
}

static void 
reconnect_cb(w, stuff, unused)
Widget  w;
char    *stuff;
caddr_t unused;
{
     aborting = 0;
     WcSetInsensitiveCB(Top, "*reconnect", NULL);
     Start_Reconnect(NULL);
}

static void 
register_action_cb(w, closure, call_data)
Widget w;
XtPointer closure;
XtPointer call_data;
{
    char *t =  (char *)closure;
    int  type, i;
   
    for (i=0;  file_actions_types[i].name; i++) {
	if (strcmp(file_actions_types[i].name, t) == 0) break;
    }

    if(file_actions_types[i].name == NULL) {
	fprintf(stderr, "Could not find file action %s\n", t);
	return;
    }

    if (num_file_actions >=MAX_ACTIONS) {
	fprintf(stderr, "Too many file action widgets\n");
	return;
    }

    file_actions[num_file_actions].type  = file_actions_types[i].type;
    file_actions[num_file_actions].flags = file_actions_types[i].flags;
    file_actions[num_file_actions].w     = w;
    num_file_actions++;
}

static void 
remote_local_toggle()
{
    char *t = NULL;

    if (Remote_local == LOCAL) {
	Set_Dir_Type(REMOTE);
    } else {
	Set_Dir_Type(LOCAL);
    }
}

static void 
remote_local_toggle_act(w, event, params, nparams)
Widget          w;
XEvent         *event;
String         *params;
Cardinal       *nparams;
{
    remote_local_toggle();
}

static void 
remote_local_toggle_cb(w, stuff, unused)
Widget  w;
char    *stuff;
caddr_t unused;
{
    remote_local_toggle();
}

static void 
reset_host_search()
{
    search_host_last = -2;
    if (search_string) XtFree(search_string);
    if (search_globreg) XtFree((char *)search_globreg);
    search_host_globreg = NULL;
    search_host_string = NULL;;
    Reset_Host_Search_String();
}

static void 
resize_percent_act(w, event, params, nparams)
Widget          w;
XEvent         *event;
String         *params;
Cardinal       *nparams;
{
    draw_percent(w);
}

static void 
retry_connect(client_data, id)
caddr_t client_data;
XtIntervalId *id;
{
    timeout_retry =  XtAppAddTimeOut(App_context, 
			       (unsigned long)(2*60*1000), 
			       (XtTimerCallbackProc)retry_connect,
			       (XtPointer)NULL);
    Start_Connect(1, NULL, NULL, NULL);
}

static void 
search_act(w, event, params, nparams)
Widget          w;
XEvent         *event;
String         *params;
Cardinal       *nparams;
{
    search_next();
}

static void 
search_cb(w, stuff, unused)
Widget  w;
char    *stuff;
caddr_t unused;
{
    search_next();
}

static void 
search_host_act(w, event, params, nparams)
Widget          w;
XEvent         *event;
String         *params;
Cardinal       *nparams;
{
    if (params && *params) {
	if(strcmp(*params, "clear") == 0) {
	    reset_host_search();
	    return;
	}
    }
    search_host_next();
}

static void  
search_host_next()
{
    int i;
    struct _dirs *dir;
    int start;
    int found = 0;
    extern int NUM_NETRC;

    if (search_host_last < -1) {
	Search_Host_Dialog();
	return;
    }

    if (search_host_last < 0) {
	start = 0;
    } else {
	if (search_host_last >= NUM_NETRC) {
	    start = 0;
	} else {
	    start = search_host_last;
	}
    }

    for (i=start; i<NUM_NETRC; i++) {
   	if(regexec(search_host_globreg, host_text[i])) {
	    found++;
	    break;
	}
    }

    if (!found && i == NUM_NETRC  && start != 0) {
        for (i=0; i<start; i++) {
   	    if(regexec(search_host_globreg, host_text[i])) {
	        found++;
	        break;
	    }
	}
    }

    if (found) {
	search_host_last = i+1;
        SetWidget(Host_w, "*hostsw.list", 0);
	SetPos(Host_w, i, 0);
	MyXawListHighlight(Host_w, i);
    } else {
        MyXawListUnhighlight(Host_w);
    }
}

static void 
search_next()
{
    int i;
    struct _dirs *dir;
    int start;
    int found = 0;

    if (search_exact < 0) {
	Search_Dialog();
	return;
    }

    dir = Lastdir;
    if (!dir) return;

    if (!search_exact && search_globreg == NULL) return;

    WcSetSensitiveCB(Top, "*next", NULL);

    if (search_last < 0) {
	start = 0;
    } else {
	if (search_last >= dir->n) {
	    start = 0;
	} else {
	    start = search_last;
	}
    }

    for (i=start; i<dir->n; i++) {
	if (search_exact) {
	    if (strcmp(search_string, dir->files[i].name) == 0) {
		found++;
	    	break;
	    }
	} else {
            if(regexec(search_globreg, dir->files[i].name)) {
		found++;
		break;
	    }
 	}
    }

    if (!found && i == dir->n  && start != 0) {
        for (i=0; i<start; i++) {
	    if (search_exact) {
	       	if (strcmp(search_string, dir->files[i].name) == 0) {
		    found++;
		    break;
		}
	    } else {
                if(regexec(search_globreg, dir->files[i].name)) {
		    found++;
		    break;
		}
 	    }
	}
    }

    if (found) {
	search_last = i+1;
	SetPos(List_w, i, 0);
	MyXawListHighlight(List_w, i);
	dir->highlight = i;
    } else {
        MyXawListUnhighlight(List_w);
	dir->highlight = -1;
    }
	
}

static void 
select_host_cb(w, closure, call_data)
Widget w;
XtPointer closure;
XtPointer call_data;
{
    extern int NUM_NETRC;
    int i;

    MyXawListReturnStruct *item = (MyXawListReturnStruct*)call_data;

    if (item->list_index < 0 ||
	item->list_index == XAW_LIST_NONE ||
        !(host_select[item->list_index]&LIST_SELECT)) {
        for (i=0; i<NUM_NETRC; i++) {
 	    if (host_select[i]&LIST_SELECT) {
       	    	host_select[i] &= ~LIST_SELECT;
            	MyXawListHighlight(w, i);
	    }
    	}
	return;
    }

    set_host(item->list_index);
    for (i=0; i<NUM_NETRC; i++) {
 	if (host_select[i]&LIST_SELECT) {
       	    host_select[i] &= ~LIST_SELECT;
            MyXawListHighlight(w, i);
	}
    }
    host_select[item->list_index] |= LIST_SELECT;
    MyXawListHighlight(w, item->list_index);
    if (item->type == TYPE_LISTOP) {
        do_connect();
    }
}

static void 
set_connect_info()
{
    int i;

    for (i=0; i < NO_CONNECT; i++) {
    	if(strcmp(connect_info[i].name, "password") == 0) {
	    if (connect_info[i].p &&  *connect_info[i].p) 
	        XtFree(*connect_info[i].p);
		*connect_info[i].p = set_text_password(connect_info[i].w, pass);
        } else if (*connect_info[i].p) {
	    SetText(connect_info[i].w, *connect_info[i].p);
	} else if (connect_info[i].init) { 
	    if (connect_info[i].p &&  *connect_info[i].p) 
		XtFree(*connect_info[i].p);
		    *connect_info[i].p = XtNewString(connect_info[i].init);
	    SetText(connect_info[i].w, *connect_info[i].p);
        } else {
	    if (connect_info[i].p &&  *connect_info[i].p) 
		XtFree(*connect_info[i].p);
	    *connect_info[i].p = XtNewString("\0");
	    SetText(connect_info[i].w, *connect_info[i].p);
	}
    }
}

static void 
set_file_action_act(w, event, params, num_params)
Widget   w;
XEvent   *event;
String   *params;
Cardinal *num_params;
{
    MyXawListReturnStruct *item;
    Arg arg[1];
    int i;
    Widget xx; 

    if (*num_params != 1) return;
    item = MyXawListShowCurrent(w);

    for (i=0; file_actions_types[i].name ; i++) {
	if (strcmp(params[0], file_actions_types[i].name) == 0) {
	    Do_action = file_actions_types[i].type;
 	    xx  = WcFullNameToWidget(Top, "*list");
	    list_notify(xx, NULL, (XtPointer)item);
            XtFree((char *)item);
            return;
 	}
    }
    XtFree((char *)item);
    fprintf(stderr,"Don't know action %s\n", params[0]);
}

static void 
set_file_action_cb(w, closure, call_data)
Widget w;
XtPointer closure;
XtPointer call_data;
{
    Arg arg[1];
    int i;
    Widget xx; 
    Widget WcFullNameToWidget();
    char *label;

    file_action(w, &items);
}

static void 
set_file_action_menu_cb(w, closure, call_data)
Widget w;
XtPointer closure;
XtPointer call_data;
{
    MyXawListReturnStruct *item;
    int i;

    SetWidget(List_w, "*listsw.list", 0);
    item = MyXawListShowCurrent(List_w);
    items = *item;
    XtFree((char *)item);
    file_action(w, &items);
}

static void 
set_host(i)
int i;
{
    int j;
    char *cp = NULL;

    for (j=0; j < NO_CONNECT; j++) {
        if (strcmp(connect_info[j].name, "host" ) == 0) {
	    if(connect_info[j].w) {
		if (connect_info[j].p &&  *connect_info[j].p) 
		    XtFree(*connect_info[j].p);
		if (netrc[i].host) {
		    *connect_info[j].p = XtNewString(netrc[i].host);
		} else {
		    *connect_info[j].p = XtNewString("");
		}
	       	SetText(connect_info[j].w, *connect_info[j].p);
	    }
	} else if (strcmp(connect_info[j].name, "login" ) == 0) {
	    if(connect_info[j].w) {
		if (connect_info[j].p && *connect_info[j].p) 
		    XtFree(*connect_info[j].p);
		if (netrc[i].user) {
		    *connect_info[j].p = XtNewString(netrc[i].user);
		} else {
		    *connect_info[j].p = XtNewString("");
		}
	       	SetText(connect_info[j].w, *connect_info[j].p);
	     }
	} else if (strcmp(connect_info[j].name, "password") == 0) {
	    if(connect_info[j].w) {
		if (connect_info[j].p && *connect_info[j].p) 
		    XtFree(*connect_info[j].p);
		*connect_info[j].p =
		     set_text_password(connect_info[j].w, netrc[i].pass);
	    }
	} else if (strcmp(connect_info[j].name, "remotedir" ) == 0) {
	    if(connect_info[j].w) {
		if (connect_info[j].p && *connect_info[j].p) 
		    XtFree(*connect_info[j].p);
		if (netrc[i].remote_dir) {
		    *connect_info[j].p = XtNewString(netrc[i].remote_dir);
		} else {
		    *connect_info[j].p = XtNewString(".");
		}
	       	SetText(connect_info[j].w, *connect_info[j].p);
	     }
	} else if (strcmp(connect_info[j].name, "localdir" ) == 0) {
	    if(connect_info[j].w) {
		if (connect_info[j].p && *connect_info[j].p) 
		    XtFree(*connect_info[j].p);
		if (netrc[i].local_dir) {
		    *connect_info[j].p = XtNewString(netrc[i].local_dir);
		} else {
		    *connect_info[j].p = XtNewString(default_local_dir);
		}
		if (chdir(*connect_info[j].p) != -1) {
		    Set_Local(*connect_info[j].p);
		} else {
		    XtFree(*connect_info[j].p);
		    *connect_info[j].p = XtNewString(".");
		}
	       	SetText(connect_info[j].w, *connect_info[j].p);
	     }
	}
    }
}

static void 
set_host_list_cb(w, closure, call_data)
Widget w;
XtPointer closure;
XtPointer call_data;
{
    extern int NUM_NETRC;
    int i;
    int n, l;
    char format[100];
    char *cp;

    SetWidget(Host_w, "*hostsw.list", 0);

    host_text = (char **)XtMalloc((NUM_NETRC+1)*sizeof(char **));
    host_select = (int *)XtMalloc((NUM_NETRC+1)*sizeof(int));

    n = 0;
    for (i=0; i<NUM_NETRC ;i++) {
        if (netrc[i].host) {
            if ((l=strlen(netrc[i].host)) > n) n = l;
        }
    }

    sprintf(format, "%%-%ds ", n);

    for (i=0; i<NUM_NETRC; i++) {
         cp = (char *)XtMalloc(n+10);
         sprintf(cp, format, netrc[i].host);
         cp = concat(cp, netrc[i].note);
	 host_text[i] = cp;
	 host_select[i] = 0;
    }
    host_text[NUM_NETRC] = NULL;
    host_select[i] = 0;

    MyXawListChange(Host_w, host_text, 0, 0, 1, host_select, 0, -1);

}

static void 
set_icons_act(w, event, params, nparams) 
Widget         w; 
XEvent         *event; 
String         *params; 
Cardinal       *nparams;
{
    if (*nparams != 1) return;
    set_iconx(w, params[0]);
}

static void 
set_icons_cb(w, closure, call_data) 
Widget w; 
XtPointer closure; 
XtPointer call_data;
{
    set_iconx(w, (char *)closure);
}

static void 
set_iconx(w, name)
Widget w;
char   *name;
{
    Arg arg;
    int i;
    Widget shell;
    Pixmap pixmap;

    for (i=0; shell_icons[i].name; i++) { 
	if (strcmp(shell_icons[i].name, name) == 0) break;
    }

    shell = WcFullNameToWidget(Top, name);
    if (!shell_icons[i].name || !shell) {
        fprintf(stderr, "Could not find shell %s\n",name);
        return;
    }
    Set_Icon(shell, shell_icons[i].icon);

    WM_DELETE(shell, shell_icons[i].callback, shell_icons[i].name);
}

static void 
set_status_icon_act(w,  event, params, num_params)
Widget  w;
XEvent  *event;
String  *params;
Cardinal *num_params;
{

    Shellstatus_w = WcFullNameToWidget(Top, "*Shellstatus");
    Set_Icon(Shellstatus_w, "status");
    WM_DELETE(Shellstatus_w, clear_status_shell, "status");
}

static char * 
set_text_password(w, p)
Widget w;
char   *p;
{
    char *cp;
    int i;
    int n = 0;

    if (p) {
        n = strlen(p);
 	if (n > 100) n = 0;
    }
    cp = (char *)XtMalloc(n+1);
    if (n) {
	strcpy(pass, p);
        for (i=0; i<n; i++) cp[i] = 'X';	
	cp[n] = '\0';
        if (w) SetText(w, cp);
    } else {
	cp[n] = '\0';
	pass[0] = '\0';
	if (w) SetText(w, "");
    }
    return cp;
}

static void 
set_tran_cb(w, stuff, unused)
Widget  w;
char    *stuff;
caddr_t unused;
{

     Tran_w = w;
     create_tran(w);
}

static void 
shell_tran_cb(w, stuff, unused)
Widget  w;
char    *stuff;
caddr_t unused;
{
    Shelltran_w = w;
}

static void 
single_file_actions_act(w, event, params, nparams)
Widget          w;
XEvent         *event;
String         *params;
Cardinal       *nparams;
{
    Single_File_Actions();
}

static void 
single_file_actions_cb(w, closure, call_data)
Widget w;
XtPointer closure;
XtPointer call_data;
{
    Single_File_Actions();
}

static void 
toggle_cb(w, closure, call_data)
Widget w;
XtPointer closure;
XtPointer call_data;
{
    char *type, *subtype;
    struct _toggle *new_t;
    char *cp, *s;
    int n;

    s = (char *)closure;
    cp = INDEX(s, ',');
    if (!cp) return;
    n = cp - s;
    type = XtMalloc(n+1);
    strncpy(type, s, n);
    type[n] = '\0';
    s += n + 1;
    while(s && *s == ' ') s++;
    subtype = XtNewString(s);
    new_t = (struct _toggle *)XtMalloc(sizeof(struct _toggle));
    if (toggle == NULL) {
	toggle = new_t;
	new_t->next = NULL;
    } else {
	new_t->next = toggle;
	toggle = new_t;
    }
    new_t->type = type;
    new_t->subtype = subtype;
    new_t->w = w;
}

static void
update_selections(dir)
struct _dirs *dir;
{
    int i;

    if (!dir || dir->n <= 0) return;
    for (i=0;i<dir->n; i++) {
	if (file_select[i]&LIST_SELECT) {
            if ((Remote_local == REMOTE && dir->files[i].local) ||
	        (Remote_local == LOCAL  && dir->files[i].remote)) {
	         dir->files[i].mode |= MODE_TYPE_SELECT;
     	    } else {
		char *cp = NULL;
        	dir->files[i].mode &= ~MODE_TYPE_SELECT;
		cp = concat(cp, dir->files[i].name);
        	if (Remote_local == REMOTE) 
            	    cp = concat(cp, " has no remote tranlation");
		else
            	    cp = concat(cp, " has no local tranlation");
        	Set_Status(cp);
        	XtFree(cp);
		file_select[i] &= ~LIST_SELECT;
    	    }
	} else {
	    dir->files[i].mode &= ~MODE_TYPE_SELECT;
	}
    }
}

static int  
verify_connect_info()
{
    if (verify_host()) return 1;
    if (verify_password()) return 1;
    if (verify_login()) return 1;
    if (verify_gateway()) return 1;
    return 0;
}

static int 
verify_gateway()
{
    if (!use_gateway) return 0;
    if (use_gateway && gateway && gateway[0]) return 0;
    Set_Status_Short("No gateway specified");
    Reset_Gateway_Button();
    use_gateway = 0;
    return 1;
}

static int  
verify_host()
{
    if (!hostname || !*hostname) {
	Set_Status_Short("No host name");
	return 1;
    }
    if (white_space(hostname)) {
	Set_Status_Short("White space in hostname");
        return 1;
    }
    return 0;
}

static int 
verify_login()
{
    char *cp = NULL;

    if (!login || !*login) {
	Set_Status_Short("No login name");
	return 1;
    }
    if(white_space(login)) {
        Set_Status_Short("White space in Login");
        return 1;
    }
    return 0;
}

static int 
verify_password()
{
    if (!password || !*password) {
	Set_Status_Short("No password");
	return 1;
    }
    return 0;
}

static void  
wait_file(closure, source, id)
XtPointer   closure;
int*        source; 
XtInputId*  id;
{
    struct _view_file *x = (struct _view_file *)closure;

    XtRemoveInput(*id);
    lpclose(x->FILEX);
    View(x->file, x->name);
    if (x->file) XtFree(x->file);
    if (x->name) XtFree(x->name);
    XtFree((char *)x);
}

static int 
white_space(string)
char *string;
{
    char *cp = string;

    while(cp && *cp) {
	if (isspace(*cp)) return 1;
	cp++;
    }
    return 0;
}
