/*
 * Copyright (c) 1993 The Regents of the University of Texas System.
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
 *
 * $Id: defs.h,v 1.1 1994/03/14 18:57:41 jones Exp $
 * $Source: /mintaka/u0/xx/ss/jones/jones.old/mftp.src/mftpnew/RCS/defs.h,v $
 *
 * $Log: defs.h,v $
 * Revision 1.1  1994/03/14  18:57:41  jones
 * Initial revision
 *
 */

/* product.h - sample header file */
#define PRODUCT	"main"

#if defined(sparc)
#define SUN
#endif

#include	<stdio.h>

#include        <X11/Xlib.h>
#include        <X11/Intrinsic.h>
#include        <X11/IntrinsicP.h>
#include 	<X11/StringDefs.h>
#include	<X11/Shell.h>
#include	"ftp.h"
#include	"dirs.h"
#include	"output.h"
#include	"regexp.h"
#include	"translate.h"
#include	"netrc.h"
#include	"callback.h"
#include 	"proto.h"
#include	"proto/global.h"


#define LINESIZ         512
#define FTP	        "ftp"

/*
 * Global functions.
 */
extern char  *strstr();
extern char  *remote_file();
extern char  *tran_remote_file();
extern int    Debug; 

extern XtAppContext  App_context;	/* Application context */
extern Widget        Top;		/* Top Widget */

extern int    get_list_position();

struct _callback * link_callback();
struct _callback * link_callback_old();

extern char *concat();			/* concat some text */
extern char *concatdir();		/* Concat a directory name */


/*
 * Global varibales.
 */
extern char         *hostname;
extern char         *login;
extern char         *password;
extern char         *remote_dir;
extern char         *local_dir;
extern char         *default_local_dir;
extern char         *localhost;
extern char	    *Ftpname;		
extern void        (*peek_func)();
extern int           connected;
extern int	     Remote_local;
extern int	     logged_in;
extern int           remote_type;
extern int           transfer_mode;
extern Widget        Status_text;
extern char          last_response[];
extern struct _dirs *first_dir;
extern struct _translate translate[];
extern struct _translate *s_tran;
extern struct _translate *s_tran_files;
extern struct _netrc netrc[];
extern int	     NO_TRAN;
extern time_t        get_unix_time();
extern time_t        get_remote_time();
extern int	     no_recconect_list;	  /* No directory list on reconnect */
extern int	     retry;

/*
 * Opetions.
 */
extern int 	     noautodir;		/* No auto directory */
extern int           ignorerrs;		/* Ignore errors */

/*
 * known remote systems.
 */
#define REMOTE_SYSTEM_UNKNOWN       1   /* We have no idea */
#define REMOTE_SYSTEM_UNIX          2   /* Standard unix system type */
#define REMOTE_SYSTEM_LOCAL	    7   /* Really local type */
#define REMOTE_SYSTEM_OTHER	    8   /* As defined by tranlation table */
/*
 * VMS          c.scs.uiuc.edu 
 * VM           utxvm.cc.utexas.edu
 * NOSVE        CDC1.CC.Lehigh.EDU 
 * VMS MULTINET orange.cc.utexas.edu
 */

/*
 * Echo options.
 */
#define FTP_ECHO        001 		/* Echo commands */
#define FTP_PROMPT	002		/* Echo last prompt */
#define FTP_PROMPT_NO   004		/* Don't echo prompt */
#define FTP_COMMAND_NO  010		/* Don't echo command */

#define REMOTE       1
#define LOCAL        2

/*
 * Icon options.
 */
#define ICON_CONNECT     0
#define ICON_DISCONNECT  1
#define ICON_BUSY1	 2
#define ICON_BUSY2	 3
#define ICON_BUSY3	 4
#define ICON_BUSY4	 5
#define ICON_CONNECTING  6
#define ICON_RECONNECT   7


/*
 * File actions.
 */
#define DO_CD          1
#define DO_UP          2
#define DO_GET         3
#define DO_PUT         4
#define DO_ASCII       5
#define DO_BINARY      6
#define DO_TENEX       7
#define DO_DEFAULT     8 
#define DO_VIEW        9
#define DO_IGNORE     10 
#define DO_USE        11
#define DO_clear_all  12
#define DO_get_all    13
#define DO_put_all    14
#define DO_delete_all 15
#define DO_DIR	      16


/*
 * Define tranfser modes.
 */
#define MODE_T_NONE    0
#define MODE_T_ASCII   1
#define MODE_T_BINARY  2
#define MODE_T_TENEX   4

/*
 * Define busy modes.
 */
#define BUSY_BUSY   ICON_BUSY1
#define BUSY_GET    ICON_BUSY2
#define BUSY_PUT    ICON_BUSY3
#define BUSY_DIR    ICON_BUSY4

/*
 * Noop actions.
 */
#define NOOP_GET         000001
#define NOOP_PUT         000002
#define NOOP_DIR         000004
#define NOOP_ACTION      000010
#define NOOP_CONNECT     000020
#define NOOP_NOTCONN     000040
#define NOOP_CLEAR_ALL   000777
#define NOOP_WAITING     001000
#define NOOP_SENSITIVE   002000
#define NOOP_IFSENSITIVE 004000

/*
 * Sort Options.
 */
#define SORT_BY_NONE	   0
#define SORT_BY_NAME       1
#define SORT_BY_SIZE       2
#define SORT_BY_AGE        3

/*
 * Do options
 */
#define DO_ABORT	   0001
#define DO_DISCONNECT      0002
#define DO_RESTART	   0004
#define DO_ONCE		   0010

/*
 * Dialog restart.
 */
#define DIALOG_RESTART_IGNORE   -1
#define DIALOG_RESTART_CONTINUE -2
