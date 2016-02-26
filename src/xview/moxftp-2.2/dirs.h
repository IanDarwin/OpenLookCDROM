/*
 * Copyright (c) 1992 The Regents of the University of Texas System.
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
 * $Id: dirs.h,v 1.1 1994/03/14 18:57:41 jones Exp $
 * $Source: /mintaka/u0/xx/ss/jones/jones.old/mftp.src/mftpnew/RCS/dirs.h,v $
 *
 * $Log: dirs.h,v $
 * Revision 1.1  1994/03/14  18:57:41  jones
 * Initial revision
 *
 */

/*
 * MODE settings.
 */
#define MODE_OTHER_EXECUTE      00000000001
#define MODE_OTHER_WRITE        00000000002
#define MODE_OTHER_READ         00000000003
#define MODE_GROUP_EXECUTE      00000000010
#define MODE_GROUP_WRITE        00000000020
#define MODE_GROUP_READ         00000000040
#define MODE_OWNER_EXECUTE      00000000100
#define MODE_OWNER_WRITE        00000000200
#define MODE_OWNER_READ         00000000400

#define MODE_TYPE_FILE          00000010000
#define MODE_TYPE_DIR           00000020000
#define MODE_TYPE_BLOCK	        00000040000
#define MODE_TYPE_OFFLINE       00000100000
#define MODE_TYPE_CHAR		00000200000
#define MODE_TYPE_LINK		00000400000 
#define MODE_TYPE_SOCK	        00001000000
#define MODE_TYPE_BINARY	00002000000
#define MODE_TYPE_ASCII		00004000000
#define MODE_TYPE_TENEX		00010000000
#define MODE_TYPE_SELECT        01000000000
#define MODE_TYPE_DO            02000000000
#define MODE_TYPE_IGNORE	04000000000

/*
 * Listing options.
 */
#define LIST_NONE         0
#define LIST_SHORT        1
#define LIST_MEDIUM       2
#define LIST_LONG         3
#define LIST_TRANSLATIONS 4
#define LIST_MAX          5

/*
 * File counts
 */
#define FCOUNT  (8+1)

struct _files {
/*
 * Minum amount of information.
 */
    char   *display;
    char   *name;
    char   *dir_name;		
    char   *remote;
    char   *local;
    int	   mode;
    int    size;
    int    temp;
/*
 * Unix info.
 */
    char   *owner;
    char   *modes;
    char   *group;
    int	   link;
    char   *month;
    char   *day;
    char   *linkname;
    char   *time_year;
    long    time;
/*
 * General
 */
    char   *g_owner;
    char   *g_group;
    char   *g_prot;
    char   *g_date;
    char   *g_time;
    char   *g_type;
    char   *g_size;
    char   *g_dir;
    char   *g_p1;
    char   *g_p2;
    char   *g_link;
    int     g_link_int;
    int     g_size_int;
};

struct _dirs {
    struct _dirs  *next;
    char          *pwd_name;
    char          *display_name;
    int	  	  link_up;
    struct _dirs  *up;
    char          *host;
    void	  (*update_file)();
    int		   type;
    int	           n;
    int		   update;
    time_t	   time;
    int	           position[LIST_MAX];
    int		   fcount[FCOUNT];
    int		   highlight;
    int		   sort_type;
    int		   reverse;
    int		   sort_by_type;
    int		   list_type;
    struct _files *files;
    struct _dirs  *odir;
};

struct _dirs *find_dir();
