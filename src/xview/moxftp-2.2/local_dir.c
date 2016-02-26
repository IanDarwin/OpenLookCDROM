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
"$Id: local_dir.c,v 1.2 1994/05/20 20:16:10 jones Exp $\n";
static char rcspath[] =
"$Source: /mintaka/u0/xx/ss/jones/jones.old/mftp.src/moxftp-2.0/RCS/local_dir.c,v $\n";
#endif

/* $Log: local_dir.c,v $
 * Revision 1.2  1994/05/20  20:16:10  jones
 * Use Push_Files and Restore_Dir_Old in
 * Update_Local_Dir.
 *
 * Revision 1.1  1994/03/14  18:55:53  jones
 * Initial revision
 *
 */

#include <machine.h>
#include "defs.h"
#include "proto/local_dir.h"

#if defined(USE_DIRENT)
#include <dirent.h>
#else
#include <sys/dir.h>
#define dirent direct
#endif

#include <pwd.h>
#include <grp.h>
#include <sys/stat.h>

#if defined(S_IREAD)
#if !defined(S_IRUSR)
#define S_IRUSR S_IREAD
#endif
#if !defined(S_IRGRP)
#define S_IRGRP (S_IREAD>3)
#endif
#if !defined(S_IROTH)
#define S_IROTH (S_IREAD>6)
#endif
#endif

#if defined(S_IWRITE)
#if !defined(S_IWUSR)
#define S_IWUSR S_IWRITE
#endif
#if !defined(S_IWGRP)
#define S_IWGRP (S_IWRITE>3)
#endif
#if !defined(S_IWOTH)
#define S_IWOTH (S_IWRITE>6)
#endif
#endif

#if defined(S_IEXEC)
#if !defined(S_IXUSR)
#define S_IXUSR S_IEXEC
#endif
#if !defined(S_IXGRP)
#define S_IXGRP (S_IEXEC>3)
#endif
#if !defined(S_IXOTH)
#define S_IXOTH (S_IEXEC>6)
#endif
#endif

#if !defined(_IFLNK)
#if defined(_S_IFLNK)
#define _IFLNK _S_IFLNK
#endif
#endif

#if !defined(_IFMT)
#if defined(_S_IFMT)
#define _IFMT _S_IFMT
#else
#if defined(S_IFMT)
#define _IFMT S_IFMT
#endif
#endif
#endif

#if !defined(S_ISDIR) && defined(S_IFDIR)
#define S_ISDIR(x) (((x) & _IFMT) == S_IFDIR)
#endif

#if !defined(S_ISBLK) && defined(S_IFBLK)
#define S_ISBLK(x) (((x) & _IFMT) == S_IFBLK)
#endif

#if !defined(S_ISCHR) && defined(S_IFCHR)
#define S_ISCHR(x) (((x) & _IFMT) == S_IFCHR)
#endif

#if !defined(S_ISREG) && defined(S_IFREG)
#define S_ISREG(x) (((x) & _IFMT) == S_IFREG)
#endif

#if defined(S_IFOFL) && !defined(S_ISOFL)
#define S_ISOFL(x) (((x) & _IFMT) == S_IFOFL)
#endif

extern struct _dirs *Lastdir;

void 
Start_Local_Dir(echo, flag, callback, data, cb)
int    echo;
int    flag;
void (*callback)();
DATA   data;
CB    *cb;
{
    struct _dirs *dir;
    CB  *cbp;
 
    dir = Find_Dir(local_dir, localhost, 0);

    if (flag || dir == NULL || (dir && dir->n < 0) || (dir && dir->update)) {
        Push_Files(dir);
        Update_Screen();
	ls();
        dir = Find_Dir(local_dir, localhost, 0);
        Restore_Dir_Old(dir);
	if(Remote_local == LOCAL) 
		Display_Files(local_dir, localhost, 0);
	if (dir) dir->update = 0;
	if (dir) dir->time = 0;
        Update_Stat_Time(dir);
    }

    dir = Find_Dir(local_dir, localhost, 0);
    if (dir && Lastdir && Lastdir->link_up)  {
         dir->up = Lastdir;
         Lastdir->link_up = 0;
    }
    Set_Status(" ");
    cbp = link_callback(NULL, callback, data, cb);
    do_callbacks(1, cbp);

}

void
Update_Local_Dir()
{
    struct _dirs  *dir;
    struct _dirs  dirold;
    struct _files *files = NULL;
    int            n, i, j, update = 0;
    int            highlight = -1;
    extern int Remote_local;

    ZERO((char *)&dirold,  sizeof(struct _dirs));
    dir = Find_Dir(local_dir, localhost, 0);

    if (!dir) return;

    for (j=0; j<dir->n; j++) {
	if(dir->files[j].mode&MODE_TYPE_DO) return;
    }

    if (Lastdir == dir) update++;
    Push_Files(dir);
    ls();
    dir->update = 0;
    Restore_Dir_Old(dir);
    if (update) Draw_Files(NULL);
}

static void
ls()
{
    DIR *dirp;
    struct dirent *dp;
    int files = 0;
    char *cp = NULL;
    struct _dirs *dir;
    int pass;


restart:

    pass = 0;

    cp = concat(cp, "Getting a directory listing of local dir ");
    cp = concat(cp, local_dir);
    Set_Status(cp);
    XtFree(cp);

    dirp = opendir(local_dir);
    if (dirp == NULL) {
        dir = Find_Dir(local_dir, localhost, 1);
	dir->n = 0;
	Set_Status("Could not open local dir"); 
        return;
    }
    for (dp = readdir(dirp); dp != NULL; dp = readdir(dirp)) {
	files++;
    }
    rewinddir(dirp);
    if (files == 0) {
        dir = Find_Dir(local_dir, localhost, 1);
	dir->n = 0;
	closedir(dirp);
    	return;
    }
    dir = Find_Dir(local_dir, localhost, 1);
    dir->files = (struct _files *)XtMalloc(files*sizeof(struct _files)); 
    ZERO((char *)dir->files, files*sizeof(struct _files));
    dir->n     = files;
    dir->type  =  REMOTE_SYSTEM_LOCAL;
    files =  0;
    for (dp = readdir(dirp); dp != NULL; dp = readdir(dirp)) {
	struct stat statbuf;
	char *path = NULL;
        char *ctime();
        char *t;
	char  month[4], day[3], time_year[6], types[11];
        int n;
	int k;

        if(pass > 20) {
	    pass = 0;
	    Update_Screen();
	}
	pass++;
	path = concatdir(path, local_dir);
	path = concat(path, dp->d_name);
#if defined(_IFLNK)||defined(IFLNK)
	if ( k = lstat(path, &statbuf) != 0){ XtFree(path); continue;}
#else
	if ( stat(path, &statbuf) != 0) {XtFree(path); continue;}
#endif
	dir->files[files].owner = XtNewString(getowner(statbuf.st_uid));
        dir->files[files].group = XtNewString(getgroup(statbuf.st_gid));
	dir->files[files].link	    = statbuf.st_nlink;
	dir->files[files].size      = statbuf.st_size;
        t = XtNewString(ctime(&statbuf.st_mtime));
        strncpy(month, (char *)&t[4], 3);
        month[3] = '\0';
	strncpy(day, (char *)&t[8], 2);
	day[2] = '\0';
	strncpy(time_year, (char *)&t[20], 4);
	time_year[4] = '\0';
	dir->files[files].month     = XtNewString(month);
	dir->files[files].day       = XtNewString(day);
	dir->files[files].time_year = XtNewString(time_year);
	dir->files[files].modes     = XtNewString("-----------");
#if defined(CRAY)
	dir->files[files].mode      = set_unix_mode(statbuf.st_dm_mode,
					            dir->files[files].modes);
#else
	dir->files[files].mode      = set_unix_mode(statbuf.st_mode,
					            dir->files[files].modes);
#endif
        dir->files[files].linkname = NULL;
#if defined(S_ISLNK)||defined(_IFLNK)
	if ( dir->files[files].mode & MODE_TYPE_LINK)  {
	    char linkname[2024+1];
	    int nc;

	    nc = readlink(path, linkname, sizeof(linkname)-1);
	    if (nc > 0) { 
		linkname[nc] = '\0';
	    	dir->files[files].linkname = XtNewString(linkname);
	    }
	} 
#endif
	dir->files[files].name      = XtNewString(dp->d_name);
	dir->files[files].local     = dir->files[files].name;
	dir->files[files].remote    = Tran_Remote_File(dp->d_name, 
						       &dir->files[files].mode);
	if (dir->files[files].mode & MODE_TYPE_DIR ||
	    dir->files[files].mode & MODE_TYPE_LINK) 
	    dir->files[files].dir_name  = dir->files[files].name;

	if (path) XtFree(path);
        files++;
	if (files > dir->n) {
	     closedir(dirp);
	     Clear_Files(dir);
	     goto restart;
	}
    }
    dir->n = files;
    closedir(dirp);
}


static int
set_unix_mode(unix_mode, status)
int unix_mode;
char *status;
{
    int mode = 0;

    if (S_ISDIR(unix_mode)) {
	mode  |= MODE_TYPE_DIR;
	status[0] = 'd';
    }
    if (S_ISBLK(unix_mode)) {
	mode  |= MODE_TYPE_BLOCK;
	status[0] = 'b';
    }
    if (S_ISCHR(unix_mode)) {
	mode  |= MODE_TYPE_CHAR;
	status[0] = 'c';
    }
    if (S_ISREG(unix_mode)) {
	mode  |= MODE_TYPE_FILE;
	status[0] = '-';
    }

#if defined(S_ISLNK)
    if (S_ISLNK(unix_mode)) {
	mode  |= MODE_TYPE_LINK;
	status[0] = 'l';
    }
#else
#if defined(_IFLNK)
    if (((unix_mode)&_IFMT) == _IFLNK) {
	mode  |= MODE_TYPE_LINK;
	status[0] = 'l';
    }
#endif
#endif

#if defined(S_IFOFL)
    if (S_ISOFL(unix_mode)) {
	mode  |=  MODE_TYPE_OFFLINE|MODE_TYPE_FILE;
	status[0] = 'm';
    }
#endif

#if defined(S_ISSOCK)
    if (S_ISSOCK(unix_mode)) {
	mode  |= MODE_TYPE_SOCK;
	status[0] = 's';
    }
#else
#if defined(_IFSOC)
    if (((unix_mode)&_IFMT) == _IFSOC) {
	 mode  |= MODE_TYPE_SOCK;
	status[0] = 's';
    }
#endif
#endif

    if (unix_mode & S_IRUSR) {
	mode  |= MODE_OWNER_READ;
	status[1] = 'r';
    }
    if (unix_mode & S_IWUSR) {
	mode  |= MODE_OWNER_WRITE;
	status[2] = 'w';
    }
    if (unix_mode & S_IXUSR) {
	mode  |= MODE_OWNER_EXECUTE;
	status[3] = 'x';
    }
    if (unix_mode & S_IRGRP) {
	mode  |= MODE_GROUP_READ;
	status[4] = 'r';
    }
    if (unix_mode & S_IWGRP) {
	mode  |= MODE_GROUP_WRITE;
	status[5] = 'w';
    }
    if (unix_mode & S_IXGRP) {
	mode  |= MODE_GROUP_EXECUTE;
	status[6] = 'x';
    }
    if (unix_mode & S_IROTH) {
	mode  |= MODE_OTHER_READ;
	status[7] = 'r';
    }
    if (unix_mode & S_IWOTH) {
	mode  |= MODE_OTHER_WRITE;
	status[8] = 'w';
    }
    if (unix_mode & S_IXOTH) {
	mode  |= MODE_OTHER_EXECUTE;
	status[9] = 'x';
    }
    return mode;
}


#define MAX_SAVE_UID  1000
static int  n_saved_uid = 0;
static struct _saved_uid {
	int uid;
	char *owen;
} saved_uid[MAX_SAVE_UID];

static char *
getowner(uid)
int uid;
{
    struct passwd *pw;
    int i;
    char *owen;
    static char temp[10];

    if (n_saved_uid) {
	for (i=0; i<n_saved_uid; i++) {
	    if (saved_uid[i].uid == uid) return saved_uid[i].owen;
	}
    }

    pw = getpwuid(uid);
    if (pw) {
	owen  = pw->pw_name;
    } else {
	sprintf(temp, "%6d",uid);
	owen = temp;
    }

    if (n_saved_uid < MAX_SAVE_UID) {
	owen = XtNewString(owen);
	saved_uid[n_saved_uid].uid = uid;
	saved_uid[n_saved_uid].owen = owen;
	n_saved_uid++;
    }
    return owen;
}

#define MAX_SAVE_GID  1000
static int  n_saved_gid = 0;
static struct _saved_gid {
	int gid;
	char *group;
} saved_gid[MAX_SAVE_GID];

static char *
getgroup(gid)
int gid;
{
    struct group *grp;
    int i;
    char *group;
    static char temp[10];

    if (n_saved_gid) {
	for (i=0; i<n_saved_gid; i++) {
	    if (saved_gid[i].gid== gid) return saved_gid[i].group;
	}
    }

    grp = getgrgid(gid);
    if (grp) {
	group = grp->gr_name;
    } else {
	sprintf(temp, "%6d",gid);
        group = temp;
    }

    if (n_saved_gid < MAX_SAVE_GID) {
	group = XtNewString(group);
	saved_gid[n_saved_gid].gid = gid;
	saved_gid[n_saved_gid].group = group;
	n_saved_gid++;
    }

    return group;
}

void
Update_Stat_Time(dir)
struct _dirs *dir;
{
    struct stat statbuff;
    extern int mult_file;

    /*
     * Some one else is worring direcotry updates.
     */
    if (mult_file) return;
    if (dir == NULL) return;
    if (stat(".", &statbuff) == 0) {
	if (dir->time != 0 && 
             dir->time != statbuff.st_mtime) dir->update++;
	dir->time = statbuff.st_mtime;
    }
}

