/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */

/*
	$Disclaimer: 
 * Permission to use, copy, modify, and distribute this software and its 
 * documentation for any purpose is hereby granted without fee, 
 * provided that the above copyright notice appear in all copies and that 
 * both that copyright notice, this permission notice, and the following 
 * disclaimer appear in supporting documentation, and that the names of 
 * IBM, Carnegie Mellon University, and other copyright holders, not be 
 * used in advertising or publicity pertaining to distribution of the software 
 * without specific, written prior permission.
 * 
 * IBM, CARNEGIE MELLON UNIVERSITY, AND THE OTHER COPYRIGHT HOLDERS 
 * DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT 
 * SHALL IBM, CARNEGIE MELLON UNIVERSITY, OR ANY OTHER COPYRIGHT HOLDER 
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
 * DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, 
 * WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS 
 * ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
 * OF THIS SOFTWARE.
 *  $
*/

#ifndef NORCSID
#define NORCSID
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/snap2/pcserver/RCS/pcsprocs.c,v 2.13 1993/09/22 19:51:06 gk5g Exp $";
#endif

/*
 *	PC Server - Services Component
 *	Access to the VICE File System for IBM PC/XT/ATs
 *
 *	(C)Copyright IBM Corporation, 1984, 1985, 1986, 1987
 *	Program Property of IBM
 *
 *	Version 2.9 by Larry Raper
 *	Based on original code written by Bob Sidebotham
 *	Developed for the Information Technology Center at
 *	Carnegie-Mellon University
 *
 *	2.5  01/87  Improved return codes from ChMod call
 *	2.6  02/87  Improved DirSearch positioning
 *	2.7  05/87  Improvements in directory case sensitivity
 *	2.8  08/87  Inaccesible files removed from DirSearch output
 *	2.9  10/87  Removed case sensitivity from MkDir & RmDir, fixed
 *		    permission handling in DirSearch, & removed unreferenced
 *		    variables
 *
 */

#include <andrewos.h> /* sys/time.h strings.h sys/file.h */
#include <stdio.h>
#include <sys/errno.h>
#include <sys/param.h>
#include <sys/stat.h>
#include <ctype.h>
#include <pwd.h>
#include <pcserver.h>
#ifdef POSIX_ENV
#include <sys/types.h>
#include <utime.h>
#endif /* POSIX_ENV */

#define byte	unsigned char
#define MAXHANDLES 32
#define NOFOLD	0
#define FOLD	1

extern int errno;
extern int PCS_debuglevel;

#if !SY_AIX12
#if defined(POSIX_ENV)
uid_t getuid();
#else
int getuid();
#endif
#endif

struct passwd *getpwuid ();
struct passwd *getpwnam ();
char *GetCurrentDir();

struct {
    byte inuse;       /* 1 if the client has a file open on this descriptor */
    char *fn;
} HandleStuff[MAXHANDLES];

extern char *HiddenPath();

int PCS_Open (Pathname, Intent, Filter, Handle, Mode, Date, Time, Size)
char *Pathname;
int Intent, Filter;
int *Handle, *Mode, *Date, *Time, *Size;
{
    struct stat filestatus;
    PCNAME name;
    PATH dir,path;
    int temp_filter, orig_hidden;

    *Handle = -1;

    if (Intent != PCS_INPUT && Intent != PCS_OUTPUT &&
	 Intent != PCS_UNSPECIFIED)
	return PCS_BADINTEN;
    if (Intent != PCS_OUTPUT && Filter != PCS_NORMAL &&
	 Filter != PCS_HIDDEN && Filter != (PCS_NORMAL+PCS_HIDDEN)) {
	DBGMSG (2,("Open - filter validation failed"));
	return PCS_BADFILT;
    }

    if (SplitPath(Pathname, dir, &name, NODOTS, NOFOLD) == -1
	 || ChangeDirectory(dir) == -1)
	return PCS_NOPATH;
    strcpy (dir, GetCurrentDir ());

    if (Intent == PCS_INPUT || Intent == PCS_UNSPECIFIED) {
	orig_hidden = name.hidden;
	if (name.hidden)	      /* An explicit dot before the name    */
	    temp_filter = PCS_HIDDEN; /* overrides any filter specification */
	else
	    temp_filter = Filter;
	while ((temp_filter & PCS_NORMAL) || (temp_filter & PCS_HIDDEN)) {
	    if (temp_filter & PCS_NORMAL) {
		temp_filter &= ~PCS_NORMAL;
		name.hidden = 0;
	    }
	    else {
		temp_filter &= ~PCS_HIDDEN;
		name.hidden = 1;
	    }
	    MakePath(path, "", &name);
	    if (Intent == PCS_UNSPECIFIED)
		*Handle = open(path, O_RDWR);
	    if (*Handle == -1)
		*Handle = open(path, O_RDONLY);
	    DBGMSG (3,("Open - handle: %d",*Handle));
	    if (*Handle != -1)
		break;
	}

	if (*Handle == -1) {

	    /* Still haven't opened file yet.  Unconditionally */
	    /* force name to lowercase and try again	       */

	    name.hidden = orig_hidden;
	    FoldLower (name.name);
	    FoldLower (name.ext);

	    if (name.hidden)
		temp_filter = PCS_HIDDEN;
	    else
		temp_filter = Filter;
	    while ((temp_filter & PCS_NORMAL) || (temp_filter & PCS_HIDDEN)) {
		if (temp_filter & PCS_NORMAL) {
		    temp_filter &= ~PCS_NORMAL;
		    name.hidden = 0;
		}
		else {
		    temp_filter &= ~PCS_HIDDEN;
		    name.hidden = 1;
		}
		MakePath(path, "", &name);
		if (Intent == PCS_UNSPECIFIED)
		    *Handle = open(path, O_RDWR);
		if (*Handle == -1)
		    *Handle = open(path, O_RDONLY);
		DBGMSG (3,("Open - handle: %d",*Handle));
		if (*Handle != -1)
		    break;
	    }
	}
    }
    else { /* Intent == PCS_OUTPUT */

	MakePath(path, "", &name);

	/* Make certain that any files "created" here have only
	 lowercase characters in their name */

	if ((*Handle = open(path, O_RDWR, 0644)) == -1) {
	    FoldLower (name.name);
	    FoldLower (name.ext);
	    MakePath (path, "", &name);
	    if ((*Handle = open(path, O_RDWR | O_CREAT, 0644)) == -1)
		return PCS_NOPATH;
	}
    }
    if (fstat(*Handle,&filestatus) == -1 ||
	 (filestatus.st_mode & S_IFMT) != S_IFREG) {
	close(*Handle);
	*Handle = -1;
	return PCS_NOPATH;
    }
    *Mode = name.hidden ? PCS_HIDDEN : 0;
    if (!(filestatus.st_mode & S_IWRITE)) {

	/* an approximation ...  if you're not the owner of the file this
	     isn't quite right ... should use access(2) here (but it takes
							      a pathname and is therefore inefficient) */

	*Mode += PCS_READONLY;
    }
    if (!*Mode)
	*Mode = PCS_NORMAL;
    GetPCDateTime(filestatus.st_mtime, Date, Time);
    *Size = filestatus.st_size;
    HandleStuff[*Handle].inuse = 1;
    if ((HandleStuff[*Handle].fn =
	  (char *) malloc (strlen(path)+strlen(dir)+1)) != NULL)
	MakePath (HandleStuff[*Handle].fn, dir, &name);
    return PCS_SUCCESS;
}

int PCS_Close (Handle)
int Handle;
{
    if ((Handle > MAXHANDLES-1) || (!HandleStuff[Handle].inuse))
	return PCS_NOHANDLE;
    HandleStuff[Handle].inuse = 0;
    if (HandleStuff[Handle].fn != NULL)
	free (HandleStuff[Handle].fn);
    HandleStuff[Handle].fn = NULL;
    if (vclose(Handle) == 0)
	return PCS_SUCCESS;
    else
	return PCS_NOSPACE;
}

int PCS_Read (Handle, Offset, Length, Return_length, Data)
int Handle, Offset, Length;
int *Return_length;
char **Data;
{
    static char *buffer = NULL;
    static int buffersize;
    int allocatesize = Length;
    if (buffer == NULL)
	buffer = (char *) malloc(buffersize=allocatesize);
    else
	if (allocatesize>buffersize)
	    buffer = (char *) realloc(buffer, buffersize=allocatesize);
    *Return_length = 0;
    if (buffer == NULL) {
	static char nulldata;
	nulldata = 0;
	*Data = &nulldata;
	return PCS_BADARG;
    }
    *Data = buffer;
    if ((Handle > MAXHANDLES-1) || (!HandleStuff[Handle].inuse))
	return PCS_NOHANDLE;
    if (lseek(Handle, Offset, 0) == -1)
	return errno == EBADF? PCS_NOHANDLE: PCS_EOF;
    if ((*Return_length = read(Handle, buffer, Length)) == -1) {
	*Return_length = 0;
	return PCS_EOF;
    }
    return *Return_length == Length? PCS_SUCCESS: PCS_EOF;
}

int PCS_Write (Handle, Offset, Length, Data, Return_length)
int Handle, Offset, Length;
char *Data;
int *Return_length;
{
    int oldoffset;
    *Return_length = 0;
    if ((Handle > MAXHANDLES-1) || (!HandleStuff[Handle].inuse))
	return PCS_NOHANDLE;
    if ((oldoffset = lseek(Handle, Offset, 0)) == -1)
	return errno == EBADF? PCS_NOACCESS: PCS_NOSPACE;
    if (Length == 0) {
	if (oldoffset < Offset && write(Handle, "!", 1) != 1)
	    return errno == EBADF? PCS_NOACCESS: PCS_NOSPACE;
	return ftruncate(Handle, Offset) == -1 ? PCS_NOACCESS: PCS_SUCCESS;
    }
    if ((*Return_length = write(Handle, Data, Length)) == -1) {
	*Return_length = 0;
	return errno==EBADF? PCS_NOACCESS: PCS_NOSPACE;
    }
    return PCS_SUCCESS;
}

int PCS_DirSearch (Generic_pathname, Filter, Starting_from, First_match, Mode,
		    Date, Time, Size)
char *Generic_pathname;
int  Filter;
char *Starting_from;
char **First_match;
int  *Mode, *Date, *Time, *Size;
{
    static DIR *dirp;			    /* Must be static */
    PATH PreviousGenericPathname;	    /* Must be static */
    static PCNAME GenericName;		    /* Must be static */
    static int last_filter;		    /* Must be static */
    static char last_match[MAXPATHLEN+1];   /* Must be static */
    DIRENT_TYPE *entry;
    struct stat filestatus;
    int slen, uid, gid;
    char localstart[MAXPATHLEN+1];


    *First_match = "";

    if ((dirp == NULL) || (*Starting_from == '\0') ||
	 (strcmp (Starting_from, last_match) != 0) ||
	 (last_filter != Filter) ||
	 (strcmp(Generic_pathname,PreviousGenericPathname) != 0)) {
	PATH Dir;
	if (dirp) {
	    closedir(dirp);
	    dirp = NULL;
	}
	strncpy(PreviousGenericPathname,Generic_pathname,MAXPATHLEN);
	PreviousGenericPathname[MAXPATHLEN] = '\0';
	if (SplitPath(Generic_pathname, Dir, &GenericName, DOTS, FOLD) == -1
	    || ChangeDirectory(Dir) == -1 || (dirp = opendir(".")) == NULL)
	    return PCS_NOPATH;
	if (Filter&PCS_HIDDEN)
	    GenericName.hidden = 1;
	/* Now reposition to "starting_from" entry */
	if ((slen = strlen (Starting_from)) != 0) {
	    for (entry = readdir(dirp); entry != NULL; entry = readdir(dirp))
		if (DIRENT_NAMELEN(entry) != 0) {
		    if ((DIRENT_NAMELEN(entry) == slen) &&
			(bcmp (Starting_from, entry->d_name, slen) == 0))
			break;
		}
	    if (entry == NULL) {
		rewinddir (dirp);
		strcpy (localstart, ".");
		strcat (localstart, Starting_from);
		Starting_from = localstart;
		slen++;
		for (entry = readdir(dirp); entry != NULL; entry = readdir(dirp))
		    if (DIRENT_NAMELEN(entry) != 0) {
			if ((DIRENT_NAMELEN(entry) == slen) &&
			    (bcmp (Starting_from, entry->d_name, slen) == 0))
			    break;
		    }
	    }
	    if (entry == NULL)
		return PCS_NOMATCH;
	}
    }
    uid = getuid ();
    gid = getgid ();
    for (entry = readdir(dirp); entry != NULL; entry = readdir(dirp))
	if (DIRENT_NAMELEN(entry) != 0) {
	    PCNAME thisname;
	    if (SplitNameAllowDots(entry->d_name, &thisname) == -1)
		continue;
	    GenericName.hidden = thisname.hidden;
	    if (NoCaseGenericEquality(&GenericName, &thisname)) {
		static PATH Path; /* Must be static */
		int readonly, normal, directory, hidden;
		MakePath(Path, "", &thisname);
		if (stat(Path, &filestatus) == -1)
		    continue;
		if (filestatus.st_uid != uid) {
		    if (filestatus.st_gid != gid) {
			if ((!(filestatus.st_mode & (S_IREAD >> 6))) && (!Vice ()))
			    continue;
			readonly = (!(filestatus.st_mode & (S_IWRITE >> 6)));
		    }
		    else {   /* same group */
			if ((!(filestatus.st_mode & (S_IREAD >> 3)))
			    && (!(filestatus.st_mode & (S_IREAD >> 6)))
			    && (!Vice ()))
			    continue;
			readonly = ((!(filestatus.st_mode & (S_IWRITE >> 3)))
				    && (!(filestatus.st_mode & (S_IWRITE >> 6))));
		    }
		}
		else  /* owner of file */
		    readonly = (!(filestatus.st_mode & S_IWRITE));
		directory = ((filestatus.st_mode & S_IFMT) == S_IFDIR);
		hidden = thisname.hidden;
		normal = !hidden && !directory && !readonly
		  && ((filestatus.st_mode&S_IFMT) == S_IFREG);
		DBGMSG (20,("Readonly: %d, Directory: %d, Hidden: %d, Normal: %d",
			    readonly, directory, hidden, normal));
		if (   ((Filter&PCS_NORMAL) && (normal ||
						(readonly && !directory && !hidden)))
		    || ((Filter&PCS_DIRECTORY) && directory && !hidden)
		    || ((Filter&PCS_READONLY) && readonly && !hidden)
		    || ((Filter&PCS_HIDDEN) && hidden)	 ){
		    if (directory && thisname.hidden)
			*First_match = &Path[0];
		    else
			*First_match = &Path[thisname.hidden];
		    GetPCDateTime(filestatus.st_mtime, Date, Time);
		    *Size = filestatus.st_size;
		    if (normal)
			*Mode = PCS_NORMAL;
		    else {
			*Mode = 0;
			if (hidden)
			    *Mode += PCS_HIDDEN;
			if (readonly)
			    *Mode += PCS_READONLY;
			if (directory)
			    *Mode += PCS_DIRECTORY;
		    }

		    strcpy (last_match, *First_match);
		    last_filter = Filter;
		    return PCS_SUCCESS;
		}
	    }
	}
    if ((strcmp (Generic_pathname, "/") == 0) &&
	 (Filter & PCS_DIRECTORY) &&
	 (strlen (Starting_from) == 0)) {
	if (stat ("/", &filestatus) != -1) {
	    GetPCDateTime (filestatus.st_mtime, Date, Time);
	    *First_match = "";
	    last_match[0] = 0;
	    last_filter = Filter;
	    *Mode = PCS_DIRECTORY;
	    return PCS_SUCCESS;
	}
    }
    closedir(dirp);
    dirp = NULL;
    *First_match = "";
    last_match[0] = 0;
    return PCS_NOMATCH;
}

int PCS_RemoveFiles (Generic_pathname)
char *Generic_pathname;
{
    int result;
    PCNAME temp;

    result = RemoveFiles (Generic_pathname, &temp, NOFOLD);
    if ((result == PCS_NOMATCH) && UpperCase(temp.name) && UpperCase(temp.ext))
	result = RemoveFiles (Generic_pathname, &temp, FOLD);
    return result;
}

int RemoveFiles (Generic_pathname, GenericName, foldoption)
char *Generic_pathname;
PCNAME *GenericName;
int foldoption;
{
    PATH Dir;
    DIR *dirp;
    DIRENT_TYPE *entry;
    int result;

    if (SplitPath(Generic_pathname, Dir,
		   GenericName, NODOTS, foldoption) == -1)
	return PCS_NOPATH;
    if (ChangeDirectory(Dir) == -1 || (dirp = opendir(".")) == NULL)
	return PCS_NOPATH;
    result = PCS_NOMATCH;
    for (entry = readdir(dirp); entry != NULL; entry = readdir(dirp))
	if (DIRENT_NAMELEN(entry) != 0) {
	    PCNAME thisname;
	    if (SplitName(entry->d_name, &thisname) == -1)
		continue;
	    if (GenericEquality(GenericName, &thisname)) {
		PATH Path;
		MakePath(Path, "", &thisname);
		if (unlink(Path) != -1)
		    /* PCS_SUCCESS returned if even one successful
			deletion occurs. */
		    result = PCS_SUCCESS;
	    }
	}
    closedir(dirp);
    return result;
}

int PCS_RenameFiles (Old_generic_pathname, New_generic_pathname)
char *Old_generic_pathname;
char *New_generic_pathname;
{
    PCNAME OldGenericName, NewGenericName;
    PATH OldDir, NewDir;
    DIR *dirp;
    DIRENT_TYPE *entry;
    int result;
    if (SplitPath (Old_generic_pathname, OldDir, &OldGenericName, NODOTS, FOLD) == -1
	 || SplitPath (New_generic_pathname, NewDir, &NewGenericName, NODOTS, FOLD)
	 == -1)
	return PCS_NOPATH;
    if (OldDir[0] != '/' || NewDir[0] != '/' || ChangeDirectory(OldDir) == -1
	 || (dirp = opendir(".")) == NULL)
	return PCS_NOPATH;

    if (NoCaseStrcmp (NewDir, GetCurrentDir()) == 0)
	strcpy (NewDir, GetCurrentDir());

    result = PCS_NOMATCH;
    for (entry = readdir(dirp); entry != NULL; entry = readdir(dirp))
	if (DIRENT_NAMELEN(entry) != 0) {
	    PCNAME thisname;
	    if (SplitName(entry->d_name, &thisname) == -1)
		continue;
	    if (GenericEquality(&OldGenericName, &thisname)) {
		PATH OldPath, NewPath;
		MakePath(OldPath, "", &thisname);
		MakeNewName(&thisname, &NewGenericName);
		MakePath(NewPath, NewDir, &thisname, 0);
		if (rename(OldPath, NewPath) != -1)
		    /* PCS_SUCCESS returned if even one successful
			rename happens. */
		    result = PCS_SUCCESS;
	    }
	}
    closedir(dirp);
    return result;
}

int PCS_MkDir (Pathname)
char *Pathname;
{
    PCNAME Name;
    PATH Dir,Path;

    /* The following sequence guarantees that the Path name produced */
    /* by MakePath will have the correct UNIX case for all components */

    if (SplitPath(Pathname, Dir, &Name, NODOTS, FOLD) == -1)
	return PCS_NOPATH;
    if (ChangeDirectory (Dir) == -1)
	return PCS_NOPATH;
    MakePath(Path, GetCurrentDir (), &Name);

    return mkdir(Path, 0755) == -1 ? PCS_NOPATH : PCS_SUCCESS;
}

int PCS_RmDir (Pathname)
char *Pathname;
{
    PCNAME Name;
    PATH Dir, Path;

    /* The following sequence guarantees that the Path name produced */
    /* by MakePath will have the correct UNIX case for all components */

    if (SplitPath(Pathname, Dir, &Name, NODOTS, NOFOLD) == -1)
	return PCS_NOPATH;
    if (ChangeDirectory (Dir) == -1)
	return PCS_NOPATH;
    MakePath(Path, GetCurrentDir(), &Name);

    if (rmdir(Path) != -1)
	return PCS_SUCCESS;
    return errno == ENOTEMPTY ? PCS_NOTEMPTY : PCS_NOPATH;
}

int PCS_ChMod (Pathname, File_mode)
char *Pathname;
int  File_mode;
{
    PCNAME Name;
    PATH Dir,filename;
    struct stat filestatus;

    if (SplitPath(Pathname, Dir, &Name, NODOTS, FOLD) == -1 ||
	 ChangeDirectory(Dir) == -1)
	return PCS_NOPATH;
    MakePath(filename, "", &Name);
    if (stat(filename,&filestatus) == -1) {
	Name.hidden = 1;
	MakePath(filename, "", &Name);
	if (stat(filename,&filestatus) == -1)
	    return PCS_NOFILE;
    }
    if ((filestatus.st_mode&S_IFMT) != S_IFREG)
	return PCS_NOFILE;
    if (((filestatus.st_mode&S_IWRITE) != 0) !=
	 ((File_mode&PCS_READONLY) == 0)) {
	if (chmod (filename, (File_mode&PCS_READONLY) ? 0444 : 0644) == -1)
	    return PCS_NOACCESS;
    }
    if (((File_mode&PCS_HIDDEN) != 0) != Name.hidden) {
	PATH newname;
	Name.hidden = 1 - Name.hidden;
	MakePath(newname, "", &Name);
	if (rename(filename, newname) == -1)
	    return PCS_NOACCESS;
    }
    return PCS_SUCCESS;
}

#define SECSPERAU   4
#define BYTESPERSEC 512
#define BYTESPERAU (SECSPERAU * BYTESPERSEC)

int PCS_SpaceQuery (Total_AUs, Remaining_AUs, Sectors_per_AU, Bytes_per_sector)
int *Total_AUs, *Remaining_AUs, *Sectors_per_AU, *Bytes_per_sector;
{
    *Sectors_per_AU = SECSPERAU;
    *Bytes_per_sector = BYTESPERSEC;

    GetQuotaInfo (GetCurrentDir (), BYTESPERAU, Total_AUs, Remaining_AUs);

    DBGMSG (12,("SpaceQuery - path: %s", GetCurrentDir()));
    DBGMSG (12,("SpaceQuery - Total AUs: %d Remaing AUs: %d",
		 *Total_AUs,*Remaining_AUs));

    return PCS_SUCCESS;
}

long time();

int PCS_TimeOfDay (pcdate, pctime)
int *pcdate, *pctime;
{
    int unixtime;
    unixtime = time (0);
    GetPCDateTime (unixtime, pcdate, pctime);
    return PCS_SUCCESS;
}

long gtime(), time();

int PCS_TimeStamp (Handle, DosDate, DosTime)
int Handle, DosDate, DosTime;
{
#ifdef POSIX_ENV
    struct utimbuf utb;
#else /* POSIX_ENV */   
    struct timeval tvp[2];
#endif /* POSIX_ENV */
    struct tm *pctime;
    long temp;

    if ((Handle > MAXHANDLES-1) || (!HandleStuff[Handle].inuse))
	return PCS_NOHANDLE;

    temp = time (0);
    pctime = localtime (&temp);

    pctime->tm_sec = (DosTime & 0x1F) << 1;
    pctime->tm_min = (DosTime & 0x7E0) >> 5;
    pctime->tm_hour = (DosTime & 0x0F800) >> 11;
    pctime->tm_mday = (DosDate & 0x1F);
    pctime->tm_mon = ((DosDate & 0x1E0) >> 5) - 1;
    pctime->tm_year = ((DosDate & 0x0FE00) >> 9) + (1980-1900);

    DBGMSG (12,("TimeStamp - yr: %d, mo: %d, day: %d, hr: %d, min: %d, sec: %d",
		 pctime->tm_year, pctime->tm_mon, pctime->tm_mday, pctime->tm_hour,
		 pctime->tm_min, pctime->tm_sec));
    DBGMSG (12,("TimeStamp - asctime(pctime): %s", asctime(pctime)));

#ifdef POSIX_ENV
    utb.modtime = utb.actime = mktime(pctime);

    DBGMSG (12,("TimeStamp - asctime(gtime): %s", asctime(localtime(utb.modtime))));
    DBGMSG (12,("TimeStamp - fn: %s", HandleStuff[Handle].fn));

    if (utime(HandleStuff[Handle].fn, &utb) == -1)
	return PCS_NOACCESS;
#else /* POSIX_ENV */
    tvp[0].tv_sec = gtime (pctime);
    tvp[0].tv_usec = 0;
    tvp[1].tv_sec = tvp[0].tv_sec;
    tvp[1].tv_usec = 0;

    DBGMSG (12,("TimeStamp - asctime(gtime): %s",
		 asctime(localtime(&tvp[0].tv_sec))));
    DBGMSG (12,("TimeStamp - fn: %s", HandleStuff[Handle].fn));

    if (utimes (HandleStuff[Handle].fn, tvp) == -1)
	return PCS_NOACCESS;
#endif /* POSIX_ENV */
    return PCS_SUCCESS;
}


int PCS_GetHomeDir (relative_dir, absolute_dir)
char *relative_dir;
char **absolute_dir;
{
    static char nullstring = 0;
    static char absdir[MAXPATHLEN+1];
    char *subdir, *user_name;
    struct passwd *passinfo;
    int i, len;

    *absolute_dir = &nullstring;
    if (*relative_dir == '/')
	return PCS_BADARG;

    subdir = "";
    user_name = "";

    if (*relative_dir == '~') {
	user_name = relative_dir+1;
	for (i=1; i<strlen(relative_dir); i++) {
	    if (relative_dir[i] == '/') {
		subdir = relative_dir+i+1;
		relative_dir[i] = 0;
		break;
	    }
	}
    }
    else
	subdir = relative_dir;

    DBGMSG (3,("GetHomeDir - userid: \"%s\", subdir: \"%s\"",
		user_name, subdir));

    if (*user_name == 0)
	passinfo = getpwuid (getuid ());
    else
	passinfo = getpwnam (user_name);

    if ((passinfo == NULL) ||
	 ((len = strlen (passinfo->pw_dir)) > MAXPATHLEN) ||
	 ((len + strlen (subdir) + 1) > MAXPATHLEN))
	return PCS_NOPATH;

    strcpy (absdir, passinfo->pw_dir);

    DBGMSG (3,("GetHomeDir - absdir: \"%s\"", absdir));

    if (*subdir) {
	strcat (absdir, "/");
	strcat (absdir, subdir);
    }

    if (ChangeDirectory (absdir) == -1)
	return PCS_NOPATH;

    *absolute_dir = absdir;
    return PCS_SUCCESS;
}
