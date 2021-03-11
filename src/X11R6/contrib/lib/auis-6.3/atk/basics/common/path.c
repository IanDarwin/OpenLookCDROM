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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/basics/common/RCS/path.c,v 1.19 1994/02/18 19:07:46 rr2b Exp $";
#endif


#include <andrewos.h>
#include <class.h>
#include <ctype.h>

#include <environ.ih>
#include <im.ih>

#include <errno.h>
#include <sys/param.h>
#include <sys/stat.h>
#include <pwd.h>
#include <util.h>

#include <filetype.ih>

#include <path.eh>

struct homestruct {
    char fullPath[MAXPATHLEN];
    char shortPath[MAXPATHLEN];
    long fullLength;
    long shortLength;
    struct homestruct *next;
};

static struct homestruct *homes = NULL;

#define INITIALCHUNK 30 /* files */

	static char *
strappend(dest, src)
	char *dest, *src;
{
	strcpy(dest, src);
	return dest+strlen(dest);
}


static void FreeList(list)
    char **list;
{
    long i;

    if (list != NULL) {
        for (i=0; list[i] != NULL; i++) {
            free(list[i]);
        }
        free(list);
    }
} /* path__FreeList */

static void FreeFilesAndDirs(self)
    struct path *self;
{
    FreeList(self->files);
    FreeList(self->dirs);
    self->files = NULL;
    self->dirs = NULL;
} /* FreeFilesAndDirs */

static void SetPath(self, filepath)
    struct path *self;
    char *filepath;
{
    if (self->filepath != NULL) {
        free(self->filepath);
    }

    if (self->truncatedPath != NULL) {
        free(self->truncatedPath);
        self->truncatedPath = NULL;
    }

    FreeFilesAndDirs(self);
 
    if (filepath != NULL) {
        /* SHOULD CACHE THESE THINGS */
        self->filepath = (char *) malloc(strlen(filepath) + 1);
        strcpy(self->filepath, filepath);
    }
    else {
        self->filepath = NULL;
    }

    self->haveScanned = FALSE;
    self->knowIsDir = FALSE;

} /* SetPath */

void path__InputTruncatedPathCache(c, fp)
    struct classheader *c;
    FILE *fp;
{
    char lens[MAXPATHLEN];
    char fpath[MAXPATHLEN];
    char spath[MAXPATHLEN];
    long flen;
    long slen;
    struct homestruct *aHome;
    struct homestruct *newHome;
    struct homestruct *endHome = NULL;
    struct homestruct *inputHomes = NULL;
    boolean gotHome;

    fgets(lens, MAXPATHLEN, fp);
    while (strncmp(lens, "no", 2) != 0) {
        sscanf(lens, "fullpathlen: %d shortpathlen: %d\n", &flen, &slen);
        fgets(fpath, MAXPATHLEN, fp);
        fgets(spath, MAXPATHLEN, fp);
        fpath[flen] = '\0';
        spath[slen] = '\0';
        gotHome = FALSE;
        for (aHome = homes; aHome != NULL; aHome = aHome->next) {
            if (strcmp(aHome->fullPath, fpath) == 0) {
                gotHome = TRUE;
                break;
            }
        }
        if (!gotHome) {
            newHome = (struct homestruct *) malloc(sizeof(struct homestruct));
            strcpy(newHome->fullPath, fpath);
            strcpy(newHome->shortPath, spath);
            newHome->fullLength = flen;
            newHome->shortLength = slen;

            if (inputHomes == NULL) {
                endHome = newHome;
            }
            newHome->next = inputHomes;
            inputHomes = newHome;
        }
        fgets(lens, MAXPATHLEN, fp);
    }

    if (endHome != NULL) {
        endHome->next = homes;
    }
    homes = inputHomes;

} /* path__InputTruncatedPathCache */

void path__OutputTruncatedPathCache(c, fp)
    struct classheader *c;
    FILE *fp;
{
    struct homestruct *aHome;

    for (aHome = homes; aHome != NULL; aHome = aHome->next) {
        fprintf(fp, "fullpathlen: %d shortpathlen: %d\n", aHome->fullLength, aHome->shortLength);
        fprintf(fp, "%s\n%s\n", aHome->fullPath, aHome->shortPath);
    }
    fprintf(fp, "no more home directories\n");
} /* path__OutputTruncatedPathCache */

boolean path__InitializeObject(c, self)
    struct classheader *c;
    struct path *self;
{
    self->filepath = NULL;
    self->truncatedPath = NULL;
    self->files = NULL;
    self->dirs = NULL;
    self->haveScanned = FALSE;
    self->knowIsDir = FALSE;
    self->mayBeWrong = TRUE;
    self->numFiles = -1;
    self->numDirs = -1;
    self->changetime = 0;

    return TRUE;
} /* path__InitializeObject */

struct path *path__Create(c, filepath)
    struct classheader *c;
    char *filepath;
{
    struct path *self = path_New();

    SetPath(self, filepath);

    return self;
} /* path__Create */


#if 1

/* FoldName -- remove all "." and ".." components from a file name 
     modifies the buffer passed to it 

RESTRICTION:
When there are symlinks, .. is meaningful and should get into 
an entirely different directory.  This does not happen here.

In general, names passed to this routine begin with slash.
However, the routine works equally well if they do not.


The pathname is broken into segments by /'s.
There is a state machine, where the state depends on the previous segment.
For each cell, there is an action and a new state.  Segments may be
	.   ..   empty   x (i.e., any name)

The states are:
	Start	-  There is no prior seg.
	ISlash	-  Prior seg is / at start of path
	IX	-  Prior seg is /x at start of path 
	NewC	-  Prior seg is /../ at start of path  (for Newcastle)
	IDot	-  Prior seg is . at strat of path
	IDots	-  Prior seg is .. at start of path
	X	-  Prior seg is a name
In states X and IX, there is a name preserved which will usually be later
generated to the output.  We refer to this saved name as y.

In the following diagram, the action is a triple: text generated to the
path, text preserved as y, and new state.  In general, slashes
are generated at the beginning of generation for a segment, if needed.

	|	Current segment  (is followed by a slash)
State	|	.		..		x		empty
___________________________________________________________________________
Start	|	, , IDot	, , IDots	, x, IX		, , ISlash
ISlash	|	, , ISlash	, , NewC	, x, X		, , ISlash
IX	|	, y, IX		, , IDot	y, x, X		, y, IX
NewC	|	, , NewC	, , NewC	/.., x, X	, , NewC
IDot	|	, , IDot	, , IDots	, x, IX		, , IDot
IDots	|	, , IDots	../, , IDots	.., x, X	, , IDots
X	|	, y, X		(1), Check	/y, x, X	, y, X


(1) the .. and y cancel each other.  We restore the state according to
how we could have transitioned to the X state.  These rules could have gotten to
state X:

ISlash - x:	, x, X		dest == path
IX - x:		y, x, X		no slash, just an identifier in dest
NewC - x:	/.., x, X	/.. in dest
IDots - x:	.., x, X	no slash, just .. in dest
X - .:		, y, X		ignore this case
X - x:		/y, x, X	/y in dest
X - empty:	, y, X		ignore this case, too


When the current segment is at the end and has no following slash,
the machine is finished.  The only action is to generate text to output.

	|	Current segment  (at the end with no following slash)
State	|	.	..	x	empty
___________________________________________________________________________
Start	|	.	..	x	.
ISlash	|	/	/..	/x	/
IX	|	y	.	y/x	y/
NewC	|	/../	/../	/../x	/../
IDot	|	.	..	x	./
IDots	|	..	../..	../x	../
X	|	/y	/	/y/x	/y/

*/

/* States */
enum state {
	Start,	/* There is no prior seg.  */
	ISlash,	/* Prior seg is / at start of path  */
	IX,	/* Prior seg is /x at start of path   */
	NewC,	/* Prior seg is /../ at start of path  (for Newcastle)  */
	IDot,	/* Prior seg is . at strat of path  */
	IDots,	/* Prior seg is .. at start of path  */
	X,	/* Prior seg is a name  */
	Check	/* used for Backup case */
};

/*      Actions  */
enum segaction {
	/*  , ,    */	Naught,
	/*  , x    */	SaveX,
	/*  , y    */	SaveY,
	/*  y, x   */	GenYSvX,
	/*  /.., x */	NewCSvX,
	/*  ../,   */	Dots,
	/*  .., x  */	DotsSvX,
	/*  (1)    */	Backup,
	/*  /y, x  */	NextX
};

enum segtype {Dot, DotDot, ID, Empty};

#define NSTATES 7
#define NTYPES 4

static struct {enum segaction Action; enum state NewState;}
	SegTrans[NSTATES][NTYPES]
= {
/* Start */  {{Naught, IDot},  {Naught, IDots},{SaveX, IX}, {Naught, ISlash}},
/* ISlash */ {{Naught, ISlash},{Naught, NewC}, {SaveX, X},  {Naught, ISlash}},
/* IX */     {{SaveY, IX},     {Naught, IDot}, {GenYSvX, X}, {SaveY, IX}},
/* NewC */   {{Naught, NewC},  {Naught, NewC}, {NewCSvX, X}, {Naught, NewC}},
/* IDot */   {{Naught, IDot},  {Naught, IDots},{SaveX, IX}, {Naught, IDot}},
/* IDots */  {{Naught, IDots}, {Dots, IDots},  {DotsSvX, X}, {Naught, IDots}},
/* X */	     {{SaveY, X},      {Backup, Check},{NextX, X},   {SaveY, X}}
};

static char *LastSeg[NSTATES][NTYPES]
	= {
/* Start */	{".",     "..",     "x",     "."},
/* ISlash */	{"/",     "/..",    "/x",    "/"},
/* IX */	{"y",     ".",      "y/x",   "y/"},
/* NewC */	{"/../",  "/../",   "/../x", "/../"},
/* IDot */	{".",     "..",     "x",     "./"},
/* IDots */	{"..",    "../..",  "../x",  "../"},
/* X */	  	{"/y",    "/",       "/y/x",  "/y/"}
};


	static void 
FoldName (path)
	register char *path;		/* path to fold */
{
	enum state CurrState, NextState;
	char SavedY [MAXPATHLEN];

	char *dest;	/* where to generate output */
	char *src;	/* start of current segment */
	char *sx;	/* segment scanner 
				segment extends from src to sx-1 */
	char *lx;	/* last segment string */
	enum segtype CurrType;

	*SavedY = '\0';
	CurrState = Start;
	src = path;
	dest = path;

	while (TRUE) {
		/* get end of segment which starts at src */
		sx = index(src, '/');
		if (sx == NULL) sx = src + strlen(src);
	
		/* determine segment type: . .. x empty */
		if (sx - src <= 0)  CurrType = Empty;
		else if (*src != '.') CurrType = ID;
		else if (sx - src == 1)  CurrType = Dot;
		else if (sx - src == 2 && *(src+1) == '.')
				CurrType = DotDot;
		else CurrType = ID;
		if (*sx != '/') 
			/* we are at the end */
			break;
	
		/* process state transition */
		NextState = (long)SegTrans[(long)CurrState][(long)CurrType].
				NewState;
		switch ((long)SegTrans[(long)CurrState][(long)CurrType].
				Action) {
		case Naught:		/*  , ,    */
		case SaveY:		/*  , y    */
			break;
		case SaveX:		/*  , x    */
	savex:
			strncpy(SavedY, src, sx-src);
			SavedY[sx-src] = '\0';
			break;
		case GenYSvX:		/*  y, x   */
			dest = strappend(dest, SavedY);
			goto savex;
		case Dots:		/*  ../,   */
			*dest++ = '.';
			*dest++ = '.';
			*dest++ = '/';
			break;
		case NewCSvX:		/*  /.., x */
			*dest++ = '/';
			/* FALL THRU */
		case DotsSvX:		/*  .., x  */
			*dest++ = '.';
			*dest++ = '.';
			goto savex;
		case NextX:		/*  /y, x  */
			*dest++ = '/';
			dest = strappend(dest, SavedY);
			goto savex;
		case Backup:		/*  (1)    */
			if (dest == path) {
				/* nothing generated so far */
				NextState = ISlash;
				break;
			}
			*dest = '\0';
			lx = rindex(path, '/');	/* find last slash */
			if (lx == NULL) {
				/* no slash generated yet */
				strcpy(SavedY, path);
				NextState = (strcmp(SavedY, "..") == 0)
					? IDots  :  IX;
				dest = path;
				break;
			}
			strcpy(SavedY, lx+1);
			dest = lx;
			if (strcmp(SavedY, "..") != 0)
				NextState = X;
			else if (*path == '/')
				NextState = NewC;
			else {
				/* dest begins with one or more "../" */
				NextState = IDots;
				dest++;		/* keep the slash */
			}
			break;
		}
		CurrState = NextState;

		/* set start of next segment */
		src = sx + 1;	/* just past the slash */
	}
	/* process final segment via LastSeg table 
	   for each char:
		x : copy src to dest
		y : copy SavedY to dest
		otherwise : copy char to dest
	*/
	lx = LastSeg[(long)CurrState][(long)CurrType];
	for ( ; *lx; lx++)
		if (*lx == 'x')
			dest = strappend(dest, src);
		else if (*lx == 'y')
			dest = strappend(dest, SavedY);
		else *dest++ = *lx;
	*dest = '\0';
}

#else

/*
	Rules:
	    buffer is	=> use this string
		/	=>  /
		.	=> .
		..	=> ..
		empty	=> .
		x	=> x
	    buffer starts    use this start
		/..	=> /..   (was => / , but must preserve /../
					for newcastle connection  -wjh)
		x/..	=> .
		./..	=> ..
		..	=> ..
		../..	=> ../..   (any number of ../)
		x/y	=> x/y
	    buffer continues    replace with
		/./	=> /
		//	=> /
		/x/../	=> /
    Does not know about ~ 
    Does not remove trailing /. or /..
    Does not merge leading /../../ into /../
    Converts /x/../../ into ../, but should be /../

*/

static void FoldName (path)
    register char *path;			/* path to fold */
{
    char   *pStart,		/* points to first char of a component */
	   *pEnd;		/* points to first char following
				   component */
    register char *dest;
    int     len;

    if (path == NULL)
	return;
    dest = path;
    pEnd = (*path == '/' ? path + 1 : path);
    for (;;) {
	pStart = pEnd;
	pEnd = index (pStart, '/');
	if (pEnd == NULL)
	    pEnd = pStart + strlen (pStart);
	len = pEnd - pStart;
	if (len == 0) { /* ignore empty components, but preserve trailing /'s. */
            if (*pEnd == '\0')
                *dest++ = '/';
        }
	else if (len == 1 && *pStart == '.' && pStart[1] == '/')
	    {}   /* ignore single dots */
	else if (len == 2 && pStart[0] == '.' && pStart[1] == '.' && pStart[2] == '/') {
	    switch (dest - path) {
#if 0
/* this old version did not preserve /../  */
	    case 0: /*  /.. => /   and   .. => .. */
		    if (*path != '/')  
			*dest++ = '.', *dest++ = '.';
		    break;
#else
	    case 0: /*  /.. => /..   and   .. => .. */
		    if (*path == '/')  *dest++ = '/';
		    *dest++ = '.', *dest++ = '.';
		    break;
#endif
	    case 1: /*  ./.. => ..   and   x/.. => . */
		    if (*path == '.') 
			*dest++ = '.';
		    else *path = '.';
		    break;
	    case 2: /*  ../.. => ../..   and   x/.. => .  */
		    if (strncmp(path, "..", 2)==0)
			*dest++ = '/', *dest++ = '.', *dest++ = '.';
		    else *path = '.', dest = path+1;
		    break;
	    default: /*  y/x/..=>y   x/..=>.  /x/..=>/   ../../..=>../../..  */
		    if (strncmp(dest-3, "/..", 3)==0)
			*dest++ = '/', *dest++ = '.', *dest++ = '.';
		    else {  /* this is the principal case for .. */
			while (dest > path && *--dest != '/') {}
			if (dest == path && *path != '/')
			    *dest++ = '.';
		    }
		    break;
	    }
	}
	else {
	    if (dest>path || *path == '/') 
		*dest++ = '/';
	    strncpy (dest, pStart, len);
	    dest += len;
	}
	if (*pEnd++ == 0)
	    break;
    }
    if (dest==path)   /* inital path was  /  .  or empty */
	*dest++ = (*path == '/' ? '/' : '.');
    *dest++ = 0;
}
#endif

static long SetNewHome(shortPathName, name, cell, dir, dirlen)
char *shortPathName;
char *name;
char *cell;
char *dir;
long dirlen;
{
    struct homestruct *newHome;
    long addedLen = 1;

    newHome = (struct homestruct *) malloc(sizeof(struct homestruct));
    newHome->next = homes;
    homes = newHome;
    strcpy(newHome->fullPath, dir);
    newHome->fullLength = dirlen;

    strcpy(shortPathName, "~");
    if (name != NULL) {
	strcat(shortPathName, name);
	addedLen += strlen(name);
    }
#ifdef AFS_ENV
    if (cell != NULL) {
	strcat(shortPathName, "@");
	strcat(shortPathName, cell);
	addedLen += strlen(cell) + 1;
    }
#endif /* AFS_ENV */
    strcpy(newHome->shortPath, shortPathName);
    newHome->shortLength = strlen(shortPathName);

    return addedLen;
}

/*
 * Truncates a path so that the end is visible.
 * (Initial code stolen from frame.c).
 *
 * If result is NULL the returned value must be freed.
 * 
 * Also, call FreeTruncatedPaths() to free up the cached entries.
 */

char *path__TruncatePath(c, frompath, result, limit, tryHome)
    struct classheader *c;
    char *frompath;
    char *result;
    long limit;
    boolean tryHome;
{
    char shorter[MAXPATHLEN];
    char foldedpath[MAXPATHLEN];
    char *path;
    int len, maxLen;
    static long lastUID = -1;
    static char passwdName[100];
    static char passwdDir[MAXPATHLEN];
    static int passwdDirLen;
    static char cellPasswdName[100];
    static char cellPasswdDir[MAXPATHLEN];
    static int cellPasswdDirLen;
    static boolean gotBaseInfo = FALSE;
    static boolean CheckOwnerHome = FALSE;
#ifdef AFS_ENV
    static char lastCellName[MAXPATHLEN];
    static char thisCell[MAXPATHLEN];
#endif /* AFS_ENV */
    
    if (frompath == NULL) {
        return NULL;
    }

    if (! gotBaseInfo) {
	char *homeDir = environ_GetHome(NULL);
	char shortName[10];

	if (homeDir != NULL) {
	    SetNewHome(shortName, NULL, NULL, homeDir, strlen(homeDir));
	}
#ifdef AFS_ENV
	thisCell[0] = '\0';
	GetCurrentWSCell(thisCell, MAXPATHLEN);
#endif /* AFS_ENV */
	CheckOwnerHome = environ_GetProfileSwitch("CheckOwnerHome", TRUE);
	gotBaseInfo = TRUE;
    }

    strcpy(foldedpath, frompath);
    FoldName(foldedpath);
    path = foldedpath;

    maxLen = limit;
    shorter[0] = '\0';

    if (tryHome && path[0] == '/' && path[1] != '\0') {
        char tmpPath[MAXPATHLEN];
        struct passwd *passwd;
        struct stat buf;
        char *slash;
        struct homestruct *aHome;
        boolean hitCache = FALSE;
#ifdef AFS_ENV
        boolean gotCell;
        char cellName[MAXPATHLEN];
#endif /* AFS_ENV */


        for (aHome = homes; aHome != NULL; aHome = aHome->next) {
            if (strncmp(aHome->fullPath, path, aHome->fullLength) == 0 && (path[aHome->fullLength] == '/' || path[aHome->fullLength] == '\0')) {
                strcpy(shorter, aHome->shortPath);
                maxLen -= aHome->shortLength;
                path += aHome->fullLength;
                hitCache = TRUE;
                break;
            }
        }

        if (!hitCache) {

	    strcpy(tmpPath, path);

#ifdef AFS_ENV
            gotCell = !GetCellFromFileName(tmpPath, cellName, MAXPATHLEN);
#endif /* AFS_ENV */

            len = strlen(tmpPath);
            slash = &tmpPath[len];
            while (len > 1 && tmpPath[len-1] == '/') {
                slash--;
                len--;
            }

	    if (len > 0) {
		/* Have more than the root directory */
		*slash = '\0';
		if (CheckOwnerHome && stat(tmpPath, &buf) == 0) {
		    if (buf.st_uid != lastUID
#ifdef AFS_ENV
			|| strcmp(cellName, lastCellName) != 0
#endif /* AFS_ENV */
			) {
			passwd = getpwuid(buf.st_uid);
			if (passwd != NULL) {
			    strcpy(passwdDir, passwd->pw_dir);
			    strcpy(passwdName, passwd->pw_name);
			    passwdDirLen = strlen(passwdDir);
			}
			else {
			    passwdName[0] = '\0';
			}
#ifdef AFS_ENV
			if (gotCell) {
			    struct passwd *cellpasswd;

			    cellpasswd = getcpwuid(buf.st_uid, cellName);
			    if (cellpasswd != NULL) {
				strcpy(cellPasswdDir, cellpasswd->pw_dir);
				strcpy(cellPasswdName, cellpasswd->pw_name);
				cellPasswdDirLen = strlen(cellPasswdDir);
			    }
			    else {
				cellPasswdName[0] = '\0';
			    }
			}
			strcpy(lastCellName, cellName);
#endif /* AFS_ENV */
			lastUID = buf.st_uid;
		    }

		    if (passwdName[0]!= '\0' && strncmp(tmpPath, passwdDir, passwdDirLen) == 0 && (tmpPath[passwdDirLen] == '/' || tmpPath[passwdDirLen] == '\0')) {
			maxLen -= SetNewHome(shorter, passwdName, NULL, passwdDir, passwdDirLen);
			path += passwdDirLen;
		    }
#ifdef AFS_ENV
		    else if (cellPasswdName[0] != '\0' && strncmp(tmpPath, cellPasswdDir, cellPasswdDirLen) == 0 && (tmpPath[cellPasswdDirLen] == '/' || tmpPath[cellPasswdDirLen] == '\0')) {
			maxLen -= SetNewHome(shorter, cellPasswdName, cellName, cellPasswdDir, cellPasswdDirLen);
			path += cellPasswdDirLen;
		    }
#endif /* AFS_ENV */
		}
            }
        }
    }

    /* put the result together */

    len = strlen(path);
    if (len > maxLen) {
        char *partialName;

	if (result == NULL) {
	    result = (char *) malloc(limit + 1);
	    if (result == NULL) return NULL;
	}

        maxLen -= sizeof("---") - 1;
        partialName = index(path + (len - maxLen), '/');
        if (partialName == NULL) {
            partialName = path + (len - maxLen);
        }
        else {
            ++partialName; /* Skip slash... */
        }
        strcpy(result, "---");
        strcat(result, partialName);
    }
    else {
	if (result == NULL) {
	    result = (char *) malloc(strlen(shorter) + len + 1);
	    if (result == NULL) return NULL;
	}
        strcpy(result, shorter);
        strcat(result, path);
    }

    return result;
} /* path__TruncatePath */

void path__FreeTruncatedPaths(c)
    struct classheader *c;
{
    struct homestruct *home;
    struct homestruct *nexthome;

    home = homes;
    while (home != NULL) {
        nexthome = home->next;
        free(home);
        home = nexthome;
    }
    homes = NULL;
} /* path__FreeTruncatedPaths */

boolean path__ModifyToParentDirectory(c, path, isDirectory)
    struct classheader *c;
    char *path;
    boolean isDirectory;
{
    long len = strlen(path);

    if (isDirectory) {
	while (len > 1 && path[len-1] == '/') {
	    len--;
	}
    }

    while (len > 0 && path[len-1] != '/') {
	len--;
    }

    path[len] = '\0';

    return (len > 0);
} /* path__ModifyToParentDirectory */

static void *HandleCellTwiddle(fromString, toString)
char *fromString;
char *toString;
{
    char *home=NULL;
    struct passwd *passwd;
    long p;

    if (fromString[1] == '\0' || fromString[1] == '/') {
	p = 1;
	home = environ_Get("HOME");
	if (home == NULL) {
	    /* Current user */
	    passwd = getpwuid(getuid());
	    if (passwd != NULL) {
		home = passwd->pw_dir;
	    }
	    if (home == NULL) {
		p = 0;
	    }
	}
    }
    else {
	long pos;
	char name[MAXPATHLEN];
	long endpos;
	char cellName[MAXPATHLEN];
	char *cn = cellName;

	for (pos = 1; fromString[pos] != '\0' && fromString[pos] != '/'; pos++) {
#ifdef AFS_ENV
	    if (fromString[pos] == '@') {

		for (endpos = pos + 1; fromString[endpos] != '\0' && fromString[endpos] != '/'; endpos++) {
		    *cn++ = fromString[endpos];
		}
		*cn = '\0';
		break;
	    }
#endif /* AFS_ENV */
	}

	strncpy(name, &fromString[1], pos - 1);
	name[pos -1] = '\0';
	if (cn == cellName) {
	    passwd = getpwnam(name);
#ifdef AFS_ENV
	    if (passwd == NULL) {
		passwd = getvpwnam(name);
	    }
#endif /* AFS_ENV */
	    p = pos;
	}
	else {
#ifdef AFS_ENV
	    passwd = getcpwnam(name, cellName);
	    p = endpos;
#endif /*AFS_ENV */
	}

	if (passwd != NULL) {
	    home = passwd->pw_dir;
	}
    }
       if (home != NULL) {
 	strcpy(toString, home);
 	strcat(toString, &fromString[p]);
     }
     else {
 	strcpy(toString, fromString);
     }
}

static void HandleRelativeFileName(fromString, toString, basefile)
char *fromString;
char *toString;
char *basefile;
{
    register char *slash;

    if (basefile == NULL) basefile = "";

    if (*basefile != '/' && im_GetDirectory(toString) != NULL)
	strcat (toString, "/");
    else
	toString[0] = '\0';    /* ??? discard error message if getwd==NULL */
 
    strcat (toString, basefile);
    slash = rindex(toString, '/');
    if (slash==NULL) {
	slash = &toString[strlen(toString)];
	*slash = '/';
    }
    *++slash = '\0';
    strcat(toString, fromString);
}

/*
    Returns either fromString or toString depending on
    if the file name needs to be unfolded.
*/

char *path__UnfoldFileName(c, fromString, toString, basefile) 
struct classheader *c;
char *fromString;
char *toString;
char *basefile;
{
    char *fs = fromString;
    char tempstr[2*MAXPATHLEN+1];
    char envname[MAXPATHLEN+1];
    char *tx, *dx, *ex, *dollar, *envval;
    char endch;

    while (isspace(*fs))
        fs++;

    /* expand $XXX, ${XXX}, or $(XXX) from the environment */
	/* obsolete RESTRICTION: If XXX is an environment variable, there is no way 
	to refer to a file or directory named $XXX. */
    /* now $$ will expand to $ and suppress expansion of
     any environment var following... */

    /* copy string to tempstr */
    tx = tempstr;
    while (TRUE) {
	char *dollar = (char *)index(fs, '$');
	if (dollar == NULL) break;
	dx = dollar+1;
	if(*dx=='$') {
	    strncpy(tx, fs, dx-fs);
	    tx+=dx-fs;
	    *tx='\0';
	    fs=dx+1;
	    continue;
	}
	if (*dx == '{')  {dx++;  endch = '}';}
	else if (*dx == '(') {dx++;   endch = ')';}
	else endch = '\0';
	ex = envname;
	while (isalnum(*dx)) {*ex++ = *dx++;}
	*ex = '\0';
	if (*dx && *dx == endch) dx++;

	/* get the value of the environment variable */
	if(strcmp(envname, "ANDREWDIR")!=0) envval = environ_Get(envname);
	else envval=environ_AndrewDir("");

	if (envval == NULL) {
		/* leave $xxx in place */
		strncpy(tx, fs, dx-fs);
		tx += dx-fs;
	}
	else {
		strncpy(tx, fs, dollar - fs);  /* copy up to $ */
		tx += dollar-fs;
		if (*envval != '\0')
			tx = strappend(tx, envval);  /* copy envval */
	}
	*tx = '\0';
	fs = dx;
    }
    /* if(*fs) isn't needed since we want to ensure the string is NUL terminated anyway... */
    strcpy(tx, fs);
	
    fs = tempstr;
    if (*fs == '/') {
	strcpy(toString, fs);
    }
    else if (*fs == '~') {
	HandleCellTwiddle(fs, toString);
    }
    else {
	HandleRelativeFileName(fs, toString, basefile);
    }

    FoldName(toString);
	    
    return toString;
} /* path__UnfoldFileName */

void path__ReleaseFiles(self, files)
    struct path *self;
    char **files;
{
} /* path__ReleaseFiles */

void path__ReleaseDirs(self, dirs)
    struct path *self;
    char **dirs;
{
} /* path__ReleaseDirs */

void path__FinalizeObject(c, self)
    struct classheader *c;
    struct path *self;
{
    if (self->filepath != NULL) {
        free(self->filepath);
    }
    if (self->truncatedPath != NULL) {
        free(self->truncatedPath);
    }

    FreeFilesAndDirs(self);
    path_NotifyObservers(self, observable_OBJECTDESTROYED);

    return;
} /* path__FinalizeObject */

int CompareFileNames(a, b)
    char **a;
    char **b;
{
    /* this puts .files before all others */

    if (**a == '.') {
        return (**b == '.') ? strcmp(*a, *b) : -1;
    }
    else {
        return (**b == '.') ? 1 : strcmp(*a, *b);
    }
} /* CompareFileNames */

boolean path__Scan(self, statEverything)
    struct path *self;
    boolean statEverything;
{
    char dirbuf[MAXPATHLEN];
    char fullName[MAXPATHLEN];
    char *filePart;
    struct stat statBuf;
    DIR *thisDir;
    DIRENT_TYPE *dirEntry;
    long filesalloced;
    long dirsalloced;
    long nextfile;
    long nextdir;
    long len;
    boolean noProblems = FALSE;
#ifdef AFS_ENV
    boolean inVICE = FALSE;
    boolean statIsSillyMakeThemDirs = FALSE;
    boolean statIsSillyMakeThemFiles = FALSE;
#endif /* AFS_ENV */

    if (self->filepath == NULL || (stat(self->filepath, &statBuf) != 0)) {
        FreeFilesAndDirs(self);
    }
    else if ((statBuf.st_ctime > self->changetime) || (self->mayBeWrong && statEverything)) {
        time_t changetime;

        /* We're going to (re)scan the path */
        FreeFilesAndDirs(self);
        changetime = statBuf.st_ctime;
        /*
          Since it's not impossible for isDir to be wrong,
              it's safer to recompute it.
          */
        self->isDir = (statBuf.st_mode & S_IFMT) == S_IFDIR;
        self->knowIsDir = TRUE;
        if (!self->isDir) {
            self->mayBeWrong = FALSE;
            noProblems = TRUE;
        }
        else {
            self->mayBeWrong = !statEverything;
            strcpy(dirbuf, self->filepath);

            if ((thisDir = opendir(dirbuf)) != NULL) {
                noProblems = TRUE;
                self->haveScanned = TRUE;

                strcpy(fullName, dirbuf);
                len = strlen(fullName);
                if (len == 0 || fullName[len - 1] != '/') {
                    fullName[len] = '/';
                    len++;
                }

#ifdef AFS_ENV
                inVICE = IsOnVice(thisDir->dd_fd);
                statIsSillyMakeThemDirs = 
                  (strcmp(fullName, "/usr/") == 0) || 
                  (strcmp(fullName, "/afs/") == 0);
                statIsSillyMakeThemFiles = strcmp(fullName, "/etc/") == 0;
#endif /* AFS_ENV  */

                filePart = &fullName[len];
                while ((dirEntry = readdir(thisDir)) != NULL) {
                    boolean isdirectory = FALSE;
                    char *name = dirEntry->d_name;

#ifdef AFS_ENV /* Use the wonderous VICE hack ... */
                    if (statIsSillyMakeThemDirs) {
                        isdirectory = TRUE;
                    }
                    else if (statIsSillyMakeThemFiles) {
                        isdirectory = FALSE;
                    }
                    else if (inVICE && !statEverything) {
                        if ((dirEntry->d_ino % 2) == 1) {
                            isdirectory = TRUE;
                        }
                    }
                    else
#endif /* AFS_ENV */
                    {
                        strcpy(filePart, name);
                        stat(fullName, &statBuf);
                        if ((statBuf.st_mode & S_IFMT) == S_IFDIR) {
                            isdirectory = TRUE;
                        }
                    }
                    if (isdirectory) {
                        if (strcmp(name, ".") != 0 && strcmp(name, "..") != 0) {
                            if (self->dirs == NULL) {
                                self->dirs = (char **) malloc(INITIALCHUNK * sizeof(char *));
                                dirsalloced = INITIALCHUNK;
                                nextdir = 0;
                            }
                            else if (nextdir >= dirsalloced) {
                                dirsalloced *= 2;
                                self->dirs = (char **) realloc(self->dirs, dirsalloced * sizeof(char *));
                            }
                            self->dirs[nextdir] = (char *) malloc(strlen(name) + 1);
                            strcpy(self->dirs[nextdir], name);
                            nextdir++;
                        }
                    }
                    else {
                        if (self->files == NULL) {
                            self->files = (char **) malloc(INITIALCHUNK * sizeof(char *));
                            filesalloced = INITIALCHUNK;
                            nextfile = 0;
                        }
                        else if (nextfile >= filesalloced) {
                            filesalloced *= 2;
                            self->files = (char **) realloc(self->files, filesalloced * sizeof(char *));
                        }
                        self->files[nextfile] = (char *) malloc(strlen(name) + 1);
                        strcpy(self->files[nextfile], name);
                        nextfile++;
                    }
                }

                closedir(thisDir);
                self->numDirs = nextdir;
                self->numFiles = nextfile;

                /* null terminate both lists and sort them */
                if (self->dirs != NULL) {
                    if (nextdir >= dirsalloced) {
                        dirsalloced++;
                        self->dirs = (char **) realloc(self->dirs, dirsalloced * sizeof(char *));
                    }
                    self->dirs[nextdir] = 0;
                    qsort(self->dirs, nextdir, sizeof(char *), CompareFileNames);
                }

                if (self->files != NULL) {
                    if (nextfile >= filesalloced) {
                        filesalloced++;
                        self->files = (char **) realloc(self->files, filesalloced * sizeof(char *));
                    }
                    self->files[nextfile] = 0;
                    qsort(self->files, nextfile, sizeof(char *), CompareFileNames);
                }
            }
        }
        path_NotifyObservers(self, observable_OBJECTCHANGED);

        if (noProblems) {
            self->changetime = changetime;
        }
    }
    else {
        noProblems = TRUE;
    }

    return noProblems;
} /* path__Scan */

void path__Input(self, fp)
struct path *self;
FILE *fp;
{
    char path[MAXPATHLEN];
    long len;
    boolean knowIsDir;
    boolean isDir;

    fgets(path, MAXPATHLEN, fp);
    if (strncmp(path, "no", 2) != 0) {
        knowIsDir = TRUE;
        sscanf(path, "is directory: %d\n", &isDir);
    }
    else {
        knowIsDir = FALSE;
    }

    fgets(path, MAXPATHLEN, fp);
    if (strncmp(path, "no", 2) != 0) {
        len = strlen(path);
        if (path[len-1] == '\n') {
            path[len-1] = '\0';
        }
        SetPath(self, &path[strlen("path: ")]);
    }

    /* SetPath alters knowIsDir */
    self->knowIsDir = knowIsDir;
    self->isDir = isDir;

    fgets(path, MAXPATHLEN, fp);
    if (strncmp(path, "no", 2) != 0) {
        long tlen;

        len = strlen(path);
        if (path[len-1] == '\n') {
            path[len-1] = '\0';
        }
        tlen = strlen("truncated path: ");
        self->truncatedPath = (char *) malloc(len + 1 - tlen);
        strcpy(self->truncatedPath, &path[tlen]);
    }
    else {
        self->truncatedPath = NULL;
    }

} /* path__Input */

void path__Output(self, fp)
struct path *self;
FILE *fp;
{
    if (self->knowIsDir) {
        fprintf(fp, "is directory: %d\n", self->isDir);
    }
    else {
        fprintf(fp, "no directory information\n");
    }

    if (self->filepath != NULL) {
        fprintf(fp, "path: %s\n", self->filepath);
    }
    else {
        fprintf(fp, "no path\n");
    }

    if (self->truncatedPath != NULL) {
        fprintf(fp, "truncated path: %s\n", self->truncatedPath);
    }
    else {
        fprintf(fp, "no truncated path\n");
    }

} /* path__Output */

boolean path__IsDirectory(self)
struct path *self;
{
    if (!self->knowIsDir) {
        struct stat statBuf;

        self->knowIsDir = TRUE;
        self->isDir = FALSE;
        if (stat(self->filepath, &statBuf) == 0 &&
             (statBuf.st_mode & S_IFMT) == S_IFDIR) {
            self->isDir = TRUE;
        }
    }
    return self->isDir;
} /* path__IsDirectory */

char **path__GetFiles(self)
struct path *self;
{
    if (!self->haveScanned) {
        path_Scan(self, FALSE);
    }
    return self->files;
} /* path__GetFiles */

char **path__GetDirs(self)
struct path *self;
{
    if (!self->haveScanned) {
        path_Scan(self, FALSE);
    }
    return self->dirs;
} /* path__GetDirs */

long path__GetNumFiles(self)
struct path *self;
{
    if (!self->haveScanned) {
        path_Scan(self, FALSE);
    }
    return self->numFiles;
} /* path__GetNumFiles */

long path__GetNumDirs(self)
struct path *self;
{
    if (!self->haveScanned) {
        path_Scan(self, FALSE);
    }
    return self->numDirs;
} /* path__GetNumDirs */

char *path__GetTruncatedPath(self)
struct path *self;
{
    if (self->truncatedPath == NULL && self->filepath != NULL) {
        self->truncatedPath = path_TruncatePath(self->filepath, NULL, MAXPATHLEN, TRUE);
    }
    return self->truncatedPath;
} /* path__GetTruncatedPath */
