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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/inst/RCS/inst.c,v 1.13 1993/12/21 19:54:17 gk5g Exp $";
#endif

/* ALSO utimes and strip the file

Generic install command.  Options are:
	-s 			strip the file	(default for executable files with no extension)
	-ns			do not strip the file	(default for other files)
	-c			ignored for compatability
	-m <mode>	chmod to this value
	-o <user>		chown to this user
	-g <group>	chgrp to this group
*/
#include <system.h>
#define MAXFILES 200
#define BUFSIZE 32768
#ifndef MAXPATHLEN
#include <sys/param.h>
#endif /* MAXPATHLEN */
#include <fcntl.h>
#include <sys/time.h>
#include <pwd.h>
#include <errno.h>
#include <sys/stat.h>
#if defined(mips) && ! defined(sgi)
#include <sys/exec.h>
#define	N_BADMAG(x) \
    (((x).magic)!=OMAGIC && ((x).magic)!=NMAGIC && ((x).magic)!=ZMAGIC)
#else
#ifndef NeXT
#ifdef SOLARIS
#include <sys/exechdr.h>

#define	PAGSIZ		0x02000
#define	OLD_PAGSIZ	0x00800

#define	N_BADMAG(x) \
    (((x).a_magic)!=OMAGIC && ((x).a_magic)!=NMAGIC && ((x).a_magic)!=ZMAGIC)

#define	N_PAGSIZ(x) \
    ((x).a_machtype == M_OLDSUN2? OLD_PAGSIZ : PAGSIZ)

#define	N_TXTOFF(x) \
    /* text segment */ \
    ((x).a_machtype == M_OLDSUN2 \
      ? ((x).a_magic==ZMAGIC ? N_PAGSIZ(x) : sizeof (struct exec)) \
      : ((x).a_magic==ZMAGIC ? 0 : sizeof (struct exec)) )
#else
#include <a.out.h>
#endif /* SOLARIS */
#endif /* NeXT */
#endif /* mips */

struct stat istat, ostat;

extern int errno;
extern int sys_nerr;
extern char *sys_errlist[];

char *ErrorString(aerrno)
    int aerrno; {
    static char tbuffer[100];
    if (aerrno < 0 || aerrno >= sys_nerr) {
	sprintf(tbuffer, "undefined error code %d", aerrno);
	return tbuffer;
    }
    return sys_errlist[aerrno];
}

stripName(aname)
    char *aname;
    {if ((char*) strrchr(aname, '.') == NULL) return 1;
    else return 0;
    }

atoo(astr)
    register char *astr;
    {register long value;
    register char tc;
    value = 0;
    while (tc = *astr++)
	{value <<= 3;
	value += tc-'0';
	}
    return value;
    }

static int quickStrip (afd, asize, fileName)
    int afd;
    long asize;
    char *fileName;
    {int n, bytesLeft;
#if defined(ZMAGIC) && ! defined(sgi)
    struct exec buf;
    struct exec *head;

    n = lseek(afd, 0, 0);
    if (n < 0) {printf("Initial lseek failed while stripping file: %s\n", ErrorString(errno)); return -1;}
    n = read(afd, &buf, sizeof(buf));
    if (n < 0) {printf("Initial read failed while stripping: %s\n", ErrorString(errno)); return -1;}
    head = &buf;
#if defined(mips) && ! defined(sgi)
    if (n >= sizeof(*head) && !N_BADMAG(head->ex_o))
#else
    if (n >= sizeof(*head) && !N_BADMAG(*head))
#endif /* mips */
	{/* This code lifted from strip.c. */
	bytesLeft = (long) head->a_text + head->a_data;
#if ! defined(mips) || defined(sgi)
	head->a_syms = head->a_trsize = head->a_drsize = 0;
#endif /* mips */
#if defined(linux)
	if ((head->a_info & 0xffff) == ZMAGIC)
#else
	if (head->a_magic == ZMAGIC)
#endif
#if defined(mips) && ! defined(sgi)
	    bytesLeft += N_TXTOFF(head->ex_f,head->ex_o) - sizeof(*head);
#else
	    bytesLeft += N_TXTOFF(*head) - sizeof(*head);
#endif /* mips */
	/* also include size of header */
	bytesLeft += sizeof(*head);
	n = lseek(afd, 0, 0);
	if (n < 0) {printf("lseek failed while stripping file: %s\n", ErrorString(errno)); return -1;}
	n = write(afd, &buf, sizeof(buf));
	if (n < 0) {printf("write failed while stripping file: %s\n", ErrorString(errno)); return -1;}
	}
    else
	bytesLeft = 0;

     /* check if size of stripped file is same as existing file */
     if (bytesLeft != 0 && bytesLeft != asize)
	{if (ftruncate(afd, bytesLeft) < 0)
	    {printf("ftruncate failed after stripping file: %s\n", ErrorString(errno)); return -1;}
	}
#else /* ZMAGIC */
     {
	 char stripCommand[MAXPATHLEN + 7];
	 sprintf(stripCommand, "strip %s", fileName);
	 if (system(stripCommand) != 0)
	     return -1;
     }
#endif /* ZMAGIC */    
    return 0;
    }

main (argc, argv)
    int argc;
    char **argv;
    {int setOwner, setMode, setGroup, ifd, ofd;
    long mode, owner, group;
    struct passwd *tpw;
    char *fnames[MAXFILES], *newNames[MAXFILES];
    long rcode, code, newcode;
    char *dname;
    char pname[1024];
    char pnametmp[1024];
    char FirstChar;
    char IsFirst;
    int pnamelen;
    static char diskBuffer[BUFSIZE];	/* must be static to avoid compiler bugs for large stuff */
    char myHostName[100];
    struct timeval tvp[2];
    int isDir;
    int strip;
    int fptr;
    register char *tp;
    register long i;

    fptr = 0;
    rcode = 0;
    strip = -1;	/* don't know yet */
    owner = 0;
    setOwner = 0;
    setMode = 0;
    group = 0;
    setGroup = 0;

    for(i=1; i<argc; i++)
	{tp = argv[i];
	if (tp[0] == '-')
	    {/* a switch */
	    if (!strcmp(tp, "-m")) mode = atoo(argv[++i]), setMode=1;
	    else if (!strcmp(tp, "-s")) strip = 1;
	    else if (!strcmp(tp, "-ns")) strip = 0;
	    else if (!strcmp(tp, "-c")) /* nothing */;
	    else if (!strcmp(tp, "-o"))
		{/* look up the dude */
		tpw = getpwnam(argv[++i]);
		if (!tpw)
		    {printf("User %s not found in passwd database, ignored\n", argv[i]);
		    }
		else
		    {owner = tpw->pw_uid;
		    setOwner =1;
		    }
		}
	    else if (!strcmp(tp, "-g"))
		{/* look up the dude */
		tpw = getpwnam(argv[++i]);
		if (!tpw)
		    {printf("Group %s not found in passwd database, ignored\n", argv[i]);
		    }
		else
		    {group = tpw->pw_gid;
		    setGroup =1;
		    }
		}
		else printf("Bad switch %s\n", argv[i]);
	    }
	else
	    {/* a file name */
	    if (fptr >= MAXFILES)
		{printf("Too many files on command line, max is %d\n", MAXFILES);
		exit(1);
		}
	    fnames[fptr++] = argv[i];
	    }
	}

    /* we've parse the commands, now *do* them */

    /* otherwise we are doing a local install, so we do the work for each file here */
    /* the last name in the fname array is the dir in which to put all this stuff */

    if (fptr < 2)
	{printf("Not enough file names\n");
	exit(1);
	}

    dname = fnames[--fptr];
    if (stat(dname, &istat) < 0) {
	if (errno == ENOENT)
	    isDir = 0;		/* creating a new file */
	else {
	    printf("Can't stat destination ``%s'': %s\n", dname, ErrorString(errno));
	    exit(1);
	}
    } else {
	if ((istat.st_mode & S_IFMT) == S_IFDIR)  isDir = 1;
	else isDir = 0;
    }

    /* either can be n files and one dir, or one file and one target file */
    if (!isDir && fptr != 1) return printf("target for multiple files must be a dir\n");

    for (i=0;i<fptr;i++)
	{/* figure out name to put as entry name for file */
	tp = (char *) strrchr(fnames[i], '/');
	if (tp) newNames[i] = tp+1;
	else newNames[i] = fnames[i];
	}
    for (i=0;i<fptr;i++)
	{/* copy newName[i] into directory dname */

	/* pname is target file in either case */
	if (isDir)
	    {strcpy(pname, dname);
	    strcat(pname, "/");
	    strcat(pname, newNames[i]);
	    }
	else strcpy(pname, dname);
	strcpy(pnametmp, pname);
	/* Make up a temporary name for a destination */
	pnamelen = strlen(pnametmp);
	gethostname(myHostName, sizeof(myHostName)-1);	/* lv room for null */
	if (pnamelen > 1020 - strlen(myHostName)) pnamelen = 1020 - strlen(myHostName);
	pnametmp[pnamelen] = '.';
	strcpy(&pnametmp[pnamelen+1], myHostName);
	if (strcmp(fnames[i], pnametmp) == 0) strcpy(&pnametmp[pnamelen], ".NeW");

	ifd = open(fnames[i], O_RDONLY, 0);
	if (ifd < 0)
	    {printf("Can't open source file ``%s'': %s\n", fnames[i], ErrorString(errno));
	    rcode = 1;
	    continue;
	    }
	if (fstat (ifd, &istat) < 0) {
	    printf("Cound not fstat input file ``%s'': %s; skipping it\n", fnames[i], ErrorString(errno));
	    close(ifd);
	    rcode = 1;
	    continue;
	}
	/* check to see if this file is hard to duplicate */
	ofd = open(pnametmp, O_RDWR | O_TRUNC | O_CREAT, 0666);
	if (ofd < 0)
	    {printf("Could not create output temp file ``%s'': %s\n", pnametmp, ErrorString(errno));
	    close(ifd);
	    rcode = 1;
	    continue;
	    }
	if (!setMode) mode = 0755;	/* this is the default for our rcs to work */
	/* here both files are open and ready to go */
	IsFirst = 1;
	FirstChar = '#';
	while (1)
	    {code = read(ifd, diskBuffer, BUFSIZE);
	    if (code == 0) break;
	    if (code < 0)
		{printf("READ ERROR %d: %s\n", errno, ErrorString(errno));
		break;
		}
	    if (IsFirst) {
		FirstChar = diskBuffer[0];
		IsFirst = 0;
	    }
	    errno = 0;
	    newcode = write(ofd, diskBuffer, code);
	    if (newcode != code)
		{printf("WRITE ERROR %d: %s\n", errno, ErrorString(errno));
		break;
		}
	    }
	if (code != 0)
	    {rcode = 1;	/* an error occurred copying the file */
	     printf("Warning: Error occurred writing output temp file %s; skipping it\n",
			 pnametmp);
	    close(ifd); unlink(pnametmp); close(ofd);
	    continue;	/* to the next file */
	    }
	/* strip the file? */
	if (strip == 1 ||
	    (strip == -1 && ((istat.st_mode & 0111) == 0111) && stripName(newNames[i]) && FirstChar != '#'))
		if (quickStrip(ofd,istat.st_size, pnametmp) < 0) {
		    printf("...strip failed for output temp file ``%s''; skipping it\n", pnametmp);
		    close(ifd); unlink(pnametmp); close(ofd);
		    rcode = 1;
		    continue;
		}

	/* do the chmod, etc calls before closing the file for max parallelism on store behind */
	close(ifd);

#ifdef ultrix
	/* We do this here to prevent gfs/afs from preventing us to set the
	   timestamp on the file after a setuid call */
	tvp[0].tv_sec = istat.st_atime;
	tvp[0].tv_usec = 0;
	tvp[1].tv_sec = istat.st_mtime;
	tvp[1].tv_usec = 0;
	if (utimes(pnametmp, tvp) < 0)
	    {printf("Couldn't utimes output temp file ``%s'': %s\n",
			pnametmp, ErrorString(errno));
	    unlink(pnametmp); close(ofd);
	    rcode = 1;
	    continue;
	    }
#endif /* ultrix */

	if (fchmod(ofd, mode) < 0)
	    {printf("Couldn't chmod output temp file ``%s'': %s\n",
			pnametmp, ErrorString(errno));
	    unlink(pnametmp); close(ofd);
	    rcode = 1;
	    continue;
	    }

#if !defined(hpux) && !defined(ultrix)
	tvp[0].tv_sec = istat.st_atime;
	tvp[0].tv_usec = 0;
	tvp[1].tv_sec = istat.st_mtime;
	tvp[1].tv_usec = 0;
	if (utimes(pnametmp, tvp) < 0)
	    {printf("Couldn't utimes output temp file ``%s'': %s\n",
			pnametmp, ErrorString(errno));
	    unlink(pnametmp); close(ofd);
	    rcode = 1;
	    continue;
	    }
#endif /* !hpux && !ultrix */

	code = close(ofd);
	if (code != 0)
	    {printf("Warning: Could not close output temp file %s (%s)\n",
			pnametmp, ErrorString(errno));
	    unlink(pnametmp);
	    rcode = 1;	/* an error occurred closing the output file */
	    continue;	/* to the next file */
	    }

	/* do this later so vice doesn't see chown of unstored file */
	if (setOwner || setGroup)
	    if (chown(pnametmp, (setOwner? owner : -1), (setGroup? group : -1)) < 0) {
		printf("Couldn't set %s for output temp file %s: %s\n",
			(setOwner? (setGroup? "owner and group" : "owner") : "group"),
			pnametmp, ErrorString(errno));
		unlink(pnametmp);
		rcode = 1;
		continue;
	    }
	if (rename(pnametmp, pname) < 0)
	    {printf("Couldn't rename temp file %s to be output file %s: %s\n",
			pnametmp, pname, ErrorString(errno));
	    unlink(pnametmp);
	    rcode = 1;
	    continue;
	    }
	}
    /* all done now */
    exit(rcode);
    }
