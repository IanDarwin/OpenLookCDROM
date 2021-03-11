
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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/class/cmd/RCS/doindex.c,v 1.28 1993/09/28 18:48:09 rr2b Exp $";
#endif

/* 
	doindex.c - make the class system index tables
 */

#include <andrewos.h> /* sys/types.h sys/file.h */
#include <stdio.h>
#ifndef NeXT
#ifndef sys_sun4_51
#include <a.out.h>
#endif /* sys_sun4_51 */
#endif
#include <setjmp.h>
#include <sys/signal.h>
#include <sys/errno.h>
#include <class.h>  /* this contains all the structs, etc. for the class system */
#include <doload.h>
#include <classind.h>


/*
 * some needed data structures.
 */

struct EntryStruct {
    char * Name;
    struct EntryStruct * Next;
    struct IndexEntry Ent;
};



/*
 * external symbols that have no include files
 */

extern int errno;


/*
 * global data
 */


static struct classheader unknownID = {NULL, "unknown", NULL, NULL};

struct EntryStruct * EntryList;
char * ProgramName;
int TimeOut;
int lockfd;


/*
 *
 * local routines
 *
 */


/**
 ** handle errors by printing the passed message to 
 ** stderr and exitting with a status of 1.
 **/
static void
Error(message, param)
char * message;
char * param;

{
    (void) fprintf(stderr, "%s: %s%s...program terminated!\n", ProgramName, message, param);
    (void) fflush(stderr);
    exit(1);
}


/**
 ** print out a usage message and exit.
 **/
static void
Usage()

{
    fprintf(stderr, "\n");
    fprintf(stderr, "%s -x -tseconds -ddirectory [files]\n", ProgramName);
    fprintf(stderr, "  -x     print this help message\n");
    fprintf(stderr, "  -t     maximum number of seconds to wait to lock index\n");
    fprintf(stderr, "  -d     directory in which to build index\n");
    fprintf(stderr, "  files  list of files to register in index\n");
    fprintf(stderr, "\n");
    Error("end of help message", "");
}


/**
 ** Add a file to process the list of entries.
 **/
static void
AddEntry(name)
char * name;

{
struct EntryStruct * ThisEntry;
struct EntryStruct * NewEntry;

    NewEntry = (struct EntryStruct *) malloc(sizeof(struct EntryStruct));
    if (NewEntry == NULL) {
	Error("can't allocate any more memory", "");
    }
    NewEntry->Name = (char *) malloc(strlen(name) + 1);
    if (NewEntry->Name == NULL) {
	free(NewEntry);
	Error("can't allocate any more memory", "");
    }
    strcpy(NewEntry->Name, name);
    NewEntry->Next = NULL;

    if (EntryList == NULL) {
	EntryList = NewEntry;
    } else {
	ThisEntry = EntryList;
	while (ThisEntry->Next != NULL) {
	    ThisEntry = ThisEntry->Next;
	}
	ThisEntry->Next = NewEntry;
    }
}


/**
 ** Delete an entry.
 **/
static void
DeleteEntry(entry)
struct EntryStruct * entry;

{
    free(entry->Name);
    free(entry);
}


/**
 ** Parse the passed arguments.
 ** 
 ** Try to be smart about this and allow the use to place a space between the 
 ** switch and it's argument.
 **/
static void
ParseArgs(argc, argv)
int argc;
char *argv[];

{
int i;

    TimeOut = 0;	    /* by default wait forever */
    ProgramName	= rindex(argv[0], '/');  /* set up program name for messages */
    if (ProgramName == NULL) {
	ProgramName = argv[0];	/* just use argv[0] */
    } else {
	ProgramName += 1;	/* jump over "/" */
    }
    EntryList =	NULL;	    /* clear this before starting */

    if (argc ==	1) {	    /* if empty command line print help */
	Usage();
    }

    for (i=1; i < argc; i++) {
	if (*argv[i] != '-') {
	    AddEntry(argv[i]);
	} else {
	    switch (*(argv[i]+1)) {
		case 'x':
		    Usage();
		    break;
		case 't':
		    if (*(argv[i]+2) != NULL) {
			(void) sscanf((argv[i]+2), "%d", &TimeOut);
		    } else {
			if ( (i+1) < argc) {
			    i += 1;
			    (void) sscanf(argv[i], "%d", &TimeOut);
			} else {
			    Error("no timeout value specified for -t switch", "");
			}
		    }
		    break;
		case 'd':
		    if (*(argv[i]+2) != NULL) {
			if (chdir(argv[i]+2) != 0) {
			    Error("can not change to directory ", (argv[i]+2));
			}
		    } else {
			if ( (i+1) < argc) {
			    i += 1;
			    if (chdir(argv[i]) != 0) {
				Error("can not change to directory ", (argv[i]+2));
			    }
			} else {
			    Error("no directory specified for -d switch", "");
			}
		    }
		    break;
		default:
		    Usage();
		    break;
	    }
	}
    }
}


/** 
 ** Update index file.
 **
 ** When we get here we must have exclusive access for
 ** the creation of the new index file.  This lock must
 ** be set by the caller.
 **/
int
UpdateAll()
{
FILE *infd;
FILE *outfd;
struct IndexEntry * ThisEntry;
register struct EntryStruct * TempEntry, *last;
int fid,loopcount;

#if sys_ps_aix12 || sys_ps_aix11
int lockfid;
/* BAAAAAD things happen on ps/2 aix if you try to
 lock an NFS file. Use the "grab a well-known file"
 hack instead. */
    loopcount = 0;
    do {
	lockfd = open(INDEXLOCKFILE, O_CREAT | O_EXCL, 022);
	if (lockfd < 0) {
	    if ((TimeOut != 0) && (loopcount > TimeOut)) { /* timeout of 0 means wait forever */
		Error("timed out while trying to lock index files", "");
	    }
	    sleep(1);	/* don't hog the CPU */
	    loopcount++;
	}
    } while (lockfd < 0);   /* keep going till we get a valid file descriptor */
#endif /* sys_ps_aix12 || sys_ps_aix11 */
RETRY:
    fid = open(INDEXNEWFILE,O_WRONLY | O_CREAT , 0644); /* Create output file */

    if(fid > 0) {
#if !sys_ps_aix12 && !sys_ps_aix11
	if(osi_ExclusiveLockNoBlock(fid) != 0){
	    if(errno == EWOULDBLOCK){
		fprintf(stdout, "%s: Waiting for locked index file...\n", ProgramName);
		fflush(stdout);
		loopcount = 0;
		do {/* wait if locked */
		    sleep(1);
		    if(TimeOut != 0 && loopcount++ > TimeOut) /* timeout of 0 means wait forever */
			Error("timed out while trying to lock index files", "");
		} while(osi_ExclusiveLockNoBlock(fid) != 0);
		close(fid);
		goto RETRY; /* reopen, as old file has probably been renamed by other process */
	    }
	    else {
		extern int sys_nerr,errno;
		extern char *sys_errlist[]; 
		if(errno > sys_nerr)
		    fprintf(stdout, "%s: Warning : file lock call failed, ignoring unknown error #%d.\n", ProgramName,errno);
		else 
		    fprintf(stdout, "%s: Warning : file lock call failed, ignoring '%s' error.\n", ProgramName,sys_errlist[errno]);
	    }
	}
#endif /* !sys_ps_aix12 && !sys_ps_aix11 */
	outfd = fdopen(fid, "w");
    }
    else {
	Error("Unable to open a new index file", "");
    }
    fprintf(stdout, "%s: Writing index file...",ProgramName);
    fflush(stdout);
    infd = fopen(INDEXFILE,"r");
    if (infd != NULL) {
	while ((ThisEntry = ReadEntry(infd)) != NULL) {
	    last = NULL;
	    for(TempEntry = EntryList; TempEntry; TempEntry = TempEntry->Next){
		if (strcmp(ThisEntry->Data,TempEntry->Ent.Data) == 0) {	/* match, replace it */
		    WriteEntry(outfd, &(TempEntry->Ent));	/* replace this entry */
		    if(last == NULL) EntryList = TempEntry->Next; /* take it out of the list */
		    else last->Next = TempEntry->Next;
		    break;
		}
		last = TempEntry;
	    }
	    if(TempEntry == NULL)
		WriteEntry(outfd, ThisEntry);	/* No match,  put the one we read back */
	}
    }
    for(TempEntry = EntryList; TempEntry; TempEntry = TempEntry->Next){
	WriteEntry(outfd, &(TempEntry->Ent)); /* write out new entrys */
    }
    fflush(outfd);
    if (fclose(outfd) == EOF) {
	Error("Error closing index file", "");
    }
    if (infd != NULL)
	(void) fclose(infd);    /* ignore return value */

    if (rename(INDEXNEWFILE, INDEXFILE) != 0) {
	Error("Error creating new index file", "");
    }

#if sys_ps_aix12 || sys_ps_aix11
    (void) close(lockfd);	/* done with this so close it */
    (void) unlink(INDEXLOCKFILE);	/* and delete it */
#endif /* if sys_ps_aix12 || sys_ps_aix11 */

    fprintf(stdout, "Done\n");
    fflush(stdout);

}


/**
 ** Process a particular entry.
 **
 ** When we get here we must have exclusive access for
 ** the creation of the new index file.  This lock must
 ** be set by the caller.
 **/
static void
ProcessEntry(entry)
struct EntryStruct * entry;

{
int objfd;
struct classinfo *info;	/* from the dynamic loaded code */
struct classinfo *(*entrypoint)();
char *base;
long textlength;
char tname[256];
long keylen;

    objfd = open(entry->Name, O_RDONLY, 0);
    if (objfd < 0) {
	Error("can't open file ", entry->Name);
    }

    fprintf(stdout, "%s: indexing %s ...", ProgramName, entry->Name);
    fflush(stdout);

#ifdef LIBDL_ENV
    /* For Solaris2 we need to add a "./" onto paths
     that are leaf nodes */
    /* The conditional compilation is just in case
     doing so would break other platforms. */
    if (strchr (entry->Name, '/') == NULL)
    {
	strcpy(tname, "./");
	strcat(tname, entry->Name);
    } else
	strcpy(tname,entry->Name);
#else /* LIBDL_ENV */
    strcpy(tname,entry->Name);
#endif /* LIBDL_ENV */

    entrypoint = ( struct classinfo * (*)() ) doload(objfd, entry->Name, &base, &textlength, tname);
    close(objfd);

    if (entrypoint == NULL){
	Error("NULL entrypoint from ", entry->Name);
    }

    /* call the loaded routine at its entry point to get the classinfo structure */
    keylen = strlen(tname);
    if (keylen > 3 && strcmp(&tname[keylen - 3], ".do") == 0) {
	tname[keylen - 3] = '\0';
    }

    unknownID.namekey = tname;

    info = (*entrypoint)(&unknownID, class_VERSIONNOTKNOWN);

    if(info == NULL){
	Error("can not obtain classinfo structure for ", entry->Name);
    }
    
    if((entry->Ent.Name = malloc(strlen(info->name) + 1)) == NULL)
	Error("Out of memory while processing ",entry->Name);
    strcpy(entry->Ent.Name ,info->name);
    if((entry->Ent.Key = malloc(strlen(info->namekey) + 1)) == NULL)
	Error("Out of memory while processing ",entry->Name);
    strcpy(entry->Ent.Key ,info->namekey);
    entry->Ent.Version = info->versionnumber;
    entry->Ent.Data = entry->Name;

#ifdef SGI_4D_ENV
    dounload(base, textlength);
#else
    free(base);
    fprintf(stdout, "OK\n");
    fflush(stdout);
#endif 
}



/** 
 ** main()
 **/

int
main(argc, argv)
int argc;
char *argv[];

{
struct EntryStruct * TempEntry;


#if sys_sun3_41 || sys_sun4_41
    int fd;

    /*
     * XXX - force "/dev/zero" to be open as a file descriptor one
     * greater than the first available one, as a workaround for a
     * 4.1 bug (also present in 4.1.1) in the run-time loader.
     * (Fixed in System V Release 4, allegedly.)
     */
    fd = open("/dev/zero", O_RDWR);
    dup(fd);		/* one greater */
    close(fd);
#endif

    ParseArgs(argc, argv);  /* this may chdir() */

    for(TempEntry = EntryList; TempEntry; TempEntry = TempEntry->Next){
	ProcessEntry(TempEntry);
    }
    UpdateAll();
 
    exit(0);			/* success */
}



