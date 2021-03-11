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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/util/lib/RCS/setprof.c,v 2.19 1992/12/15 21:10:49 rr2b R6tape $";
#endif


 

/* 
 * WARNING WARNING WARNING WARNING WARNING WARNING 
 * 
 * This code contains a possible filename length bug (appending .NEW 
 * to the profileFileName).  This isn't worth fixing now because 
 * this code will soon be revamped for the port to SysV.
 *
 *							-t
 *
 * WARNING WARNING WARNING WARNING WARNING WARNING 
 */





/* ************************************************************ *\
	setprof.c -- routines to set preferences.
	    (routines to read preferences are in profile.c) 
\* ************************************************************ */
#include <system.h>
#include <stdio.h>
#include <ctype.h>
#include <andrewos.h>		/* sys/file.h */
#include <sys/param.h>
#include <pwd.h>
#include <errno.h>
#include <util.h>

extern int errno;
extern char ProgramName[];
extern char *GetProfileFileName();
extern char *GetFirstProfileFileName();

#define BIGPREF 2000

setprofilestring(prog, pref, val) 
char *prog, *pref, *val;
{
    FILE *oldR;
    FILE *newR;
    FILE *newW;
    char newProfileFileName[MAXPATHLEN+20]; /* the extra because we */
					    /* tack extensions onto */
					    /* existing filenames */
    char finalProfileFileName[MAXPATHLEN];
    char LineBuf[BIGPREF];
    char *program;
    char *key;
    char *condition;
    char *profileFileName;
    char k[BIGPREF];
    char pgm[BIGPREF]; 
    int MatchAllProgs;
    int programLength;
    int keyLength;
    int conditionLength;
    int retVal;

    if (prog == NULL)
	prog = ProgramName;

    MatchAllProgs = !strcmp(prog, "*");


    if (!(profileFileName = GetProfileFileName()) ||
	strcmp(profileFileName, (char*) AndrewDir("/lib/global.prf")) == 0 ||
	 access(profileFileName, W_OK))
	if (!(profileFileName = GetFirstProfileFileName()))  /* ~/preferences */
	    return -1;

    /* 
     * If the profile file is a symbolic link, we want to deal with 
     * the physical file underlying it since we'll be renaming on top 
     * of it and want to preserve the link.
     */

    {
	char buf1[MAXPATHLEN];
	char buf2[MAXPATHLEN];
	char *lastbuf;
	char *nextbuf;

	strcpy(buf1, profileFileName);
	lastbuf = buf1;
	nextbuf = buf2;

	while (osi_readlink(lastbuf, nextbuf, MAXPATHLEN) > 0) {
	    char *t;

	    /* symlinks may be stated relative to their directory. */
	    /* bletch. */

	    if (nextbuf[0] != '/' && (t = rindex(lastbuf, '/')) != NULL)
		strcpy(t + 1, nextbuf);
	    else {
		t = lastbuf;
		lastbuf = nextbuf;
		nextbuf = t;
	    }

	}

	strcpy(finalProfileFileName, lastbuf);
    }


    /* Strictly speaking, this test just saves time.  It could be omitted. */
    if ((access(finalProfileFileName, W_OK) != 0) && (errno != ENOENT)) {
	fprintf(stderr,
		"<error:setprofile>No write access on file '%s'.\n",
		finalProfileFileName); 
	return -1;
    }

#ifdef USESHORTFILENAMES
    sprintf(newProfileFileName, "%s+", profileFileName);
#else
    sprintf(newProfileFileName, "%s.NEW", profileFileName);
#endif

    /* Open preference file for locking */
    oldR = fopen(profileFileName, osi_F_READLOCK);
    if (oldR == NULL) {
	if (errno == ENOENT) {
	    fprintf(stderr,
		    "<warning:setprofile>You have no '%s' file; creating one.\n",
		    profileFileName); 
	    newW = fopen(profileFileName, "w");
	    if (newW == NULL) {
		return(-2);
	    }
	    fclose(newW);
	    oldR = fopen(profileFileName, osi_F_READLOCK);
	    if (oldR == NULL) {
		return(-3);
	    }
	}
    }

    if (osi_ExclusiveLockNoBlock(fileno(oldR))){
	fclose(oldR);
	return(-4);
    }

    /* And we open the write copy */
    newW = fopen(newProfileFileName, "w");
    if (newW == NULL) {
	fclose(oldR);
	return(-5);
    }

    /* Now we open and lock a copy for reading, in order to keep 
	the lock when we close the write copy */
    newR = fopen(newProfileFileName, osi_F_READLOCK);
    if (newR == NULL) {
	fclose(oldR);
	fclose(newW);
	return(-6);
    }
    if (osi_ExclusiveLockNoBlock(fileno(newR))){
	fclose(newR);
	fclose(oldR);
	fclose(newW);
	return(-7);
    }

    refreshprofile();  /* Force re-reading on next getprofilexxx call */

    fprintf(newW, "%s.%s:%s\n", prog, pref, val);
    while ((retVal = ReadConfigureLine(oldR, LineBuf, BIGPREF,
				       &program, &programLength, &key,
				       &keyLength, NULL, NULL,
				       &condition, &conditionLength))
	   != CONFIG_EOF) {
	if (retVal == CONFIG_FOUNDENTRY &&
	    (condition == NULL || conditionLength == 0) &&
	    program != NULL && programLength != 0) { 

	    strncpy(pgm, program, programLength);
	    pgm[programLength] = '\0';
	    strncpy(k, key, keyLength);
	    k[keyLength] = '\0';
	    if ((!MatchAllProgs && !FOLDEDEQ(prog, pgm)) ||
		!FOLDEDEQ(pref, k))
		fputs(LineBuf, newW);
	} else
	    fputs(LineBuf, newW);
    }

    if (vfclose(newW)) {
	fclose(oldR);
	fclose(newR);
	unlink(newProfileFileName);
	return(-8);
    }

    if (rename(newProfileFileName, finalProfileFileName)) {
	fclose(newR);
	fclose(oldR);
	return(-9);
    }

    fclose(oldR);
    fclose(newR);
    return(0);
}

#ifdef TESTINGONLYTESTING
/* 
Needs to compile with:
  cc -DTESTINGONLYTESTING setprof.c /usr/andrew/lib/libutil.a -o setprof 
*/
char ProgramName[100] = "foobar";
main(argc, argv)
char **argv;
{
    int x;
    extern int errno;

    printf("Setting preference %s.%s: %s\n", argv[1], argv[2], argv[3]);
    x = setprofilestring(argv[1], argv[2], argv[3]);
    printf("setprofilestring() returned %d (errno %d)\n", x, errno);
}
#endif /* TESTINGONLYTESTING */
