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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/basics/common/RCS/profile.c,v 2.23 1993/10/27 18:07:17 gk5g Exp $";
#endif

#include <class.h>

#if !defined(NeXT) && !defined(SOLARIS)
#include <a.out.h>
#endif
#include <andrewos.h> /* sys/file.h */
#include <sys/param.h>

/*
 * AIX is SYSVish when it comes to profiling.
 */
#ifdef AIX
#ifndef SYSV
#define SYSV 1
#endif /* #ifndef SYSV */
#endif /* #ifdef AIX */

#if (SY_B4x)
#include <sys/gprof.h>
#endif /* ! BSD */

#ifdef sys_pmax_mach
/* this is wrong but it should at least compile */
#define lpc lowpc
#define hpc highpc
#define ncnt pc_bytes
#endif

#include <profile.eh>

#define SCALE_1_TO_1 0x10000L

static char *profBuf=NULL;
#ifndef SYSV
static long profBufSize;
static char monfilename[MAXPATHLEN];
static struct phdr header;
#endif /* SYSV */

/* Allocate the profiling buffer */
static int allocProfBuf(size)
{
#ifndef SYSV
    if(profBuf!=NULL && size!=profBufSize)
	free(profBuf);

    profBufSize=size;

    profBuf=(char *)malloc(profBufSize);
    if (profBuf == NULL) 
	return FALSE;

    bzero(profBuf,profBufSize);

    return TRUE;
#else /* SYSV */
    return FALSE;
#endif /* SYSV */
}

int profile__StartClass(classId,classname,filename)
struct classheader *classId;
char *classname,*filename;
{
#ifndef SYSV
    struct classinfo *info=class_Load(classname);

    if(info==NULL || class_GetTextBase(info)==NULL)
	return FALSE;

    return profile_Start(class_GetTextBase(info),
			 class_GetTextLength(info),
			 filename);
#else /* SYSV */
    return FALSE;
#endif /* SYSV */
}

int profile__Start(classId,textbase,textlength,filename)
struct classheader *classId;
char *textbase;
long textlength;
char *filename;
{
#ifndef SYSV
    if(textbase == NULL){
	textbase = (char *)profile_STATICBASE;
	textlength = ((char *)class_GetEText()) - textbase;
    }

    profile_Stop(); /* in case one is already started */

    if(!allocProfBuf(textlength))
	return FALSE;

    /* save data for writing to file */
    header.lpc = (char *)profile_STATICBASE; /* not correct, but what gprof understands */ 
    header.hpc=header.lpc+textlength;
    header.ncnt=textlength+sizeof(header);

    if(filename==NULL)
	strcpy(monfilename,profile_DEFAULTFILE);
    else
	strcpy(monfilename,filename);

    /* the following code is to replace the monitor call below  */
    /*  monitor (textbase,textbase+textlength, profBuf, profBufSize, 0); */
    profil(profBuf,profBufSize,textbase,SCALE_1_TO_1);

    return TRUE;
#else /* SYSV */
    return FALSE;
#endif /* SYSV */
}

int profile__Active(classId)
struct classheader *classId;
{
    return (profBuf!=NULL);
}

int profile__Stop(classId)
struct classheader *classId;
{
#ifndef SYSV
    if(profBuf!=NULL){
	int fd;

	/* monitor(0); */
	profil(0,0,0,0);

	fd=open(monfilename,O_CREAT|O_TRUNC|O_WRONLY,0666);
	if(fd<0)
	    return FALSE;

	write(fd,&header,sizeof(header));
	write(fd,profBuf,profBufSize);
	close(fd);

	free(profBuf);
	profBuf=NULL;

	return TRUE;
    }else
	return FALSE;
#else /* SYSV */
    return FALSE;
#endif /* SYSV */
}
