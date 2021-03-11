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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/util/lib/RCS/vclose.c,v 2.19 1993/11/11 19:45:43 gk5g Exp $";
#endif


 


/*
                vclose.c -- routines that are specific to Vice file system.
 
        vclose -- Do a close to & force it to wait until
                  the close completes.  This is for Vice,
                  which normally does not wait.
 
        vdown  -- Return non-zero if error number passed indicates that
                  Vice/Venus is down.
 
        IsOnVice -- return non-zero if we believe that the file behind
                the given file-descriptor is in Vice.
 
*/

#include <andyenv.h>
#include <errno.h>
extern int errno;
#include <stdio.h>
#include <ctype.h>
#include <andrewos.h>		/* sys/types.h */
#include <sys/ioctl.h>
#include <sys/stat.h>
#ifdef AFS_ENV
#include <netinet/in.h>
#include <afs/param.h>
#include <sys/signal.h>
#include <setjmp.h>
#include <afs/venus.h>
#include <afs/errors.h>
#endif /* AFS_ENV */

extern int errno;

#define ALLSIGS	    0xffffffff

#ifdef AFS_ENV
static int HasCheckedVice = 0, ViceRuns = 1;

static jmp_buf noVice;

static
#if POSIX_ENV
void
#endif
JumpNoVice() {
    longjmp(noVice, 1);
}

static CkVice()
{
#ifdef POSIX_ENV
    struct sigaction SOld, SNew;
#else
    struct sigvec SOld, SNew;
#endif
    int GotSignal;
    struct ViceIoctl dummy;

    dummy.in_size = 0;	/* Prepare the trial pioctl() call */
    dummy.out_size = 0;

    GotSignal = 0;	/* nothing so far. */

#ifdef POSIX_ENV
    SNew.sa_handler = JumpNoVice;
    sigemptyset(&SNew.sa_mask);	/* no additional sigs to mask */
    SNew.sa_flags = 0;	/* on regular stack, please */
    if (sigaction(SIGSYS, &SNew, &SOld) != 0) return;
#else
    SNew.sv_handler = JumpNoVice;
    SNew.sv_mask = 0;	/* no additional sigs to mask */
    SNew.sv_onstack = 0;	/* on regular stack, please */
    if (sigvec(SIGSYS, &SNew, &SOld) != 0) return;
#endif

    if (setjmp(noVice) == 0) {	/* test whether Vice runs */
	(void) pioctl("/afs", VIOC_GET_WS_CELL, &dummy, 1);	/* Just see if this signals! */
    } else {			/* oops--caught SIGSYS. */
	GotSignal = 1;
    }
#ifdef POSIX_ENV
    sigaction(SIGSYS, &SOld, NULL); /* Reset the old behavior. */
#else
    sigvec(SIGSYS, &SOld, NULL); /* Reset the old behavior. */
#endif
    ViceRuns = (GotSignal ? 0 : 1); /* Remember whether this is a Vice kernel. */

    HasCheckedVice = 1;
}

static DoIoctl(fd)
int fd;
{
    struct ViceIoctl dummy;

    if (!HasCheckedVice) CkVice();

    if (ViceRuns) {
	dummy.in_size = 0;
	dummy.out_size = 0;
	ioctl(fd, VIOCCLOSEWAIT, &dummy);
    }
}
#endif /* AFS_ENV */

int ViceIsRunning() {
#ifdef AFS_ENV
    if (!HasCheckedVice) CkVice();
    return ViceRuns;
#else /* AFS_ENV */
    return 0;
#endif /* AFS_ENV */
}

int vclose(fd)
int fd;
{
#ifdef AFS_ENV
    SIGSET_TYPE newmask, oldmask;
#endif /* AFS_ENV */
    register int rc;

#ifdef AFS_ENV
    DoIoctl(fd);
#ifdef POSIX_ENV
    (void) sigfillset(&newmask);
    if (sigprocmask(SIG_BLOCK, &newmask, &oldmask) < 0)
	perror("sigprocmask");
#else
    oldmask = sigsetmask(ALLSIGS);
#endif
#endif /* AFS_ENV */

    rc = close(fd);

#ifdef AFS_ENV
#ifdef POSIX_ENV
    (void) sigprocmask(SIG_SETMASK, &oldmask, (sigset_t *)NULL);
#else
    sigsetmask(oldmask);
#endif
#endif /* AFS_ENV */

    return rc;
}

int vfclose(f)
FILE *f;
{
#ifdef AFS_ENV
    SIGSET_TYPE oldmask, newmask;
#endif /* AFS_ENV */
    register int rc;

    /* Barf immediately on NULL arg */
    if (f == NULL) {
	errno = EINVAL;
	return EOF;
    }

#ifdef AFS_ENV
    DoIoctl(fileno(f));
#ifdef POSIX_ENV
    (void) sigfillset(&newmask);
    if (sigprocmask(SIG_BLOCK, &newmask, &oldmask) < 0)
	perror("sigprocmask");
#else
    oldmask = sigsetmask(ALLSIGS);
#endif
#endif /* AFS_ENV */

    rc = fclose(f);

#ifdef AFS_ENV
#ifdef POSIX_ENV
    (void) sigprocmask(SIG_SETMASK, &oldmask, (sigset_t *)NULL);
#else
    sigsetmask(oldmask);
#endif
#endif /* AFS_ENV */

    return rc;
}

int vdown(err)
int err;
{
/*	ETIMEDOUT:	Venus has timed out the connection to the file server
      ENXIO:	the Venus process handling the kernel device has terminated
      ENOTTY:	Venus doesn't know about this file descriptor;
probably it's left over from a previous Venus run
      ENODEV:	Volume off line (Venus error code mapping)
      EIO:	AFS read-ahead has failed; file was openable but not fully readable.
    VOFFLINE:	Volume is off line, for reason given in offline message
     VBUSY:	Volume temporarily unavailable; try again; not usually
	      propagated to application level.
      255, -1:    Occasionally erroneously propagated from AFS cache managers to indicate connection failure (the ETIMEDOUT case).
*/
#ifdef AFS_ENV
    if (!HasCheckedVice) CkVice();
    if (ViceRuns) {
	return (err == ETIMEDOUT || err == ENXIO || err == ENOTTY || err == EIO
#ifdef EFAULT
		|| err == EFAULT
#endif /* EFAULT */
		|| err == ENODEV || err == VOFFLINE || err == VBUSY || err == 255 || err == -1);
    } else
#endif /* AFS_ENV */
	return (err == ENXIO
#ifdef ETIMEDOUT
		|| err == ETIMEDOUT
#endif /* ETIMEDOUT */
#ifdef EFAULT
		|| err == EFAULT
#endif /* EFAULT */
		);
}

int IsOnVice(fd)
int fd;
{
    /* Return TRUE if we believe that the file open on the given file descriptor is in Vice, and FALSE otherwise.
	*/
#ifdef AFS_ENV
    struct ViceIoctl dummy;
    auto char CellN[64];
    int Err;
    register int rc;
    SIGSET_TYPE oldmask, newmask;

    if (!HasCheckedVice) CkVice();

    if (ViceRuns) {
	dummy.in_size = 0;
	dummy.out_size = sizeof(CellN);
	dummy.out = CellN;
#ifdef AFS_ENV
	DoIoctl(fd);
#ifdef POSIX_ENV
	(void) sigfillset(&newmask);
	if (sigprocmask(SIG_BLOCK, &newmask, &oldmask) < 0)
	    perror("sigprocmask");
#else
	oldmask = sigsetmask(ALLSIGS);
#endif
#endif /* AFS_ENV */

	rc = ioctl(fd, VIOCIGETCELL, &dummy);
	Err = errno;

#ifdef AFS_ENV
#ifdef POSIX_ENV
	(void) sigprocmask(SIG_SETMASK, &oldmask, (sigset_t *)NULL);
#else
	sigsetmask(oldmask);
#endif
#endif /* AFS_ENV */
	return (rc == 0 || (Err != ENOTTY && vdown(Err)));
    } else
#endif /* AFS_ENV */
	return 0;
}

#ifdef TESTINGONLYTESTING
main()
{
    printf("Vdown(etimedout) is %d.\n", vdown(ETIMEDOUT));
}
#endif /* TESTINGONLYTESTING */
