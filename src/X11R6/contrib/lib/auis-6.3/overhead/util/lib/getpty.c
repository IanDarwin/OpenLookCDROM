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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/util/lib/RCS/getpty.c,v 1.17 1994/05/10 18:08:33 rr2b Exp $";
#endif

/*
This module contains the routine getpty.  It returns file descriptors
for a pty. It takes two pointers to ints and returns the file decriptors.
It returns TRUE if it succeeds.
*/

#include <andrewos.h>

#ifdef SGI_4D_ENV
#include <sys/stat.h>
#include <sys/sysmacros.h>
#endif

#include <fcntl.h>
#include <sys/param.h>

#if defined(sys_telmat)
#include <stdio.h>
#include <sys/stropts.h>
#define MASTER_PTY_PREFIX "/dev/ptmx"
#define SLAVE_PTY_PREFIX "/dev/pts"
#define MAX_PTYS 64
#endif /* sys_telmat */


#ifdef hpux
#include <sys/bsdtty.h>
#include <sys/ptyio.h>
#endif /* hpux */

#ifndef	SGI_4D_ENV  /* for bug in makedepend */
#if SY_AIX221
#include <sys/ioctl.h>
#include <sys/devinfo.h>
#include <sys/pty.h>
#include <sys/tty.h>
#endif /* SY_AIX221 */
#endif


#ifdef hpux
#define MASTER_PTY_PREFIX "/dev/ptym/pty"
#define SLAVE_PTY_PREFIX "/dev/pty/tty"
#define MAX_PTYS 64
#endif /* HPUX */

#if SY_AIX221
#define MASTER_PTY_PREFIX "/dev/ptc"
#define SLAVE_PTY_PREFIX "/dev/pts"
#define MAX_PTYS 256
#endif /* SY_AIX221 */

#ifndef MASTER_PTY_PREFIX
#define MASTER_PTY_PREFIX "/dev/pty"
#define SLAVE_PTY_PREFIX "/dev/tty"
#define MAX_PTYS 256
#endif

#if POSIX_ENV
#define OPEN_FLAGS O_RDWR|O_NOCTTY
#else
#define OPEN_FLAGS O_RDWR
#endif

static char *GetPtyNumberString(num)
int num;
{
static char ptyNum[10];

#if SY_AIX221 || defined (SGI_4D_ENV) || defined(sys_telmat)
    sprintf(ptyNum, "%d", num);
#else /* SY_AIX221 || SGI_4D_ENV || sys_telmat */
    ptyNum[0] = 'p' + (num/16);
    ptyNum[1] = "0123456789abcdef"[num%16];
    ptyNum[2] = '\0';
#endif /* SY_AIX221 */
    return ptyNum;
}

int GetPtyandName(masterFD, slaveFD,name,len)
int *masterFD;
int *slaveFD;
char *name;
int len;
{
#if defined(sys_telmat)
  int master;
  int slave;
  char *slavename;
  char ptcname [sizeof(MASTER_PTY_PREFIX)+1];

  strcpy(ptcname,MASTER_PTY_PREFIX);
  if ((master = open(ptcname, OPEN_FLAGS)) < 0)
    return 0;
  grantpt(master);
  unlockpt(master);
  slavename = (char*) ptsname(master);
  if ((slave = open(slavename, OPEN_FLAGS)) < 0) {
    close(master);
    return 0;
  }
  ioctl(slave,I_PUSH,"ptem");
  ioctl(slave,I_PUSH,"ldterm");
  if (name!=NULL)
    strncpy(name,slavename,len);
  if (masterFD != NULL)
    *masterFD=master;
  if (slaveFD != NULL)
    *slaveFD=slave;
  return 1;
  
#else /* sys_telmat */
#if SY_AIX3
    /* AIX 3.1 lets us open a single device "/dev/ptc" which
     * finds an available pty for us.  ttyname() returns the
     * name of that device.
     */
    int master;
    int slave;
    char *ptyname;

    if ((master = open("/dev/ptc", OPEN_FLAGS)) < 0)
	return 0;	/* none left */
    if ((ptyname = ttyname(master)) == NULL) {
	/* major weirdness */
	close(master);
	return 0;
    }
    if ((slave = open(ptyname, OPEN_FLAGS)) < 0) {
	/* more weirdness */
	close(master);
	return 0;
    }
    if (name != NULL)
        strncpy(name,ptyname,len);
    if (masterFD != NULL)
	*masterFD = master;
    if (slaveFD != NULL)
	*slaveFD = slave;
    return 1;
#else
#ifdef SGI_4D_ENV
    int master;
    int slave;
    char ptyname[30];
    struct stat stBuf;

    do {
	if ((master = open("/dev/ptc", OPEN_FLAGS)) < 0)
	    return 0;	/* none left */
	if (fstat(master, &stBuf) < 0) {
	    return 0;
	}
	sprintf(ptyname, "/dev/ttyq%d", minor(stBuf.st_rdev));
	slave = open(ptyname, OPEN_FLAGS);
    } while (slave < 0);

    if (name != NULL)
        strncpy(name,ptyname,len);
    if (masterFD != NULL)
	*masterFD = master;
    if (slaveFD != NULL)
	*slaveFD = slave;
    return 1;

#else
    int PtyNumber = 0;
    char ptcname[MAXPATHLEN];
    char ptyname[MAXPATHLEN];
    char *ptyNum;
    int master;
    int slave;

    while (PtyNumber++ < MAX_PTYS) {
	ptyNum = GetPtyNumberString(PtyNumber);
	strcpy(ptcname, MASTER_PTY_PREFIX);
	strcat(ptcname, ptyNum);
	if ((master = open (ptcname, OPEN_FLAGS)) >= 0) {
#if SY_AIX221
	{
            /* Be sure this pty really is available. */
	    int value;

	    if ((value = ioctl(master, PTYSTATUS, 0)) == -1 || (value & 0xffff) != 0 || (value >> 16) != 1)  {
		close(master);
		continue;
	    }
	}
#endif /* SY_AIX221 */
	    strcpy(ptyname, SLAVE_PTY_PREFIX);
	    strcat(ptyname, ptyNum);
	    if(name != NULL) strncpy(name,ptyname,len);
	    if ((slave = open(ptyname, OPEN_FLAGS)) >= 0)  {
		if (masterFD != NULL)
		    *masterFD = master;
		if (slaveFD != NULL)
		    *slaveFD = slave;
		return 1;
	    }
	    else
		close(master);
	}
    }

    return 0;
#endif /* SGI_4D_ENV */
#endif /* SY_AIX31 */
#endif /* sys_telmat */
}

int GetPty(masterFD, slaveFD)
int *masterFD;
int *slaveFD;
{
    return GetPtyandName(masterFD, slaveFD,NULL,0);
}
