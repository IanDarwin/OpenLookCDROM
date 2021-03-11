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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/util/lib/RCS/fdplumb.c,v 2.15 1993/05/04 00:53:32 susan Exp $";
#endif


 

#include <stdio.h>
#include <andrewos.h>
#include <fdplumbi.h>

#ifndef _IBMR2
extern char *malloc();
#endif /* _IBMR2 */

static int HasInitializedFDPlumbing = 0;
int fdplumb_LogAllFileAccesses = 0;
static int NumberOfFileDescriptors = 0;
static char **OpenedFileNames = NULL;
static int *OpenCodes = NULL;

char *
DescribeOpenCode(code)
int code;
{
    switch(code) {
	case FDLEAK_OPENCODE_OPEN: return "open";
	case FDLEAK_OPENCODE_FOPEN: return "fopen";
	case FDLEAK_OPENCODE_POPEN: return "popen";
	case FDLEAK_OPENCODE_QOPEN: return "qopen";
	case FDLEAK_OPENCODE_TOPEN: return "topen";
	case FDLEAK_OPENCODE_T2OPEN: return "t2open";
	case FDLEAK_OPENCODE_CREAT: return "creat";
	case FDLEAK_OPENCODE_DUP: return "dup";
	case FDLEAK_OPENCODE_PIPE: return "pipe";
	case FDLEAK_OPENCODE_SOCKET: return "socket";
	case FDLEAK_OPENCODE_OPENDIR: return "opendir";
	default: return "unknown";
    }
}

int fdplumb_SpillGuts() {
    return fdplumb_SpillGutsToFile(stderr, 0);
}

int fdplumb_SpillGutsToFile(fp, ExtraNewLines)
FILE *fp;
int ExtraNewLines;
{
    int i, total = 0;

    if (HasInitializedFDPlumbing) {
	for (i=0; i<NumberOfFileDescriptors; ++i) {
	    if (OpenedFileNames[i]) {
		fprintf(fp, "<warning:fdplumb>File '%s' (opened by %s) is still open.\n", OpenedFileNames[i], DescribeOpenCode(OpenCodes[i]));
		if (ExtraNewLines) fputc('\n', fp);
		++total;
	    }
	}
    } else {
	fprintf(fp, "<warning:fdplumb>Cannot check open files -- never initialized!\n");
    }
    if (total > 0) {
	fprintf(fp, "<warning:fdplumb>Total of %d open files.\n", total);
    }
    return total;
}

void RegisterOpenFile(fd, path, Code)
int fd, Code;
char *path;
{
    int i;

    if (fdplumb_LogAllFileAccesses) fprintf(stderr, "<warning:fdplumb>Opened '%s'.\n", path);
    if (!HasInitializedFDPlumbing) {
	i = getdtablesize();
	OpenedFileNames = (char **) malloc(sizeof(char *) * (1+i));
	OpenCodes = (int *) malloc(sizeof(int) * (1+i));
	if (!OpenedFileNames || !OpenCodes) {
	    fprintf(stderr, "<critical:fdplumb>Out of memory for file descriptor tables!");
	    return;
	}
	NumberOfFileDescriptors = i;
	for (i=0; i<=NumberOfFileDescriptors; ++i) {
	    OpenedFileNames[i] = NULL;
	}
	HasInitializedFDPlumbing = 1;
    }
    if (fd <0 || fd >= NumberOfFileDescriptors) {
	fprintf(stderr, "<critical:fdplumb>Illegal file number in file opening! (%d, %s, %d)\n", fd, path, Code);
    } else {
	if (OpenedFileNames[fd]) {
	    fprintf(stderr, "<critical:fdplumb>File descriptor replaced!  Did you close %s? (%d, %s, %d)", OpenedFileNames[fd], fd, path, Code);
	    free(OpenedFileNames[fd]);
	}
	OpenedFileNames[fd] = malloc(1+strlen(path));
	strcpy(OpenedFileNames[fd], path);
	OpenCodes[fd] = Code;
    }
}

void RegisterCloseFile(fd)
int fd;
{
    if (!HasInitializedFDPlumbing) {
	if (fdplumb_LogAllFileAccesses) fprintf(stderr, "<critical:fdplumb>Attempt to close fd %d before any opens!", fd);
    } else if (!OpenedFileNames[fd]) {
	if (fdplumb_LogAllFileAccesses) fprintf(stderr, "<critical:fdplumb>Attempt to close fd %d before it is opened!", fd);
    } else {
	if (fdplumb_LogAllFileAccesses) fprintf(stderr, "<warning:fdplumb>Closed '%s'.\n", OpenedFileNames[fd]);
	free(OpenedFileNames[fd]);
	OpenedFileNames[fd] = NULL;
    }
}
	
   
int dbg_creat(path, mode)
char *path;
int mode;
{
    int fd;

    fd = creat(path, mode);
    if (fd>=0) RegisterOpenFile(fd, path, FDLEAK_OPENCODE_CREAT);
    return(fd);
}

int dbg_open(path, flags, mode)
char *path;
int flags, mode;
{
    int fd;

    fd = open(path, flags, mode);
    if (fd>=0) RegisterOpenFile(fd, path, FDLEAK_OPENCODE_OPEN);
    return(fd);
}

FILE *
dbg_fopen(path, type)
char *path, *type;
{
    FILE *fp;

    fp = fopen(path, type);
    if (fp) RegisterOpenFile(fileno(fp), path, FDLEAK_OPENCODE_FOPEN);
    return(fp);
}

int dbg_close(fd)
int fd;
{
    RegisterCloseFile(fd);
    return(close(fd));
}

int dbg_fclose(fp)
FILE *fp;
{
    RegisterCloseFile(fileno(fp));
    return(fclose(fp));
}

int dbg_dup(oldfd)
int oldfd;
{
    int newfd;

    newfd = dup(oldfd);
    if (newfd>=0) RegisterOpenFile(newfd, "via-dup", FDLEAK_OPENCODE_DUP);
    return(newfd);
}

int dbg_dup2(oldfd, newfd)
int oldfd, newfd;
{
    int res;

    RegisterCloseFile(newfd);
    res = dup2(oldfd, newfd);
    if (res == 0) RegisterOpenFile(newfd, "via-dup2", FDLEAK_OPENCODE_DUP);
    return(res);
}

int dbg_pipe(fdarr)
int fdarr[2];
{
    int res;

    res = pipe(fdarr);
    if (res == 0) {
	RegisterOpenFile(fdarr[0], "pipe-r", FDLEAK_OPENCODE_PIPE);
	RegisterOpenFile(fdarr[1], "pipe-w", FDLEAK_OPENCODE_PIPE);
    }
    return(res);
}

int dbg_socket(af, typ, prot)
int af, typ, prot;
{
    int fd;

    fd = socket(af, typ, prot);
    if (fd>=0) RegisterOpenFile(fd, "socket", FDLEAK_OPENCODE_SOCKET);
    return(fd);
}

#if !defined(hp9000s300) && !defined(M_UNIX)
int dbg_socketpair(dom, typ, prot, sv)
int dom, typ, prot, sv[2];
{
    int res;

    res = socketpair(dom, typ, prot, sv);
    if (res == 0) {
	RegisterOpenFile(sv[0], "socketpair-0", FDLEAK_OPENCODE_SOCKET);
	RegisterOpenFile(sv[1], "socketpair-1", FDLEAK_OPENCODE_SOCKET);
    }
    return(res);
}
#endif /* hpux */
