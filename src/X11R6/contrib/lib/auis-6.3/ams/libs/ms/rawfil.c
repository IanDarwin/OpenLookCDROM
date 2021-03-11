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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/libs/ms/RCS/rawfil.c,v 1.13 1993/08/25 20:36:11 susan Exp $";
#endif

#include <andyenv.h>
#include <ms.h>
#include <andrewos.h> /* sys/file.h */
#include <sys/stat.h>
#include <pwd.h>
#include <util.h>
#ifdef WHITEPAGES_ENV
#include <wp.h>
#endif /* WHITEPAGES_ENV */
#include <mailconf.h>

#define OLDLOCK 1200 /* 20 minutes */

extern char MyMailDomain[];

ReadRawFile(File, NewMessage, DoLocking)
char *File;
struct MS_Message *NewMessage;
Boolean DoLocking;
{
    struct stat statbuf;
#ifdef AFS_ENV
    char *CellName[250];
#endif /* AFS_ENV */
    char *RawBody, *uname = NULL;
    struct passwd *p;
    int ct, HeaderSize, fdtemp, errsave;

    debug(1, ("Entering ReadRawFile %s\n", File));
    NewMessage->OpenFD = -1;
    if ((fdtemp = open(File, (DoLocking ? osi_O_READLOCK : O_RDONLY), 0)) < 0) {
	if (RetryBodyFileName(File) < 0 ||
	    (fdtemp = open(File, (DoLocking ? osi_O_READLOCK : O_RDONLY), 0)) < 0) {
	    AMS_RETURN_ERRCODE(errno, EIN_OPEN, EVIA_READRAWFILE);
	}
    }
    if (DoLocking && osi_ExclusiveLockNoBlock(fdtemp)){
	errsave = errno;
	close(fdtemp);
	if (errno == EWOULDBLOCK) {
	    char ErrTxt[100+MAXPATHLEN];
	    /* File is locked by someone else and we respect that... */
	    sprintf(ErrTxt, "File in mailbox, %s, is locked", ap_Shorten(File));
	    NonfatalBizarreError(ErrTxt);
	}
	AMS_RETURN_ERRCODE(errsave, EIN_FLOCK, EVIA_READRAWFILE);
    }
    if (lstat(File, &statbuf) != 0) {   /* Changed from fstat to close a security hole */
	errsave = errno;
	close(fdtemp);
	AMS_RETURN_ERRCODE(errsave, EIN_FSTAT, EVIA_READRAWFILE);
    }
    NewMessage->AuthUid = 0;	/* Don't trust any file owner unless it's in AFS. */
    NewMessage->AuthCell = NULL;
#ifdef AFS_ENV
    if (AMS_ViceIsRunning) {
	if (IsOnVice(fdtemp)) {
	    if (GetCellFromFileName(File, CellName, sizeof(CellName))) {
		errsave = errno;
		close(fdtemp);
		AMS_RETURN_ERRCODE(errsave, EIN_GETCELLFROMFILE, EVIA_READRAWFILE);
	    }
	    NewMessage->AuthUid = statbuf.st_uid;
	    NewMessage->AuthCell = NewString(CellName);
	    if (!NewMessage->AuthCell) {
		close(fdtemp);
		AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_READRAWFILE);
	    }
	}
    }
#endif /* AFS_ENV */
    debug(1, ("Set auth user to %d\n", NewMessage->AuthUid));
    if (statbuf.st_size > 0) {
	if (GetHeaderSize(fdtemp, &HeaderSize)) {
	    close(fdtemp);
	    return(mserrcode);
	}
	if ((RawBody = malloc(HeaderSize + 1)) == NULL) {
	    close(fdtemp);
	    AMS_RETURN_ERRCODE( ENOMEM, EIN_MALLOC, EVIA_READRAWFILE);
	}
	ct = read(fdtemp, RawBody, HeaderSize);
	if (ct != HeaderSize) {
	    errsave = errno;
	    free(RawBody);
	    close(fdtemp);
	    if (ct >= 0) {
		char ErrorText[200+MAXPATHLEN];

		sprintf(ErrorText, "Stat of file %s said it had %d bytes, but I could only read %d bytes!", ap_Shorten(File), statbuf.st_size, ct);
		NonfatalBizarreError(ErrorText);
		errsave = EMSBADFILESIZE;
	    }
	    AMS_RETURN_ERRCODE(errsave, EIN_READ, EVIA_READRAWFILE);
	}
	RawBody[HeaderSize] = 0;
	NewMessage->FullSize = statbuf.st_size;
	NewMessage->HeadSize = HeaderSize;
	NewMessage->BodyOffsetInFD = HeaderSize;
    } else {
	static char *StdPrefix = "From: Message Server\nSubject: Empty file found in mailbox.\n\n";
	char *ViceOnlyString;

#ifdef AFS_ENV
	if (AMS_ViceIsRunning) {
	    ViceOnlyString = "To check your storage allocation on the Andrew File System,\nuse the `fs quota' command.\n\n";
	} else 
#endif /* AFS_ENV */
	{
	    ViceOnlyString = "";
	}
	if (DoLocking && (time(0) - statbuf.st_mtime) < OLDLOCK) {
	    close(fdtemp);
	    AMS_RETURN_ERRCODE( EMSYOUNGMAIL, EIN_PARAMCHECK, EVIA_READRAWFILE);
	}
	if ((RawBody = malloc(strlen(File) + 1500)) == NULL) {
	    close(fdtemp);
	    AMS_RETURN_ERRCODE( ENOMEM, EIN_MALLOC, EVIA_READRAWFILE);
	}
        if (IsOnVice(fdtemp) && NewMessage->AuthCell) {
            p = getcpwuid(statbuf.st_uid, NewMessage->AuthCell);
            if (p) {
                GetNameFromGecos(p->pw_gecos, p->pw_name, NewMessage->AuthCell, &uname);
            }
        } else {
            p = getpwuid(statbuf.st_uid);
            if (p) {
                GetNameFromGecos(p->pw_gecos, p->pw_name, MyMailDomain, &uname);
            }
        }
        sprintf(RawBody,
                 "%sAn empty file was found in your mailbox.  This can occur when\nan attempt to deliver mail to you is unsuccessful.\n\n",
                 StdPrefix);
        strcat(RawBody,
                "Although the delivery attempt was unsuccessful, the mail is\nnot lost.  Further attempts to deliver the piece of mail will be\nmade until it is successfully delivered.\n\nNote that delivery attempts will continue to fail if delivery of the mail will put\nyou over your File System quota.  The message system will not be\nable to successfully deliver your mail until you have enough space\nfor the message.  ");
        sprintf(RawBody + strlen(RawBody),
                 "%sTo reduce disk usage, you might\nwant to compress some of your files (using the compress or\ncompact commands) and/or delete old mail and unnecessary \nfiles such as 'core', checkpoint, and backup files.\n\nOther reasons for delivery failure are transient in nature,\nand will clear up without action on your part.  In fact, you may already have\nreceived the mail that caused the empty file to appear.\n\n\n",
                 ViceOnlyString);
        sprintf(RawBody + strlen(RawBody),
                 "File name: %s\nWritten by: user %s (%d)\nDate and time of file writing: %s\n",
                 File, uname ? uname : "unknown user", statbuf.st_uid, ctime(&(statbuf.st_mtime)));
	NewMessage->WeFiddled = TRUE;
	NewMessage->FullSize = strlen(RawBody);
	if (uname) free(uname);
	NewMessage->HeadSize = NewMessage->FullSize; /* only slightly bogus */
	NewMessage->BodyOffsetInFD = strlen(StdPrefix);
    }	
    NewMessage->RawBits = RawBody;
    NewMessage->RawFileDate = statbuf.st_mtime;
    NewMessage->OpenFD = fdtemp;
    return(0);
}


#define READCHUNKSIZE (1024)

GetHeaderSize(fd, size)
int fd, *size;
{
    int oldpos, result, looping = TRUE, NLAtEnd = FALSE;
    char buffer[1 + READCHUNKSIZE + 1];
    char *t;

    oldpos = lseek(fd, 0, L_INCR);
    *size = 0;
    if (oldpos < 0)
	AMS_RETURN_ERRCODE(errno, EIN_LSEEK, EVIA_GETHEADERSIZE);
    if (lseek(fd, 0, L_SET) < 0)
	AMS_RETURN_ERRCODE(errno, EIN_LSEEK, EVIA_GETHEADERSIZE);
    while (looping) {
	result = read(fd, buffer, READCHUNKSIZE);
	if (result < 0)
	    AMS_RETURN_ERRCODE(errno, EIN_READ, EVIA_GETHEADERSIZE);
	if (result) {
	    buffer[result] = '\0';
	    if (NLAtEnd) {
		t = buffer-1;
	    } else {
		t = strchr(buffer, '\n');
	    }
	    while (t) {
		if (*(++t) == '\n') {
		    looping = FALSE;
		    *size += (t + 1) - buffer;
		    t = NULL;
		}
		else {
		    t = strchr(t, '\n');
		}
	    }
	}
	else    /* read returned 0; end of file */
	    looping = FALSE;
	if (looping) {
	    *size += result;
	    NLAtEnd = (buffer[result-1] == '\n');
	}
    }
    if (lseek(fd, oldpos, L_SET) < 0)
	AMS_RETURN_ERRCODE(errno, EIN_LSEEK, EVIA_GETHEADERSIZE);
    debug(16, ("The header size for this message seems to be %d\n", *size));
    return (0);
}
