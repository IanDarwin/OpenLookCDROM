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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/libs/ms/RCS/rawdb.c,v 2.73 1993/09/21 21:58:29 gk5g Exp $";
#endif

/*
        rawdb.c -- Routines for accessing MS database files in the raw.
*/

#include <andrewos.h>                  /* sys/file.h */
#include <ms.h>
#include <stdio.h>
#include <netinet/in.h>
#include <util.h>
#include <sys/stat.h>
#include <ctype.h>
#include <pwd.h>
#include <hdrparse.h>
#ifdef WHITEPAGES_ENV
#include <wp.h>
#endif                                 /* WHITEPAGES_ENV */
#ifdef AFS_ENV
#include <afs/param.h>
#ifdef CMUCS
#include <afs/ptint.h>
#include <afs/ptserver.h>
#else
#include <afs/print.h>
#include <afs/prserver.h>
#endif
#endif                                 /* AFS_ENV */
#include <mail.h>
#include <mailconf.h>

extern char    *permanentmalloc();
extern char     MyMailDomain[];

static char    *EFBIGFormat = "Maximum number of files per directory may have been exceeded closing %s";

/* Snapshot buffer */
#define MAXSNAPSHOTBUFLEN 27	/* Just short of 4096 bytes of snapshots */
static int SnapshotBufStart;	/* First snapshot # stored in SnapshotBuf */
static int SnapshotBufLen;	/* # of snapshots stored in SnapshotBuf */
static char SnapshotBuf[MAXSNAPSHOTBUFLEN*AMS_SNAPSHOTSIZE];
static struct MS_Directory *SnapshotBufDir;

/* Forward Declarations */
static void     FreeUpdates();
static void     AnnounceBadDirFormat();
static void     BuildAttrNameBuf();

/* Version number for database format.  This number gets stored
        in each message directory when it is written.  If the
        message server detects an inconsistency between its version
        and the file's version, it must run a conversion program or
        otherwise figure out how to cope.
*/

/* The next line should change each time the actual file format changes.*/
/* Don't forget to change the same define in recon.c! */

#define MS_DB_VERSION 4

int             AdjustIDs(Dir, HowMany)
struct MS_Directory *Dir;
int             HowMany;
{                                      /* Makes sure that HowMany entries
                                        * are allocated in the IDs array
                                        * hanging off Dir->IDs.  Returns 0
                                        * if OK, -1 if not. */
    int             Ix;

    if (HowMany <= 0)
        return -1;
    if (HowMany <= Dir->NumIDs)
        return 0;
    if (Dir->IDs != NULL) {
        Dir->IDs = (struct MS_IDs *) realloc(Dir->IDs, HowMany * (sizeof(struct MS_IDs)));
    }
    else {
        Dir->IDs = (struct MS_IDs *) malloc(HowMany * (sizeof(struct MS_IDs)));
    }
    if (Dir->IDs == NULL) {
        Dir->NumIDs = 0;
        return -1;
    }
    for (Ix = Dir->NumIDs; Ix < HowMany; ++Ix) {
        Dir->IDs[Ix].Chn = -1;
        Dir->IDs[Ix].midH = noKRHash;
        Dir->IDs[Ix].repH = noKRHash;
    }
    Dir->NumIDs = HowMany;
    return 0;
}

void            FreeIDs(Dir)
struct MS_Directory *Dir;
{                                      /* Deallocated the IDs. */
    if (Dir->NumIDs > 0 && Dir->IDs != NULL)
        free(Dir->IDs);
    Dir->NumIDs = 0;
    Dir->IDs = NULL;
}

/* The following routine creates a new directory, with the appropriate
        zero-record initialization, but with no messages.
*/

CreateNewMSDirectory(Dirname, NewDir, Overwrite)
char           *Dirname;
struct MS_Directory **NewDir;
int             Overwrite;
{
    int             status;
    struct stat     stbuf;
    char            DirPath[MAXPATHLEN + 1], *slash;

    debug(1, ("CreateNewMSDirectory (%s) %s\n", Overwrite ? "Overwrite" : "Safe", Dirname));
    slash = strrchr(Dirname, '/');
    if (!slash) {
        AMS_RETURN_ERRCODE(EINVAL, EIN_PARAMCHECK, EVIA_CREATENEWMSDIRECTORY);
    }
    ++slash;
    if ((*slash == '+') || strchr(slash, ' ') || strchr(slash, '.')) {
        AMS_RETURN_ERRCODE(EINVAL, EIN_PARAMCHECK, EVIA_CREATENEWMSDIRECTORY);
    }
    *NewDir = (struct MS_Directory *) permanentmalloc(sizeof(struct MS_Directory));
    if (*NewDir == NULL) {
        AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_CREATENEWMSDIRECTORY);
    }
    debug(2, ("Setting up directory parameters\n"));
    (*NewDir)->UNIXDir = permanentmalloc(strlen(Dirname) + 1);
    if (!(*NewDir)->UNIXDir) {
        AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_CREATENEWMSDIRECTORY);
    }
    strcpy((*NewDir)->UNIXDir, Dirname);
    (*NewDir)->MessageCount = 0;
    (*NewDir)->fd = -1;
    (*NewDir)->CurPos = -1;
    (*NewDir)->OpenMode = -1;
    (*NewDir)->MaxOpenMode = -1;
    (*NewDir)->LastIDHit = -1;
    (*NewDir)->DBMajorVersion = MS_DB_VERSION;
    (*NewDir)->MaxChainVal = 0;
    (*NewDir)->AttrNames = NULL;
    (*NewDir)->AttrCount = 0;
    (*NewDir)->IDs = NULL;
    (*NewDir)->NumIDs = 0;
    strcpy((*NewDir)->LastMsgDate, "000000");
    (*NewDir)->CheckedWritable = 0;
    (*NewDir)->Writable = 1; /* If I can create it... */
    if (stat(Dirname, &stbuf) || ((stbuf.st_mode & S_IFMT) != S_IFDIR)) {
        if (mkdir(Dirname, 0777) && errno != EEXIST) {
            AMS_RETURN_ERRCODE(errno, EIN_MKDIR, EVIA_CREATENEWMSDIRECTORY);
        }
    }
    sprintf(DirPath, "%s/%s", Dirname, MS_DIRNAME);
    if (!stat(DirPath, &stbuf)) {
        debug(2, ("Would overwrite\n"));
        if (Overwrite) {
            MarkInProgress(Dirname);   /* If we overwrite, wouldn't hurt
                                        * to check for lost files */
        }
        else {
            AMS_RETURN_ERRCODE(EMSWOULDOVERWRITE, EIN_PARAMCHECK, EVIA_CREATENEWMSDIRECTORY);
        }
    }
    if (OpenMSDirectory(*NewDir, MD_CREATE)) {
        debug(2, ("Cannot lock\n"));
        return (mserrcode);
    }
    debug(2, ("Calling destructive write\n"));
    status = DestructivelyWriteDirectoryHead((*NewDir));
    if (CloseMSDir(*NewDir, MD_CREATE)) {
        return (mserrcode);
    }
    if (status)
        return (status);
    if (AddToDirCache(*NewDir, TRUE)) {
        return (mserrcode);
    }
    mserrcode = EnsureInSubscriptionMap(Dirname);
    if (!mserrcode) {
        mserrcode = DropHint(Dirname);
    }
    return (mserrcode);
}

/* The following routine destructively writes a directory header.
   This routine will alter the FileDateWhenRead.
   This routine returns 0 for success, otherwise sets mserrcode.
*/

#define PADSIZE 10

DestructivelyWriteDirectoryHead(Dir)
struct MS_Directory *Dir;
{
    char            DirectoryHeader[AMS_DIRHEADSIZE + 2];
    char            majstr[PADSIZE + 1], minstr[PADSIZE + 1], killstr[PADSIZE + 1], AttrBuf[ATTNAMESLEN + 1];
    int             i;

    debug(1, ("In destructively write directory head\n"));
    if (Dir->fd < 0) {
        AMS_RETURN_ERRCODE(EMSDIRNOTOPEN, EIN_PARAMCHECK, EVIA_DESTRUCTIVELYWRITEDIR);
    }
    if (Dir->OpenMode < MD_WRITE) {
        AMS_RETURN_ERRCODE(EINVAL, EIN_PARAMCHECK, EVIA_DESTRUCTIVELYWRITEDIR);
    }
    if (Dir->CurPos != 0 && lseek(Dir->fd, 0, L_SET) < 0) {
        AMS_RETURN_ERRCODE(errno, EIN_LSEEK, EVIA_DESTRUCTIVELYWRITEDIR);
    }
    itops((long) MS_DB_VERSION, majstr, PADSIZE);       /* itops is in util.c */
    itops((long) Dir->MaxChainVal, minstr, PADSIZE);
    itops(time(0), killstr, PADSIZE);  /* Killer mod time, more or less */
    BuildAttrNameBuf(Dir, AttrBuf);
    sprintf(DirectoryHeader, "%s%s%s%s",
            AMS_DIRECTORY_PREFIX_STRING, majstr, minstr, killstr);
    i = strlen(DirectoryHeader) + 1;   /* include FIVE trailing nulls */
    bzero(DirectoryHeader + i, 4);     /* zero out the last four */
    bcopy(AttrBuf, DirectoryHeader + i + 4, ATTNAMESLEN);
    if (MSDebugging & 2) {
        char time64[1 + AMS_DATESIZE];
        unsigned long timenum;

        strncpy(time64, Dir->LastMsgDate, AMS_DATESIZE);
        time64[AMS_DATESIZE] = '\0';
        timenum = (unsigned long) conv64tolong(time64);
        debug(2, ("Placed LastMsgDate into raw head; date is %s",
                   ctime(&timenum)));
    }
    bcopy(Dir->LastMsgDate, DirectoryHeader + i + 4 + ATTNAMESLEN,
          AMS_DATESIZE);               /* Record the oldest date ever to
                                        * appear in this folder */
    bzero(DirectoryHeader + i + 4 + ATTNAMESLEN + AMS_DATESIZE,
          AMS_DIRHEADSIZE - (i + 4 + ATTNAMESLEN + AMS_DATESIZE));
    if (writeall(Dir->fd, DirectoryHeader, AMS_DIRHEADSIZE) != AMS_DIRHEADSIZE) {
	Dir->CurPos = -1;
        AMS_RETURN_ERRCODE(errno, EIN_FWRITE, EVIA_DESTRUCTIVELYWRITEDIR);
    }
    Dir->CurPos = AMS_DIRHEADSIZE;
    /*
     * Most things that write to a msdir call this function afterwards.
     * Therefore, invalidate the SnapshotBuf if it's on this directory
     */
    if (SnapshotBufDir == Dir) SnapshotBufDir = 0;
    debug(2, ("Wrote raw head out\n"));
    return (0);
}

/* The following routine fills the supplied structure with the up-to-date
        contents of the named directory.  Any previous contents to the
        structure will be lost -- this is not the right routine to
        use for updating an old representation of a directory
*/

ReadOldMSDirectoryHead(Dir)
struct MS_Directory *Dir;
{
    return (ReadOldMSDirectoryHead_Complain(Dir, TRUE));
}

ReadOldMSDirectoryHead_Complain(Dir, DoComplain)
struct MS_Directory *Dir;
int             DoComplain;
{
    int             numbytes;
    char            DirHead[AMS_DIRHEADSIZE + 1], AttrBuf[ATTNAMESLEN + 1];
    char            ConvBuf[PADSIZE + 1];       /* For conversion of ascii
                                                 * long ints */
    struct stat     statbuf;

    if (Dir->fd < 0) {
        AMS_RETURN_ERRCODE(EMSDIRNOTOPEN, EIN_PARAMCHECK, EVIA_READOLDMSDIR);
    }
    if (fstat(Dir->fd, &statbuf)) {
        AMS_RETURN_ERRCODE(errno, EIN_FSTAT, EVIA_READOLDMSDIR);
    }
    if (statbuf.st_size < AMS_DIRHEADSIZE
        || ((statbuf.st_size - AMS_DIRHEADSIZE) % AMS_SNAPSHOTSIZE)) {
        if (DoComplain)
            AnnounceBadDirFormat(Dir->UNIXDir);
        AMS_RETURN_ERRCODE(EMSBADDIRFORMAT, EIN_SIZECHECK, EVIA_READOLDMSDIR);
    }
    if (Dir->CurPos != 0 && lseek(Dir->fd, 0, L_SET) < 0) {
        AMS_RETURN_ERRCODE(errno, EIN_LSEEK, EVIA_READOLDMSDIR);
    }
    if ((numbytes = read(Dir->fd, DirHead, AMS_DIRHEADSIZE)) != AMS_DIRHEADSIZE) {
        if (numbytes >= 0) {
            if (DoComplain)
                AnnounceBadDirFormat(Dir->UNIXDir);
            errno = EMSBADDIRFORMAT;
        }
	Dir->CurPos = -1;
        AMS_RETURN_ERRCODE(errno, EIN_READ, EVIA_READOLDMSDIR);
    }
    Dir->CurPos = AMS_DIRHEADSIZE;
    if (bcmp(DirHead, AMS_DIRECTORY_PREFIX_STRING, sizeof(AMS_DIRECTORY_PREFIX_STRING) - 1)) {
        if (DoComplain)
            AnnounceBadDirFormat(Dir->UNIXDir);
        AMS_RETURN_ERRCODE(EMSBADDIRFORMAT, EIN_BCMP, EVIA_READOLDMSDIR);
    }
    bcopy(DirHead + sizeof(AMS_DIRECTORY_PREFIX_STRING) - 1, ConvBuf, PADSIZE);
    ConvBuf[PADSIZE] = 0;
    Dir->DBMajorVersion = atoi(ConvBuf);
    debug(2, ("major version %d\n", Dir->DBMajorVersion));

    /*
     * NOTE THAT THE NEXT chunk of bytes are currently wasted -- these are
     * PADSIZE bytes that start at DirHead +
     * sizeof(AMS_DIRECTORY_PREFIX_STRING) + PADSIZE -1 and are being
     * reserved for future use
     */
    bcopy(DirHead + sizeof(AMS_DIRECTORY_PREFIX_STRING) + PADSIZE - 1, ConvBuf, PADSIZE);
    ConvBuf[PADSIZE] = 0;
    Dir->MaxChainVal = atoi(ConvBuf);
    if (Dir->DBMajorVersion < 4)
        Dir->MaxChainVal = 0;
    bcopy(DirHead + sizeof(AMS_DIRECTORY_PREFIX_STRING) + PADSIZE + PADSIZE - 1, ConvBuf, PADSIZE);     /* Currently obsolete ? */
    ConvBuf[PADSIZE] = 0;
    Dir->AttrCount = 0;
    if (Dir->DBMajorVersion >= 4) {
        char            ABuf[AMS_ATTRNAMEMAX + 1], *Ptrs[AMS_NUM_UATTRS];
        int             i;

        bcopy(DirHead + sizeof(AMS_DIRECTORY_PREFIX_STRING) + (3 * PADSIZE) + 3, AttrBuf, ATTNAMESLEN);
        while ((Dir->AttrCount < AMS_ATTRNAMEMAX) && (AttrBuf[(Dir->AttrCount * AMS_ATTRNAMEMAX) + 1] != '\0')) {
            strncpy(ABuf, &AttrBuf[(Dir->AttrCount * AMS_ATTRNAMEMAX) + 1],
                    AMS_ATTRNAMEMAX);
            ABuf[AMS_ATTRNAMEMAX] = '\0';
            debug(2, ("Found attribute named %s\n", ABuf));
            Ptrs[Dir->AttrCount] = malloc(1 + strlen(ABuf));
            if (!Ptrs[Dir->AttrCount]) {
                AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_READOLDMSDIR);
            }
            strcpy(Ptrs[Dir->AttrCount], ABuf);
            ++(Dir->AttrCount);
        }
        /* Always leave room for all of the attribute names here */
        if (!Dir->AttrNames) {
            Dir->AttrNames = (char **) permanentmalloc(AMS_NUM_UATTRS * sizeof(char *));
            if (!Dir->AttrNames) {
                AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_READOLDMSDIR);
            }
            bzero(Dir->AttrNames, AMS_NUM_UATTRS * sizeof(char *));
        }
        if (Dir->AttrCount) {
            for (i = 0; i < Dir->AttrCount; ++i) {
                if (Dir->AttrNames[i])
                    free(Dir->AttrNames[i]);
                Dir->AttrNames[i] = Ptrs[i];
            }
        }
        bcopy(DirHead + sizeof(AMS_DIRECTORY_PREFIX_STRING)
               + (3 * PADSIZE) + 4 + ATTNAMESLEN,
               Dir->LastMsgDate, AMS_DATESIZE); /* It'll either be a valid date, or all zeroes */
        if (MSDebugging & 2) {
            if (Dir->LastMsgDate[0]) {
                char time64[AMS_DATESIZE + 1];
                unsigned long timenum;

                bcopy(Dir->LastMsgDate, time64, AMS_DATESIZE);
                time64[AMS_DATESIZE] = '\0';
                timenum = (unsigned long) conv64tolong(time64);
                debug(2, ("LastMsgDate for %s is %s",
                           Dir->UNIXDir, ctime(&timenum)));
            }
            else
                debug(2, ("LastMsgDate for %s is not set...\n",
                           Dir->UNIXDir));
        }
    }
    else {
        Dir->AttrNames = NULL;
    }
    debug(2, ("Number of user attribute names: %d\n", Dir->AttrCount));
    if ((MSDebugging & 2) && Dir->AttrNames) {
        int             i;

        for (i = 0; i < Dir->AttrCount; ++i) {
            debug(2, ("Attribute #%d is named %s\n", i, Dir->AttrNames[i]));
        }
    }
    Dir->MessageCount = ((statbuf.st_size - AMS_DIRHEADSIZE) / AMS_SNAPSHOTSIZE);
    if (!(Dir->LastMsgDate[0])) {
        if (Dir->MessageCount > 0) {
            char            MySnapshot[AMS_SNAPSHOTSIZE];

            if (GetSnapshotByNumber(Dir, Dir->MessageCount - 1, MySnapshot)) {
                return (mserrcode);
            }
            strncpy(Dir->LastMsgDate, AMS_DATE(MySnapshot), AMS_DATESIZE);
            if (MSDebugging & 2) {
                unsigned long timenum;

                timenum = (unsigned long) conv64tolong(AMS_DATE(MySnapshot));
                debug(2, ("Set LastMsgDate for %s to %s",
                           Dir->UNIXDir, ctime(&timenum)));
            }
        }
        else {
            debug(2, ("No messages in %s, not setting LastMsgDate.\n", Dir->UNIXDir));
        }
    }
    Dir->FileDateWhenRead = statbuf.st_mtime;
    debug(2, ("File mod date is %ld\n", Dir->FileDateWhenRead));
    return (0);
}

char           *GenAuthField(Msg)
struct MS_Message *Msg;
{
    char            NameBuf[1000], *aname, *abuf;

    if (Msg->AuthName) {
        aname = Msg->AuthName;
    }
    else {
        GetCellularUserName(Msg->AuthUid, Msg->AuthCell, NameBuf, sizeof(NameBuf));
        aname = NameBuf;
    }
    abuf = malloc(25 + strlen(Msg->AuthCell ? Msg->AuthCell : WorkstationName) + (2 * strlen(aname)));
    if (!abuf)
        return (NULL);
#ifdef AFS_ENV
    if (Msg->AuthUid == ANONYMOUSID || Msg->AuthUid == ANYUSERID) {
        sprintf(abuf, "%d", ANONYMOUSID);
    }
    else
#endif                                 /* AFS_ENV */
    {
        char           *newpos;

        sprintf(abuf, "%d;%s;", Msg->AuthUid, Msg->AuthCell ? Msg->AuthCell : WorkstationName);
        newpos = &abuf[strlen(abuf)];
        while (*aname) {
            if (*aname == ';' || *aname == '\\')
                *newpos++ = '\\';
            *newpos++ = *aname++;
        }
        *newpos = '\0';
    }
    return (abuf);
}

WritePureFile(Msg, File, Overwrite, Mode)
struct MS_Message *Msg;
char           *File;
Boolean         Overwrite;
int             Mode;
{
    int             fd, myerrno, bytestoread, bytesleft;
    char            BigBuf[5000];

    debug(1, ("Write pure file %s\n", File));
    if (Msg->RawBits == NULL) {
        AMS_RETURN_ERRCODE(EMSBADMSGFORMAT, EIN_PARAMCHECK, EVIA_WRITEPUREFILE);
    }
/* Bogus -- what if not overwrite? */
    fd = open(File,
              Overwrite ? O_CREAT | O_TRUNC | O_WRONLY : O_CREAT | O_EXCL | O_WRONLY,
              Mode);
    if (fd < 0) {
#ifdef AFS_ENV
        if (AMS_ViceIsRunning) {
            if (errno == EFBIG) {
                char            ErrorText[100 + MAXPATHLEN];

                sprintf(ErrorText, EFBIGFormat, ap_Shorten(File));
                CriticalBizarreError(ErrorText);
            }
        }
#endif                                 /* AFS_ENV */
        myerrno = errno;
        (void) unlink(File);
        AMS_RETURN_ERRCODE(myerrno, EIN_OPEN, EVIA_WRITEPUREFILE);
    }
    if (!Msg->ParsedStuff->HeadBody[HP_AUTHENTICATED_AS]) {
        char            MyAuthHead[800], *authfield;

        authfield = GenAuthField(Msg);
        if (!authfield) {
            (void) unlink(File);
            close(fd);
            AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_WRITEPUREFILE);
        }
        sprintf(MyAuthHead, "X-Andrew-Authenticated-As: %s\n", authfield);
        free(authfield);
        writeall(fd, MyAuthHead, strlen(MyAuthHead));
    }
    if (strlen(Msg->RawBits) > Msg->HeadSize) {
        char            ErrorText[1000];

        sprintf(ErrorText, "Warning!  Message amt read in is %d but should be at least %d, reading %s.", Msg->HeadSize, strlen(Msg->RawBits), ap_Shorten(File));
        NonfatalBizarreError(ErrorText);
        Msg->HeadSize = strlen(Msg->RawBits);
    }
    if (writeall(fd, Msg->RawBits, Msg->HeadSize) != Msg->HeadSize) {
        myerrno = errno;
        (void) unlink(File);
        vclose(fd);
        AMS_RETURN_ERRCODE(myerrno, EIN_WRITE, EVIA_WRITEPUREFILE);
    }
    if (lseek(Msg->OpenFD, Msg->BodyOffsetInFD, L_SET) < 0) {
        myerrno = errno;
        (void) unlink(File);
        vclose(fd);
        AMS_RETURN_ERRCODE(myerrno, EIN_LSEEK, EVIA_WRITEPUREFILE);
    }
    bytesleft = Msg->FullSize - Msg->HeadSize;
    while (bytesleft > 0) {
        bytestoread = (bytesleft > (sizeof(BigBuf) - 1)) ? (sizeof(BigBuf) - 1) : bytesleft;
        if (read(Msg->OpenFD, BigBuf, bytestoread) != bytestoread) {
            myerrno = errno;
            (void) unlink(File);
            vclose(fd);
            AMS_RETURN_ERRCODE(myerrno, EIN_READ, EVIA_WRITEPUREFILE);
        }
        if (writeall(fd, BigBuf, bytestoread) != bytestoread) {
            myerrno = errno;
            (void) unlink(File);
            vclose(fd);
            AMS_RETURN_ERRCODE(myerrno, EIN_FWRITE, EVIA_WRITEPUREFILE);
        }
        bytesleft -= bytestoread;
    }
    if (vclose(fd)) {
#ifdef AFS_ENV
        if (AMS_ViceIsRunning) {
            if (errno == EFBIG) {
                char            ErrorText[100 + MAXPATHLEN];

                sprintf(ErrorText, EFBIGFormat, ap_Shorten(File));
                CriticalBizarreError(ErrorText);
            }
        }
#endif                                 /* AFS_ENV */
        myerrno = errno;
        (void) unlink(File);
        AMS_RETURN_ERRCODE(myerrno, EIN_VCLOSE, EVIA_WRITEPUREFILE);
    }
    return (0);
}

int             OpenMSDirectory(Dir, Code)
struct MS_Directory *Dir;
int             Code;
{
    int             openmode, fd, errsave, cleanupmess, Quietly;
    char            DirPath[1 + MAXPATHLEN];

    if (Dir->fd >= 0) {
        AMS_RETURN_ERRCODE(EMSDIRALREADYOPEN, EIN_PARAMCHECK, EVIA_OPENMSDIR);
    }
    sprintf(DirPath, "%s/%s", Dir->UNIXDir, MS_DIRNAME);
    Dir->BadScavenge = 0;
    if (Dir->CheckedWritable == 0) {
        Dir->CheckedWritable = 1;
        if (access(DirPath, W_OK)) {
            Dir->Writable = 0;
        }
        else {
            Dir->Writable = 1;
        }
    }
    switch (Code) {
        case MD_CREATE:
            openmode = O_RDWR | O_CREAT | O_TRUNC;
            break;
        case MD_OK:
        case MD_READ:
            openmode = O_RDONLY;
            break;
        case MD_WRITE:
        case MD_APPEND:
            openmode = O_RDWR;
            break;
        default:
            AMS_RETURN_ERRCODE(EINVAL, EIN_PARAMCHECK, EVIA_OPENMSDIR);
    }
    fd = open(DirPath, openmode, 0664);
    if (fd < 0) {
#ifdef AFS_ENV
        if (AMS_ViceIsRunning) {
            if (errno == EFBIG) {
                char            ErrorText[100 + MAXPATHLEN];

                sprintf(ErrorText, EFBIGFormat, ap_Shorten(DirPath));
                CriticalBizarreError(ErrorText);
            }
        }
#endif                                 /* AFS_ENV */
        AMS_RETURN_ERRCODE(errno, EIN_OPEN, EVIA_OPENMSDIR);
    }
    debug(128, ("Opened dir %s\n", DirPath));
    if (Code >= MD_WRITE) {
        if (osi_ExclusiveLockNoBlock(fd)) {
            errsave = errno;
            close(fd);
            if (Code >= MD_CREATE) {
                unlink(DirPath);
            }
            debug(128, ("Lock failed -- Closed dir %s\n", DirPath));
            AMS_RETURN_ERRCODE(errsave, EIN_FLOCK, EVIA_OPENMSDIR);
        }
    }
    Quietly = FALSE;
    if (Code >= MD_APPEND) {
        if (CheckHintDroppingPermission(Dir->UNIXDir)) {
            errsave = mserrcode;
            close(fd);
            if (Code >= MD_CREATE) {
                unlink(DirPath);
            }
            return (mserrcode);
        }
        cleanupmess = CheckMarksInProgress(Dir, &Quietly);
        if (MarkInProgress(Dir->UNIXDir)) {
            errsave = mserrcode;
            close(fd);
            if (Code >= MD_CREATE) {
                unlink(DirPath);
            }
            return (mserrcode);
        }
    }
    else {
        cleanupmess = 0;
    }
    Dir->fd = fd;
    Dir->CurPos = 0;
    Dir->OpenMode = Dir->MaxOpenMode = Code;
    if (Code == MD_OK) {
	Dir->OpenMode = Dir->MaxOpenMode = MD_READ;
        CacheDirectoryForClosing(Dir, MD_READ);
    }
    if (cleanupmess) {
        if (HandleMarksInProgress(Dir, Quietly)) {
            Dir->BadScavenge = 1;
            if (!Quietly) {
                char            ErrorText[500];

                sprintf(ErrorText, "Could not handle 'in progress' mail in directory %s", ap_Shorten(Dir->UNIXDir));
                NonfatalBizarreError(ErrorText);
            }
        }
    }
    Dir->LastOpenTimestamp = time(0);
    return (0);
}

CloseMSDir(Dir, CloseMode)
struct MS_Directory *Dir;
int             CloseMode;
{
    Dir->CurPos = -1;
    if (Dir->fd < 0) {
        AMS_RETURN_ERRCODE(EMSDIRNOTOPEN, EIN_PARAMCHECK, EVIA_CLOSEMSDIR);
    }
    if (CloseMode < Dir->OpenMode) {
        /* It is already open for a higher purpose */
        debug(128, ("Skipping close of %s due to low open mode (%d, %d)\n", Dir->UNIXDir, CloseMode, Dir->OpenMode));
        return (0);
    }
    FreeIDs(Dir);
    if (SnapshotBufDir == Dir) SnapshotBufDir = 0;
    if (vclose(Dir->fd)) {
        debug(128, ("Close failed on dir %s\n", Dir->UNIXDir));
        Dir->fd = -1;
        Dir->OpenMode = -1;
#ifdef AFS_ENV
        if (AMS_ViceIsRunning) {
            if (errno == EFBIG) {
                char            ErrorText[100 + MAXPATHLEN];

                sprintf(ErrorText, EFBIGFormat, ap_Shorten(Dir->UNIXDir));
                CriticalBizarreError(ErrorText);
            }
        }
#endif                                 /* AFS_ENV */
        AMS_RETURN_ERRCODE(errno, EIN_VCLOSE, EVIA_CLOSEMSDIR);
    }
    if ((Dir->MaxOpenMode >= MD_APPEND) && Dir->BadScavenge == 0 && UnmarkInProgress(Dir->UNIXDir)) {
        char            ErrBuf[100 + MAXPATHLEN];

        sprintf(ErrBuf, "Unmark in progress failed on dir %s; next append may be needlessly slow.", ap_Shorten(Dir->UNIXDir));
        NonfatalBizarreError(ErrBuf);
    }
    debug(128, ("Closed dir %s\n", Dir->UNIXDir));
    if (Dir->MaxOpenMode >= MD_APPEND) {
        DropHint(Dir->UNIXDir);
    }
    Dir->fd = -1;
    Dir->OpenMode = Dir->MaxOpenMode = -1;
    return (0);
}

char           *fixDate(dPtr)
char           *dPtr;
{                                      /* Fix up the base-64 value (size
                                        * AMS_DATESIZE) at dPtr.  Return a
                                        * pointer to it. */
    int             X;
    register char  *cp;

    cp = dPtr;
    for (X = AMS_DATESIZE; X > 0; X--) {
        if (*cp == '_')
            *cp = ':';
        ++cp;
    }
    return dPtr;
}

GetSnapshotByID(Dir, id, msgnum, snapshot)
struct MS_Directory *Dir;
char           *id, *snapshot;
int            *msgnum;
{
    int             i, startpt, readct;
    long            fpos;
    char           *curSnapshot;
    unsigned long int uLong;

    if (Dir->fd < 0) {
        AMS_RETURN_ERRCODE(EMSDIRNOTOPEN, EIN_PARAMCHECK, EVIA_GETSNAPSHOTBYID);
    }
    if (Dir != SnapshotBufDir) {
	SnapshotBufDir = Dir;
	SnapshotBufLen = 0;
    }

    startpt = Dir->LastIDHit;
    if (startpt < 0 || startpt >= Dir->MessageCount)
        startpt = Dir->MessageCount - 1;
    if (startpt < 0)
        startpt = 0;
    if (startpt >= SnapshotBufStart && startpt < SnapshotBufStart + SnapshotBufLen) {
	startpt = SnapshotBufStart;
    }
    for (i = startpt; i < Dir->MessageCount; ++i) {
	if (i < SnapshotBufStart || i >= SnapshotBufStart + SnapshotBufLen) {
	    fpos = AMS_DIRHEADSIZE + (i * AMS_SNAPSHOTSIZE);
	    if (Dir->CurPos != fpos && lseek(Dir->fd, fpos, L_SET) < 0) {
		AMS_RETURN_ERRCODE(errno, EIN_LSEEK, EVIA_GETSNAPSHOTBYID);
	    }
	    readct = read(Dir->fd, SnapshotBuf, sizeof(SnapshotBuf));
	    if (readct < AMS_SNAPSHOTSIZE) {
		if (readct >= 0) {
		    AnnounceBadDirFormat(Dir->UNIXDir);
		    errno = EMSBADDIRFORMAT;
		}
		Dir->CurPos = -1;
		SnapshotBufLen = 0;
		AMS_RETURN_ERRCODE(errno, EIN_READ, EVIA_GETSNAPSHOTBYID);
	    }
	    Dir->CurPos = fpos + readct;
	    SnapshotBufStart = i;
	    SnapshotBufLen = readct / AMS_SNAPSHOTSIZE;
	}

	curSnapshot = SnapshotBuf + (i-SnapshotBufStart)*AMS_SNAPSHOTSIZE;

        if (i < Dir->NumIDs) {
            bcopy(AMS_CHAIN(curSnapshot), &uLong, sizeof(unsigned long));
            Dir->IDs[i].Chn = ntohl(uLong);
            bcopy(AMS_MIDHASH(curSnapshot), &uLong, sizeof(unsigned long));
            Dir->IDs[i].midH = ntohl(uLong);
            bcopy(AMS_REPLYHASH(curSnapshot), &uLong, sizeof(unsigned long));
            Dir->IDs[i].repH = ntohl(uLong);
        }
        if (!strcmp(id, AMS_ID(curSnapshot))) {
	    bcopy(curSnapshot, snapshot, AMS_SNAPSHOTSIZE);
            *msgnum = i;
            fixDate(AMS_DATE(snapshot));
           if (Dir->Writable) {
                AMS_SET_ATTRIBUTE(snapshot, AMS_ATT_MAYMODIFY);
            }
            else {
                AMS_UNSET_ATTRIBUTE(snapshot, AMS_ATT_MAYMODIFY);
            }
            Dir->LastIDHit = i;
            return (0);
        }
    }
    for (i = startpt - 1; i >= 0; --i) {
	if (i < SnapshotBufStart || i >= SnapshotBufStart + SnapshotBufLen) {
	    SnapshotBufStart = i - MAXSNAPSHOTBUFLEN + 1;
	    if (SnapshotBufStart < 0) SnapshotBufStart = 0;

	    fpos = AMS_DIRHEADSIZE + (SnapshotBufStart * AMS_SNAPSHOTSIZE);
	    if (lseek(Dir->fd, fpos, L_SET) < 0) {
		AMS_RETURN_ERRCODE(errno, EIN_LSEEK, EVIA_GETSNAPSHOTBYID);
	    }
	    readct = read(Dir->fd, SnapshotBuf, sizeof(SnapshotBuf));
	    if (readct < (i-SnapshotBufStart+1)*AMS_SNAPSHOTSIZE) {
		if (readct >= 0) {
		    AnnounceBadDirFormat(Dir->UNIXDir);
		    errno = EMSBADDIRFORMAT;
		}
		Dir->CurPos = -1;
		SnapshotBufLen = 0;
		AMS_RETURN_ERRCODE(errno, EIN_READ, EVIA_GETSNAPSHOTBYID);
	    }
	    Dir->CurPos = fpos + readct;
	    SnapshotBufLen = readct / AMS_SNAPSHOTSIZE;
	}

	curSnapshot = SnapshotBuf + (i-SnapshotBufStart)*AMS_SNAPSHOTSIZE;

        if (i < Dir->NumIDs) {
            bcopy(AMS_CHAIN(curSnapshot), &uLong, sizeof(unsigned long));
            Dir->IDs[i].Chn = ntohl(uLong);
            bcopy(AMS_MIDHASH(curSnapshot), &uLong, sizeof(unsigned long));
            Dir->IDs[i].midH = ntohl(uLong);
            bcopy(AMS_REPLYHASH(curSnapshot), &uLong, sizeof(unsigned long));
            Dir->IDs[i].repH = ntohl(uLong);
        }
        if (!strcmp(id, AMS_ID(curSnapshot))) {
	    bcopy(curSnapshot, snapshot, AMS_SNAPSHOTSIZE);
            *msgnum = i;
            fixDate(AMS_DATE(snapshot));
            if (Dir->Writable) {
                AMS_SET_ATTRIBUTE(snapshot, AMS_ATT_MAYMODIFY);
            }
            else {
                AMS_UNSET_ATTRIBUTE(snapshot, AMS_ATT_MAYMODIFY);
            }
            Dir->LastIDHit = i;
            return (0);
        }
    }
    AMS_RETURN_ERRCODE(EMSNOSUCHMESSAGE, EIN_PARAMCHECK, EVIA_GETSNAPSHOTBYID);
}

/* Note in the following routine that numbers start a zero
        and go to Dir->MessageCount -1 */

GetSnapshotByNumber(Dir, msgnum, snapshot)
struct MS_Directory *Dir;
char           *snapshot;
int             msgnum;
{
    int             readct;
    long            fpos;
    unsigned long int uLong;

    if (msgnum >= Dir->MessageCount || (msgnum < 0)) {
        AMS_RETURN_ERRCODE(EINVAL, EIN_PARAMCHECK, EVIA_GETSNAPSHOTBYNUMBER);
    }
    if (Dir->fd < 0) {
        AMS_RETURN_ERRCODE(EMSDIRNOTOPEN, EIN_PARAMCHECK, EVIA_GETSNAPSHOTBYNUMBER);
    }

    if (Dir != SnapshotBufDir) {
	SnapshotBufDir = Dir;
	SnapshotBufLen = 0;
    }

    if (msgnum < SnapshotBufStart || msgnum >= SnapshotBufStart + SnapshotBufLen) {
	fpos = AMS_DIRHEADSIZE + (msgnum * AMS_SNAPSHOTSIZE);
	if (fpos != Dir->CurPos && lseek(Dir->fd, fpos, L_SET) < 0) {
	    AMS_RETURN_ERRCODE(errno, EIN_LSEEK, EVIA_GETSNAPSHOTBYNUMBER);
	}
	readct = read(Dir->fd, SnapshotBuf, sizeof(SnapshotBuf));
	if (readct < AMS_SNAPSHOTSIZE) {
	    if (readct >= 0) {
		AnnounceBadDirFormat(Dir->UNIXDir);
		errno = EMSBADDIRFORMAT;
	    }
	    Dir->CurPos = -1;
	    SnapshotBufLen = 0;
	    AMS_RETURN_ERRCODE(errno, EIN_READ, EVIA_GETSNAPSHOTBYNUMBER);
	}
	Dir->CurPos = fpos + readct;
	SnapshotBufStart = msgnum;
	SnapshotBufLen = readct / AMS_SNAPSHOTSIZE;
    }
    bcopy(SnapshotBuf + (msgnum-SnapshotBufStart)*AMS_SNAPSHOTSIZE,
	  snapshot, AMS_SNAPSHOTSIZE);
    fixDate(AMS_DATE(snapshot));
    if (Dir->Writable) {
        AMS_SET_ATTRIBUTE(snapshot, AMS_ATT_MAYMODIFY);
    }
    else {
        AMS_UNSET_ATTRIBUTE(snapshot, AMS_ATT_MAYMODIFY);
    }
    if (msgnum < Dir->NumIDs) {
        bcopy(AMS_CHAIN(snapshot), &uLong, sizeof(unsigned long));
        Dir->IDs[msgnum].Chn = ntohl(uLong);
        bcopy(AMS_MIDHASH(snapshot), &uLong, sizeof(unsigned long));
        Dir->IDs[msgnum].midH = ntohl(uLong);
        bcopy(AMS_REPLYHASH(snapshot), &uLong, sizeof(unsigned long));
        Dir->IDs[msgnum].repH = ntohl(uLong);
    }
    return (0);
}

RewriteSnapshotInDirectory(Dir, num, snapshot)
struct MS_Directory *Dir;
int             num;
char           *snapshot;
{
    long fpos;

    if (Dir->fd < 0) {
        AMS_RETURN_ERRCODE(EMSDIRNOTOPEN, EIN_PARAMCHECK, EVIA_REWRITESNAPSHOTINDIR);
    }
    fpos = AMS_DIRHEADSIZE + (num * AMS_SNAPSHOTSIZE);
    if (fpos != Dir->CurPos && lseek(Dir->fd, fpos, L_SET) < 0) {
        AMS_RETURN_ERRCODE(errno, EIN_LSEEK, EVIA_REWRITESNAPSHOTINDIR);
    }
    if (writeall(Dir->fd, snapshot, AMS_SNAPSHOTSIZE) != AMS_SNAPSHOTSIZE) {
	Dir->CurPos = -1;
        AMS_RETURN_ERRCODE(errno, EIN_WRITE, EVIA_REWRITESNAPSHOTINDIR);
    }
    Dir->CurPos = fpos + AMS_SNAPSHOTSIZE;
    if (Dir == SnapshotBufDir && num >= SnapshotBufStart &&
	num < SnapshotBufStart + SnapshotBufLen) {
	bcopy(snapshot, SnapshotBuf + (num-SnapshotBufStart)*AMS_SNAPSHOTSIZE,
	      AMS_SNAPSHOTSIZE);
    }
    return (0);
}

static int      CheckHintDroppingPermission(Dirname)
char           *Dirname;
{
    char            UpdateFileName[1 + MAXPATHLEN];

    debug(2, ("Checking hint dropping permission.\n"));
    if (FindTreeRoot(Dirname, UpdateFileName, FALSE))
        return (0);                    /* no MUF here either */
    strcat(UpdateFileName, "/");
    strcat(UpdateFileName, MS_MASTERDIR);
    if (access(UpdateFileName, W_OK)) {
        if (errno == ENOENT) {
            return (0);                /* No MUF on this tree */
        }
        AMS_RETURN_ERRCODE(errno, EIN_ACCESS, EVIA_UPDATEUPDATES);
    }
    return (0);
}

char           *GetMyHost()
{
    static char     ThisHost[150] = "";

    if (ThisHost[0] == '\0') {
        GetHostDomainName(ThisHost, sizeof(ThisHost));
        if (ThisHost[0] == '\0') {
            strcpy(ThisHost, "unknown-host");
        }
    }
    return (ThisHost);
}

DropHint(Dirname)
char           *Dirname;
{
    char            ScratchBuf[1 + MAXPATHLEN], UpdateFileName[1 + MAXPATHLEN], *maybehere;
    struct stat statbuf;
    int             fd;

    debug(2, ("Dropping hint for %s\n", Dirname));
    if (FindTreeRoot(Dirname, UpdateFileName, FALSE)) {
        AMS_RETURN_ERRCODE(EINVAL, EIN_PARAMCHECK, EVIA_UPDATEUPDATES);
    }
    strcat(UpdateFileName, "/");
    strcat(UpdateFileName, MS_MASTERDIR);
    if (stat(UpdateFileName, &statbuf) && errno == ENOENT) {
        return (0);                    /* No MUF on this tree */
    }
    maybehere = &UpdateFileName[strlen(UpdateFileName)];
    strcpy(maybehere, "/");
    strcat(maybehere, GetMyHost());
    debug(2, ("Possible update file name is %s\n", UpdateFileName));
    if (access(UpdateFileName, W_OK)) {
	strcpy(maybehere, "/other");
	if (access(UpdateFileName, W_OK)) {
	    *maybehere = '\0';
	}
    }
    strcat(UpdateFileName, "/");
    strcat(UpdateFileName, MS_MASTERHINT);
    BuildNickName(Dirname, ScratchBuf);
    strcat(UpdateFileName, ScratchBuf);
    fd = open(UpdateFileName, O_CREAT | O_WRONLY, 0666);
    if (fd < 0 && errno == EACCES) {   /* tolerate insert-only permission */
        if (access(ScratchBuf, F_OK) == 0) {
            debug(2, ("OK: Hint file '%s' already exists; things should be fine.\n", UpdateFileName));
            return (0);
        }
    }
    if (fd < 0) {
        sprintf(ScratchBuf, "Cannot open hint file '%s'", ap_Shorten(UpdateFileName));
        NonfatalBizarreError(ScratchBuf);
        AMS_RETURN_ERRCODE(errno, EIN_OPEN, EVIA_UPDATEUPDATES);
    }
    close(fd);                         /* It was already created, it is
                                        * empty, who cares about a close
                                        * error? */
    debug(2, ("OK: Left hint file '%s', things should be fine.\n", UpdateFileName));
    return (0);
}


#define NUMLOCKRETRIES 60
#define LOCKSLEEPPERIOD 60

int      MS_LockMUF(LockDirName, lockfd)
char           *LockDirName;
int            *lockfd;
{
    int             errsave, locktriesleft = NUMLOCKRETRIES;
    char            LockFileName[1 + MAXPATHLEN], ErrorText[1 + MAXPATHLEN];

    debug(1, ("Trying to lock master update file %s\n", LockDirName));
    sprintf(LockFileName, "%s/%s", LockDirName, MS_UPDATELOCK);
    while (TRUE) {
        *lockfd = open(LockFileName, osi_O_READLOCK | O_CREAT, 0664);
        if (*lockfd < 0) {
            AMS_RETURN_ERRCODE(errno, EIN_OPEN, EVIA_LOCKMASTERFILE);
        }
        else {
            if (!osi_ExclusiveLockNoBlock(*lockfd)) {
                return (0);
            }
            errsave = errno;
            close(*lockfd);
            *lockfd = -1;
            if ((errsave != EWOULDBLOCK) || (locktriesleft-- <= 0)) {
                AMS_RETURN_ERRCODE(errsave, EIN_FLOCK, EVIA_LOCKMASTERFILE);
            }
            sprintf(ErrorText, "Update file %s is locked (%d); sleeping %d seconds (%d retries remain)...", ap_Shorten(LockFileName), errsave, LOCKSLEEPPERIOD, locktriesleft);
            NonfatalBizarreError(ErrorText);
            sleep(LOCKSLEEPPERIOD);
        }
    }
}

MS_TakeHints(DoAll, ProtFailures)
int             DoAll;
int            *ProtFailures;
{
    int             i = 0;
    char            PathElt[1 + MAXPATHLEN];

    NonfatalBizarreError(DoAll ? "Taking ALL hints" : "Taking hints");
    *ProtFailures = 0;
    while (MS_GetSearchPathEntry(i++, PathElt, MAXPATHLEN) == 0) {
        if (CheckPathForMUFHints(PathElt, DoAll)) {
            if (AMS_ERRNO == EACCES) {
                ++*ProtFailures;
            }
            else {
                return (mserrcode);
            }
        }
    }
    return (0);
}

static int      CheckPathForMUFHints(PathElt, DoAll)
char           *PathElt;
int             DoAll;
{
    char            MUFDir[1 + MAXPATHLEN];
    int             i, lockfd;

    debug(1, ("Checking for %s MUF hints in tree %s.\n", DoAll ? "ALL" : "MY", PathElt));
    strcpy(MUFDir, PathElt);
    strcat(MUFDir, "/");
    strcat(MUFDir, MS_MASTERDIR);
    if (access(MUFDir, W_OK)) {
        if (errno == ENOENT)
            return (0);                /* No MUF to check */
        AMS_RETURN_ERRCODE(errno, EIN_ACCESS, EVIA_UPDATEUPDATES);
    }
    if (MS_LockMUF(MUFDir, &lockfd)) {
        return (mserrcode);
    }
    i = CheckForMUFHints(MUFDir, DoAll, PathElt);
    mserrcode = ClearUpdates(PathElt);
    close(lockfd);
    return (i ? i : mserrcode);
}

#define MAXUPDATES 30

static struct takenupdate {
    char           *FullDirName;
    char           *HintFileName;
    char            date64[AMS_DATESIZE];
    long            modtime;
    int             Deleted;
}               TakenUpdates[MAXUPDATES + 1];

static int      UpdatesPending = 0;

PlanHint(PathElt, shortname, MUFDir, MUFHint)
char           *PathElt, *shortname, *MUFDir, *MUFHint;
{
    char           *fulldir, *fullhint;
    struct MS_Directory *Dir;
    int             saveerr, i;

    debug(1, ("PlanHint %s /%s %s /%s\n", PathElt, shortname, MUFDir, MUFHint));
    fullhint = malloc(2 + strlen(MUFDir) + strlen(MUFHint));
    if (!fullhint)
        AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_UPDATEUPDATES);
    fulldir = malloc(2 + strlen(PathElt) + strlen(shortname));
    if (!fulldir) {
        free(fullhint);
        AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_UPDATEUPDATES);
    }
    sprintf(fullhint, "%s/%s", MUFDir, MUFHint);
    sprintf(fulldir, "%s/%s", PathElt, shortname);
    for (i = 0; i < UpdatesPending; ++i) {
        if (!strcmp(fulldir, TakenUpdates[i].FullDirName)) {
            debug(2, ("Aha!  Would have hit a duplicate!\n"));
            /* Skip it and delete the second hint file */
            unlink(fullhint);          /* Can ignore errors, I guess */
            free(fullhint);
            free(fulldir);
            return (0);
        }
    }
    if (ReadOrFindMSDir(fulldir, &Dir, MD_READ)) {
        char            ErrBuf[100 + MAXPATHLEN];

        if (errno != ENOENT) {
            free(fullhint);
            free(fulldir);
            return (mserrcode);
        }
        /* It has been deleted! */
        sprintf(ErrBuf, "Hint found for non-existent folder %s; ignoring...", ap_Shorten(fulldir));
        NonfatalBizarreError(ErrBuf);
        TakenUpdates[UpdatesPending].Deleted = 1;
    }
    else {
        struct stat     stbuf;

        TakenUpdates[UpdatesPending].Deleted = 0;
        if (Dir->MessageCount > 0) {
            char            SnapshotDum[1 + AMS_SNAPSHOTSIZE];

            if (GetSnapshotByNumber(Dir, Dir->MessageCount - 1, SnapshotDum)) {
                saveerr = mserrcode;
                CloseMSDir(Dir, MD_READ);
                free(fullhint);
                free(fulldir);
                return (saveerr);
            }
            strncpy(TakenUpdates[UpdatesPending].date64, AMS_DATE(SnapshotDum), AMS_DATESIZE);
        }
        else {
            strcpy(TakenUpdates[UpdatesPending].date64, "000000");
        }
        CloseMSDir(Dir, MD_READ);
        if (stat(fulldir, &stbuf)) {
            free(fullhint);
            free(fulldir);
            AMS_RETURN_ERRCODE(errno, EIN_STAT, EVIA_REBUILDMASTERUPS);
        }
        TakenUpdates[UpdatesPending].modtime = stbuf.st_mtime;
    }
    TakenUpdates[UpdatesPending].FullDirName = fulldir;
    TakenUpdates[UpdatesPending].HintFileName = fullhint;

    if (++UpdatesPending >= MAXUPDATES)
        return (ClearUpdates(PathElt));
    return (0);
}

/* The following is like a strcmp, but compares two pathnames
 * one element at a time.  This is to preserve the same ordering in
 * a MUF as appears in a SubscriptionMap, which is a preorder directory
 * traversal. -- bobg, 10/18/88
 */

static int      ComparePathsByElts(p1, p2)
char           *p1, *p2;
{
    char           *elt1start, *elt1end, *elt2start, *elt2end;
    int             result = 0;

    elt1start = strchr(p1, '/');        /* Assuming slashes, not dots; hope
                                        * that's OK */
    elt2start = strchr(p2, '/');
    while (elt1start && elt2start) {
        elt1end = strchr(++elt1start, '/');
        elt2end = strchr(++elt2start, '/');
        if (elt1end)
            *elt1end = '\0';
        if (elt2end)
            *elt2end = '\0';
        result = strcmp(elt1start, elt2start);
        if (elt1end)
            *elt1end = '/';
        if (elt2end)
            *elt2end = '/';
        if (result)
            return (result);
        elt1start = elt1end;
        elt2start = elt2end;
    }
    if (elt1start == 0 && elt2start == 0)
        return (0);
    else
        return (elt1start != 0 ? 1 : -1);       /* (can't count on
                                                 * pointers being
                                                 * arithmetically
                                                 * positive) */
}

static int      CompareTakenUpdates(t1, t2)
struct takenupdate *t1, *t2;
{
    return (ComparePathsByElts(t1->FullDirName, t2->FullDirName));
}

static int      ClearUpdates(PathElt)
char           *PathElt;
{
    char            UpdateFileName[1 + MAXPATHLEN], NewUpdateFileName[1 + MAXPATHLEN], LineBuf[100 + MAXPATHLEN], NewLine[1 + MAXPATHLEN + 50], *Dirname, ErrorText[200 + MAXPATHLEN + MAXPATHLEN], *s, *t, *u, *v, PrevLine[100 + MAXPATHLEN];
    FILE           *oldR, *newW;
    int             saveerr, code, whichupdate = 0, mistakes;
    Boolean         TryAgain = FALSE;

    debug(1, ("ClearUpdates %s\n", PathElt));
    if (UpdatesPending <= 0)
        return (0);

    qsort(TakenUpdates, UpdatesPending, sizeof(struct takenupdate), CompareTakenUpdates);

    strcpy(UpdateFileName, PathElt);
    strcat(UpdateFileName, "/");
    strcat(UpdateFileName, MS_MASTERUPDATE);
    if (access(UpdateFileName, W_OK)) {
        FreeUpdates(FALSE);
        if (errno == ENOENT) {
            return (0);                /* No update file means we don't
                                        * create one */
        }
        AMS_RETURN_ERRCODE(errno, EIN_ACCESS, EVIA_UPDATEUPDATES);
    }
    oldR = fopen(UpdateFileName, "r");
    if (!oldR) {
        FreeUpdates(FALSE);
        AMS_RETURN_ERRCODE(errno, EIN_FOPEN, EVIA_UPDATEUPDATES);
    }
    strcpy(NewUpdateFileName, UpdateFileName);
    strcat(NewUpdateFileName, ".NEW");
    newW = fopen(NewUpdateFileName, "w");
    if (!newW) {
        fclose(oldR);
        FreeUpdates(FALSE);
        AMS_RETURN_ERRCODE(errno, EIN_FOPEN, EVIA_UPDATEUPDATES);
    }

    Dirname = TakenUpdates[whichupdate].FullDirName;
    if (TakenUpdates[whichupdate].Deleted) {
        NewLine[0] = '\0';
    }
    else {
        sprintf(NewLine, "%s %s %d\n", Dirname, TakenUpdates[whichupdate].date64, TakenUpdates[whichupdate].modtime);
    }

    PrevLine[0] = '\0';
    while (TryAgain || fgets(LineBuf, sizeof(LineBuf), oldR)) {
        if (!TryAgain) {               /* Did read a new line */
            if (u = strchr(LineBuf, ' ')) {
                *u = '\0';
                fixDate(u + 1);
            }
            if (v = strchr(PrevLine, ' ')) {
                *v = '\0';
                fixDate(v + 1);
            }
            if (ComparePathsByElts(LineBuf, PrevLine) <= 0) {
                if (u)
                    *u = ' ';
                if (v)
                    *v = ' ';
                s = strchr(LineBuf, '\n');
                if (s)
                    *s = '\0';
                t = strchr(PrevLine, '\n');
                if (t)
                    *t = '\0';
                sprintf(ErrorText, "Mis-ordered MUF line (discarding & hinting): %s (PrevLine is %s), in %s", LineBuf, PrevLine, ap_Shorten(UpdateFileName));
                if (t)
                    *t = '\n';
                NonfatalBizarreError(ErrorText);
                s = strchr(LineBuf, ' ');
                if (s)
                    *s = '\0';
                DropHint(LineBuf);
                continue;
            }
            else {
                if (u)
                    *u = ' ';
                if (v)
                    *v = ' ';
            }
            strcpy(PrevLine, LineBuf);
        }
        TryAgain = FALSE;

        /*
         * Do not propogate bogus lines. In the past, illegal chars have
         * gotten into MUF files from bad Ethernet interfaces, sigh...
         */

        mistakes = 0;
        for (s = LineBuf; *s; ++s) {
            if (!isprint(*s) && !isspace(*s)) {
                sprintf(ErrorText, "Illegal character in update file %s: ASCII %d (decimal)", ap_Shorten(UpdateFileName), *s);
                NonfatalBizarreError(ErrorText);
                *s = 'X';              /* The error message should be kind
                                        * to console */
                ++mistakes;
            }
        }
        if (LineBuf[0] != '/') {
            ++mistakes;                /* Must be a full path name */
        }
        if (mistakes) {
            sprintf(ErrorText, "Discarding illegal MUF %s entry: %s", ap_Shorten(UpdateFileName), LineBuf);
            NonfatalBizarreError(ErrorText);
            continue;
        }
        if (whichupdate >= UpdatesPending) {    /* finished with hint
                                                 * entries */
            fputs(LineBuf, newW);
            continue;
        }
        s = strchr(LineBuf, ' ');
        if (s)
            *s = '\0';
        code = ComparePathsByElts(Dirname, LineBuf);
        if (s)
            *s = ' ';
        if (code <= 0) {
            /* We have just NOW Passed it or this is exactly it. */
            fputs(NewLine, newW);
            if (code) {
                TryAgain = TRUE;

                /*
                 * Used to be fputs(LineBuf, newW) but this screwed up
                 * when two new things needed to be inserted consecutively
                 */
            }
            if (++whichupdate < UpdatesPending) {
                Dirname = TakenUpdates[whichupdate].FullDirName;
                if (TakenUpdates[whichupdate].Deleted) {
                    NewLine[0] = '\0';
                }
                else {
                    sprintf(NewLine, "%s %s %d\n", Dirname, TakenUpdates[whichupdate].date64, TakenUpdates[whichupdate].modtime);
                }
            }
        }
        else {
            /* We have not yet gotten there */
            fputs(LineBuf, newW);
        }
    }
    while (whichupdate < UpdatesPending) {
        fputs(NewLine, newW);
        if (++whichupdate < UpdatesPending) {
            if (TakenUpdates[whichupdate].Deleted) {
                NewLine[0] = '\0';
            }
            else {
                sprintf(NewLine, "%s %s %d\n", TakenUpdates[whichupdate].FullDirName, TakenUpdates[whichupdate].date64, TakenUpdates[whichupdate].modtime);
            }
        }
    }
    fclose(oldR);
    errno = 0;                         /* Paranoia regarding ferror */
    if (ferror(newW) || feof(newW)) {
        saveerr = errno;
        fclose(newW);
        unlink(NewUpdateFileName);
        FreeUpdates(FALSE);
        AMS_RETURN_ERRCODE(saveerr, EIN_FERROR, EVIA_UPDATEUPDATES);
    }
    if (vfclose(newW)) {
        saveerr = errno;
        unlink(NewUpdateFileName);
        FreeUpdates(FALSE);
        AMS_RETURN_ERRCODE(saveerr, EIN_VFCLOSE, EVIA_UPDATEUPDATES);
    }
    if (rename(NewUpdateFileName, UpdateFileName)) {
        saveerr = errno;
        unlink(NewUpdateFileName);
        FreeUpdates(FALSE);
        AMS_RETURN_ERRCODE(saveerr, EIN_RENAME, EVIA_UPDATEUPDATES);
    }
    FreeUpdates(TRUE);
    return (0);
}

static void     FreeUpdates(UnlinkHints)
int             UnlinkHints;
{
    while (--UpdatesPending >= 0) {
        if (UnlinkHints)
            unlink(TakenUpdates[UpdatesPending].HintFileName);
        free(TakenUpdates[UpdatesPending].HintFileName);
        free(TakenUpdates[UpdatesPending].FullDirName);
    }
    UpdatesPending = 0;
}

static int      CheckForMUFHints(MUFDir, DoAll, PathElt)
char           *MUFDir, *PathElt;
int             DoAll;
{
    DIR            *dirp;
    DIRENT_TYPE    *dp;
    char           *s, namebuf[1 + MAXPATHLEN];

    debug(1, ("Looking for hints in directory %s\n", MUFDir));
    dirp = opendir(MUFDir);
    if (!dirp) {
        AMS_RETURN_ERRCODE(errno, EIN_OPENDIR, EVIA_UPDATEUPDATES);
    }
    while (dp = readdir(dirp)) {
        if (strncmp(dp->d_name, MS_MASTERHINT, sizeof(MS_MASTERHINT) - 1)) {
            if ((*dp->d_name != '.') && (DoAll || !strcmp(dp->d_name, GetMyHost()))) {
                struct stat     stbuf;

                debug(2, ("Considering recursion into %s\n", dp->d_name));
                strcpy(namebuf, MUFDir);
                strcat(namebuf, "/");
                strcat(namebuf, dp->d_name);
                if (stat(namebuf, &stbuf)) {
                    char            ErrorText[100 + MAXPATHLEN];

                    sprintf(ErrorText, "Cannot stat %s/%s; may be missing some hints", ap_Shorten(MUFDir), dp->d_name);
                    NonfatalBizarreError(ErrorText);
                    continue;
                }
                if ((stbuf.st_mode & S_IFMT) == S_IFDIR) {
                    CheckForMUFHints(namebuf, DoAll, PathElt);
                }
            }
            else {
                debug(2, ("Ignoring %s\n", dp->d_name));
            }
            continue;
        }
        strcpy(namebuf, dp->d_name + sizeof(MS_MASTERHINT) - 1);
        for (s = namebuf; *s; ++s) {
            if (*s == '.')
                *s = '/';
        }
        debug(2, ("Handling update hint for %s.\n", namebuf));
        if (PlanHint(PathElt, namebuf, MUFDir, dp->d_name)) {
            closedir(dirp);
            return (mserrcode);
        }
    }
    closedir(dirp);
    return (0);
}

static void     AnnounceBadDirFormat(line)
char           *line;
{
    char            ErrorText[100 + MAXPATHLEN];

    sprintf(ErrorText, "The folder '%s' is corrupted & needs reconstruction.", ap_Shorten(line));
    CriticalBizarreError(ErrorText);
}

static void     BuildAttrNameBuf(Dir, AttrBuf)
struct MS_Directory *Dir;
char           *AttrBuf;
{
    int             i;
    char           *s = AttrBuf;

    bzero(AttrBuf, ATTNAMESLEN);
    if (!Dir->AttrNames)
        return;
    for (i = 0; i < Dir->AttrCount; ++i) {
        strncpy(s, Dir->AttrNames[i], AMS_ATTRNAMEMAX);
        s[AMS_ATTRNAMEMAX - 1] = '\0';
        s += AMS_ATTRNAMEMAX;
    }
}

EmitBE2PrefixAndLSeekPastIt(fd, fp, SkippedBytes)
int             fd;
FILE           *fp;
int            *SkippedBytes;
{
    struct LinePromState *lps;
    char            Lbuf[2000];        /* actually, 120 *should* suffice */
    int             oldpos, curpos;

    *SkippedBytes = 0;
    if ((oldpos = lseek(fd, 0, L_INCR)) < 0) {
        AMS_RETURN_ERRCODE(errno, EIN_LSEEK, EVIA_EMITBE2PREFIX);
    }
    BE2LinePromoteInit(&lps);
    curpos = 0;
    while (TRUE) {
        if (read(fd, &Lbuf[curpos], 1) != 1)
            break;
        if (Lbuf[curpos] == '\n') {
            Lbuf[curpos + 1] = '\0';
            if (BE2LinePromote(Lbuf, lps) != 2)
                break;
            fputs(Lbuf, fp);
            *SkippedBytes += curpos;
            oldpos += curpos;
            curpos = 0;
        }
        else {
            ++curpos;
        }
    }
    BE2LinePromoteEnd(lps);
    if (lseek(fd, oldpos, L_SET) < 0) {
        AMS_RETURN_ERRCODE(errno, EIN_LSEEK, EVIA_EMITBE2PREFIX);
    }
    return (0);
}

GetCellularUserName(uid, cell, NameBuf, lim)
int             uid, lim;
char           *cell, *NameBuf;
{
    struct passwd  *p;
    char           *newName;

    NameBuf[0] = '\0';

    p = getcpwuid(uid, cell ? cell : MyMailDomain);
    if (p) {
        newName = NULL;
        GetNameFromGecos(p->pw_gecos, p->pw_name, (cell ? cell : MyMailDomain), &newName);
        strncpy(NameBuf, (newName ? newName : p->pw_gecos), lim);
        if (newName)
            free(newName);
    }
    if (!NameBuf[0]) {
        sprintf(NameBuf, "User %d in domain %s", uid, cell ? cell : MyMailDomain);
    }
}

int             MarkInProgress(dirname)
char           *dirname;
{
    return (SetProgressMark(dirname, TRUE, FALSE));
}

int             MarkQuietlyInProgress(dirname)
char           *dirname;
{
    return (SetProgressMark(dirname, TRUE, TRUE));
}

int             UnmarkInProgress(dirname)
char           *dirname;
{
    return (SetProgressMark(dirname, FALSE, FALSE));
}

static char     MS_DIRINPROGRESSFILE[] = ".AMS_DIRMOD";
static char     QuietMark[] = "shhh\n";

static int      SetProgressMark(dirname, TurnOnMark, Quietly)
char           *dirname;
Boolean         TurnOnMark, Quietly;
{
    char            MarkFile[1 + MAXPATHLEN];
    int             fd;

    strcpy(MarkFile, dirname);
    strcat(MarkFile, "/");
    strcat(MarkFile, MS_DIRINPROGRESSFILE);
    debug(1, ("Turning %s in-progress mark file %s\n", TurnOnMark ? "ON" : "OFF", MarkFile));
    if (TurnOnMark) {
        fd = open(MarkFile, O_RDWR | O_CREAT, 0666);
        if (fd < 0)
            AMS_RETURN_ERRCODE(errno, EIN_OPEN, EVIA_MARKINPROGRESS);
        if (Quietly)
            (void) write(fd, QuietMark, sizeof(QuietMark) - 1);
        close(fd);
    }
    else {
        if (unlink(MarkFile)) {
            if (errno != ENOENT) {
                AMS_RETURN_ERRCODE(errno, EIN_UNLINK, EVIA_MARKINPROGRESS);
            }
        }
    }
    return (0);
}

static int      CheckMarksInProgress(Dir, pQuietly)
struct MS_Directory *Dir;
int            *pQuietly;
{
    auto char       MarkFile[1 + MAXPATHLEN];
    auto struct stat sbuf;

    strcpy(MarkFile, Dir->UNIXDir);
    strcat(MarkFile, "/");
    strcat(MarkFile, MS_DIRINPROGRESSFILE);
    if (stat(MarkFile, &sbuf) != 0)
        return (0);
    *pQuietly = (sbuf.st_size >= (sizeof(QuietMark) - 1));
    return (1);
}

struct FileInfo {
    char           *Name;
    unsigned long int FileDate;
};

struct CheckList {
    char           *Label;
    struct FileInfo *ToCheck;
    int             Allocated, Used, UsesDates;
}               FilesToCheck = {

    "file", 0, 0, 0, 0
},

SnapsToCheck = {
    "snapshot", 0, 0, 0, 0
},

Orphans = {
    "orphan", 0, 0, 0, 1
},

Missing = {
    "missing", 0, 0, 0, 0
};

int             HandleMarksInProgress(Dir, Quiet)
struct MS_Directory *Dir;
int             Quiet;
{
    DIR            *dirp;
    DIRENT_TYPE  *dp;
    int             i, alien;
    char            SBuf[1 + AMS_SNAPSHOTSIZE], ErrorText[100 + MAXPATHLEN];
    struct stat     sbuf;

    if (!Quiet) {
        sprintf(ErrorText, "Checking for hidden messages in folder %s.", ap_Shorten(Dir->UNIXDir));
        NonfatalBizarreError(ErrorText);
    }
    if (IsDirAlien(Dir->UNIXDir, &alien)) {
        return (mserrcode);
    }
    FilesToCheck.ToCheck = 0;
    SnapsToCheck.ToCheck = 0;
    Orphans.ToCheck = 0;
    Missing.ToCheck = 0;
    FreeCheckLists();
    if (ReadOldMSDirectoryHead(Dir))
        return (mserrcode);
    dirp = opendir(Dir->UNIXDir);
    if (!dirp) {
        AMS_RETURN_ERRCODE(errno, EIN_OPENDIR, EVIA_MARKINPROGRESS);
    }
    while (dp = readdir(dirp)) {
        if (*dp->d_name == '+') {
            AddToCheckList(dp->d_name + 1, &FilesToCheck);
        }
        else
            if (*dp->d_name != '.') {
                char            ErrText[100 + MAXPATHLEN];

                sprintf(ErrText, "%s/%s", Dir->UNIXDir, dp->d_name);
                if (stat(ErrText, &sbuf) == 0 && (sbuf.st_mode & S_IFMT) != S_IFDIR) {
                    if ((sbuf.st_mode & S_IFMT) == S_IFREG) {
                        if (alien) {
                            AddToCheckList(dp->d_name, &FilesToCheck);
                        }
                        else {
                            sprintf(ErrText, "There is a bogus file '%s' in folder %s.", dp->d_name, ap_Shorten(Dir->UNIXDir));
                            NonfatalBizarreError(ErrText);
                        }
                    }
                    else {
                        sprintf(ErrText, "There is a bogus file '%s' (type %#o) in folder %s.", dp->d_name, (sbuf.st_mode & S_IFMT), ap_Shorten(Dir->UNIXDir));
                        NonfatalBizarreError(ErrText);
                    }
                }
            }
    }
    closedir(dirp);
    for (i = 0; i < Dir->MessageCount; ++i) {
        if (GetSnapshotByNumber(Dir, i, SBuf)) {
            closedir(dirp);
            FreeCheckLists();
            return (mserrcode);
        }
        AddToCheckList(AMS_ID(SBuf), &SnapsToCheck);
    }
    return (CheckCheckLists(Dir, Quiet, alien));
}

static int      AddToCheckList(name, CheckList)
char           *name;
struct CheckList *CheckList;
{
    if (CheckList->Used >= CheckList->Allocated) {
        CheckList->Allocated *= 2;
        CheckList->Allocated += 25;
        if (CheckList->ToCheck) {
            CheckList->ToCheck = (struct FileInfo *) realloc(CheckList->ToCheck, CheckList->Allocated * sizeof(struct FileInfo));
        }
        else {
            CheckList->ToCheck = (struct FileInfo *) malloc(CheckList->Allocated * sizeof(struct FileInfo));
        }
        if (!CheckList->ToCheck)
            AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_MARKINPROGRESS);
        debug(262144, ("The %s checklist now has %d/%d entries.\n", CheckList->Label, CheckList->Used, CheckList->Allocated));
    }
    CheckList->ToCheck[CheckList->Used].FileDate = 0;
    CheckList->ToCheck[CheckList->Used].Name = (char *) malloc(1 + strlen(name));
    if (!CheckList->ToCheck[CheckList->Used].Name)
        AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_MARKINPROGRESS);
    strcpy(CheckList->ToCheck[CheckList->Used].Name, name);
    ++CheckList->Used;
    return (0);
}

static          FreeCheckLists()
{
    FreeCheckList(&Orphans);
    FreeCheckList(&Missing);
    FreeCheckList(&SnapsToCheck);
    FreeCheckList(&FilesToCheck);
}

static          FreeCheckList(CheckList)
struct CheckList *CheckList;
{
    int             i;

    if (CheckList->ToCheck) {
        for (i = 0; i < CheckList->Used; ++i) {
            free(CheckList->ToCheck[i].Name);
        }
        free(CheckList->ToCheck);
        CheckList->ToCheck = NULL;
    }
    CheckList->Used = 0;
    CheckList->Allocated = 0;
}

static int      CompareStrings(s1, s2)
struct FileInfo *s1, *s2;
{
    return (strcmp(s1->Name, s2->Name));
}

static int      CompareTimes(s1, s2)
struct FileInfo *s1, *s2;
{
    return (s1->FileDate - s2->FileDate);
}

static int      CheckCheckLists(Dir, Quiet, alien)
struct MS_Directory *Dir;
int             Quiet, alien;
{
    char            FileName[1 + MAXPATHLEN], *partstart, ErrorText[200 + MAXPATHLEN], FileBuf2[1 + MAXPATHLEN];
    int             foundct = 0, errs = 0, i, j, code, missingct = 0;

    strcpy(FileName, Dir->UNIXDir);
    strcat(FileName, alien ? "/" : "/+");
    partstart = FileName + strlen(FileName);

    debug(262144, ("%d messages in directory %s.\n", FilesToCheck.Used, Dir->UNIXDir));
    debug(262144, ("%d snapshots in its .MS_MsgDir file.\n", SnapsToCheck.Used));

    qsort(FilesToCheck.ToCheck, FilesToCheck.Used, sizeof(struct FileInfo), CompareStrings);
    qsort(SnapsToCheck.ToCheck, SnapsToCheck.Used, sizeof(struct FileInfo), CompareStrings);
    for (i = 0, j = 0; i < FilesToCheck.Used || j < SnapsToCheck.Used;) {
        if (j >= SnapsToCheck.Used) {
            debug(2, ("Automatically using the file\n"));
            code = -1;
        }
        else
            if (i >= FilesToCheck.Used) {
                debug(2, ("Automatically using the snapshot\n"));
                code = 1;
            }
            else {
                code = strcmp(FilesToCheck.ToCheck[i].Name, SnapsToCheck.ToCheck[j].Name);
                debug(2, ("Comparing the file and snapshot\n"));
            }
        if (!code) {
            debug(2, ("They are the same\n"));
            ++j;
            ++i;
            continue;
        }
        if (code > 0) {
            debug(2, ("Adding from snapshot: %s\n", SnapsToCheck.ToCheck[j].Name));
            AddToCheckList(SnapsToCheck.ToCheck[j].Name, &Missing);
            ++j;
        }
        else {
            debug(2, ("Adding from files: %s\n", FilesToCheck.ToCheck[i].Name));
            AddToCheckList(FilesToCheck.ToCheck[i].Name, &Orphans);
            ++i;
        }
    }
    if (Orphans.Used > 0) {
        struct stat     SBuf;

        debug(262144, ("The directory %s DOES have hidden mail in it-- %d messages!\n", Dir->UNIXDir, Orphans.Used));
        for (i = 0; i < Orphans.Used; ++i) {
            strcpy(partstart, Orphans.ToCheck[i].Name);
            Orphans.ToCheck[i].FileDate = (stat(FileName, &SBuf) == 0 ? SBuf.st_ctime : 0);
        }
        qsort(Orphans.ToCheck, Orphans.Used, sizeof(struct FileInfo), CompareTimes);
        for (i = 0; i < Orphans.Used; ++i) {
            ++foundct;
            debug(2, ("The directory %s DOES have hidden mail in it -- %s!\n", Dir->UNIXDir, Orphans.ToCheck[i].Name));
            strcpy(partstart, Orphans.ToCheck[i].Name);
            strcpy(FileBuf2, FileName);/* make a temp copy of the name of
                                        * the file to add */
            if (AppendFileToMSDirInternal(FileBuf2, Dir, TRUE, alien))
                ++errs;
            if ((i & 077) == 077)
                debug(262144, ("...finished adding %d of %d hidden files\n", i, Orphans.Used));
        }
        debug(262144, ("Finished adding all %d hidden files.\n", Orphans.Used));
    }
    if (Missing.Used > 0) {
        debug(262144, ("The directory %s has %d orphaned snapshots in it!\n", Dir->UNIXDir, Missing.Used));
        for (i = 0; i < Missing.Used; ++i) {
            int             msgnum;
            char            snapshot[AMS_SNAPSHOTSIZE + 1];

            ++missingct;
            debug(2, ("The directory %s has an orphaned snapshot in it -- %s!\n", Dir->UNIXDir, Missing.ToCheck[i].Name));
            mserrcode = GetSnapshotByID(Dir, Missing.ToCheck[i].Name, &msgnum, snapshot);
            if (mserrcode) {
                ++errs;
            }
            else {
                strcpy(AMS_CAPTION(snapshot), "Deleted message:  Caption will disappear with next purge.");
                AMS_SET_ATTRIBUTE(snapshot, AMS_ATT_DELETED);
                if (RewriteSnapshotInDirectory(Dir, msgnum, snapshot))
                    ++errs;
            }
            if ((i & 077) == 077)
                debug(262144, ("Finished fixing %d of %d snapshots\n", i, Missing.Used));
        }
        debug(262144, ("Finished fixing all %d snapshots.\n", Missing.Used));
    }
    if (!Quiet || foundct > 0) {
        sprintf(ErrorText, "Found %d old hidden messages in %s (%d orphaned snapshots, %d processing errors).", foundct, ap_Shorten(Dir->UNIXDir), missingct, errs);
        NonfatalBizarreError(ErrorText);
    }
    FreeCheckLists();
    return (0);
}

IsDirAlien(Dir, alien)
char *Dir;
int            *alien;
{
    static int     *pathcodes = NULL;
    int             i, whichpath;
    char            Root[1 + MAXPATHLEN];

    if (pathcodes == NULL) {
        pathcodes = (int *) malloc((MS_NumDirsInSearchPath + 1) * sizeof(int));
        if (pathcodes == NULL) {
            AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_MARKINPROGRESS);
        }
        for (i = 0; i <= MS_NumDirsInSearchPath; ++i) {
            pathcodes[i] = -1;
        }
    }
    FindTreeRoot(Dir, Root, FALSE);
    whichpath = WhichPath(Root);
    if (whichpath < 0) {
        *alien = 0;
        return (0);
    }
    if (pathcodes[whichpath] == -1) {
        char            AlienFlagFile[1 + MAXPATHLEN];
        struct stat     stbuf;

        sprintf(AlienFlagFile, "%s/.amsalien", SearchPathElements[whichpath].Path);
        if (stat(AlienFlagFile, &stbuf)) {
            if (errno != ENOENT) {
                AMS_RETURN_ERRCODE(errno, EIN_STAT, EVIA_MARKINPROGRESS);
            }
            pathcodes[whichpath] = 0;
        }
        else {
            pathcodes[whichpath] = 1;
        }
    }
    *alien = pathcodes[whichpath];
    return (0);
}

#if 0
GetBodyFileName(DirName, id, FileName)
char           *DirName, *id, *FileName;
{
    struct stat statbuf;

    sprintf(FileName, "%s/+%s", DirName, id);
    if (stat(FileName, &statbuf)) {
        sprintf(FileName, "%s/%s", DirName, id);
    }
}
#endif

/* The following two functions added to replace the old
 * GetBodyFileName() with something faster.  cn0h--11/22/91
 */
QuickGetBodyFileName(DirName, id, FileName)
char           *DirName, *id, *FileName;
{
    sprintf(FileName, "%s/+%s", DirName, id);
}

/* returns -1 on failure, 0 on success
 */
int RetryBodyFileName(FileName)
    char *FileName;
{
    char *s;

    /* find the "/+" */
    s = strrchr(FileName, '/');
    if (errno != ENOENT || s == (char *) NULL || *++s != '+') {
	return (-1);
    }
    /* remove the "+" */
    do {    
	s[0] = s[1];
    } while (*++s != '\0');

    return (0);
}
