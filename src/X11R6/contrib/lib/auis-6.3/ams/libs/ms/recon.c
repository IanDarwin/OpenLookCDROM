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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/libs/ms/RCS/recon.c,v 2.36 1993/09/21 21:59:16 gk5g Exp $";
#endif

#include <andrewos.h>
#include <sys/stat.h>
#include <ctype.h>
#include <netinet/in.h>
#include <ms.h>
#include <hdrparse.h>

#define MS_RECONDIREXT (".r")
#define PADSIZE 10                     /* BOGUS -- duplicates stuff in rawdb.c */

#define MSGLIST_GROWSIZE (64)
#define SNAPSHOTLIST_GROWSIZE (64)
#define MERGELIST_GROWSIZE (8)
#define HASHLIST_GROWSIZE (8)

#define CRITICAL_MIN_ID_LEN (8)
#define CRITICAL_MAX_ID_LEN (24)

#define MAX_OK_CAPTIONLENGTH (78)
#define MIN_OK_CAPTIONLENGTH (14)

#define isbase64(c) (isalnum(c) || ((c) == ':') || ((c) == '='))

/* The next line should change each time the actual file format changes.*/
/* Don't forget to change the same define in rawdb.c! */

#define MS_DB_VERSION 4

struct hashlist {
    unsigned long  *hashes;
    unsigned long   MIDHash;
    Boolean         checkMIDHash;
    int             used, size;
};

struct msglistentry {
    struct MS_Message *msg;
    struct hashlist h;
};

struct msglist {
    struct msglistentry *entries;
    int             used, size;
};

struct snapshotlistentry {
    char           *snapshot;
};

struct snapshotlist {
    struct snapshotlistentry *entries;
    int             used, size;
};

struct mergelistentry {
    int             chain, num;
};

struct mergelist {
    struct mergelistentry *entries;
    int             used, size;
};

static int      ContainsOnlyBase64Chars(str)
char           *str;
{
    char           *p;
    int             result;

    for (p = str, result = TRUE;
         *p && (result = (result && isbase64(*p)));
         ++p);
    return (result);
}

static int      OKAMSFileName(name)
char           *name;
{
    int             len;

    return (((*name) == '+')
            && ((len = (strlen(name + 1))) >= CRITICAL_MIN_ID_LEN)
            && (len <= CRITICAL_MAX_ID_LEN)
            && (ContainsOnlyBase64Chars(name + 1)));
}

static int      GetOldIDSomehow(Msg, FileName, DirName)
struct MS_Message *Msg;
char           *FileName;
char *DirName;
{
    char           *result;
    int             alien = FALSE;

    if (DirName && IsDirAlien(DirName, &alien)) {
        return (mserrcode);
    }
    if (alien) {
        result = rindex(FileName, '/');
        if (result) {
            ++result;
            if (strlen(result) < AMS_IDSIZE) {
                strcpy(AMS_ID(Msg->Snapshot), result);
                return (0);
            }
        }
    }
    else {
        result = rindex(FileName, '+');
        if (result && OKAMSFileName(result)) {
            strcpy(AMS_ID(Msg->Snapshot), ++result);
            return (0);
        }
    }
    AMS_RETURN_ERRCODE(EMSUNKNOWN, EIN_PARAMCHECK,
                       EVIA_RECONSTRUCTDIRECTORY);
}

static int      CaptionIsHealthy(caption)
char           *caption;
{
    int             len, numtabs = 0;
    char           *cptr = caption;

    if (((len = strlen(caption)) > MAX_OK_CAPTIONLENGTH)
        || (len < MIN_OK_CAPTIONLENGTH))
        return (0);
    for (cptr = caption; *cptr; ++cptr) {
        if (!isascii(*cptr))
            return (0);
        if (*cptr == '\t')
            ++numtabs;
        else
            if (!isprint(*cptr))
                return (0);
    }
    if ((numtabs < 1) || (numtabs > 2))
        return (0);
    return (1);
}

static int      MergeList_ContainsChain(m, chain)
struct mergelist *m;
int             chain;
{
    int             i;

    for (i = 0; i < m->used; ++i) {
        if (chain == (m->entries[i].chain))
            return (TRUE);
    }
    return (FALSE);
}

static void     MergeList_Free(m)
struct mergelist *m;
{
    if (m->entries)
        free(m->entries);
}

static int      MergeList_Add(m, chain, num)
struct mergelist *m;
int             chain, num;
{
    if (!MergeList_GrowIfNecessary(m))
        return (0);
    m->entries[m->used].chain = chain;
    m->entries[(m->used)++].num = num;
    return (1);
}

static int      MergeList_NeedToMerge(m, chain, num)
struct mergelist *m;
int             chain, num;
{
    int             i, decided = FALSE, result = FALSE;

    for (i = 0; (i < m->used) && (!decided); ++i) {
        if (chain == m->entries[i].chain) {
            decided = TRUE;
            result = (num > m->entries[i].num);
        }
    }
    return (result);
}

static struct hashlist *MsgList_GetHashList(mlist, num)
struct msglist *mlist;
int             num;
{
    return (&(mlist->entries[num].h));
}

static struct MS_Message *MsgList_GetMsg(mlist, num)
struct msglist *mlist;
int             num;
{
    return (mlist->entries[num].msg);
}

static char    *MsgList_GetSnapshot(mlist, num)
struct msglist *mlist;
int             num;
{
    return (mlist->entries[num].msg->Snapshot);
}

static void     MsgList_Remove(mlist, num, FreeSnapshot)
struct msglist *mlist;
int             num, FreeSnapshot;
{
    int             i;

    FreeMessage(mlist->entries[num].msg, FreeSnapshot);
    --(mlist->used);
    for (i = num; i < mlist->used; ++i) {
        bcopy(&(mlist->entries[i + 1]), &(mlist->entries[i]), sizeof(struct msglistentry));
    }
}

static int      MsgList_Size(mlist)
struct msglist *mlist;
{
    return (mlist->used);
}

static void     MergeList_Init(m)
struct mergelist *m;
{
    m->used = m->size = 0;
    m->entries = (struct mergelistentry *) 0;
}

static char    *SnapshotList_GetSnapshot(slist, num)
struct snapshotlist *slist;
int             num;
{
    return (slist->entries[num].snapshot);
}

static int      SnapshotList_Size(slist)
struct snapshotlist *slist;
{
    return (slist->used);
}

static void     HashList_Free(h)
struct hashlist *h;
{
    if (h->hashes)
        free(h->hashes);
}

static int      HashList_Size(h)
struct hashlist *h;
{
    return (h->used);
}

static int      HashList_AnyMatches(h1, h2)
struct hashlist *h1, *h2;
{
    int             i, j;
    unsigned long   cmp1, cmp2;

    for (i = -1; i < h1->used; ++i) {

        if (i < 0) {
            if (h1->checkMIDHash)
                cmp1 = h1->MIDHash;
            else
                continue;
        }
        else
            cmp1 = h1->hashes[i];

        for (j = -1; j < h2->used; ++j) {
            if (j < 0) {
                if (h2->checkMIDHash)
                    cmp2 = h2->MIDHash;
                else
                    continue;
            }
            else
                cmp2 = h2->hashes[j];

            if ((0x7fffffff & cmp1) == (0x7fffffff & cmp2))
                return (TRUE);
        }
    }
    return (FALSE);
}

static int      MsgList_GrowIfNecessary(mlist)
struct msglist *mlist;
{
    struct msglistentry *tmp;

    if (mlist->used != mlist->size)
        return (1);
    if (mlist->size) {
        if (!(tmp = (struct msglistentry *)
              realloc(mlist->entries, (MSGLIST_GROWSIZE + mlist->size) *
                      (sizeof(struct msglistentry)))))
            return (0);
    }
    else {
        if (!(tmp = (struct msglistentry *)
              malloc(MSGLIST_GROWSIZE *
                     (sizeof(struct msglistentry)))))
            return (0);
    }
    mlist->entries = tmp;
    mlist->size += MSGLIST_GROWSIZE;
    return (1);
}

static int      MergeList_GrowIfNecessary(mlist)
struct mergelist *mlist;
{
    struct mergelistentry *tmp;

    if (mlist->used != mlist->size)
        return (1);
    if (mlist->size) {
        if (!(tmp = (struct mergelistentry *)
              realloc(mlist->entries, (MERGELIST_GROWSIZE + mlist->size) *
                      (sizeof(struct mergelistentry)))))
            return (0);
    }
    else {
        if (!(tmp = (struct mergelistentry *)
              malloc(MERGELIST_GROWSIZE *
                     (sizeof(struct mergelistentry)))))
            return (0);
    }
    mlist->entries = tmp;
    mlist->size += MERGELIST_GROWSIZE;
    return (1);
}

static int      MsgListEntry_CompareAMSIDs(mle1, mle2)
struct msglistentry *mle1, *mle2;
{
    return (strcmp(AMS_ID(mle1->msg->Snapshot), AMS_ID(mle2->msg->Snapshot)));
}

static int      SnapshotListEntry_CompareAMSIDs(sle1, sle2)
struct snapshotlistentry *sle1, *sle2;
{
    return (strcmp(AMS_ID(sle1->snapshot), AMS_ID(sle2->snapshot)));
}

static int      MsgListEntry_CompareTimes(mle1, mle2)
struct msglistentry *mle1, *mle2;
{
    return (strcmp(AMS_DATE(mle1->msg->Snapshot), AMS_DATE(mle2->msg->Snapshot)));
}

/* Does a minor sort on AMSIDs */
static int      MsgListEntry_CompareMIDs(mle1, mle2)
struct msglistentry *mle1, *mle2;
{
    char           *mid1, *mid2;
    int             result;

    GetRightMid(mle1->msg, &mid1);
    GetRightMid(mle2->msg, &mid2);
    if (mid1) {
        if (mid2) {
            result = strcmp(mid1, mid2);
            free(mid1);
            free(mid2);
        }
        else {
            result = 1;
            free(mid1);
        }
    }
    else {
        if (mid2) {
            result = -1;
            free(mid2);
        }
        else {
            result = 0;
        }
    }
    return (result ? result :
            strcmp(AMS_ID(mle1->msg->Snapshot),
                   AMS_ID(mle2->msg->Snapshot)));
}

static void     MsgList_SortByAMSID(mlist)
struct msglist *mlist;
{
    qsort(mlist->entries, mlist->used,
          sizeof(struct msglistentry),
          MsgListEntry_CompareAMSIDs);
}

/* Does a minor sort on AMSIDs */
static void     MsgList_SortByMID(mlist)
struct msglist *mlist;
{
    qsort(mlist->entries, mlist->used,
          sizeof(struct msglistentry),
          MsgListEntry_CompareMIDs);
}

static void     MsgList_SortByTime(mlist)
struct msglist *mlist;
{
    qsort(mlist->entries, mlist->used,
          sizeof(struct msglistentry),
          MsgListEntry_CompareTimes);
}

static void     SnapshotList_SortByAMSID(slist)
struct snapshotlist *slist;
{
    qsort(slist->entries, slist->used,
          sizeof(struct snapshotlistentry),
          SnapshotListEntry_CompareAMSIDs);
}

static int      SnapshotList_GrowIfNecessary(slist)
struct snapshotlist *slist;
{
    struct snapshotlistentry *tmp;

    if (slist->used != slist->size)
        return (1);
    if (slist->size) {
        if (!(tmp = (struct snapshotlistentry *)
              realloc(slist->entries,
                      (SNAPSHOTLIST_GROWSIZE + slist->size) *
                      (sizeof(struct snapshotlistentry)))))
            return (0);
    }
    else {
        if (!(tmp = (struct snapshotlistentry *)
              malloc(SNAPSHOTLIST_GROWSIZE *
                     (sizeof(struct snapshotlistentry)))))
            return (0);
    }
    slist->entries = tmp;
    slist->size += SNAPSHOTLIST_GROWSIZE;
    return (1);
}

static void     MsgList_Init(mlist)
struct msglist *mlist;
{
    mlist->entries = (struct msglistentry *) 0;
    mlist->used = mlist->size = 0;
}

static void     HashList_Init(hlist)
struct hashlist *hlist;
{
    hlist->hashes = (unsigned long *) 0;
    hlist->used = hlist->size = 0;
}

static int      HashList_GrowIfNecessary(hlist)
struct hashlist *hlist;
{
    unsigned long  *tmp;

    if (hlist->used != hlist->size)
        return (1);
    if (hlist->size) {
        if (!(tmp = (unsigned long *)
              realloc(hlist->hashes,
                      (HASHLIST_GROWSIZE + hlist->size) *
                      (sizeof(unsigned long)))))
            return (0);
    }
    else {
        if (!(tmp = (unsigned long *)
              malloc(HASHLIST_GROWSIZE *
                     (sizeof(unsigned long)))))
            return (0);
    }
    hlist->hashes = tmp;
    hlist->size += HASHLIST_GROWSIZE;
    return (1);
}

static int      HashList_Add(hlist, val)
struct hashlist *hlist;
unsigned long   val;
{
    if (!HashList_GrowIfNecessary(hlist))
        return (0);
    hlist->hashes[(hlist->used)++] = val;
    return (1);
}

static void     SnapshotList_Init(slist)
struct snapshotlist *slist;
{
    slist->entries = (struct snapshotlistentry *) 0;
    slist->used = slist->size = 0;
}

static int      MyConstructHashList(msg, h)
struct MS_Message *msg;
struct hashlist *h;
{
    int             len = 0;
    char           *LineBuf = NULL, *s = NULL, *t = NULL;
    unsigned long   hashedVal;

    if (msg->ParsedStuff->HeadBody[HP_INREPLYTO]) {
        len = msg->ParsedStuff->HeadBodyLen[HP_INREPLYTO];
        LineBuf = malloc(1 + len);
        if (!LineBuf) {
            return (FALSE);
        }
        strncpy(LineBuf, msg->ParsedStuff->HeadBody[HP_INREPLYTO], len);
        LineBuf[len] = '\0';
    }
    if (msg->ParsedStuff->HeadBody[HP_REFERENCES]) {
        int             rlen;

        rlen = msg->ParsedStuff->HeadBodyLen[HP_REFERENCES];
        if (LineBuf) {
            LineBuf = realloc(LineBuf, rlen + len + 3);
            if (!LineBuf) {
                return (FALSE);
            }
            strcat(LineBuf, ", ");
            strncat(LineBuf, msg->ParsedStuff->HeadBody[HP_REFERENCES], rlen);
            LineBuf[rlen + len + 2] = '\0';
        }
        else {
            LineBuf = malloc(rlen + 1);
            if (!LineBuf) {
                return (FALSE);
            }
            strncpy(LineBuf, msg->ParsedStuff->HeadBody[HP_REFERENCES], rlen);
            LineBuf[rlen] = '\0';
        }
    }
    t = LineBuf;
    while (t) {
        if (s = index(t, '<')) {
            t = index(++s, '>');
            if (t)
                *t++ = '\0';

            hashedVal = KRHash(s);

            if (!HashList_Add(h, hashedVal)) {
                if (LineBuf)
                    free(LineBuf);
                return (FALSE);        /* Failure! */
            }
        }
        else
            t = NULL;
    }
    if (LineBuf) {
        free(LineBuf);
        LineBuf = NULL;
    }
    GetRightMid(msg, &LineBuf);
    if (LineBuf) {
        h->MIDHash = KRHash(LineBuf);
        h->checkMIDHash = TRUE;
        free(LineBuf);
    }
    else {
        h->checkMIDHash = FALSE;
    }
    return (TRUE);                     /* Success! */
}

static int      MsgList_Add(mlist, msg)
struct msglist *mlist;
struct MS_Message *msg;
{
    if (!MsgList_GrowIfNecessary(mlist))
        return (0);
    mlist->entries[mlist->used].msg = msg;
    HashList_Init(&(mlist->entries[mlist->used].h));
    return (MyConstructHashList(msg, &(mlist->entries[(mlist->used)++].h)));
}

static int      SnapshotList_Add(slist, snapshot)
struct snapshotlist *slist;
char           *snapshot;
{
    if (!SnapshotList_GrowIfNecessary(slist))
        return (0);
    slist->entries[(slist->used)++].snapshot = snapshot;
    return (1);
}

static void     SnapshotList_Free(slist, Free)
struct snapshotlist *slist;
int             Free;
{
    int             i;

    if (Free) {
        for (i = 0; i < slist->used; ++i)
            free(slist->entries[i].snapshot);
    }
    if (slist->entries)
        free(slist->entries);
}

static void     MsgList_Free(mlist, FreeSnapshots)
struct msglist *mlist;
int             FreeSnapshots;
{
    int             i;

    for (i = 0; i < mlist->used; ++i)
        if (mlist->entries[i].msg) {
            FreeMessage(mlist->entries[i].msg, FreeSnapshots);
            HashList_Free(&(mlist->entries[i].h));
        }
    if (mlist->entries) {
        free(mlist->entries);
        mlist->entries = (struct msglistentry *) 0;
    }
    mlist->used = mlist->size = 0;
}

static int      SameMsg(m1, m2)
struct MS_Message *m1, *m2;
{
    char           *mid1, *mid2;

    GetRightMid(m1, &mid1);
    if (!mid1)
        return (0);
    GetRightMid(m2, &mid2);
    if (!mid2) {
        free(mid1);
        return (0);
    }
    if (strcmp(mid1, mid2)
        || FieldsDiffer(m1, m2, HP_SUBJECT)
        || FieldsDiffer(m1, m2, HP_FROM)
        || FieldsDiffer(m1, m2, HP_RESENTFROM)
        || FieldsDiffer(m1, m2, HP_DATE)) {
        free(mid1);
        free(mid2);
        return (0);
    }
    free(mid1);
    free(mid2);
    return (1);
}

static void     ElimDups(dirname, mlist, alienDir)
char           *dirname;
struct msglist *mlist;
int             alienDir;
{
    int             i = 0;
    char            fname[1 + MAXPATHLEN], *amsid;

    MsgList_SortByMID(mlist);

    while (i < (MsgList_Size(mlist) - 1)) {
        if (SameMsg(MsgList_GetMsg(mlist, i),
                    MsgList_GetMsg(mlist, i + 1))) {
            if (strcmp(AMS_ID(MsgList_GetSnapshot(mlist, i)),
                       amsid = AMS_ID(MsgList_GetSnapshot(mlist,
                                                          i + 1)))) {
                /* Different files? */
                sprintf(fname, "%s/%s%s", dirname, (alienDir ? "" : "+"), amsid);
                unlink(fname);         /* Yipes! */
            }

            /*
             * If not different files, we've somehow gotten two entries for
             * the same file in this list.  In either case, remove the latter
             * entry from the list.
             */
            MsgList_Remove(mlist, i + 1, TRUE);
        }
        else {
            ++i;
        }
    }
}

/*
 * Currently salvages the caption and attributes.
 * Resets the "deleted" attribute.
 */

static void     SalvageOldSnapshot(oldsnap, newsnap)
char           *oldsnap, *newsnap;
{
    if (strcmp(AMS_CAPTION(oldsnap), AMS_CAPTION(newsnap))) {   /* If they differ */
        if (CaptionIsHealthy(AMS_CAPTION(oldsnap))) {
            bcopy(AMS_CAPTION(oldsnap), AMS_CAPTION(newsnap), AMS_CAPTIONSIZE);
        }
    }
    bcopy(AMS_ATTRIBUTES(oldsnap), AMS_ATTRIBUTES(newsnap), AMS_ATTRIBUTESIZE);
    AMS_UNSET_ATTRIBUTE(newsnap, AMS_ATT_DELETED);
}

static int      SetFileTimeStamp(filename, time)
char           *filename;
long            time;
{
#if defined(hpux)
    return (utime(filename, 0));
#else                                  /* hpux */
    struct timeval  tvp[2];

    tvp[0].tv_sec = time;
    tvp[0].tv_usec = 0;
    tvp[1].tv_sec = time;
    tvp[1].tv_usec = 0;
    return (utimes(filename, tvp));
#endif                                 /* hpux */
}

MS_ReconstructDirectory(DirName, NumGood, NumBad, TrustTimeStamp)
char           *DirName;
int            *NumGood, *NumBad, TrustTimeStamp;
{
    struct msglist  newMsgList;
    struct snapshotlist oldSnapshots;
    char            minstr[PADSIZE + 1], currentFile[1 + MAXPATHLEN], oldMSDirName[1 + MAXPATHLEN], newMSDirName[1 + MAXPATHLEN], headBuf[1 + AMS_DIRHEADSIZE], snapshotBuf[1 + AMS_SNAPSHOTSIZE], *tmpbuf;
    struct MS_CaptionTemplate captiontemplate;
    struct MS_Directory *oldMSDir;
    int             k, oldDirReadable, newMSDirfd, errsave, notOldID, newMaxChainVal;
    int             alienDir;
    DIR            *dirp;
    DIRENT_TYPE    *dirent;
    struct stat     statbuf;
    struct MS_Message *Msg;

    if (CloseDirsThatNeedIt())
        return (mserrcode);

    *NumGood = *NumBad = 0;
    MsgList_Init(&newMsgList);
    SnapshotList_Init(&oldSnapshots);

    /* Try to open old MSDir; mode MD_APPEND to get a lock */

    sprintf(oldMSDirName, "%s/%s", DirName, MS_DIRNAME);
    oldDirReadable = !ReadOrFindMSDir_Complain(DirName, &oldMSDir, MD_APPEND, FALSE);

    /*
     * Create reconstruction MSDir, to be manipulated "by hand" (not by
     * rawdb.c)
     */

    sprintf(newMSDirName, "%s/%s%s", DirName, MS_DIRNAME, MS_RECONDIREXT);
    if ((newMSDirfd = open(newMSDirName,
                           O_RDWR | O_CREAT | O_TRUNC,
                           0664)) < 0) {
        errsave = errno;
        if (oldDirReadable)
            CloseMSDir(oldMSDir, MD_APPEND);
        AMS_RETURN_ERRCODE(errsave, EIN_OPEN,
                           EVIA_RECONSTRUCTDIRECTORY);
    }

    /* Lock it */

    if (osi_ExclusiveLockNoBlock(newMSDirfd) != 0) {
        errsave = errno;
        close(newMSDirfd);
        if (oldDirReadable)
            CloseMSDir(oldMSDir, MD_APPEND);
        AMS_RETURN_ERRCODE(errsave, EIN_FLOCK,
                           EVIA_RECONSTRUCTDIRECTORY);
    }

    /* Set up Directory Header buffer, from old MSDir if possible */

    if (oldDirReadable) {
        if (ReadOldMSDirectoryHead(oldMSDir)) {
            oldDirReadable = FALSE;
            CloseMSDir(oldMSDir, MD_APPEND);
        }
        else {
            if (oldMSDir->CurPos != 0 && lseek(oldMSDir->fd, 0, L_SET) < 0) {
                oldDirReadable = FALSE;
                CloseMSDir(oldMSDir, MD_APPEND);
            }
            else {
                if (read(oldMSDir->fd, headBuf, AMS_DIRHEADSIZE)
                    != AMS_DIRHEADSIZE) {
                    oldDirReadable = FALSE;
                    CloseMSDir(oldMSDir, MD_APPEND);
                }
		oldMSDir->CurPos = AMS_DIRHEADSIZE;
            }
        }
    }

    /* Couldn't set it up that way?  Try this: */

    if (!oldDirReadable) {             /* BOGUS -- this is duplicated from
                                        * rawdb.c */
        char            majstr[PADSIZE + 1], killstr[PADSIZE + 1], attributesBuf[ATTNAMESLEN + 1];
        int             i;

        itops((long) MS_DB_VERSION, majstr, PADSIZE);
        itops((long) 0, minstr, PADSIZE);       /* MaxChainVal */
        itops(time(0), killstr, PADSIZE);
        bzero(attributesBuf, ATTNAMESLEN);
        sprintf(headBuf, "%s%s%s%s", AMS_DIRECTORY_PREFIX_STRING, majstr, minstr, killstr);
        i = strlen(headBuf) + 1;
        bzero(headBuf + i, 4);
        bcopy(attributesBuf, headBuf + i + 4, ATTNAMESLEN);
        bzero(headBuf + i + 4 + ATTNAMESLEN,
               AMS_DATESIZE);   /* Space for LastMsgDate.  This should be replaced later on with the AMS_DATE of the last snapshot */
        bzero(headBuf + i + 4 + ATTNAMESLEN + AMS_DATESIZE,
               AMS_DIRHEADSIZE - (i + ATTNAMESLEN + AMS_DATESIZE + 5));
    }

    /* Defer writing of new header until we know newMaxChainVal (and LastMsgDate) */

/* CCG - why does this insist on an old dir for aliens? */
/* oh.. because IsDirAlien wants a MSDir!! */
#if 0
    if (oldDirReadable) {
        if (IsDirAlien(oldMSDir->UNIXDir, &alienDir)) {
            return (mserrcode);
        }
    }
    else
        alienDir = FALSE;
#endif
        if (IsDirAlien(DirName, &alienDir)) {
            return (mserrcode);
        }

    /* Enumerate all the messages: */

    captiontemplate.datetype = DATETYPE_FROMFILE;
    captiontemplate.basictype = BASICTEMPLATE_NORMAL;

    /* Read everything in the directory */

    if (!(dirp = opendir(DirName))) {
        errsave = errno;
        if (oldDirReadable)
            CloseMSDir(oldMSDir, MD_APPEND);
        unlink(newMSDirName);
        close(newMSDirfd);
        AMS_RETURN_ERRCODE(errsave, EIN_OPENDIR, EVIA_RECONSTRUCTDIRECTORY);
    }

    /*
     * First do all the renames we may have to do to avoid bizarreness in the
     * main readdir loop
     */

    if (!alienDir) {
        int             needToLoop;

        do {
            needToLoop = FALSE;
            while (dirent = readdir(dirp)) {
                if (dirent->d_name[0] == '.') {
                    continue;
                }

                sprintf(currentFile, "%s/%s", DirName, dirent->d_name);
                if (stat(currentFile, &statbuf)) {
                    errsave = errno;
                    if (oldDirReadable)
                        CloseMSDir(oldMSDir, MD_APPEND);
                    unlink(newMSDirName);
                    close(newMSDirfd);
                    closedir(dirp);
                    AMS_RETURN_ERRCODE(errsave, EIN_STAT,
                                        EVIA_RECONSTRUCTDIRECTORY);
                }

                /* Ignore subdirs */

                if ((statbuf.st_mode & S_IFMT) == S_IFDIR) {
                    continue;
                }

                if (!OKAMSFileName(dirent->d_name)) {
                    char            from[1 + MAXPATHLEN], to[1 + MAXPATHLEN];

                    sprintf(from, "%s/%s", DirName, dirent->d_name);
                    sprintf(to, "%s/+%s", DirName, ams_genid(1));
                    if (RenameEvenInVice(from, to)) {
                        errsave = errno;
                        if (oldDirReadable)
                            CloseMSDir(oldMSDir, MD_APPEND);
                        unlink(newMSDirName);
                        close(newMSDirfd);
                        closedir(dirp);
                        AMS_RETURN_ERRCODE(errsave, EIN_RENAME,
                                           EVIA_RECONSTRUCTDIRECTORY);
                    }
                    needToLoop = TRUE;
                }
            }
            closedir(dirp);
            if (!(dirp = opendir(DirName))) {
                errsave = errno;
                if (oldDirReadable)
                    CloseMSDir(oldMSDir, MD_APPEND);
                unlink(newMSDirName);
                close(newMSDirfd);
                AMS_RETURN_ERRCODE(errsave, EIN_OPENDIR, EVIA_RECONSTRUCTDIRECTORY);
            }
        } while (needToLoop);
    }

    while (dirent = readdir(dirp)) {

        /* Ignore dot files */

        if (dirent->d_name[0] == '.') {
            continue;
        }

        sprintf(currentFile, "%s/%s", DirName, dirent->d_name);
        if (stat(currentFile, &statbuf)) {
            errsave = errno;
            if (oldDirReadable)
                CloseMSDir(oldMSDir, MD_APPEND);
            unlink(newMSDirName);
            close(newMSDirfd);
            closedir(dirp);
            MsgList_Free(&newMsgList, TRUE);
            AMS_RETURN_ERRCODE(errsave, EIN_STAT,
                               EVIA_RECONSTRUCTDIRECTORY);
        }

        /* Ignore subdirs */

        if ((statbuf.st_mode & S_IFMT) == S_IFDIR) {
            continue;
        }

        /* Create a buffer */

        if (!(Msg = (struct MS_Message *)
              malloc(sizeof(struct MS_Message)))) {
            if (oldDirReadable)
                CloseMSDir(oldMSDir, MD_APPEND);
            unlink(newMSDirName);
            close(newMSDirfd);
            closedir(dirp);
            MsgList_Free(&newMsgList, TRUE);
            AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC,
                               EVIA_RECONSTRUCTDIRECTORY);
        }

        /* Read in the message in the approved fashion */

        bzero(Msg, sizeof(struct MS_Message));
        Msg->OpenFD = -1;
        if (ReadRawFile(currentFile, Msg, FALSE)
            || ParseMessageFromRawBody(Msg)
            || BuildDateField(Msg, TrustTimeStamp ?
                              DATETYPE_FROMFILE :
                              DATETYPE_FROMHEADER)
            || ((notOldID =
                 GetOldIDSomehow(Msg,
                                 currentFile,
                                 oldDirReadable ?
                                 oldMSDir->UNIXDir :
                                 DirName))
                && InventID(Msg))
            || BuildReplyField(Msg)
            || CheckAuthUid(Msg)
            || BuildAttributesField(Msg)) {
            FreeMessage(Msg, TRUE);
            ++(*NumBad);
            continue;
        }

        /* Reset the file time? */

        if (!TrustTimeStamp)
            SetFileTimeStamp(currentFile, conv64tolong(AMS_DATE(Msg->Snapshot)));

        /* Build the caption */

        if (BuildCaption(Msg, &captiontemplate, FALSE)) {
            errsave = mserrcode;
            FreeMessage(Msg, TRUE);
            if (oldDirReadable)
                CloseMSDir(oldMSDir, MD_APPEND);
            unlink(newMSDirName);
            close(newMSDirfd);
            closedir(dirp);
            MsgList_Free(&newMsgList, TRUE);
            return (errsave);
        }

        /* Extend the message list */

        if (!MsgList_Add(&newMsgList, Msg)) {
            FreeMessage(Msg, TRUE);
            if (oldDirReadable)
                CloseMSDir(oldMSDir, MD_APPEND);
            unlink(newMSDirName);
            close(newMSDirfd);
            closedir(dirp);
            MsgList_Free(&newMsgList, TRUE);
            AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC,
                               EVIA_RECONSTRUCTDIRECTORY);
        }

        /* If the message ID has been newly invented */

        if (notOldID) {
            char            tempFileNameBuf[1 + MAXPATHLEN];

            sprintf(tempFileNameBuf, "%s/+%s",
                    DirName, AMS_ID(Msg->Snapshot));
            if (RenameEvenInVice(currentFile, tempFileNameBuf)) {
                errsave = errno;
                FreeMessage(Msg, TRUE);
                if (oldDirReadable)
                    CloseMSDir(oldMSDir, MD_APPEND);
                unlink(newMSDirName);
                close(newMSDirfd);
                closedir(dirp);
                MsgList_Free(&newMsgList, TRUE);
                AMS_RETURN_ERRCODE(errsave, EIN_RENAME,
                                   EVIA_RECONSTRUCTDIRECTORY);
            }
        }

        /* We won't be needing this any more */

        close(Msg->OpenFD);
        Msg->OpenFD = -1;
    }
    closedir(dirp);

    /*
     * Now enumerate old snapshots, if possible.
     */

    if (oldDirReadable) {
        int             i;

        for (i = 0; oldDirReadable && (i < oldMSDir->MessageCount); ++i) {

            /* Get the snapshot */

            if (errsave = GetSnapshotByNumber(oldMSDir, i,
                                              snapshotBuf)) {
                oldDirReadable = FALSE;
                CloseMSDir(oldMSDir, MD_APPEND);
                SnapshotList_Free(&oldSnapshots, TRUE);
            }
            else {

                /* Make a buffer */

                if (!(tmpbuf = malloc(AMS_SNAPSHOTSIZE))) {
                    oldDirReadable = FALSE;
                    CloseMSDir(oldMSDir, MD_APPEND);
                    SnapshotList_Free(&oldSnapshots, TRUE);
                }
                else {

                    /* Place it in the snapshot list */

                    bcopy(snapshotBuf, tmpbuf, AMS_SNAPSHOTSIZE);
                    if (!SnapshotList_Add(&oldSnapshots, tmpbuf)) {
                        errsave = mserrcode;
                        oldDirReadable = FALSE;
                        CloseMSDir(oldMSDir, MD_APPEND);
                        free(tmpbuf);
                        SnapshotList_Free(&oldSnapshots, TRUE);
                    }
                }
            }
        }
    }

    /*
     * Now salvage info (if possible).  OldMSDir could be closed now, but
     * defer to end to preserve lock
     */

    if (oldDirReadable) {
        int             i = 0, j = 0, cmp;

        MsgList_SortByAMSID(&newMsgList);
        SnapshotList_SortByAMSID(&oldSnapshots);

        /* The following nested loops cdr down each of the two lists, */
        /* searching for entries with identical AMSID's */

        while ((i < SnapshotList_Size(&oldSnapshots))
               && (j < MsgList_Size(&newMsgList))) {
            while ((strcmp(AMS_ID(SnapshotList_GetSnapshot(&oldSnapshots, i)),
                           AMS_ID(MsgList_GetSnapshot(&newMsgList, j))) > 0)
                   && ((++j) < MsgList_Size(&newMsgList)));
            if (j >= MsgList_Size(&newMsgList))
                continue;
            while (((cmp =
                   strcmp(AMS_ID(SnapshotList_GetSnapshot(&oldSnapshots, i)),
                          AMS_ID(MsgList_GetSnapshot(&newMsgList, j)))) < 0)
                   && ((++i) < SnapshotList_Size(&oldSnapshots)));
            if (!cmp) {
                SalvageOldSnapshot(SnapshotList_GetSnapshot(&oldSnapshots, i++),
                                   MsgList_GetSnapshot(&newMsgList, j));
            }
        }
    }

    /*
     * Old info has been salvaged, if it was possible.  Get rid of old
     * snapshots
     */

    SnapshotList_Free(&oldSnapshots, TRUE);

    /* Eliminate duplicates */

    ElimDups(DirName, &newMsgList, alienDir);

    /* Set chain values */

    newMaxChainVal = SetChains(&newMsgList);

    /* Modify the headbuf to reflect the maxchainval */

    itops((long) newMaxChainVal, minstr, PADSIZE);
    bcopy(minstr, headBuf + strlen(AMS_DIRECTORY_PREFIX_STRING) + PADSIZE, PADSIZE);

    /* Put snapshots in chronological order */

    MsgList_SortByTime(&newMsgList);

    /* Modify the headbuf to reflect the new LastMsgDate */

    if (MsgList_Size(&newMsgList) > 0) {
	bcopy(AMS_DATE(MsgList_GetSnapshot(&newMsgList,
					   (MsgList_Size(&newMsgList) - 1))),
	      headBuf + sizeof(AMS_DIRECTORY_PREFIX_STRING)
	      + (3 * PADSIZE) + 4 + ATTNAMESLEN, AMS_DATESIZE);
    }
    else {
	bcopy("000000",		/* BOGUS; I shouldn't know that
				 * AMS_DATESIZE is 6 */
	      (headBuf + sizeof(AMS_DIRECTORY_PREFIX_STRING)
	       + (3 * PADSIZE) + 4 + ATTNAMESLEN),
	      AMS_DATESIZE);
    }


    /* Write out the header of the new MSDir */

    if (writeall(newMSDirfd, headBuf, AMS_DIRHEADSIZE) !=
        AMS_DIRHEADSIZE) {
        errsave = errno;
        if (oldDirReadable)
            CloseMSDir(oldMSDir, MD_APPEND);
        unlink(newMSDirName);
        close(newMSDirfd);
        MsgList_Free(&newMsgList, TRUE);
        AMS_RETURN_ERRCODE(errno, EIN_WRITE,
                           EVIA_RECONSTRUCTDIRECTORY);
    }

    /* Append snapshots to new msdir */

    for (k = 0; k < MsgList_Size(&newMsgList); ++k) {
        if (lseek(newMSDirfd,
                  AMS_DIRHEADSIZE + (k * AMS_SNAPSHOTSIZE),
                  L_SET) < 0) {
            errsave = errno;
            if (oldDirReadable)
                CloseMSDir(oldMSDir, MD_APPEND);
            MsgList_Free(&newMsgList, TRUE);
            unlink(newMSDirName);
            close(newMSDirfd);
            AMS_RETURN_ERRCODE(errno, EIN_LSEEK, EVIA_RECONSTRUCTDIRECTORY);
        }
        if (writeall(newMSDirfd,
                     MsgList_GetSnapshot(&newMsgList, k),
                     AMS_SNAPSHOTSIZE)
            != AMS_SNAPSHOTSIZE) {
            errsave = errno;
            if (oldDirReadable)
                CloseMSDir(oldMSDir, MD_APPEND);
            MsgList_Free(&newMsgList, TRUE);
            unlink(newMSDirName);
            close(newMSDirfd);
            AMS_RETURN_ERRCODE(errno, EIN_WRITE, EVIA_RECONSTRUCTDIRECTORY);
        }
        ++(*NumGood);
    }

    /*
     * Now all the snapshots have been written to the new MSDir.  Free the
     * list...
     */

    MsgList_Free(&newMsgList, TRUE);

    /* Now put the new MSDir in place.  Do it carefully... */

    if (oldDirReadable) {
        int             savefd;

        /* BOGUS: This section is largely copied from purge.c */
        /* Does this stuff really WORK? */

        oldMSDir->MessageCount = k;
        savefd = oldMSDir->fd;
        oldMSDir->fd = newMSDirfd;
        if (CloseMSDir(oldMSDir, MD_APPEND)) {
            errsave = mserrcode;
            unlink(newMSDirName);
            close(savefd);
            return (errsave);
        }
        if (RenameEvenInVice(newMSDirName, oldMSDirName)) {
            errsave = errno;
            unlink(newMSDirName);
            close(savefd);
            AMS_RETURN_ERRCODE(errsave, EIN_RENAME, EVIA_RECONSTRUCTDIRECTORY);
        }
        (void) close(savefd);
        return (0);
    }

    /* Finally, if we're not bothering with any old MSDir... */

    if (vclose(newMSDirfd)) {
        errsave = errno;
        unlink(newMSDirName);
        AMS_RETURN_ERRCODE(errsave, EIN_VCLOSE, EVIA_RECONSTRUCTDIRECTORY);
    }
    if (RenameEvenInVice(newMSDirName, oldMSDirName)) {
        errsave = errno;
        unlink(newMSDirName);
        AMS_RETURN_ERRCODE(errsave, EIN_RENAME, EVIA_RECONSTRUCTDIRECTORY);
    }
    return (0);
}

/* Returns the maxchainval */

static int      SetChains(mlist)
struct msglist *mlist;
{
    int             i, curmax = 0, newval;

    for (i = 0; i < MsgList_Size(mlist); ++i)
        if ((newval = SetChain(mlist, i, curmax)) > curmax)
            curmax = newval;
    return (curmax);
}

/* Returns the chain val it used */

static int      SetChain(mlist, mnum, maxchainval)
struct msglist *mlist;
int             mnum, maxchainval;
{
    struct hashlist *hlCurrent = MsgList_GetHashList(mlist, mnum), *hlTemp;
    int             i, thisChain = 0, chosenChain = 0, goBackTo = mnum;
    char           *curSnapshot = MsgList_GetSnapshot(mlist, mnum), *tempSnapshot;
    unsigned long   aChain;
    struct mergelist ml;

    MergeList_Init(&ml);
    if (HashList_Size(hlCurrent) > 0) {
        for (i = mnum - 1; i >= 0; --i) {
            hlTemp = MsgList_GetHashList(mlist, i);
            bcopy(AMS_CHAIN(tempSnapshot = MsgList_GetSnapshot(mlist, i)),
                  &aChain, sizeof(unsigned long));
            thisChain = ntohl(aChain);
            if (HashList_AnyMatches(hlCurrent, hlTemp)) {
                if (!chosenChain) {
                    if (thisChain) {
                        chosenChain = thisChain;
                    }
                    else {
                        chosenChain = maxchainval + 1;
                    }
                }
                if ((!thisChain) || (chosenChain != thisChain)) {
                    aChain = htonl(chosenChain);
                    bcopy(&aChain, AMS_CHAIN(tempSnapshot), sizeof(unsigned long));
                    if (thisChain) {
                        if (!MergeList_ContainsChain(&ml, thisChain))
                            if (!MergeList_Add(&ml, thisChain, i)) {
                                MergeList_Free(&ml);
                                return (chosenChain);
                            }
                        goBackTo = i;
                    }
                }
            }
            else {
                if (MergeList_ContainsChain(&ml, thisChain)) {
                    aChain = htonl(chosenChain);
                    bcopy(&aChain, AMS_CHAIN(tempSnapshot), sizeof(unsigned long));
                }
            }
        }
        for (i = mnum - 1; i > goBackTo; --i) {
            bcopy(AMS_CHAIN(tempSnapshot = MsgList_GetSnapshot(mlist, i)),
                  &aChain, sizeof(unsigned long));
            thisChain = ntohl(aChain);
            if (MergeList_NeedToMerge(&ml, thisChain, i)) {
                aChain = htonl(chosenChain);
                bcopy(&aChain, AMS_CHAIN(tempSnapshot), sizeof(unsigned long));
            }
        }
    }
    aChain = htonl(chosenChain);
    bcopy(&aChain, AMS_CHAIN(curSnapshot), sizeof(unsigned long));
    MergeList_Free(&ml);
    return (chosenChain);
}
