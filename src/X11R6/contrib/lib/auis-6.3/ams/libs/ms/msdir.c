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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/libs/ms/RCS/msdir.c,v 2.46 1993/08/25 20:36:01 susan Exp $";
#endif

/*
  msdir.c -- Routines for manipulating message directory structures
      */

#include <andrewos.h>                  /* sys/file.h sys/types.h */
#include <ms.h>
#include <hdrparse.h>
#include <stdio.h>
#include <sys/stat.h>
#include <netinet/in.h>                /* for htonl, etc. */
#include <ctype.h>

extern char    *permanentmalloc();
extern unsigned long KRHash();

struct dcnode {
    struct MS_Directory *Dir;
    struct dcnode  *next;
    int             refcount;          /* Number of times accessed */
    int             refwhen;           /* value of CacheAccesses when last
					 * accessed */
};


int             CheckOpenMSDirectory(Dir, Code)
struct MS_Directory *Dir;
int             Code;
{
    struct stat     statbuf;
    char            fname[MAXPATHLEN + 1];
    int             saveerr;

    debug(1, ("CheckOpenMSDirectory code %d dir code %d\n", Code, Dir->OpenMode));
    if (Dir->fd >= 0) {
	NoteRecentDirUse(Dir);
	if (Code == MD_OK) {
	    CacheDirectoryForClosing(Dir, MD_OK);
	    return (0);
	}
	if (Dir->MaxOpenMode == Code) {
	    if (Dir->MaxOpenMode != MD_READ) {
		Dir->OpenMode = Code;
		return (0);            /* A-OK */
	    }
	    sprintf(fname, "%s/%s", Dir->UNIXDir, MS_DIRNAME);
	    if (stat(fname, &statbuf) != 0) {
		AMS_RETURN_ERRCODE(errno, EIN_STAT, EVIA_CHECKOPENMSDIR);
	    }
	    if (statbuf.st_mtime == Dir->FileDateWhenRead) {
		debug(16, ("Directory %s is up to date\n", fname));
		Dir->OpenMode = Code;
		return (0);            /* Representation is up-to-date */
	    }
	    debug(16, ("Directory %s has changed; I will reread it now\n", fname));
	}
	if (Dir->MaxOpenMode >= MD_WRITE && Code == MD_READ) {
	    if (Code > Dir->OpenMode) {
		Dir->OpenMode = Code;
	    }
	    return (0);
	}
	if (CloseMSDir(Dir, Dir->OpenMode)) {
	    return (mserrcode);
	}
    }
    if (OpenMSDirectory(Dir, Code)) {
	return (mserrcode);
    }
    if (saveerr = ReadOldMSDirectoryHead(Dir)) {
	CloseMSDir(Dir, Code);
	return (saveerr);
    }
    return (0);
}

/* The following routine appends a message to a directory.
  The directory need not already be open.  It will NOT
  be closed by this routine.
  */

int             AppendMessageToMSDir(Msg, Dir)
struct MS_Message *Msg;
struct MS_Directory *Dir;
{
    char            SBuf[AMS_SNAPSHOTSIZE];
    unsigned long int mTime, timeTemp, bumpLastMsgDate = (unsigned long) 0;
    long            fpos;
    char            ErrBuf[100 + MAXPATHLEN];

    debug(1, ("AppendMessageToMSDir\n"));
    if ((Dir->fd < 0) || (Dir->OpenMode < MD_APPEND)) {
	AMS_RETURN_ERRCODE(EMSDIRNOTOPEN, EIN_PARAMCHECK, EVIA_APPENDMESSAGETOMSDIR);
    }

#ifdef NO_LONGER_CHECKING_FOR_DUPLICATES

    if (IsMessageAlreadyThere(Msg, Dir)) {
	AMS_RETURN_ERRCODE(EMSALREADYTHERE, EIN_PARAMCHECK, EVIA_APPENDMESSAGETOMSDIR);
    }

#endif                                 /* NO_LONGER_CHECKING_FOR_DUPLICATES */

    /* Check the date field to preserve strict ordering by date field */
    /* Do not accept any dates in the *future* */
    timeTemp = time(0);                /* Current time */
    if (timeTemp > (unsigned long) 0xc0000000) {
	AMS_RETURN_ERRCODE(EMSCLOCKBOGUS, EIN_PARAMCHECK, EVIA_APPENDMESSAGETOMSDIR);
    }
    mTime = conv64tolong(AMS_DATE(Msg->Snapshot));
    if (mTime > timeTemp) {
	mTime = timeTemp;
	if (mTime > (unsigned long) 0xc0000000) {
	    sprintf(ErrBuf, "Date in message %s, %d=%s, seems bogus.", AMS_ID(Msg->Snapshot), mTime, NiceTime(mTime));
	    NonfatalBizarreError(ErrBuf);
	}
	strncpy(AMS_DATE(Msg->Snapshot), convlongto64(mTime, 0), AMS_DATESIZE);
    }

    /* But even more important, do not allow the dates to get out of order */
    if (Dir->MessageCount > 0) {
        if (Dir->LastMsgDate[0]) {
            char time64[AMS_DATESIZE + 1];

            bcopy(Dir->LastMsgDate, time64, AMS_DATESIZE);
            time64[AMS_DATESIZE] = '\0';
            timeTemp = conv64tolong(time64);
            if (timeTemp >= mTime) {
                mTime = timeTemp + 1;
                debug(16, ("New message is older than LastMsgDate, bumping both...\n"));
		if (mTime > (unsigned long) 0xc0000000 || mTime > ((unsigned long) time(0) + 7 * 24 * 60 * 60)) {
		    sprintf(ErrBuf, "Dates in folder %s, %d=%s, are bogus; folder needs reconstruction.", ap_Shorten(Dir->UNIXDir), mTime, NiceTime(mTime));
		    NonfatalBizarreError(ErrBuf);
		}
		strncpy(AMS_DATE(Msg->Snapshot), convlongto64(mTime, 0), AMS_DATESIZE);
	    }
	    bumpLastMsgDate = mTime;
        }
        else {
            if (GetSnapshotByNumber(Dir,
                                     Dir->MessageCount - 1,
                                     SBuf)) {
                return (mserrcode);
            }
            timeTemp = conv64tolong(AMS_DATE(SBuf));
            if (timeTemp >= mTime) {
                mTime = timeTemp + 1;
                debug(16, ("New message is older than LastMsgDate, bumping both...\n"));
                if (mTime > (unsigned long) 0xc0000000 || mTime > ((unsigned long) time(0) + 7 * 24 * 60 * 60)) {
                    sprintf(ErrBuf, "Dates in folder %s, %d=%s, are bogus; folder needs reconstruction.", ap_Shorten(Dir->UNIXDir), mTime, NiceTime(mTime));
                    NonfatalBizarreError(ErrBuf);
                }
                strncpy(AMS_DATE(Msg->Snapshot), convlongto64(mTime, 0), AMS_DATESIZE);
            }
            bumpLastMsgDate = mTime;
        }
    }

    if (SetChainField(Msg, Dir, TRUE))
	return (mserrcode);

    fpos = AMS_DIRHEADSIZE + (Dir->MessageCount * AMS_SNAPSHOTSIZE);
    if (Dir->CurPos != fpos && lseek(Dir->fd, fpos, L_SET) < 0) {
	AMS_RETURN_ERRCODE(errno, EIN_LSEEK, EVIA_APPENDMESSAGETOMSDIR);
    }
    if (writeall(Dir->fd, Msg->Snapshot, AMS_SNAPSHOTSIZE) != AMS_SNAPSHOTSIZE) {
	Dir->CurPos = -1;
	AMS_RETURN_ERRCODE(errno, EIN_WRITE, EVIA_APPENDMESSAGETOMSDIR);
    }
    Dir->CurPos = fpos + AMS_SNAPSHOTSIZE;
    Dir->MessageCount++;
    if (bumpLastMsgDate > 0) {
        strncpy(Dir->LastMsgDate, convlongto64(bumpLastMsgDate, 0),
                 AMS_DATESIZE);
        debug(16, ("Set LastMsgDate to %s",
                    ctime(&bumpLastMsgDate)));
    }
    debug(16, ("Almost done appending message; writing out head again\n"));
    return (DestructivelyWriteDirectoryHead(Dir));
}



struct HashList {
    int             num, size;
    unsigned long  *hashes;
    Boolean         checkMIDHash;
    unsigned long   MIDHash;
};

struct MergeEntry {
    int             chain, snapshotNum;
};

struct MergeList {
    int             num, size;
    struct MergeEntry *merges;
};

int             SetChainField(Msg, Dir, PlanningHeadWrite)
struct MS_Message *Msg;
struct MS_Directory *Dir;
Boolean         PlanningHeadWrite;
{
    struct HashList h;
    struct MergeList m;
    int             i, thisChain = 0, chosenChain = 0, goBackTo = Dir->MessageCount;
    unsigned long   aChain;
    char            SBuf[AMS_SNAPSHOTSIZE + 1];
    Boolean         SBufFull;
    Boolean         MustWriteHead = FALSE;
    int             CacheTries, CacheMisses, CacheGets;

    debug(2, ("Setting chain field from hashed values\n"));

    h.num = h.size = 0;
    h.hashes = NULL;

    m.num = m.size = 0;
    m.merges = NULL;

    if (Dir->NumIDs < Dir->MessageCount && Dir->MessageCount > 0) {
	if (AdjustIDs(Dir, Dir->MessageCount) != 0) {
	    AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_SETCHAIN);
	}
    }

    if (!ConstructHashList(Msg, &h))
	AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_SETCHAIN);

    CacheTries = CacheMisses = CacheGets = 0;

    if (h.num) {                       /* Are we looking for anything at all? */
	for (i = Dir->MessageCount - 1; i >= 0; --i) {  /* Then start looking */

	    ++CacheTries;
	    SBufFull = 0;
	    if (Dir->IDs[i].midH == noKRHash || Dir->IDs[i].repH == noKRHash) {
		if (GetSnapshotByNumber(Dir, i, SBuf)) {
		    if (h.hashes)
			free(h.hashes);
		    if (m.merges)
			free(m.merges);
		    return (mserrcode);
		}
		SBufFull = 1;
		++CacheMisses;
	    }

	    if (AnythingMatches(Dir->IDs[i].midH, Dir->IDs[i].repH, &h)) {      /* Should msg 'i' be in
										 * this chain? */

		debug(16, ("Found a match in message %d\n", i));

		thisChain = Dir->IDs[i].Chn;
		if (!chosenChain) {
		    if (thisChain)
			chosenChain = thisChain;
		    else {
			chosenChain = ++(Dir->MaxChainVal);
			MustWriteHead = TRUE;
		    }
		    debug(16, ("Chose chain %d\n", chosenChain));
		}
		if ((!thisChain) || (chosenChain != thisChain)) {
		    if (SBufFull == 0) {
			if (GetSnapshotByNumber(Dir, i, SBuf)) {
			    if (h.hashes)
				free(h.hashes);
			    if (m.merges)
				free(m.merges);
			    return (mserrcode);
			}
			SBufFull = 1;
			++CacheGets;
		    }
		    aChain = htonl(chosenChain);
		    bcopy(&aChain, AMS_CHAIN(SBuf), sizeof(unsigned long));
		    if (RewriteSnapshotInDirectory(Dir, i, SBuf)) {
			if (h.hashes)
			    free(h.hashes);
			if (m.merges)
			    free(m.merges);
			return (mserrcode);
		    }
		    Dir->IDs[i].Chn = chosenChain;
		    if (thisChain) {
			debug(16, ("Considering merging chain %d... ", thisChain));
			if (!AlreadyMergingChain(thisChain, &m)) {
			    if (!AddMerge(thisChain, i, &m)) {
				if (h.hashes)
				    free(h.hashes);
				if (m.merges)
				    free(m.merges);
				AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_SETCHAIN);
			    }
			    debug(16, ("queued.\n"));
			    goBackTo = i;       /* This tells us where we can
						 * stop in the next loop */
			}
			else
			    debug(16, ("already considered it.\n"));
		    }
		}
	    }
	    else {                     /* Nothing matches, but we might merge
					* this chain anyway */
		thisChain = Dir->IDs[i].Chn;
		if (AlreadyMergingChain(thisChain, &m)) {
		    debug(16, ("No hashed matches, but we're advance-merging this chain (%d)\n",
			       thisChain));
		    if (SBufFull == 0) {
			if (GetSnapshotByNumber(Dir, i, SBuf)) {
			    if (h.hashes)
				free(h.hashes);
			    if (m.merges)
				free(m.merges);
			    return (mserrcode);
			}
			SBufFull = 1;
			++CacheGets;
		    }
		    aChain = htonl(chosenChain);
		    bcopy(&aChain, AMS_CHAIN(SBuf), sizeof(unsigned long));
		    if (RewriteSnapshotInDirectory(Dir, i, SBuf)) {
			if (h.hashes)
			    free(h.hashes);
			if (m.merges)
			    free(m.merges);
			return (mserrcode);
		    }
		    Dir->IDs[i].Chn = chosenChain;
		}
	    }
	}

	/* Now take care of the merges we missed on the first pass */
	/* Hopefully, goBackTo is large. */
	for (i = Dir->MessageCount - 1; i > goBackTo; --i) {

	    ++CacheTries;
	    SBufFull = 0;
	    if (Dir->IDs[i].Chn == -1) {
		if (GetSnapshotByNumber(Dir, i, SBuf)) {
		    if (h.hashes)
			free(h.hashes);
		    if (m.merges)
			free(m.merges);
		    return (mserrcode);
		}
		SBufFull = 1;
		++CacheMisses;
	    }

	    thisChain = Dir->IDs[i].Chn;
	    if (NeedToMerge(thisChain, i, &m)) {

		debug(16, ("Merging a missed message in chain %d\n", thisChain));

		if (SBufFull == 0) {
		    if (GetSnapshotByNumber(Dir, i, SBuf)) {
			if (h.hashes)
			    free(h.hashes);
			if (m.merges)
			    free(m.merges);
			return (mserrcode);
		    }
		    SBufFull = 1;
		    ++CacheGets;
		}
		aChain = htonl(chosenChain);
		bcopy(&aChain, AMS_CHAIN(SBuf), sizeof(unsigned long));
		if (RewriteSnapshotInDirectory(Dir, i, SBuf)) {
		    if (h.hashes)
			free(h.hashes);
		    if (m.merges)
			free(m.merges);
		    return (mserrcode);
		}
		Dir->IDs[i].Chn = chosenChain;
	    }
	}
    }

    if (m.num)
	debug(16, ("We should have just finished merging %d chains\n", m.num));
    debug(16, ("Dir size %d: %d attempts, %d cache misses, %d overwrite-misses\n",
		Dir->MessageCount, CacheTries, CacheMisses, CacheGets));

    if (h.hashes)
	free(h.hashes);
    if (m.merges)
	free(m.merges);

    aChain = htonl(chosenChain);
    bcopy(&aChain, AMS_CHAIN(Msg->Snapshot), sizeof(unsigned long));

    if (MustWriteHead && !PlanningHeadWrite) {
	debug(2, ("Bumped chain count: writing out dir head\n"));
	return (DestructivelyWriteDirectoryHead(Dir));
    }
    return (0);
}

static int      AlreadyMergingChain(chain, m)
int             chain;
struct MergeList *m;
{
    int             result = FALSE, i;

    for (i = 0; (i < m->num) && !result; ++i)
	result = (chain == m->merges[i].chain);
    return (result);
}

static int      NeedToMerge(chain, snapshotNum, m)
int             chain, snapshotNum;
struct MergeList *m;
{
    int             i, decided = FALSE, result = FALSE;

    for (i = 0; (i < m->num) && !decided; ++i) {
	if (chain == m->merges[i].chain) {
	    decided = TRUE;
	    result = (snapshotNum > m->merges[i].snapshotNum);
	}
    }
    return (result);
}

/* Returns TRUE on success, FALSE on failure */
static int      ConstructHashList(Msg, h)
struct MS_Message *Msg;
struct HashList *h;
{
    int             len = 0;
    char           *LineBuf = NULL, *s = NULL, *t = NULL;
    unsigned long   hashedVal;

    if (Msg->ParsedStuff->HeadBody[HP_INREPLYTO]) {
	len = Msg->ParsedStuff->HeadBodyLen[HP_INREPLYTO];
	LineBuf = malloc(1 + len);
	if (!LineBuf) {
	    return (FALSE);
	}
	strncpy(LineBuf, Msg->ParsedStuff->HeadBody[HP_INREPLYTO], len);
	LineBuf[len] = '\0';
    }
    if (Msg->ParsedStuff->HeadBody[HP_REFERENCES]) {
	int             rlen;

	rlen = Msg->ParsedStuff->HeadBodyLen[HP_REFERENCES];
	if (LineBuf) {
	    LineBuf = realloc(LineBuf, rlen + len + 3);
	    if (!LineBuf) {
		return (FALSE);
	    }
	    strcat(LineBuf, ", ");
	    strncat(LineBuf, Msg->ParsedStuff->HeadBody[HP_REFERENCES], rlen);
	    LineBuf[rlen + len + 2] = '\0';
	}
	else {
	    LineBuf = malloc(rlen + 1);
	    if (!LineBuf) {
		return (FALSE);
	    }
	    strncpy(LineBuf, Msg->ParsedStuff->HeadBody[HP_REFERENCES], rlen);
	    LineBuf[rlen] = '\0';
	}
    }
    t = LineBuf;
    while (t) {
	if (s = strchr(t, '<')) {
	    t = strchr(++s, '>');
	    if (t)
		*t++ = '\0';

	    hashedVal = KRHash(s);

	    debug(16, ("Adding hash value %d, which is a hash for %s\n", hashedVal, s));

	    if (!AddHash(hashedVal, h)) {
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
    GetRightMid(Msg, &LineBuf);
    if (LineBuf) {
	h->MIDHash = KRHash(LineBuf);
	h->checkMIDHash = TRUE;
	debug(16, ("Also adding hash value %d (for message id %s)\n", h->MIDHash, LineBuf));
	free(LineBuf);
    }
    else {
	debug(16, ("Not adding a hash entry for the message id\n"));
	h->checkMIDHash = FALSE;
    }
    return (TRUE);                     /* Success! */
}

#define GROWARRAY (16)

/* Returns TRUE on success, FALSE on failure */
static int      AddHash(hash, h)
unsigned long   hash;
struct HashList *h;
{
    if (h->num == h->size) {
	if (h->size) {
	    if (!(h->hashes = (unsigned long *) realloc(h->hashes,
							(h->size + GROWARRAY) * sizeof(unsigned long))))
		return (FALSE);
	}
	else {
	    if (!(h->hashes = (unsigned long *) malloc(GROWARRAY * sizeof(unsigned long))))
		return (FALSE);
	}
	h->size += GROWARRAY;
    }
    h->hashes[(h->num)++] = hash;
    return (TRUE);
}

static int      AddMerge(chain, snapshotNum, m)
int             chain, snapshotNum;
struct MergeList *m;
{
    if (m->num == m->size) {
	if (m->size) {
	    if (!(m->merges = (struct MergeEntry *) realloc(m->merges,
							    (m->size + GROWARRAY) * sizeof(struct MergeEntry))))
		return (FALSE);
	}
	else {
	    if (!(m->merges = (struct MergeEntry *) malloc(GROWARRAY * sizeof(struct MergeEntry))))
		return (FALSE);
	}
	m->size += GROWARRAY;
    }
    m->merges[m->num].chain = chain;
    m->merges[(m->num)++].snapshotNum = snapshotNum;
    return (TRUE);
}

static int      AnythingMatches(midHash, repHash, h)
unsigned long   midHash, repHash;
struct HashList *h;
{
    int             i, result = FALSE;

    /* First, check SBuf's MIDHASH against the various reply-to entries in h */

    for (i = 0; !result && (i < h->num); ++i)
	result = (midHash == h->hashes[i]);

    if (!result && h->checkMIDHash) {  /* Just won't give up, will you? */
	result = ((0x7fffffff & repHash) == h->MIDHash);        /* high bit means only
								 * that the message has
								 * multiple
								 * in-reply-to:/reference
								 * s: values.  Low 31
								 * bits still are a hash
								 * value. */
    }

    return (result);
}

/*   Beginning of directory cache code */

static int      CacheAccesses = 0;     /* Incremented for each cache access,
    * used to tell which entry was most
    * recently accessed */

struct dcnode  *Darray[DIRHASHSIZE];

InitializeDirCacheState()
{
    bzero(Darray, DIRHASHSIZE * sizeof(struct dcnode *));
}

/* This routine is for consistency checking in the attempt to find out faster
    about certain kinds of fd leaks that keep getting introduced and ferreted
    out in almost every major release of the messageserver */

static int      CloseEntireDirCache()
{
    int             i;
    struct dcnode  *dc;
    char            ErrorText[100 + MAXPATHLEN];
    int             count = 0;

    for (i = 0; i < DIRHASHSIZE; ++i) {
	if (Darray[i]) {
	    for (dc = Darray[i]; dc; dc = dc->next) {
		if (dc->Dir->fd >= 0) {
		    ++count;
		    if (CloseMSDir(dc->Dir, dc->Dir->OpenMode)) {
			sprintf(ErrorText, "Unexpectedly found folder %s open; close FAILED! (%d, %d, %d).", dc->Dir->UNIXDir, AMS_ERRNO, AMS_ERRCAUSE, AMS_ERRVIA);
		    }
		    else {
			sprintf(ErrorText, "Unexpectedly found folder %s open; closed it satisfactorily.", ap_Shorten(dc->Dir->UNIXDir));
		    }
		    CriticalBizarreError(ErrorText);
		}
	    }
	}
    }
    debug(1, ("Consistency check found %d open folders\n", count));
    if (count > 1) {
	sprintf(ErrorText, "Consistency check found %d open folders", count);
	CriticalBizarreError(ErrorText);
    }
    return 0;
}

static int      HashDir(name)
char           *name;
{
    int             total = 0;
    char           *s;

    debug(1, ("HashDir\n"));
    for (s = name; *s != '\0'; ++s) {
	total += (int) *s;
    }
    return (total & (DIRHASHSIZE - 1));
}

struct MS_Directory *
FindInDirCache(FullName)
char           *FullName;
{
    int             hval;
    struct dcnode  *dc;

    debug(1, ("Searching in directory cache for %s\n", FullName));
    hval = HashDir(FullName);
    for (dc = Darray[hval];
	  dc != NULL && strcmp(dc->Dir->UNIXDir, FullName);
	  dc = dc->next) {
	debug(16, ("Hash collison: %s and %s\n", dc->Dir->UNIXDir, FullName));
    }
    if (dc == NULL) {
	return ((struct MS_Directory *) - 1);   /* Error return */
    }
    ++dc->refcount;
    dc->refwhen = ++CacheAccesses;
    return (dc->Dir);
}


AddToDirCache(Dir, ReplaceIfExists)
struct MS_Directory *Dir;
Boolean         ReplaceIfExists;
{
    int             hval;
    struct dcnode  *dc, *prevdc;

    debug(1, ("Adding %s to directory cache\n", Dir->UNIXDir));
    hval = HashDir(Dir->UNIXDir);
    prevdc = NULL;
    for (dc = Darray[hval];
	  dc != NULL && strcmp(dc->Dir->UNIXDir, Dir->UNIXDir);
	  dc = dc->next) {
	prevdc = dc;
	debug(16, ("Hash collison: %s and %s\n", dc->Dir->UNIXDir, Dir->UNIXDir));
    }
    if (dc != NULL) {
	if (ReplaceIfExists) {
	    EnsureNotInCache(dc->Dir);

	    /*
	     * FreeDirectory(dc->Dir);
	     *//* Cannot free a permanent malloc, so we waste it */
	    dc->Dir = Dir;
	    ++dc->refcount;
	    dc->refwhen = ++CacheAccesses;
	    return (0);
	}
	debug(16, ("Error: trying to add a cache entry that's already there\n"));
	AMS_RETURN_ERRCODE(EMSDIRNAMECOLLISION, EIN_PARAMCHECK, EVIA_ADDTODIRCACHE);
    }
    dc = (struct dcnode *) permanentmalloc(sizeof(struct dcnode));
    if (dc == NULL) {
	AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_ADDTODIRCACHE);
    }
    dc->Dir = Dir;
    dc->next = NULL;
    dc->refcount = 1;
    dc->refwhen = ++CacheAccesses;
    /* strcpy(dc->name, Dir->UNIXDir); */
    if (prevdc == NULL) {
	Darray[hval] = dc;
    }
    else {
	prevdc->next = dc;
    }
    return (0);
}

/* An amazingly useful performance hack */

static struct MS_Directory *LastFind = NULL;

/* Which occasionally needs to be corrected */

static int      EnsureNotInCache(Dir)
struct MS_Directory *Dir;
{
    if (Dir == LastFind)
	LastFind = NULL;
    return 0;
}

/* Note that in the following, pDir is a pointer to a pointer -- this routine
  actually sets the pointer */
int             ReadOrFindMSDir(Name, pDir, Code)
char           *Name;
struct MS_Directory **pDir;
int             Code;
{
    return (ReadOrFindMSDir_Complain(Name, pDir, Code, TRUE));
}

int             ReadOrFindMSDir_Complain(Name, pDir, Code, DoComplain)
char           *Name;
struct MS_Directory **pDir;
int             Code, DoComplain;
{
    int             saveerr;

    debug(1, ("ReadOrFindMSDir %s (%d)\n", Name, Code));
#ifdef CMU_ENV
    logging_hook();
#endif
    if (LastFind && !strcmp(LastFind->UNIXDir, Name)) {
	*pDir = LastFind;
    }
    else {
	*pDir = FindInDirCache(Name);
	if (*pDir == (struct MS_Directory *) - 1) {
	    *pDir = (struct MS_Directory *) permanentmalloc(sizeof(struct MS_Directory));
	    if (!*pDir) {
		AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_READORFINDMSDIR);
	    }
	    bzero(*pDir, sizeof(struct MS_Directory));
	    (*pDir)->fd = -1;
	    (*pDir)->CurPos = -1;
	    (*pDir)->LastIDHit = -1;
	    (*pDir)->OpenMode = -1;
	    (*pDir)->MaxOpenMode = -1;
	    (*pDir)->AttrNames = NULL;
	    (*pDir)->AttrCount = 0;
	    (*pDir)->IDs = NULL;
	    (*pDir)->NumIDs = 0;
	    (*pDir)->CheckedWritable = 0;
	    (*pDir)->Writable = 0;
	    (*pDir)->BadScavenge = 0;
	    (*pDir)->UNIXDir = permanentmalloc(1 + strlen(Name));
	    if (!(*pDir)->UNIXDir) {
		*pDir = NULL;          /* Cannot free a permanent malloc,
					* sigh... */
		AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_READORFINDMSDIR);
	    }
	    strcpy((*pDir)->UNIXDir, Name);
	    if (OpenMSDirectory(*pDir, (Code == MD_OK) ? MD_READ : Code)) {
		*pDir = NULL;          /* Cannot free a permanent malloc,
					* sigh... */
		return (mserrcode);
	    }
	    if (saveerr = ReadOldMSDirectoryHead_Complain(*pDir, DoComplain)) {
		CloseMSDir(*pDir, (Code == MD_OK ? MD_READ : Code));
		*pDir = NULL;          /* Cannot free a permanent malloc, sigh... */
		return (saveerr);
	    }
	    if ((Code == MD_OK) && CacheDirectoryForClosing(*pDir, MD_READ)) {
		*pDir = NULL;          /* Cannot free a permanent malloc, sigh... */
		return (mserrcode);
	    }
	    if (AddToDirCache(*pDir, FALSE)) {
		*pDir = NULL;          /* Cannot free a permanent malloc, sigh... */
		return (mserrcode);
	    }
	    LastFind = *pDir;
	    return (0);
	}
    }
    if (Code != MD_OK && CheckOpenMSDirectory(*pDir, Code)) {
	/* error code was set */
	return (mserrcode);
    }
    LastFind = *pDir;
    return (0);
}

/* The following is for asynchronous processing of things like the deleted bit */

extern int      NeedToTimeOut;

#define MAXPENDINGCLOSES 4
static struct MS_Directory *PendingCloses[MAXPENDINGCLOSES];
static int      ClosesPending = 0;

/* The following is called by CheckOpenMSDirectory to remove the directory
   from the list of pending closes */

static int NoteRecentDirUse(Dir)
struct MS_Directory *Dir;
{
    int             i;

    Dir->LastOpenTimestamp = time(0);
    for (i = 0; i < ClosesPending; ++i) {
	if (PendingCloses[i] == Dir) {
	    break;
	}
    }
    if (i < ClosesPending) {
	ClosesPending--;
    }
    for (;i < ClosesPending; ++i) {
	PendingCloses[i] = PendingCloses[i+1];
    }
    return 0;
}

int             CacheDirectoryForClosing(Dir, CloseCode)
struct MS_Directory *Dir;
int             CloseCode;
{
    char            ErrorText[256];
    int             i;

    Dir->CurPos = -1;
    if (Dir->fd < 0) {
        AMS_RETURN_ERRCODE(EMSDIRNOTOPEN, EIN_PARAMCHECK, EVIA_CLOSEMSDIR);
    }
    if (CloseCode < Dir->OpenMode) {
        /* It is already open for a higher purpose */
        debug(128, ("Skipping close of %s due to low open mode (%d, %d)\n", Dir->UNIXDir, CloseCode, Dir->OpenMode));
	return (0);
    }
    for (i = 0; i < ClosesPending; ++i) {
	if (!strcmp(Dir->UNIXDir, PendingCloses[i]->UNIXDir)) {
	    sprintf(ErrorText, "The directory %s had two Dirnodes -- this shouldn't happen!", ap_Shorten(Dir->UNIXDir));
	    CriticalBizarreError(ErrorText);
	}
    }
    if (Dir->OpenMode != CloseCode) {
	sprintf(ErrorText, "Caching %s to close as %d but it is really %d!", ap_Shorten(Dir->UNIXDir), CloseCode, Dir->OpenMode);
	NonfatalBizarreError(ErrorText);
	CriticalBizarreError("I've found a bug!  Please send the automatic bug report!");
    }
    if (ClosesPending >= MAXPENDINGCLOSES) {
	mserrcode = CloseMSDir(PendingCloses[0], MD_OK);
	if (mserrcode) {
	    if (AMS_ERRNO == EMSDIRNOTOPEN && AMS_ERRCAUSE == EIN_PARAMCHECK
		&& AMS_ERRVIA == EVIA_CLOSEMSDIR) {
		sprintf(ErrorText, "MS directory %s was closed prematurely; everything should still be OK.", ap_Shorten(PendingCloses[0]->UNIXDir));
	    }
	    else {
		sprintf(ErrorText, "Could not close directory %s; some state may have been lost (%d %d %d).", ap_Shorten(PendingCloses[0]->UNIXDir), AMS_ERRNO, AMS_ERRCAUSE, AMS_ERRVIA);
	    }
	    NonfatalBizarreError(ErrorText);
	}
	for (i = 0; i < (MAXPENDINGCLOSES - 1); ++i) {
	    PendingCloses[i] = PendingCloses[i + 1];
	}
	ClosesPending = MAXPENDINGCLOSES - 1;
    }
    PendingCloses[ClosesPending++] = Dir;
    Dir->OpenMode = MD_OK;
    NeedToTimeOut = 1;
    return (0);
}

#define SHORTOPENTIME 60               /* one minute */

int             CloseDirsThatNeedIt()
{
    return (CloseNeedyDirs(TRUE));
}

int             CloseNeedyDirs(InsistOnCloses)
Boolean         InsistOnCloses;
{
    int             i, mycode = 0, errct = 0;
    struct MS_Directory *StillPendingCloses[MAXPENDINGCLOSES];
    int             ClosesStillPending = 0;
    Boolean         NoProblems = TRUE;;

    for (i = 0; i < ClosesPending; ++i) {
	if (!InsistOnCloses && (time(0) - PendingCloses[i]->LastOpenTimestamp) < SHORTOPENTIME) {
	    /* char ErrorText[100+MAXPATHLEN]; */

	    /* sprintf(ErrorText, "Folder %s is recently used and is being kept open through a checkpoint.",  ap_Shorten(PendingCloses[i]->UNIXDir)); */
	    /* NonfatalBizarreError(ErrorText); */
	    NoProblems = FALSE;
	    StillPendingCloses[ClosesStillPending++] = PendingCloses[i];
	}
	else {
	    if (PendingCloses[i]->fd >= 0) {
		mserrcode = CloseMSDir(PendingCloses[i], MD_OK);
		if (mserrcode) {
		    ++errct;
		    mycode = mserrcode;
		}
	    }
	}
    }
    ClosesPending = ClosesStillPending;
    for (i = 0; i < ClosesStillPending; ++i) {
	PendingCloses[i] = StillPendingCloses[i];
    }
    if (errct) {
	if (errct > 1) {
	    mserrcode = mycode;
	    if (vdown(AMS_ERRNO))
		return (mserrcode);
	    AMS_RETURN_ERRCODE(EMSMULTIPLECLOSEERRS, EIN_CLOSEMSDIR, EVIA_CLOSEDIRSTHATNEEDIT);
	}
	else {
	    return (mycode);
	}
    }
    if (NoProblems)
	CloseEntireDirCache();         /* Just a consistency check, reports
					* its own problems */
    return (0);
}

int             DeleteFromDirCache(Dir)
struct MS_Directory *Dir;
{
    int             hval;
    struct dcnode  *dc, *prevdc;

    debug(1, ("Deleting %s from dir cache\n", Dir->UNIXDir));
    hval = HashDir(Dir->UNIXDir);
    prevdc = NULL;
    for (dc = Darray[hval]; dc != NULL; dc = dc->next) {
	if (!strcmp(dc->Dir->UNIXDir, Dir->UNIXDir)) {
	    /* This is the guy!  Nuke him! */
	    if (prevdc) {
		prevdc->next = dc->next;
	    }
	    else {
		Darray[hval] = dc->next;
	    }
	    /* free(dc); *//* Nope, Cannot free a permanent malloc */
	    return (0);
	}
	prevdc = dc;
    }
    return (0);
}

/* Get message-id or resent-mid (if any resent-xxx) or null string if no appropriate mid */

int             GetRightMid(Msg, mid)
struct MS_Message *Msg;
char          **mid;
{
    char           *start, *end, *ats;
    int             len;

    if (Msg->ParsedStuff->HeadBody[HP_RESENTDATE] || Msg->ParsedStuff->HeadBody[HP_RESENTFROM]) {
	start = Msg->ParsedStuff->HeadBody[HP_RESENTMESSAGEID];
	len = Msg->ParsedStuff->HeadBodyLen[HP_RESENTMESSAGEID];
    }
    else {
	start = Msg->ParsedStuff->HeadBody[HP_MESSAGEID];
	len = Msg->ParsedStuff->HeadBodyLen[HP_MESSAGEID];
    }
    if (!start || (len <= 0)) {
	*mid = NULL;
	return (0);
    }
    end = start + len - 1;
    while ((start < end) && isspace(*start)) {
	++start;
	--len;
    }
    while ((end > start) && isspace(*end)) {
	--end;
	--len;
    }
    for (ats = start; ats < end; ++ats)
	if (*ats == '@')
	    break;
    if ((*start != '<') || (*end != '>') || (*ats != '@') || len <= 0) {
	*mid = NULL;
	return (0);
    }
    len -= 2;
    ++start;
    *mid = malloc(1 + len);
    if (!*mid)
	AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_SETCHAIN);
    strncpy(*mid, start, len);
    (*mid)[len] = '\0';
    return (0);
}


int             IsMessageAlreadyThere(Msg, Dir)
struct MS_Message *Msg;
struct MS_Directory *Dir;
{
    char           *TxtMsg, *mid, *secondmid, SBuf[AMS_SNAPSHOTSIZE + 1], SourceFileName[1 + MAXPATHLEN];
    int             i;
    int             unCached;
    long            midhash, refint;
    struct MS_Message *MsgToCheck;

    SBuf[0] = '\0';
    GetRightMid(Msg, &mid);
    if (!mid)
	return (0);
    midhash = KRHash(mid);
    debug(2, ("Message-id %s hashed to %d; looking for a match\n", mid, midhash));
    if (Dir->NumIDs < Dir->MessageCount && Dir->MessageCount > 0) {
	if (AdjustIDs(Dir, Dir->MessageCount) != 0)
	    return (0);                /* malloc failure */
    }
    unCached = 0;
    for (i = Dir->MessageCount - 1; i >= 0; --i) {
	refint = Dir->IDs[i].midH;
	if (refint == noKRHash) {      /* get it if we haven't already gotten
		      * it */
	    ++unCached;
	    if (GetSnapshotByNumber(Dir, i, SBuf)) {
		continue;              /* slightly bogus -- could lead to
					* duplicate messages */
	    }
	    refint = Dir->IDs[i].midH; /* Field is loaded by
					* GetSnapshotByNumber */
	}
	if (refint != midhash) {
	    /* debug(2, ("snapshot #%d is not a match\n", i)); */
	    continue;
	}
	/* The hashes match, we must look at the REAL Message-ID */
	debug(2, ("Got a match with snapshot %d hash; looking more carefully\n", i, refint));
	if ((MsgToCheck = (struct MS_Message *) malloc(sizeof(struct MS_Message))) == NULL) {
	    continue;                  /* slightly bogus -- ENOMEM ->
					* occasional duplicate messages */
	}
	bzero(MsgToCheck, sizeof(struct MS_Message));
	MsgToCheck->OpenFD = -1;
	if (SBuf[0] == '\0')	/* To avoid duplicate msgs */
	    GetSnapshotByNumber(Dir, i, SBuf);
	QuickGetBodyFileName(Dir->UNIXDir, AMS_ID(SBuf), SourceFileName);
	if (ReadRawFile(SourceFileName, MsgToCheck, FALSE)
	    || ParseMessageFromRawBody(MsgToCheck)) {
	    FreeMessage(MsgToCheck, FALSE);
	    continue;                  /* slightly bogus -- could lead to
					* duplicate messages */
	}
	GetRightMid(MsgToCheck, &secondmid);
	debug(2, ("Comparing message-id %s to message-id %s\n", mid, secondmid));
	if (!secondmid || strcmp(mid, secondmid)) {
	    if (secondmid)
		free(secondmid);
	    FreeMessage(MsgToCheck, FALSE);
	    continue;                  /* they are different */
	}

	/*
	 * Technically, that should be all we need, but we add a few more
	 * checks out of sheer paranoia and past experience.
	 */
	if (FieldsDiffer(Msg, MsgToCheck, HP_SUBJECT) || FieldsDiffer(Msg, MsgToCheck, HP_FROM) || FieldsDiffer(Msg, MsgToCheck, HP_RESENTFROM) || FieldsDiffer(Msg, MsgToCheck, HP_DATE)) {
	    if (secondmid)
		free(secondmid);
	    FreeMessage(MsgToCheck, FALSE);
	    continue;                  /* they are different */
	}
	TxtMsg = malloc(100 + strlen(mid) + strlen(Dir->UNIXDir));
	if (TxtMsg) {
	    sprintf(TxtMsg, "Message %s was already in %s and won't be put there again.", mid, ap_Shorten(Dir->UNIXDir));
	    NonfatalBizarreError(TxtMsg);
	    free(TxtMsg);
	}
	else {
	    NonfatalBizarreError("A message was already in a directory and was not duplicated.");
	}
	FreeMessage(MsgToCheck, FALSE);
	free(mid);
	free(secondmid);
	debug(2, ("Found: had to read %d of %d (%d total)\n", unCached, (Dir->MessageCount - i), Dir->MessageCount));
	return (1);
    }
    free(mid);
    debug(2, ("Not found: had to read %d of %d\n", unCached, Dir->MessageCount));
    return (0);
}

int             FieldsDiffer(M1, M2, field)
struct MS_Message *M1, *M2;
int             field;
{
    char           *paranoid1, *paranoid2;
    int             plen1, plen2;

    paranoid1 = M1->ParsedStuff->HeadBody[field];
    paranoid2 = M2->ParsedStuff->HeadBody[field];
    plen1 = M1->ParsedStuff->HeadBodyLen[field];
    plen2 = M2->ParsedStuff->HeadBodyLen[field];
    if ((plen1 != plen2) || (paranoid1 && !paranoid2) || (!paranoid1 && paranoid2)
	 || (paranoid1 && paranoid2 && strncmp(paranoid1, paranoid2, plen1))) {
	return (1);
    }
    return (0);
}
