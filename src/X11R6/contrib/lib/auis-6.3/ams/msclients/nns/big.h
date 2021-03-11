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


#include <andrewos.h>
#include <ms.h>
#include <hdrparse.h>
#include <stdio.h>
#include <sys/types.h>

#define DEFAULT_CONFIG_FILENAME "/etc/nns.conf"
#define CANT_OPEN_DIR (-1)
#define DIRPROBLEMS (2)
#define FILECHUNKSIZE (1024)
#define GL_GROWSIZE (128)
#define GL_TOOBIG (8000)                         /* Arbitrary */
#define GROUPSLINESIZE (2048)                    /* Arbitrary, hope it's
                                                  * enough (I've seen one
                                                  * case requiring 1105
                                                  * chars! */
#define LOSTDIR ("lost")
#define NEWSGROUPNAMELEN (80)
#define NOTIMPLEMENTED (5)
#define NUMMCBUCKETS (64)
#define NUMSTBUCKETS (64)
#define RETRY_INTERVAL (900)
#define STB_GROWSIZE (128)
#define STUPIDUSER (1)
#define CONFIGERR (4)
#define UNKNOWN (3)

typedef char   *STableBucketEntry_t;

typedef struct {
    int             num, size;
    STableBucketEntry_t *entries;
}               STableBucket_t;

typedef struct {
    int             (*HashFn) ();
    STableBucket_t  buckets[NUMSTBUCKETS];
}               STable_t;

typedef struct {
    int             verbose, runOnce, dontDo;
    char           *configFileName;
}               Options_t;

typedef struct {
    Options_t       Options;
    STable_t        STable;
}               Globals_t;

extern Globals_t Globals;

typedef struct {
    char           *filename, *folder;
    int             ahead, before, ignore;
    long            time;
}               GListEntry_t;

typedef struct {
    int             num, size;
    GListEntry_t   *entries;
}               GList_t;

typedef struct MCacheBucketEntry_t {
    char           *filename;
    struct MS_Message *Msg;
    struct MCacheBucketEntry_t *next;
}               MCacheBucketEntry_t;

typedef MCacheBucketEntry_t *MCacheBucket_t;

typedef struct {
    int             (*HashFn) ();
    MCacheBucket_t  buckets[NUMMCBUCKETS];
}               MCache_t;

extern char ReadyBox[], DelayedDir[], FailedDir[], ControlDir[], HoldDir[];

extern GListEntry_t *GLGetEntry();
extern MCacheBucketEntry_t *MCBEGetNext();
extern char    *ConfDirForGroup();
extern char    *GLEGetFilename();
extern char    *GLEGetFolder();
extern char    *MCBEGetFilename();
extern char    *STBEGetString();
extern char    *STBFind();
extern char    *STBMake();
extern char    *STFind();
extern char    *STFindOrMake();
extern char    *STMake();
extern char    *xmalloc();
extern int      ConfCheckPeakTime();
extern int      ConfIsPeakTime();
extern int      GLBuildList();
extern int      GLECompare();
extern int      GLEGetAhead();
extern int      GLEGetBefore();
extern int      GLEGetIgnore();
extern int      GLGetNum();
extern int      MCBMake();
extern int      MCMake();
extern int      ProcessEntry();
extern int      ProcessDir();
extern int      ShouldAlreadyBeCached();
extern int      ShouldBeCached();
extern int      ShouldRename();
extern int      ShouldUnlink();
extern struct MS_Message *MCBEGetMsg();
extern struct MS_Message *MCBFind();
extern struct MS_Message *MCFind();
extern void     ConfInit();
extern void     DotsToSlashesInPlace();
extern void     GLESet();
extern void     GLESetAhead();
extern void     GLESetBefore();
extern void     GLESetIgnore();
extern void     GLIgnore();
extern void     GLInit();
extern void     GLPurge();
extern void     MCBDelete();
extern void     MCBESet();
extern void     MCBESetNext();
extern void     MCBInit();
extern void     MCBPurge();
extern void     MCDelete();
extern void     MCInit();
extern void     MCPurge();
extern void     ProcessList();
extern void     STBESetString();
extern void     STBInit();
extern void     STBPurge();
extern void     STInit();
extern void     STPurge();
extern void     Verbiage();
