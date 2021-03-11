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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/support/RCS/strcache.c,v 1.4 1992/12/15 21:42:39 rr2b R6tape $";
#endif


 

/* A string cache */

#include <andrewos.h>
#include <class.h>
#include <strcache.eh>
#include <util.h>
#include <ctype.h>

static struct strcache *gcache=NULL;

static int lchash(key)
char *key;
{
    char c;
    int index=0;

    while ((c = *key++) != '\0') {
	index += isupper(c)?tolower(c):c;
    }
    index &= (ghash_BUCKETS-1);
    return index;
}

static int lccomp(a,b)
char *a, *b;
{
    if(a==NULL && b==NULL) return 0;
    if(a==NULL) return -1;
    if(b==NULL) return 1;
    return lc_strcmp(a, b);
}

boolean strcache__InitializeClass(classID)
struct classheader *classID;
{
    gcache=strcache_New();
    if(gcache==NULL) return FALSE;
    strcache_SetCompareKey(gcache, lccomp);
    strcache_SetHash(gcache, lchash);
    return TRUE;
}

boolean strcache__InitializeObject(classID,self)
struct classheader *classID;
struct strcache *self;
{
    return TRUE;
}

char *strcache__SaveStr(classID,str)
struct classheader *classID;
char *str;
{
    char *result;
    if(gcache==NULL) return NULL;
    result=strcache_LookupKey(gcache, str);
    if(result) return result;
    if(!strcache_Store(gcache, str, NULL)) return NULL;
    result=strcache_LookupKey(gcache, str);
    return result;
}

#include <glist.ih>
struct enumerate {
    boolean found;
    procedure proc;
    long rock;
    struct ghash *self;
};

struct egg {
    char *key,*value;
};


static boolean EnumProc(e, rock)
struct egg *e;
struct enumerate *rock;
{
    boolean result;
    result=rock->proc(rock->rock, e->value,  e->key, rock->self);
    rock->found=result;
    return result;
}

char *Enumerate(self,proc,rock)
struct ghash *self;
procedure proc;
long rock;
{
    char *result;
    struct enumerate r;
    int i;
    if(self->buckets==NULL) return NULL;
    r.found=FALSE;
    r.proc=proc;
    r.rock=rock;
    r.self=self;
    for(i=0;i<ghash_BUCKETS;i++) {
	printf("bucket %d\n", i);
	if(self->buckets[i]) result=glist_Find(self->buckets[i], EnumProc, &r);
	printf("------\n");
	if(r.found) return result;
    }
    return NULL;
}
    
boolean DumpStr(rock, val, key, self)
long rock;
char *val;
char *key;
struct strcache *self;
{
    printf("key:%s\n", key);
    return FALSE;
}

void strcache__Dump(classID)
struct classheader *classID;
{
    Enumerate(gcache, DumpStr, NULL);
}

void strcache__FinalizeObject(classID,self)
struct classheader *classID;
struct strcache *self;
{

}

char *strcache__Delete(self,key)
struct strcache *self;
char *key;
{
    return NULL;
}
    

char *strcache__Rename(self,key,new)
struct strcache *self;
char *key,*new;
{
    return NULL;
}

