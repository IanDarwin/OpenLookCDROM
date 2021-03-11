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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/support/RCS/ghash.c,v 1.9 1994/04/13 21:55:32 rr2b Exp $";
#endif


/* A hash table */
#include <andrewos.h>
#include <class.h>
#include <glist.ih>
#include <util.h>
#include <ghash.eh>

struct egg {
    char *key,*value;
};



static int DefaultHash(key)
char *key;
{
    char c;
    int index=0;

    while ((c = *key++) != '\0') {
        index += c;
    }
    index &= (ghash_BUCKETS-1);
    return index;
}


boolean ghash__InitializeClass(classID)
struct classheader *classID;
{
    return TRUE;
}

static int safestrcmp(a,b)
char *a, *b;
{
    if(a==NULL && b==NULL) return 0;
    if(a==NULL) return -1;
    if(b==NULL) return 1;
    return strcmp(a, b);
}

boolean ghash__InitializeObject(classID,self)
struct classheader *classID;
struct ghash *self;
{
    int i;
    for(i=0;i<ghash_BUCKETS;i++)
        self->buckets[i] = NULL;
    self->hash = DefaultHash;
    ghash_SetFreeKey(self, free);
    ghash_SetCopyKey(self, NewString);
    ghash_SetCompareKey(self, safestrcmp);
    ghash_SetCopyVal(self, NULL);
    ghash_SetFreeVal(self, NULL);
    return TRUE;
}

void ghash__FinalizeObject(classID,self)
struct classheader *classID;
struct ghash *self;
{
    int i;
    for (i=0;i<ghash_BUCKETS;i++)
        if (self->buckets[i] != NULL)
            glist_Destroy(self->buckets[i]);
}

boolean ghash__Store(self,key,value)
struct ghash *self;
char *key;
char *value;
{
    int bucket = (*self->hash)(key);
    struct egg *egg = (struct egg *)malloc(sizeof(struct egg));

    if(egg==NULL) return FALSE;
    
    if(ghash_GetCopyKey(self)) key = ghash_GetCopyKey(self)(key);
    egg->key = key;
    
    if(ghash_GetCopyVal(self)) value = ghash_GetCopyVal(self)(value);
    egg->value = value;

    if (self->buckets[bucket] == NULL)
        self->buckets[bucket] = glist_Create(NULL);

    glist_Insert(self->buckets[bucket],egg);
    return TRUE;
}

static int (*compkey)()=NULL;

static int FindEgg(egg,key)
struct egg *egg;
char *key;
{

    if (compkey) {
	if(compkey(egg->key, key)==0) return TRUE;
	else return FALSE;

    } else if(egg->key==key)
        return TRUE;
    else
        return FALSE;
}



char *ghash__Lookup(self,key)
struct ghash *self;
char *key;
{
    int bucket = (*self->hash)(key);
    struct egg *egg;

    if (self->buckets[bucket] == NULL) {
        return NULL;
    }
    else {
	compkey=ghash_GetCompareKey(self);
        egg = (struct egg *) glist_Find(self->buckets[bucket],FindEgg,key);
        if (egg != NULL) {
            return egg->value;
        }
        else {
            return NULL;
        }
    }
}

char *ghash__LookupKey(self,key)
struct ghash *self;
char *key;
{
    int bucket = (*self->hash)(key);
    struct egg *egg;

    if (self->buckets[bucket] == NULL) {
        return NULL;
    }
    else {
	compkey=ghash_GetCompareKey(self);
        egg = (struct egg *) glist_Find(self->buckets[bucket],FindEgg,key);
        if (egg != NULL) {
            return egg->key;
        }
        else {
            return NULL;
        }
    }
}

char * ghash__Delete(self,key)
struct ghash *self;
char *key;
{
    int bucket = (self->hash)(key);
    struct egg *egg;
    if (self->buckets[bucket] == NULL)
	return NULL;
    compkey=ghash_GetCompareKey(self);
    egg = (struct egg *)glist_Find(self->buckets[bucket],FindEgg,key);
    if (egg != NULL) {
	char *val = egg->value;
        glist_Delete(self->buckets[bucket],egg,FALSE);
	if(ghash_GetFreeVal(self)) ghash_GetFreeVal(self)(egg->value);
	if(ghash_GetFreeKey(self)) ghash_GetFreeVal(self)(egg->key);
        free(egg);
        return val;
    }
    else
        return NULL;
    
}

struct enumerate {
    boolean found;
    procedure proc;
    long rock;
    struct ghash *self;
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

char *ghash__Enumerate(self,proc,rock)
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
	if(self->buckets[i]) result=glist_Find(self->buckets[i], EnumProc, &r);
	if(r.found) return result;
    }
    return NULL;
}
    

char *ghash__Rename(self,key,new)
struct ghash *self;
char *key,*new;
{
    int bucket = (*self->hash)(key);
    struct egg *egg;
    if (self->buckets[bucket] == NULL)
	return NULL;
    compkey=ghash_GetCompareKey(self);
    egg = (struct egg *)glist_Find(self->buckets[bucket],FindEgg,key);
    if (egg == NULL)
        return NULL;
    else {
        glist_Delete(self->buckets[bucket],egg,FALSE);
	if(ghash_GetFreeKey(self)) ghash_GetFreeKey(self)(egg->key);
	if(ghash_GetCopyKey(self)) key = ghash_GetCopyKey(self)(key);
	egg->key = key;
        bucket = (*self->hash)(new);
        if (self->buckets[bucket] == NULL)
            self->buckets[bucket] = glist_Create(NULL);
        glist_Insert(self->buckets[bucket],egg);
        return egg->value;
    }
}

void ghash__Clear(self)
struct ghash *self;
{
    int i;
    struct egg *egg;
    struct glist *gl;

    for (i=0;i<ghash_BUCKETS;i++) {
	if ((gl = self->buckets[i]) != NULL)  {
	    while ((egg = (struct egg *) glist_Pop(gl)) != NULL)  {
		if (egg->key != NULL)  {
		    if(ghash_GetFreeKey(self)) ghash_GetFreeKey(self)(egg->key);
		}
		if (ghash_GetFreeVal(self))  {
		    ghash_GetFreeVal(self)(egg->value);
		}
		free(egg);
	    }
	    glist_Destroy(self->buckets[i]);
	    self->buckets[i] = NULL;
	}
    }
}


static PrintAll(egg,nothing)
struct egg *egg;
int nothing;
{
    printf("Egg (%s) contains (%s)\n",egg->key,egg->value);
    return FALSE;
}



void ghash__Debug(self)
struct ghash *self;
{
    int i;
    for (i=0;i<ghash_BUCKETS;i++) {
        if (self->buckets[i] == NULL)
            printf("Bucket %d is NULL\n",i);
        else {
            printf("Bucket %d ------------------>\n",i);
            glist_Find(self->buckets[i],PrintAll,NULL);
        }
    }
}
