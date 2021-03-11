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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/index/RCS/index.c,v 2.12 1993/09/21 21:07:07 gk5g Exp $";
#endif


 

#include <andrewos.h> /* sys/types.h sys/file.h */
#include <stdio.h>
#include "index.h"

/* given an index and a record id, copy out key into abuffer, a buffer of max size alen */
index_GetKey(ai, arid, abuffer, alen)
struct Index *ai;
register struct recordID *arid;
char *abuffer;
register long alen;
{
    register struct indexBucket *tb;
    register struct indexComponent *tc;
    register long rval;
    tb = index_CGetHash(ai, rhash(*arid));
    if (!tb) return INDEXNOENT;
    tc = index_FindID(tb, arid);
    if (tc) {
	strncpy(abuffer, tc->name, alen);
	rval = 0;
    }
    else {
	rval = INDEXNOENT;
    }
    index_CPut(ai, tb);
    return rval;
}

/* given an index and a record id, copy out data into abuffer, a buffer of max size alen */
index_GetData(ai, arid, abuffer, alen)
struct Index *ai;
register struct recordID *arid;
char *abuffer;
register long alen;
{
    register struct indexBucket *tb;
    register struct indexComponent *tc;
    register long rval;
    tb = index_CGetHash(ai, rhash(*arid));
    if (!tb) return INDEXNOENT;
    tc = index_FindID(tb, arid);
    if (tc) {
	strncpy(abuffer, tc->data, alen);
	rval = 0;
    }
    else {
	rval = INDEXNOENT;
    }
    index_CPut(ai, tb);
    return rval;
}

/* internal routine: given a bucket, tell if there are any references from a secondary
    * record the given record id.
    */
static index_RecordInUse(ab, arid)
register struct indexBucket *ab;
register struct recordID *arid;
{
    register struct indexComponent *tc;
    for(tc = ab->list; tc; tc=tc->next) {
	if (tc->primary == 0 && req(*arid, tc->id)) return 1;
    }
    return 0;
}

/*
  * Internal routine: given a bucket, return a pointer to the named primary record, or null
  * if none exist.
      */
struct indexComponent *index_FindID(ab, arid)
register struct indexBucket *ab;
register struct recordID *arid;
{
    register struct indexComponent *tc;
    for(tc = ab->list; tc; tc=tc->next) {
	if (tc->primary == 1 && req(*arid, tc->id)) return tc;
    }
    return (struct indexComponent *) 0;
}

/*
  * Internal routine: given a bucket pointer and a record id, generate the next unique
  * record id for records placed in that bucket.
      */
static index_GenerateKey(ab, arid)
register struct indexBucket *ab;
register struct recordID *arid;
{
    arid->word1 = ab->hashIndex;
    arid->word2 = ab->nextID++;
}

/*
  * Given an index and a key, add a new primary record with the provided associated data.
  * This routine always creates a new record, even if there is already a primary record
      * with the same key.
      */
index_AddPrimary(ai, akey, adata)
register struct Index *ai;
register char *akey;
char *adata;
{
    register struct indexBucket *tb;
    register struct indexComponent *tc;
    tb = index_CGet(ai, akey);
    tc = (struct indexComponent *) malloc(sizeof (struct indexComponent));
    tc->primary = 1;
    index_GenerateKey(tb, &tc->id);
    tc->hashes = index_NewHL();
    tc->name = (char *) malloc(1+strlen(akey));
    strcpy(tc->name, akey);
    tc->data = (char *) malloc(1+strlen(adata));
    strcpy(tc->data, adata);
    tb->modified = 1;
    tc->next = tb->list;
    tb->list = tc;
    index_CPut(ai, tb);
    return 0;
}

/*
  * Given an index, a record id of a primary record and a new key, this function
  * creates a new secondary record pointing to the named primary record.
  */
index_AddSecondary(ai, arid, akey)
register struct Index *ai;
register char *akey;
struct recordID *arid;
{
    register struct indexBucket *tb;
    register struct indexComponent *tc;
    long idUsed, keyHash;
    tb = index_CGet(ai, akey);
    idUsed = index_RecordInUse(tb, arid);
    tc = (struct indexComponent *) malloc(sizeof (struct indexComponent));
    tc->primary = 0;
    rset(tc->id, *arid);
    tc->name = (char *) malloc(1+strlen(akey));
    strcpy(tc->name, akey);
    tb->modified = 1;
    tc->next = tb->list;
    tb->list = tc;
    /* check if need to add to hash list */
    index_CPut(ai, tb);
    if (!idUsed) {	/* if id was not previously referenced from this bucket, add this bucket in */
	/* now add to primary's hash list */
	keyHash = index_Hash(akey, ai->hashTableSize);
	tb = index_CGetHash(ai, rhash(*arid));
	tc = index_FindID(tb, arid);
	if (tc) {
	    if (!index_HashPresent(tc->hashes, keyHash)) {
		index_HashAdd(tc->hashes, keyHash);
		tb->modified = 1;
	    }
	}
	index_CPut(ai, tb);
    }
    return 0;
}

/*
  * Given an index and a primary record id, this routine deletes the record and all secondary
  * records pointing to the specified primary id.
  */
index_DeletePrimary(ai, arid)
struct Index *ai;
register struct recordID *arid;
{
    register struct indexBucket *tb;
    register struct indexComponent *tc;
    register int i;
    struct indexComponent **lc;
    register struct hashList *mh = NULL;
    register struct hashList *th;
    tb = index_CGetHash(ai, rhash(*arid));
    lc = &tb->list;
    for(tc = *lc; tc; tc=tc->next) {
	if (req(*arid, tc->id) && tc->primary) {
	    /* found the record */
	    *lc = tc->next;	/* splice it out */
	    /* free everything but the hashList */
	    mh = tc->hashes;
	    free(tc->name);
	    free(tc->data);
	    free(tc);
	    tb->modified = 1;
	}
	else lc = &tc->next;
    }
    index_CPut(ai, tb);
    if (mh != NULL){
	for(th=mh;th;th=th->next) {
	    for(i=0;i<th->nentries;i++) {
		index_PurgeBucket(ai, th->entries[i], arid);
	    }
	}
    }
    else return(1);
	
    index_FreeHL(mh);	/* finally free the hash list */
    return 0;
}

/*
  * Internal routine: given an index, a record id and a hash bucket, purge the bucket of all
  * references to the speicfied record id.
  */
index_PurgeBucket (ai, ahash, arid)
struct Index *ai;
long ahash;
register struct recordID *arid;
{
    register struct indexComponent *tc, *nc;
    register struct indexBucket *tb;
    register struct indexComponent **lc;
    tb = index_CGetHash(ai, ahash);
    lc = &tb->list;
    for(tc=tb->list; tc; tc=nc) {
	nc = tc->next;
	if (req(*arid, tc->id) && tc->primary == 0) {
	    *lc =nc;
	    tb->modified = 1;
	    free(tc);
	}
	else lc = &tc->next;
    }
    index_CPut(ai, tb);
}

/*
  * Given an index, a primary record's record id, and a key, delete the secondary record
	 * with the specified key that refers to the given primary record id.
	 */
index_DeleteSecondary(ai, arid, akey)
register struct Index *ai;
char *akey;
register struct recordID *arid;
{
    register struct indexComponent *tc, *nc;
    struct indexComponent **lc;
    register struct indexBucket *tb;
    char flag;

    tb = index_CGet(ai, akey);
    lc = &tb->list;
    flag = 0;
    for(tc=tb->list; tc; tc=nc) {
	nc = tc->next;
	if (req(*arid, tc->id) && strcmp(akey, tc->name) == 0 && tc->primary == 0) {
	    *lc =nc;
	    tb->modified = 1;
	    free(tc);
	    flag = 1;
	    break;
	}
	lc = &tc->next;
    }
    if (flag == 0) return 0;		/* didn't do anything */
    flag = index_RecordInUse(tb, arid);	/* find out if any references are left */
    index_CPut(ai, tb);
    if (!flag) {		/* none left? delete pointer to this bucket */
	tb = index_CGetHash(ai, rhash(*arid));
	if (tb) {
	    tc = index_FindID(tb, arid);
	    if (tc) index_HashRemove(tc->hashes, index_Hash(akey, ai->hashTableSize));
	    tb->modified = 1;
	    index_CPut(ai, tb);
	}
    }
    return 0;
}

/*
  * Internal routine: compute the hash for a string, given a hash table size (usually
									       * found in the index structure).
	*/
long index_Hash(astring, hashSize)
register short hashSize;
register char *astring;
{
    register long aval;
    register short tc;
    aval = 0;
    while (tc  = *astring++) {
	aval *= 173;
	aval += tc;
    }
    aval = aval % hashSize;
    if (aval < 0) aval += hashSize;
    return aval;
}

/*
  * Internal routine: given an index, a hash bucket and a flag indicating whether the
  * file is to be opened for reading or writing, open the appropriate bucket file and return
      * a standard I/O FILE * for the file.  If opening for writing, the new file will be created
	  * if necessary, and truncated.
	      */
FILE *index_HashOpen(ai, ahash, awrite)
register struct Index *ai;
register long awrite;
register long ahash;
{
    char tpath[1024];
    char tbuffer[20];
    strcpy(tpath, ai->pathName);
    strcat(tpath, "/");
    sprintf(tbuffer, "H%d", ahash);
    strcat(tpath, tbuffer);
    return fopen(tpath, (awrite? "w+" : "r"));
}

/*
  * Given an index, enumerate all of the records in the index.  Takes an index, a proc and
  * a rock, and calls the proc with the index, the record (struct indexComponent) and rock.
  */
index_Enumerate(ai, aproc, arock)
struct Index *ai;
int (*aproc)();
char *arock;
{
    register long i;
    register struct indexBucket *tb;
    register struct indexComponent *tc;

    for(i=0;i<ai->hashTableSize;i++) {
	tb = index_CGetHash(ai, i);
	for(tc=tb->list;tc;tc=tc->next) {
	    (*aproc)(ai, tc, arock);
	}
	index_CPut(ai, tb);
    }
}

/*
  * Given a pathname for an index file, return an open index structure for the index.
      * The pathname is the pathname of the directory containing all of the hash bucket
      * and version number files.
      */
struct Index *index_Open(apath)
register char *apath;
{
    register DIR *td;
    register DIRENT_TYPE *tde;
    register struct Index *ti;
    register long code;
    long htSize, version, foundFlag;

    td = opendir(apath);
    if (!td) {
	return (struct Index *) 0;
    }
    foundFlag = 0;
    while(tde=readdir(td)) {
	if (tde->d_name[0] == 'V') {
	    /* found the version number file */
	    if (foundFlag) {
		closedir(td);
		return (struct Index *) 0;
	    }
	    foundFlag = 1;
	    code = sscanf(tde->d_name, "V%d.%d", &htSize, &version);
	    if (code != 2) {
		closedir(td);
		return (struct Index *) 0;
	    }
	}
    }
    closedir(td);
    if (!foundFlag) {
	return (struct Index *) 0;
    }
    ti = (struct Index *) malloc (sizeof (struct Index));
    ti->pathName = (char *) malloc(strlen(apath) + 1);
    strcpy(ti->pathName, apath);
    ti->hashTableSize = htSize;
    ti->version = version;
    ti->blist = (struct indexBucket *) 0;
    return ti;
}

/*
  * Close an open index file, freeing all associated files.
  */
index_Close(ai)
register struct Index *ai;
{
    register struct indexBucket *tb, *nb;
    for(tb=ai->blist;tb;tb=nb) {
	nb = tb->next;		/* pull it out before freeing */
	if (tb->modified) {
	    index_CWrite(ai, tb);
	    index_FreeIndex(ai, tb);
	}
    }
    free(ai->pathName);
    free(ai);
}

/*
  * Given an index and a key, return a new recordSet containing the record IDs of all of the
  * primary records having the specified key.  This recordSet must be freed, using
  * recordset_Free, when the caller is finished with it.
  */
struct recordSet *index_GetPrimarySet(ai, akey)
register struct Index *ai;
register char *akey;
{
    struct indexBucket *tb;
    register struct indexComponent *tlist;
    register struct recordSet *rs;

    tb = index_CGet(ai, akey);
    rs = recordset_New(4);
    /* now we have the index in core, so we scan it */
    for(tlist = tb->list;tlist;tlist=tlist->next) {
	if (tlist->primary && strcmp(tlist->name, akey) == 0) recordset_Add(rs, &tlist->id);
    }
    index_CPut(ai, tb);
    return rs;
}

/*
  * Given an index and a key, return a new recordSet containing the record IDs of all
  * records (primary or secondary) having the specified key.  This recordSet must be freed, using
  * recordset_Free, when the caller is finished with it.
  */
struct recordSet *index_GetAnySet(ai, akey)
register struct Index *ai;
register char *akey;
{
    struct indexBucket *tb;
    register struct indexComponent *tlist;
    register struct recordSet *rs;

    tb = index_CGet(ai, akey);
    rs = recordset_New(4);
    /* now we have the index in core, so we scan it */
    for(tlist = tb->list;tlist;tlist=tlist->next) {
	if (strcmp(tlist->name, akey) == 0) recordset_Add(rs, &tlist->id);
    }
    index_CPut(ai, tb);
    return rs;
}

/*
  * Quasi-internal routine (if you need it, you need it): produces a dupm of an open index
  * on standard output.  Very useful for debugging things.
      */
index_Dump(ai)
register struct Index *ai;
{
    register struct indexBucket *tb;
    register struct indexComponent *tc;
    register long i;
    struct hashList *th;
    long j;
    for(i=0;i<ai->hashTableSize;i++) {
	tb = index_CGetHash(ai, i);
	if (!tb) printf("Failed to get bucket %d\n", i);
	else {
	    printf("Bucket %d next id %d\n", i, tb->nextID);
	    for(tc=tb->list;tc;tc=tc->next) {
		printf(" Record named '%s' ", tc->name);
		if (tc->primary) {
		    printf("id %d.%d data '%s'\n", tc->id.word1, tc->id.word2, tc->data);
		    printf("  hashes:");
		    for(th=tc->hashes;th;th=th->next) {
			for(j=0;j<th->nentries;j++) {
			    printf(" %d", th->entries[j]);
			}
		    }
		    printf("\n");
		}
		else {
		    printf("refers to %d.%d\n", tc->id.word1, tc->id.word2);
		}
	    }
	}
	index_CPut(ai, tb);
    }
}
