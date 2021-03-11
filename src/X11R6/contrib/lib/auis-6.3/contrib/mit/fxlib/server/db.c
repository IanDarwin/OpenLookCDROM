/*
 * The FX (File Exchange) Server
 *
 * $Author: rr2b $
 * $Source: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/fxlib/server/RCS/db.c,v $
 * $Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/fxlib/server/RCS/db.c,v 1.3 1992/12/15 21:55:23 rr2b R6tape $
 *
 * Copyright 1989, 1990 by the Massachusetts Institute of Technology.
 *
 * For copying and distribution information, please see the file
 * <mit-copyright.h>.
 */

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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/fxlib/server/RCS/db.c,v 1.3 1992/12/15 21:55:23 rr2b R6tape $";
#endif

#include <mit-copyright.h>

/*
 * This file contains the database system.
 */

 

#include <fxserver.h>
#include <sys/file.h>
#include <sys/types.h>
#include <ndbm.h>

datum *make_dbm_key(), *make_dbm_contents();
time_t time();

#define MAX_NUM_DBS 64		/* strict maximum on open databases */
#define DB_HOLDTIME 300		/* hold db open at least 5 min... */
#define DB_LIMIT 16		/* ...unless this limit is reached */
#define IDX (curconn->index-1)

static struct DBM_ent {
  char *path;
  DBM *dbm;
  int ref;
  time_t atime;
  char **keylist;
  int nkeys;
} dblist[MAX_NUM_DBS];

static int cur_key;

DBVers db_vers;
int database_uptodate;

/*
 * Keys contain:
 * 	author
 * 	assignment
 * 	file
 * 	paper id (sec, usec, host)
 */

/*
 * Initialize the db library.  Called once at startup.
 */

db_init()
{
  char pathspec[MAXPATHLEN];
  int i;
  FILE *fp;

  for (i=0; i<MAX_NUM_DBS; i++) {
    dblist[i].path = "";
    dblist[i].dbm = NULL;
  }
  strcpy(pathspec, root_dir);
  strcat(pathspec, "/");
  strcat(pathspec, DB_VERS_FILE);
  fp = fopen(pathspec, "r");
  if (!fp) {
    fp = fopen(pathspec, "w");
    if (!fp)
      fatal("Can't open %s for writing!", pathspec);
    fprintf(fp, "0\n0\n");
    fclose(fp);
    bzero(&db_vers, sizeof(db_vers));
  }
  else {
    fscanf(fp, "%ld\n%ld", &db_vers.synctime, &db_vers.commit);
    fclose(fp);
  }
  DebugMulti(("Local database sync %ld, commit %ld\n", db_vers.synctime,
	      db_vers.commit));
  /* Only one server? Always up to date... */
#ifdef MULTI
  database_uptodate = (nservers == 1);
#else
  database_uptodate = 1;
#endif /* MULTI */
}

/*
 * Open the named database and try to allocate space for it in the
 * database list.  If this database has already been opened, increment
 * the reference count and return the appropriate index.
 */

db_open(name)
     char *name;
{
  int idx;

  DebugDB(("db_open: %s\n", name));
  
  for (idx=0; idx<MAX_NUM_DBS; idx++) {
    if (!strcmp(dblist[idx].path, name)) {
      dblist[idx].ref++;
      DebugDB(("Returning reference to %d, ref count %d\n", idx,
	       dblist[idx].ref));
      return idx+1;
    }
  }
  
  for (idx=0; idx<MAX_NUM_DBS; idx++)
    if (!dblist[idx].dbm)
      break;
  if (idx == MAX_NUM_DBS) {
    DebugDB(("Too many open database files!\n"));
    return 0;
  }

  dblist[idx].dbm = dbm_open(name, O_RDWR|O_CREAT, 0600);
  if (!dblist[idx].dbm)
    return 0;
  dblist[idx].path = xsave_string(name);
  dblist[idx].ref = 1;
  dblist[idx].atime = time(0);
  dblist[idx].keylist = NULL;
  DebugDB(("Created new DB entry # %d\n", idx));
  
  return idx+1;
}

/*
 * Flush a database file from the cache if it's there
 * XXX What if a connection has this db open?
 */

db_flush(name)
     char *name;
{
  int i;

  for(i=0; i<MAX_NUM_DBS; i++) {
    if (!dblist[i].dbm) continue;
    if (!strcmp(dblist[i].path, name))
      _db_close(i+1);
  }
  return;
}
    
/*
 * Close a database file.  Handle reference counts properly.
 */

db_close(idx)
     int idx;
{
  int i, n=0, oldest= -1;	/* n = number of open databases */

  DebugDB(("db_close: %d\n", idx));

  idx--;
  if (dblist[idx].ref) dblist[idx].ref--;

  for(i=0; i<MAX_NUM_DBS; i++) {
    if (!dblist[i].dbm) continue;
    if (!dblist[i].ref && time(0) - dblist[i].atime > DB_HOLDTIME) {
      _db_close(i+1);
      continue;
    }
    n++;
    if (dblist[i].ref) continue;
    if (oldest == -1 || dblist[i].atime < dblist[oldest].atime)
      oldest = i;
  }
  if (n > DB_LIMIT && oldest != -1) _db_close(oldest+1);
  return;
}

/*
 * Close a database file.
 */

_db_close(idx)
     int idx;
{
  int i;

  DebugDB(("_db_close %d %s\n", idx, dblist[idx].path));

  idx--;
  dbm_close(dblist[idx].dbm);
  dblist[idx].dbm = NULL;
  xfree(dblist[idx].path);
  dblist[idx].path = "";
  if (dblist[idx].keylist) {
    for (i=0; i < dblist[idx].nkeys; i++)
      xfree(dblist[idx].keylist[i]);
    xfree(dblist[idx].keylist);
  }
  return;
}

/*
 * Read in and sort the key database if necessary.
 * XXX More efficient!
 */

_db_strcmp(ptr1, ptr2)
     char **ptr1;
     char **ptr2;
{
  return strcmp(*ptr1, *ptr2);
}

_db_read_and_sort()
{
  datum key;

  dblist[IDX].atime = time(0);
  if (dblist[IDX].keylist)
    return;
  dblist[IDX].keylist = (char**)xmalloc(sizeof(char*));

  dblist[IDX].nkeys = 0;
  
  for (key = dbm_firstkey(dblist[IDX].dbm);
       key.dptr; key = dbm_nextkey(dblist[IDX].dbm)) {
    dblist[IDX].nkeys++;
    dblist[IDX].keylist = (char**)xrealloc((char*)dblist[IDX].keylist,
					   dblist[IDX].nkeys*
					   sizeof(char*));
    dblist[IDX].keylist[dblist[IDX].nkeys-1] = xsave_string(key.dptr);
    DebugDB(("Key: %s\n", key.dptr));
  }
  qsort((char*)dblist[IDX].keylist, dblist[IDX].nkeys, sizeof(char*),
	_db_strcmp);
  dblist[IDX].atime = time(0);
}

/*
 * Add a key to the key list and keep it sorted.
 */

_db_add_key(key)
     char *key;
{
  int i=0;
  
  dblist[IDX].atime = time(0);
  if (!dblist[IDX].keylist)
    return;

  if (dblist[IDX].nkeys) {
    i = db_key_position(key);
    if (i < dblist[IDX].nkeys)
      if (!strcmp(dblist[IDX].keylist[i], key)) return;
  }

  dblist[IDX].keylist = (char**)xrealloc((char*)dblist[IDX].keylist,
					 (dblist[IDX].nkeys+1)*
					 sizeof(char*));

  if (i < dblist[IDX].nkeys)
    bcopy((char*)(dblist[IDX].keylist+i), (char*)(dblist[IDX].keylist+i+1),
	  (dblist[IDX].nkeys-i)*sizeof(char*));
  dblist[IDX].keylist[i] = xsave_string(key);
  dblist[IDX].nkeys++;
}

/*
 * Find the next field in a key or database entry.  Fields are
 * delimited by Ctrl-A's.
 */

char *_db_next_field(ptr)
     char *ptr;
{
  ptr = (char *)index(ptr, (char)1);
  if (!ptr)
    ptr += strlen(ptr);
  return ptr;
}

/*
 * Given a key number, return a "Contents" structure representing it.
 * If complete==0, only fill in key fields.
 */

Contents *_db_contents_of_key(keynum, complete)
     int keynum, complete;
{
  static char keybfr[2048], databfr[2048];
  static Contents ret;
  char *key, *ptr;
  datum other, dkey;

  dblist[IDX].atime = time(0);
  key = keybfr;
  strcpy(key, dblist[IDX].keylist[keynum]);

  ptr = _db_next_field(key);
  *ptr = '\0';
  ret.p.author = key;
  key = ptr+1;
  ptr = _db_next_field(key);
  *ptr = '\0';
  ret.p.assignment = atoi(key);
  key = ptr+1;
  ptr = _db_next_field(key);
  *ptr = '\0';
  ret.p.filename = key;
  key = ptr+1;
  ptr = _db_next_field(key);
  *ptr = '\0';
  ret.p.location.time.tv_sec = atoi(key);
  key = ptr+1;
  ptr = _db_next_field(key);
  *ptr = '\0';
  ret.p.location.time.tv_usec = atoi(key);
  key = ptr+1;
  ret.p.location.host = key;
  if (!complete) return &ret;

  dkey.dptr = dblist[IDX].keylist[keynum];
  dkey.dsize = strlen(dkey.dptr)+1;
  other = dbm_fetch(dblist[IDX].dbm, dkey);
  strcpy(databfr, other.dptr);
  ptr = databfr;

  DebugDB(("Contents of key %d (%s): %s\n", keynum,
	   dblist[IDX].keylist[keynum], other.dptr));
  
  ret.p.type = (PaperType)atoi(ptr);
  ptr = _db_next_field(ptr);
  key = ptr+1;
  ptr = _db_next_field(key);
  *ptr++ = '\0';               /* null-terminate owner */
  ret.p.owner = key;
  ret.p.desc = ptr;
  ptr = _db_next_field(ptr);
  *ptr++ = '\0';               /* null-terminate desc */
  ret.p.created.tv_sec = atoi(ptr);
  ptr = _db_next_field(ptr)+1;
  ret.p.created.tv_usec = atoi(ptr);
  ptr = _db_next_field(ptr)+1;
  ret.p.modified.tv_sec = atoi(ptr);
  ptr = _db_next_field(ptr)+1;
  ret.p.modified.tv_usec = atoi(ptr);
  ptr = _db_next_field(ptr)+1;
  ret.p.size = atoi(ptr);
  ptr = _db_next_field(ptr)+1;
  ret.p.words = atoi(ptr);
  ptr = _db_next_field(ptr)+1;
  ret.p.lines = atoi(ptr);
  ptr = _db_next_field(ptr)+1;
  ret.p.flags = atoi(ptr);
  ptr = _db_next_field(ptr)+1;
  key = ptr;
  (void) strcpy(ret.ptrfile, key);
  
  return &ret;
}

/*
 * Return the first "contents" of the database file, and set up the
 * necessary pointers so we can retrieve additional ones in order in
 * the future.
 */

Contents *db_firstcontents()
{
  cur_key = 0;

  _db_read_and_sort();
  
  if (!dblist[IDX].nkeys)
    return (Contents *)NULL;
  return _db_contents_of_key(0, 1);
}

Contents *db_firstkey(criterion)
     Contents *criterion;
{
  char keybuf[2048];

  _db_read_and_sort();
  MAKEKEY(keybuf, criterion);
  cur_key = db_key_position(keybuf);
  if (cur_key >= dblist[IDX].nkeys) return(NULL);
  return(_db_contents_of_key(cur_key, 0));
}  

/*
 * Return the next "contents" of the database file.
 */

Contents *db_nextcontents()
{
  cur_key++;

  if (cur_key >= dblist[IDX].nkeys)
    return (Contents *)NULL;
  return _db_contents_of_key(cur_key, 1);
}

Contents *db_nextkey()
{
  cur_key++;

  if (cur_key >= dblist[IDX].nkeys) return (Contents *)NULL;
  return(_db_contents_of_key(cur_key, 0));
}

/*
 * Fill in all the contents of the current key.
 */

Contents *db_fullcontents()
{
  return(_db_contents_of_key(cur_key, 1));
}

/*
 * Return an index to a given key if it exists,
 * or to the position where such a key should appear if created.
 */

db_key_position(key)
     char *key;
{
  int hi, lo=0, med, i;
  
  _db_read_and_sort();

  hi = dblist[IDX].nkeys - 1;
  if (hi<0) return(0);

  /* Make sure low bound really is low bound */
  i = strcmp(dblist[IDX].keylist[lo], key);
  if (i>0 || i==0) return(lo);

  /* make sure high bound really is high bound */
  i = strcmp(dblist[IDX].keylist[hi], key);
  if (i<0) return(hi+1);
  if (i==0) return(hi);

  /* do binary search */
  while (hi - lo > 1) {
    med = (lo+hi)/2;
    if (!(i = strcmp(dblist[IDX].keylist[med], key))) return(med);
    if (i<0) lo = med;
    if (i>0) hi = med;
  }
  return(hi);
}

/*
 * Store a "contents" into the database, replacing one with the same
 * key if necessary.
 */

db_store(contents)
     Contents *contents;
{
  datum key, data;
  char keybfr[1024], databfr[1024];
#ifdef MULTI
  int i;
  long *res;
#endif /* MULTI */

  MAKEKEY(keybfr, contents);
  key.dptr = keybfr;
  key.dsize = strlen(key.dptr)+1;

  MAKEDATA(databfr, contents);
  data.dptr = databfr;
  data.dsize = strlen(data.dptr)+1;

  DebugDB(("db_store: key %s data %s\n", key.dptr, data.dptr));

  if (dbm_store(dblist[IDX].dbm, key, data, DBM_REPLACE) < 0)
    return(1);
  _db_add_key(keybfr);

#ifdef MULTI
  if (!curconn->server_num) {
    multi_set_course();
    for (i=0; i<nservers; i++) {
      if (servers[i].cl) {
	DebugMulti(("Calling server_store for server %s\n",
		    servers[i].name));
	res = (long *)_server_store_1(contents, servers[i].cl);
	/* XXX */
	if (!res)
	  multi_conn_dropped(i);
      }
    }
    multi_commit();
  }
#endif /* MULTI */
  return(0);
}

/*
 * Retrieve a "contents" from the database.  Fills in the given
 * contents structure.  The paper portion of this structure must be
 * already filled in.  Returns 1 if the key could not be found, or 0
 * if the fetch was successful.
 */

db_fetch(contents)
     Contents *contents;
{
  char keybfr[1024];
  int i;

  _db_read_and_sort();

  MAKEKEY(keybfr, contents);

  i = db_key_position(keybfr);
  DebugDB(("db_fetch: Seeking %s\n", keybfr));
  if (i >= dblist[IDX].nkeys) {
    DebugDB(("db_fetch: Finding nothing.\n"));
    return(1);
  }
  DebugDB(("db_fetch: Finding %s\n", dblist[IDX].keylist[i]));
  if (strcmp(dblist[IDX].keylist[i], keybfr)) return(1);
  *contents = *_db_contents_of_key(i, 1);
  return 0;
}

/*
 * Delete a "contents" from the database.
 */

db_delete(contents)
     Contents *contents;
{
  char keybfr[1024];
  datum d;
  int i;

  MAKEKEY(keybfr, contents);

  if (dblist[IDX].keylist) {
    i = db_key_position(keybfr);
    if (i >= dblist[IDX].nkeys) return;
    if (!strcmp(dblist[IDX].keylist[i], keybfr)) {
      bcopy((char*)(dblist[IDX].keylist+i+1),
	    (char*)(dblist[IDX].keylist+i),
	    (dblist[IDX].nkeys-i-1)*sizeof(char*));
      dblist[IDX].nkeys--;
    }
  }

  d.dptr = keybfr;
  d.dsize = strlen(keybfr)+1;
  
  (void) dbm_delete(dblist[IDX].dbm, d);
  dblist[IDX].atime = time(0);
#ifdef MULTI
  if (!curconn->server_num) {
    long *res;
    multi_set_course();
    for (i=0; i<nservers; i++) {
      if (servers[i].cl) {
	DebugMulti(("Calling server_store for server %s\n",
		    servers[i].name));
	res = (long *)_server_delete_1(contents, servers[i].cl);
	/* XXX */
	if (!res)
	  multi_conn_dropped(i);
      }
    }
    multi_commit();
  }
#endif /* MULTI */
}

db_inc_vers()
{
  db_vers.commit++;
  db_set_vers();
}

db_set_vers()
{
  char pathspec[MAXPATHLEN];
  FILE *fp;

  strcpy(pathspec, root_dir);
  strcat(pathspec, "/");
  strcat(pathspec, DB_VERS_FILE);
  fp = fopen(pathspec, "w");
  if (!fp)
    fatal("Can't open %s for writing!", pathspec);
  fprintf(fp, "%ld\n%ld\n", db_vers.synctime, db_vers.commit);
  fclose(fp);
  DebugMulti(("Updated version to synctime %ld, commit %ld\n",
	      db_vers.synctime, db_vers.commit));
  database_uptodate = 1; /* XXX Only 'til downloading is written */
}
