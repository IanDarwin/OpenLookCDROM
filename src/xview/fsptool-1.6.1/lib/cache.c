/********************************************************************************/
/* cache.c --									*/
/*										*/
/* As from Version 1.3.2 will handle most of the file related functions.	*/
/* Has no dependency on XView or any other X Toolkit so forms the basis of the	*/
/* interface to fsp directories used by the forthcoming Motif/XView FSPtools.	*/
/*										*/
/* (c)1993 Andrew J. Doherty							*/
/********************************************************************************/

#include "../config.h"

#include "common.h"
#include "cache.h"
#include "unix.h"
#include "fsp.h"
#include "file.h"

#include <string.h>

/********************************************************************************/
/* One dependence on external routines need a fn to display to the user what	*/
/* is currently going on when directory reading. This fn is expected to do this	*/

extern void footer_message(const char *, ...);

/********************************************************************************/
/* Static vars internal to this module */

static char	error_string[256];

static DirCache	dircache[MAX_CACHE_SIZE];		/* -- cache storage	*/

static int	cache_max      = MAX_CACHE_SIZE,	/* -- size limit	*/
		cache_timeout  = MAX_CACHE_TIMEOUT,	/* -- timeout length	*/
		cache_contents = 0;			/* -- dirs in cache	*/
 
static CacheData       **file_entry;			/* -- current read item	*/
static DirCache	        *cache_entry;			/* -- current cache	*/
static int		 cache_position;		/* -- current position	*/

/********************************************************************************/
/* vars external to this module */

extern char		*fsp_ls_cmd;			/* -- flscmd name	*/

/********************************************************************************/

int set_cache_size ( int cache_size )

/* this fn sets the variable maximum size of the cache, no smaller than 0 and	*/
/* no larger than MAX_CACHE_SIZE. returns actual size set.			*/

{
if (cache_size < 0)
    cache_size = 0;
else
    if (cache_size > MAX_CACHE_SIZE)
	cache_size = MAX_CACHE_SIZE;

cache_max = cache_size;
return(cache_max);
}

/********************************************************************************/

int get_cache_timeout (void)

/* this fn returns the number of seconds from being read before a directory is	*/
/* deemded to be out of date and needs to be re-read.				*/

{
return(cache_timeout);
}

/********************************************************************************/

int set_cache_timeout ( int cache_time )

/* this fn sets the variable timeout size of the cache, no smaller than 0 and	*/
/* no larger than MAX_CACHE_TIMEOUT. returns actual size set.			*/

{
if (cache_time < 0)
    cache_time = 0;
else
    if (cache_time > MAX_CACHE_TIMEOUT)
	cache_time = MAX_CACHE_TIMEOUT;

cache_timeout = cache_time;
return(cache_timeout);
}

/********************************************************************************/

int get_cache_size (void)

/* this fn returns the maximum number of entries which can be placed in cache	*/

{
return(cache_max);
}

/********************************************************************************/

char *cache_perror ( int error_no )

/* returns a text string indicating the nature of the error indicated in the	*/
/* error number.								*/

{
switch (error_no) {
    case CACHE_OK:		return("Successful request completion.");
    case CACHE_NO_SUCH:		return("No such directory at remote host.");
    case CACHE_NO_RESPONSE:	return("Remote host not responding.");
    case CACHE_TIMED_OUT:	return("Remote connection timed out.");
    case CACHE_ERROR:		return(error_string);
    }

return("Unknown error number");
}

/********************************************************************************/

CacheData **list_to_block ( CacheList *list, int entry_count )

{ CacheData **ptr, **ptr2;
  int         loop;


ptr2 = c_malloc(entry_count*sizeof(CacheData*));
ptr  = ptr2;

for ( loop = 0; loop < entry_count; loop++ ) {
    *ptr = list->data;
    list = list->next;
    ptr++;
    }

return(ptr2);
}

/********************************************************************************/

int clear_cache(void)

/* clears out the file cache, used when setting new host value			*/

{ int loop,
      tmp = cache_contents;	/* -- as clear_slot does cache_contents--	*/


for ( loop = 0; loop < tmp; loop++ )
    if ( dircache[loop].name )
	clear_slot(loop);

cache_contents = 0;
return(CACHE_OK);
}

/********************************************************************************/

void clear_slot ( int slot_no )

/* this fn will clear out the numbered dircache slot. All data storage is freed	*/

{ CacheData **ptr;
	int   loop;


free(dircache[slot_no].name);
ptr = dircache[slot_no].entry;

for ( loop = 0; loop < dircache[slot_no].entries; loop++,ptr++ )
    free((*ptr)->name);

free(dircache[slot_no].entry);
dircache[slot_no].entries = 0;
dircache[slot_no].name    = (char*) NULL;
cache_contents--;
}

/********************************************************************************/

void free_slot(void)

/* this fn will attempt to free up a cache directory slot. If no slots are	*/
/* occupied then nothing will be freed. Otherwise it will free the oldest dir	*/
/* entry that it finds.								*/

{ long oldest = 86400000;
   int loop, freeslot = -1;


for ( loop = 0; loop < cache_max; loop++ )
    if (dircache[loop].name)
	if (dircache[loop].timestamp < oldest) {
	    oldest   = dircache[loop].timestamp;
	    freeslot = loop;
	    }

if (freeslot == -1)	/* -- no slots to free */
    return;

clear_slot(freeslot);	/* -- clear found slot */
}

/********************************************************************************/

DirCache *return_free(void)

/* this fn returns the address of the first unused slot which it comes across.	*/

{ int loop;

for ( loop = 0; loop < cache_max; loop++ )
    if (!dircache[loop].name)
	return(&(dircache[loop]));

return((DirCache*)NULL);
}

/********************************************************************************/

DirCache *return_slot(void)

/* this routine will use a cache slot to be used for a new directory. It will	*/
/* first attempt to use an empty slot, if none exist then the oldest found	*/
/* record will be freed and that slot used. If even this fails then the base	*/
/* [0] index record of the dircache is returned.				*/

{ DirCache *ptr;


if ((ptr = return_free()) != (DirCache*)NULL)
    return(ptr);

free_slot();

if ((ptr = return_free()) != (DirCache*)NULL)
    return(ptr);

clear_slot(0);		/* -- haven't been able to free specific slot so	*/
return(&(dircache[0]));	/* -- overwrite dircache[0] - so free data there	*/
}

/********************************************************************************/

int cache_directory ( const char *name, DirCache **cache_ptr, DirCache *useptr )

/* this routine reads in the named directory - if it exists - from the current	*/
/* fsp host. It caches it in the most appropriate free cache			*/

{ static char	name_str[256], ch;
  char	       *argv[] = { fsp_ls_cmd, "-l", NULL };
  FILE	       *stream;
  DirCache     *dirc;
  CacheList    *list_head, *file_list = c_malloc(sizeof(CacheList));

list_head = file_list;

if (useptr == (DirCache*)NULL)
    dirc = return_slot();
else
    dirc = useptr;

dirc->name	= c_strdup(name);
dirc->entries	= 0;
dirc->timestamp = csectime();
dirc->sorttype  = Alpha;

stream = perropen(argv);

fgets(name_str,255,stream);

if (strstr(name_str,"total")) {
    footer_message("Reading Directory ...");

    while (!feof(stream))
	if ((ch = fgetc(stream)) != EOF) {
	    file_list->data = interpret_filename(stream,ch);
	    dirc->entries   = dirc->entries+1;
	    file_list->next = c_malloc(sizeof(CacheList));
	    file_list	    = file_list->next;
	    }

    perrclose(stream);
    dirc->entry = list_to_block(list_head,dirc->entries);
    *cache_ptr  = dirc;
    cache_contents++;
    return(CACHE_OK);
    }

perrclose(stream);
free(dirc->name);
dirc->name    = (char*) NULL;
dirc->entries = 0;

if (strstr(name_str,"not responding"))
    return(CACHE_NO_RESPONSE);

if (strstr(name_str,"no such"))
    return(CACHE_NO_SUCH);

strcpy(error_string,name_str);
return(CACHE_ERROR);
}

/********************************************************************************/

int open_cache ( const char *dirname )

/* this routine initialises the directory querying routines to the named	*/
/* directory. From now on, each call to next_entry will return the next file	*/
/* in the directory cache. When finished with this directory call close_cache	*/
/* If the named directory is not in cache when this is called then it will be	*/
/* read and the above carried out.						*/

{ int loop = 0, found = FALSE, op_status;


while ((loop < cache_max) && !found) {
    if ( dircache[loop].name )
	if (strcmp(dircache[loop].name,dirname) == 0)
	    found = TRUE;

    loop++;
    }

if (!found)
    op_status = cache_directory(dirname,&cache_entry,(DirCache*) NULL);
else {
    if ((csectime()-dircache[loop-1].timestamp) < cache_timeout)
	op_status = cache_directory(dirname,&cache_entry,(DirCache*) NULL);
    else {
	cache_entry = &(dircache[loop-1]);
	op_status = CACHE_OK;
	}
    }

if (op_status == CACHE_OK) {
    file_entry = cache_entry->entry;
    sort_cache(get_fsp_sorttype(),cache_entry);
    }

cache_position = 0;
return(op_status);
}

/********************************************************************************/

int close_cache(void)

/* finish with the cache which is currently set up to be queryable.		*/

{
return(CACHE_OK);
}

/********************************************************************************/

CacheData *query_file_data(void)

/* returns the next entry from the directory cache, in raw file data form.	*/

{
if (cache_position == cache_entry->entries)
    return((CacheData*)NULL);

cache_position++;
return(*file_entry++);
}

/********************************************************************************/

char **return_cache_contents ( int *dir_count )

/* this fn returns the count of directories in the cache in the int, and places	*/
/* the group of char *'s to the directory names in the databuf, it allocates	*/
/* space for the correct number of strings, it is the callers responsibility to	*/
/* to free this space when it is finished with.	The routine always allocates an	*/
/* extra byte in case it would be calling c_malloc(0) otherwise. Some systems	*/
/* would return a NULL pointer so fouling up a later free(ptr) call.		*/

{ int   loop;
 char **ptr, **ptr2;

*dir_count = 0;

for ( loop = 0; loop < cache_max; loop++ )
    if ( dircache[loop].name )
	(*dir_count)++;

ptr2 = ptr = c_malloc(sizeof(char*)*(*dir_count)+1);

if (*dir_count == 0)
    return(ptr);

for ( loop = 0; loop < cache_max; loop++ )
    if ( dircache[loop].name ) {
	*ptr = dircache[loop].name;
	ptr++;
	}

return(ptr2);
}

/********************************************************************************/

int cache_dir_size ( const char *dirname, long int *dirsize )

/* this routine determines the size of the specified remote cache directory.	*/
/* It will recurse through subdirectories as required and so is inherently	*/
/* recursive. Returns total of files in bytes, if a directory is found in cache	*/
/* then it will be used rather than reading it in again.			*/

{ int 	      loop = 0, found = FALSE, op_status;
  DirCache   *return_ptr, *cache_ptr = c_malloc(sizeof(DirCache));
  CacheData **file_datas;
  char	     *host_dir = c_malloc(MAXPATHLEN);


strcpy(host_dir,get_fsp_dir());
set_fsp_dir(dirname);

while ((loop < cache_max) && !found) {
    if ( dircache[loop].name )
	if (strcmp(dircache[loop].name,dirname) == 0)
	    found = TRUE;

    loop++;
    }

if (!found)
    op_status = cache_directory(dirname,&return_ptr,cache_ptr);
else {
    cache_entry = &(dircache[loop-1]);
    op_status = CACHE_OK;
    }

if (op_status == CACHE_OK) {
    file_datas = return_ptr->entry;

    for ( loop = 0; loop < return_ptr->entries; loop++,file_datas++ ) {
	if ((*file_datas)->filetype == DIRECTORY) {
	    char *dnm = c_malloc(MAXPATHLEN);
	     int  status;

	    strcpy(dnm,dirname);

	    if (strcmp(dirname,"/") != 0)
		strcat(dnm,"/");

	    strcat(dnm,(*file_datas)->name);

	    status = cache_dir_size(dnm,dirsize);
	    free(dnm);

	    if ( status != CACHE_OK ) {
		free(cache_ptr);
		set_fsp_dir(host_dir);
		free(host_dir);
		return(status);
		}
	    }
	else
	    *dirsize += (*file_datas)->size;
	}
    }

if (!found)
    free(cache_ptr);

set_fsp_dir(host_dir);
free(host_dir);
return(op_status);
}

/********************************************************************************/

void sort_cache ( SortFormat sortby, DirCache *cache )

/* this fn calls out to an appropriate sort routine in order to sort the named	*/
/* cache into the requested order. If the cache is already in that order then	*/
/* it returns and a sort is not performed.					*/

{ CacheData **entry;

if (sortby == cache->sorttype)	/* -- already in this order */
    return;

entry		= cache->entry;
cache->sorttype = sortby;

if (cache->entries < 2)
    return;

switch (sortby) {
    case Alpha	  : sort_alpha(entry,0,cache->entries-1);	break;
    case RevAlpha : sort_revalpha(entry,0,cache->entries-1);	break;
    case Size	  : sort_size(entry,0,cache->entries-1);	break;
    case RevSize  : sort_revsize(entry,0,cache->entries-1);	break;
    case Date	  : sort_date(entry,0,cache->entries-1);	break;
    case RevDate  : sort_revdate(entry,0,cache->entries-1);	break;
    }
}

/********************************************************************************/

void sort_alpha ( CacheData *item[], int left, int right )

/* this fn sorts cache items into ascending alphabetical order.			*/

{ CacheData *ptr, *temp;
	int  i = left, j = right;

ptr = item[(left+right)/2];

do {
    while ((strcmp(item[i]->name,ptr->name) < 0) && (i < right)) i++;
    while ((strcmp(item[j]->name,ptr->name) > 0) && (j > left))  j--;

    if (i <= j) {
	temp    = item[i];
	item[i] = item[j];
	item[j] = temp;
	i++; j--;
	}
    }
while (i<=j);

if (left<j)  sort_alpha(item, left, j);
if (i<right) sort_alpha(item, i, right);
}

/********************************************************************************/

void sort_revalpha ( CacheData *item[], int left, int right )

/* this fn sorts cache items into descending alphabetical order.		*/

{ CacheData *ptr, *temp;
	int  i = left, j = right;

ptr = item[(left+right)/2];

do {
    while ((strcmp(item[i]->name,ptr->name) > 0) && (i < right)) i++;
    while ((strcmp(item[j]->name,ptr->name) < 0) && (j > left))  j--;

    if (i <= j) {
	temp    = item[i];
	item[i] = item[j];
	item[j] = temp;
	i++; j--;
	}
    }
while (i<=j);

if (left<j)  sort_revalpha(item, left, j);
if (i<right) sort_revalpha(item, i, right);
}

/********************************************************************************/

void sort_size ( CacheData *item[], int left, int right )

/* this fn sorts cache items into ascending size order.				*/

{ CacheData *ptr, *temp;
	int  i = left, j = right;

ptr = item[(left+right)/2];

do {
    while ((item[i]->size < ptr->size) && (i < right)) i++;
    while ((item[j]->size > ptr->size) && (j > left))  j--;

    if (i <= j) {
	temp    = item[i];
	item[i] = item[j];
	item[j] = temp;
	i++; j--;
	}
    }
while (i<=j);

if (left<j)  sort_size(item, left, j);
if (i<right) sort_size(item, i, right);
}

/********************************************************************************/

void sort_revsize ( CacheData *item[], int left, int right )

/* this fn sorts cache items into descending size order.			*/

{ CacheData *ptr, *temp;
	int  i = left, j = right;

ptr = item[(left+right)/2];

do {
    while ((item[i]->size > ptr->size) && (i < right)) i++;
    while ((item[j]->size < ptr->size) && (j > left))  j--;

    if (i <= j) {
	temp    = item[i];
	item[i] = item[j];
	item[j] = temp;
	i++; j--;
	}
    }
while (i<=j);

if (left<j)  sort_revsize(item, left, j);
if (i<right) sort_revsize(item, i, right);
}

/********************************************************************************/

void sort_date ( CacheData *item[], int left, int right )

/* this fn sorts cache items into newer->older date order.			*/

{ CacheData *ptr, *temp;
	int  i = left, j = right;

ptr = item[(left+right)/2];

do {
    while ((compare_date(item[i]->date,ptr->date) < 0) && (i < right)) i++;
    while ((compare_date(item[j]->date,ptr->date) > 0) && (j > left))  j--;

    if (i <= j) {
	temp    = item[i];
	item[i] = item[j];
	item[j] = temp;
	i++; j--;
	}
    }
while (i<=j);

if (left<j)  sort_date(item, left, j);
if (i<right) sort_date(item, i, right);
}

/********************************************************************************/

void sort_revdate ( CacheData *item[], int left, int right )

/* this fn sorts cache items into older->newer date order.			*/

{ CacheData *ptr, *temp;
	int  i = left, j = right;

ptr = item[(left+right)/2];

do {
    while ((compare_date(item[i]->date,ptr->date) > 0) && (i < right)) i++;
    while ((compare_date(item[j]->date,ptr->date) < 0) && (j > left))  j--;

    if (i <= j) {
	temp    = item[i];
	item[i] = item[j];
	item[j] = temp;
	i++; j--;
	}
    }
while (i<=j);

if (left<j)  sort_revdate(item, left, j);
if (i<right) sort_revdate(item, i, right);
}

/********************************************************************************/
