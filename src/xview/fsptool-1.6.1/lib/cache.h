/********************************************************************************/
/* cache.h --									*/
/*										*/
/* (c)1993 Andrew J. Doherty							*/
/********************************************************************************/

#ifndef _FSPtoollib_CACHE_H_
#define _FSPtoollib_CACHE_H_

#include <stdio.h>

typedef struct { int day, month, year, time; } 			 DateData;
typedef enum   { Alpha, RevAlpha, Size, RevSize, Date, RevDate } SortFormat;

typedef struct {
    char    *name;		/* -- file name					*/
    int      filetype;		/* -- type of file if any.			*/
    int      compression;	/* -- compression status			*/
    long int size;		/* -- size of file in bytes.			*/
    int      access;		/* -- file access bit mask.			*/
    DateData date;		/* -- struct holding date/time information.	*/
    }
  CacheData;

typedef struct {
    char	*name;		/* -- name of directory, unique for each entry	*/
    int   	entries;	/* -- number of entries in the directory	*/
    SortFormat	sorttype;	/* -- order in which files are sorted		*/
    long	timestamp;	/* -- cachetime (c-sec since 00:00 GMT 1/1/70 )	*/
    CacheData **entry;		/* -- pointer to file information block		*/
    }
  DirCache;

struct listdata {
    CacheData	     *data;
    struct listdata  *next;
    };

typedef struct listdata CacheList;

/********************************************************************************/
/* maximum number of directories cached						*/

#define MAX_CACHE_SIZE		25

/********************************************************************************/
/* maximum number of seconds before a directory times out (minutes)		*/

#define MAX_CACHE_TIMEOUT	30		/* 30 minute timeout value	*/

/********************************************************************************/
/* Cache Routine Return Codes */

#define CACHE_OK		1	/* -- successful request completion.	*/
#define CACHE_ERROR		2	/* -- undefined general error.		*/
#define CACHE_TIMED_OUT		3	/* -- the remote read timed out.	*/
#define CACHE_NO_SUCH		4	/* -- no such cache, or remote dir	*/
#define CACHE_NO_RESPONSE	5	/* -- remote server not responding	*/
#define CACHE_NO_REVERSE	6	/* -- remote couldn't reverse name us	*/

/********************************************************************************/

extern int	set_cache_size(int);
extern int	get_cache_size(void);
extern int	set_cache_timeout(int);
extern int	get_cache_timeout(void);
extern int	clear_cache(void);
extern int	open_cache(const char*);
extern int	close_cache(void);
extern int	cache_directory(const char*,DirCache**,DirCache*);
extern int	cache_dir_size(const char*,long int*);

extern char   **return_cache_contents(int*);
extern char    *cache_perror(int);

extern CacheData  *query_file_data(void);
extern CacheData **list_to_block(CacheList*,int);

extern DirCache	 *return_slot(void);
extern DirCache  *return_free(void);

extern void	sort_cache(SortFormat,DirCache*);
extern void	sort_alpha(CacheData*[],int,int);
extern void	sort_revalpha(CacheData*[],int,int);
extern void	sort_size(CacheData*[],int,int);
extern void	sort_revsize(CacheData*[],int,int);
extern void	sort_date(CacheData*[],int,int);
extern void	sort_revdate(CacheData*[],int,int);

extern void	clear_slot(int);
extern void	set_fsp_client(const char*);

/********************************************************************************/
#endif
