/********************************************************************************/
/* lib/file.h --								*/
/*										*/
/* Author : A.J.Doherty								*/
/********************************************************************************/

#ifndef _FSPtoollib_FILE_H_
#define _FSPtoollib_FILE_H_ 1

#include "common.h"

/********************************************************************************/
/* Return codes for local(or NFS) filing system reads.				*/

#define	FILEDIR_OK		0	/* -- everything ok			*/
#define FILEDIR_UNOPENED	1	/* -- couldn't open/read directory	*/
#define FILEDIR_ERROR		2	/* -- general error reading file/dir	*/

/********************************************************************************/

#define isdir(a)  (a->filetype == DIRECTORY)
#define islink(a) (a->filetype == FILE_LINK)

/********************************************************************************/

extern CacheData *interpret_filename(FILE*,char);
extern CacheData *read_file_dir(void);

extern void return_filetype(char*,CacheData*);
extern void strip_pathname(char*);

extern char *cachedata_to_line(CacheData*);
extern char *return_month_str(int);
extern char *unit_file_size(long int);

extern char *get_cwd(void);

extern int  return_month_val(char*);
extern int  compare_date(DateData,DateData);
extern int  open_file_dir(char*);
extern int  close_file_dir(void);
extern int  file_exists(const char*);
extern int  make_dir_hier(const char*);

/********************************************************************************/
#endif

