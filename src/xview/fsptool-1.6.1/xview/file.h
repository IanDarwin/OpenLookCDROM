/********************************************************************************/
/* file.h --									*/
/*										*/
/* Author : A.J.Doherty								*/
/* Date   : 14/05/93								*/
/* Version: 0.6	(19/05/93)							*/
/********************************************************************************/

#ifndef _FSPtool_FILE_H_
#define _FSPtool_FILE_H_ 1

/********************************************************************************/

#include "../lib/cache.h"
#include "../lib/file.h"

/********************************************************************************/

extern void set_filter(Panel_item,int,Event*);
extern void set_file_actions(Panel_item,int,Event*);
extern void exec_file(CacheData*);
extern void do_exec_file(CacheData*);
extern void do_exec_local_file(CacheData*);
extern void read_fsp_directory(Panel_item);
extern void read_local_directory(Panel_item,char*);
extern void set_file_action(int,const char*);

extern int  infilter(CacheData*);

extern char *get_file_action(int);

/********************************************************************************/
#endif

