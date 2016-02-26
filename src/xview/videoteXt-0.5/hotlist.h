#ifndef _HOTLIST_H
#define _HOTLIST_H

/* Copyright (c) 1994-95 Martin Buck  <martin.buck@student.uni-ulm.de>
 * Read COPYING for more information
 */

#include "toptext.h"


extern int hotlist_count;


void hotlist_init(void);
void hotlist_exit(void);
void hotlist_add(int page, int pos);
int hotlist_remove(int pos);
int hotlist_get(int pos);
int hotlist_get_count(int station);
int hotlist_get_stcount(void);
void hotlist_set_current(int station);
int hotlist_get_current(void);
void hotlist_put_queue(void);
char** hotlist_mkstr(int *count);
char* hotlist_get_name(int entry);
int hotlist_search(const byte_t *vtx_name, int hint);
void hotlist_new_name(const char *name, int station);
int hotlist_station_changed(void);
int hotlist_get_timeout(void);
void hotlist_set_timeout(int timeout);
int hotlist_get_interleave(void);
void hotlist_set_interleave(int interleave);
int hotlist_get_lookahead(void);
void hotlist_set_lookahead(int lookahead);
int hotlist_get_topdisable(void);
void hotlist_set_topdisable(int disable);
int hotlist_get_index_page(void);
void hotlist_set_index_page(int page);
tt_pageclass_t hotlist_get_searchlevel(void);
void hotlist_set_searchlevel(tt_pageclass_t level);

#endif /* _HOTLIST_H */
