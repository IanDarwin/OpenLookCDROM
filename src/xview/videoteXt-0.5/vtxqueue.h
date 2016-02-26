#ifndef _VTXQUEUE_H
#define _VTXQUEUE_H

/* Copyright (c) 1994-95 Martin Buck  <martin.buck@student.uni-ulm.de>
 * Read COPYING for more information
 */


#include <time.h>
#include <linux/vtx.h>
#include "xevents.h"


#define VTXQOK 0
#define VTXQNOTFOUND 1

enum { PRI_LOW = 1, PRI_MED, PRI_HIGH, PRI_TOP, PRI_FGROUND_NH, PRI_FGROUND, PRI_ALL };
enum { STAT_NEW, STAT_SEARCH, STAT_TIMEOUT, STAT_FOUND, STAT_READ, STAT_READ_MP, STAT_FOUND_MP,
       STAT_SEARCH_MP, STAT_COUNT };				/* STAT_COUNT must be last !!! */


typedef struct {
  int page, subpage, firstsubpg, lastsubpg, priority, page_found, keep_running;
  time_t time;
} queue_entry_t;


extern const int sizeof_bgbuf_t;
extern int vtx_dev_open, dau_status_changed;
extern queue_entry_t *dau_status;


int get_page(int priority, int page, int subpage, int hour, byte_t *buffer,
    vtxpgwin_t *vtxwin);
void top_getpage(int page, int hour, int minute);
int top_flush_getpage(void);
int remove_priority(int priority);
int queue_itimer(void);
void insert_bgbuf(const byte_t *buffer, const vtx_pageinfo_t *info);
int query_bgbuf(int page, int subpage, byte_t *buffer, vtxpgwin_t *vtxwin);
int count_bgbuf_subpg(int page);
void flush_bgbuf(void);
void history_insert(int page, int do_select);
void history_flush(void);
void history_set_status(int page, int status);

#endif /* _VTXQUEUE_H */
