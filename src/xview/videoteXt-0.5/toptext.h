#ifndef _TOPTEXT_H
#define _TOPTEXT_H

/* Copyright (c) 1994-95 Martin Buck  <martin.buck@student.uni-ulm.de>
 * Read COPYING for more information
 */

typedef enum { TT_NONE, TT_BLOCK, TT_GROUP, TT_NORM, TT_SUBPG } tt_pageclass_t;
typedef struct ttdesc_s {
  char fullname[32], pagename[19];
  int page, is_block;
  struct ttdesc_s *next;
} ttdesc_t;
typedef void (*topreq_cb)(int, int, int);


extern int toptext_ok;


int toptext_newpage(const byte_t *buffer, const vtx_pageinfo_t *pginf, topreq_cb getpage);
int toptext_getnext(int current, int inc, tt_pageclass_t initlevel, tt_pageclass_t maxlevel);
void toptext_reset(void);
int toptext_numsubpg(int page);
int toptext_nextblkpg(int pgnum);
int toptext_nextgrppg(int pgnum);
int toptext_pginc(int page);
int toptext_pgdec(int page);
char* toptext_pgdesc(int page);
ttdesc_t* toptext_mkdesctable(int *count);

#endif /* _TOPTEXT_H */
