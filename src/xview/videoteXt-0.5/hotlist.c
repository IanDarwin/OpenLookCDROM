/*
 * hotlist.c: Routines to deal with the list of favourite VTX-pages
 *
 * Copyright (c) 1994-95 Martin Buck  <martin.buck@student.uni-ulm.de>
 * Read COPYING for more information
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <linux/vtx.h>
#include <X11/Xlib.h>
#include "safe_malloc.h"
#include "misc.h"
#include "vtxtools.h"
#include "vtxqueue.h"
#include "toptext.h"
#include "hotlist.h"


const int TIMEOUT_DEFAULT = 60, INTERLEAVE_DEFAULT = 25, LOOKAHEAD_DEFAULT = 2;
#define HOTLIST_FNAME "vtx-hotlist"

typedef struct {
  char name[25];
  int count, *pages, page_timeout, top_interleave, top_disable, lookahead, index_page;
  tt_pageclass_t search_level;
} hotlist_t;


static int hotlist_count, station_changed = TRUE;
static hotlist_t *hotlist, *hotlist_current;



static int
hotlist_load(const char *fname) {
  char line[256], name[25];
  int err, entry, page, stfound = 0, linecount = 0;
  FILE *file;
  hotlist_t *newentry = NULL;

  if (!(file = fopen(fname, "r")))
    return -1;
  while (1) {
nextline:
    if (!fgets(line, 256, file)) {
      if (feof(file))
        break;
      err = errno;
      fclose(file);
      errno = err;
      return -1;
    }
    linecount++;
    
    if (!sscanf(line, " %c ", name)) {
      continue;
    } else if (sscanf(line, " station %24[^\n\r] ", name) == 1) {
      stfound = 2;
      for (entry = 0; entry < hotlist_count; entry++) {
        if (!strcmp(name, hotlist[entry].name)) {
          newentry = hotlist + entry;
          goto nextline;
        }
      }
      hotlist_count++;
      hotlist = hotlist_current = srealloc(hotlist, hotlist_count * sizeof(hotlist_t));
      newentry = hotlist + hotlist_count - 1;
      *newentry = hotlist[0];
      strcpy(newentry->name, name);
      newentry->pages = smalloc(sizeof(int));
      newentry->count = 0;
    } else if (stfound != 2) {
      if (!stfound) {
        fprintf(stderr, "%s:%d: Warning: Missing station-name.\n", fname, linecount);
        stfound = 1;
      }
    } else if (sscanf(line, " page_timeout = %d ", &newentry->page_timeout)) {
      if (newentry->page_timeout < 10 || newentry->page_timeout > 180) {
        fprintf(stderr, "%s:%d: Warning: Invalid page_timeout.\n", fname, linecount);
        newentry->page_timeout = TIMEOUT_DEFAULT;
      }
    } else if (sscanf(line, " top_interleave = %d ", &newentry->top_interleave)) {
      if (newentry->top_interleave < -100 || newentry->top_interleave > 100) {
        fprintf(stderr, "%s:%d: Warning: Invalid top_interleave.\n", fname, linecount);
        newentry->top_interleave = INTERLEAVE_DEFAULT;
      }
    } else if (sscanf(line, " page_lookahead = %d ", &newentry->lookahead)) {
      if (newentry->lookahead < 0 || newentry->lookahead > 5) {
        fprintf(stderr, "%s:%d: Warning: Invalid page_lookahead.\n", fname, linecount);
        newentry->lookahead = LOOKAHEAD_DEFAULT;
      }
    } else if (sscanf(line, " top_disable = %24[a-zA-Z] ", name)) {
      if (!strcasecmp(name, "true")) {
        newentry->top_disable = TRUE;
      } else if (!strcasecmp(name, "false")) {
        newentry->top_disable = FALSE;
      } else {
        fprintf(stderr, "%s:%d: Warning: Invalid value, using 'false'.\n", fname, linecount);
        newentry->top_disable = FALSE;
      }
    } else if (sscanf(line, " index_page = %x ", &newentry->index_page)) {
      if (!vtx_chkpgnum(newentry->index_page, TRUE)) {
        fprintf(stderr, "%s:%d: Warning: Invalid index_page.\n", fname, linecount);
        newentry->index_page = 0x100;
      }
    } else if (sscanf(line, " auto_search_pages = %24[a-zA-Z] ", name)) {
      if (!strcasecmp(name, "none")) {
        newentry->search_level = TT_NONE;
      } else if (!strcasecmp(name, "blocks")) {
        newentry->search_level = TT_BLOCK;
      } else if (!strcasecmp(name, "groups")) {
        newentry->search_level = TT_GROUP;
      } else if (!strcasecmp(name, "normal")) {
        newentry->search_level = TT_NORM;
      } else if (!strcasecmp(name, "subpages")) {
        newentry->search_level = TT_SUBPG;
      } else {
        fprintf(stderr, "%s:%d: Warning: Invalid value, using 'normal'.\n", fname, linecount);
        newentry->search_level = TT_NORM;
      }
    } else if (sscanf(line, " %x ", &page)) {
      if (!vtx_chkpgnum(page, TRUE)) {
        fprintf(stderr, "%s:%d: Warning: Invalid page: %03X.\n", fname, linecount, page);
      } else {
        newentry->pages = srealloc(newentry->pages, (newentry->count + 2) * sizeof(int));
        newentry->pages[newentry->count++] = page;
      }
    } else {
      fprintf(stderr, "%s:%d: Warning: Syntax error\n", fname, linecount);
    }
  }
  return fclose(file);
}


void
hotlist_init(void) {
  char *hl_fname, *home;

  hotlist = hotlist_current = smalloc(1 * sizeof(hotlist_t));
  strcpy(hotlist[0].name, "Unknown");
  hotlist[0].pages = smalloc(sizeof(int));
  hotlist[0].count = 0;
  hotlist[0].page_timeout = TIMEOUT_DEFAULT;
  hotlist[0].top_interleave = INTERLEAVE_DEFAULT;
  hotlist[0].lookahead = LOOKAHEAD_DEFAULT;
  hotlist[0].top_disable = FALSE;
  hotlist[0].index_page = 0x100;
  hotlist[0].search_level = TT_NORM;
  hotlist_count = 1;

  if (hotlist_load(VTX_CONFDIR "/" HOTLIST_FNAME) && errno != ENOENT) {
    fprintf(stderr, "%s: " VTX_CONFDIR "/" HOTLIST_FNAME ": %s\n", smalloc_progname,
        strerror(errno));
    exit(1);
  }
  if ((home = getenv("HOME"))) {
    hl_fname = sstrdup(home);
    hl_fname = sstrapp(hl_fname, "/." HOTLIST_FNAME);
    if (hotlist_load(hl_fname) && errno != ENOENT) {
      fprintf(stderr, "%s: %s: %s\n", smalloc_progname, hl_fname, strerror(errno));
      exit(1);
    }
    free(hl_fname);
  } else {
    fprintf(stderr, "%s: Warning: $HOME is unset -- can't load user-hotlist\n", smalloc_progname);
  }
}


void
hotlist_exit(void) {
  int entry, page;
  char *hl_fname, *home;
  FILE *file;
  
  if (!(home = getenv("HOME"))) {
    fprintf(stderr, "%s: Warning: $HOME is unset -- can't save user-hotlist\n", smalloc_progname);
    return;
  }
  hl_fname = sstrdup(home);
  hl_fname = sstrapp(hl_fname, "/." HOTLIST_FNAME);
  file = fopen(hl_fname, "w");
  for (entry = 0; entry < hotlist_count; entry++) {
    fprintf(file, "station %s\npage_timeout=%d\ntop_interleave=%d\npage_lookahead=%d\n"
        "top_disable=%s\nindex_page=%X\nauto_search_pages=",
        hotlist[entry].name, hotlist[entry].page_timeout, hotlist[entry].top_interleave,
        hotlist[entry].lookahead, (hotlist[entry].top_disable ? "true" : "false"),
        hotlist[entry].index_page);
    switch (hotlist[entry].search_level) {
      case TT_NONE:
        fprintf(file, "none\n");
      break;
      case TT_BLOCK:
        fprintf(file, "blocks\n");
      break;
      case TT_GROUP:
        fprintf(file, "groups\n");
      break;
      case TT_NORM:
        fprintf(file, "normal\n");
      break;
      case TT_SUBPG:
        fprintf(file, "subpages\n");
      break;
    }
    if (entry) {			/* Don't save hotlist of station Unknown, just options */
      for (page = 0; page < hotlist[entry].count; page++) {
        fprintf(file, "%3x\n", hotlist[entry].pages[page]);
      }
    }
    fprintf(file, "\n");
  }
  if (ferror(file) || fclose(file)) {
    fprintf(stderr, "%s: Warning: Couldn't write %s: %s\n", smalloc_progname, hl_fname,
        strerror(errno));
    fclose(file);
  }
  free(hl_fname);
}


void
hotlist_add(int page, int pos) {
  hotlist_current->pages = srealloc(hotlist_current->pages,
      (hotlist_current->count + 1) * sizeof(int));
  if (pos >= hotlist_current->count)
    pos = hotlist_current->count;
  memmove(hotlist_current->pages + pos + 1, hotlist_current->pages + pos,
      (hotlist_current->count - pos) * sizeof(int));
  hotlist_current->count++;
  hotlist_current->pages[pos] = page;
}


int
hotlist_remove(int pos) {
  int retval;

  retval = hotlist_current->pages[pos];
  memmove(hotlist_current->pages + pos, hotlist_current->pages + pos + 1,
      (hotlist_current->count - pos - 1) * sizeof(int));
  /* Kluge to avoid getting a NULL pointer when alloc'ing 0 bytes: */
  hotlist_current->pages = srealloc(hotlist_current->pages,
      ((hotlist_current->count - 1) > 0 ? (hotlist_current->count - 1) : 1) * sizeof(int));
  hotlist_current->count--;
  return retval;
}


int
hotlist_get(int pos) {
  return hotlist_current->pages[pos];
}


int
hotlist_get_count(int station) {
  return hotlist[station].count;
}


int
hotlist_get_stcount(void) {
  return hotlist_count;
}


void
hotlist_set_current(int station) {
  station_changed = TRUE;
  hotlist_current = hotlist + station;
}


int
hotlist_get_current(void) {
  return hotlist_current - hotlist;
}


void
hotlist_put_queue(void) {
  int page;
  
  for (page = 0; page < hotlist_current->count; page++) {
    get_page(PRI_MED, hotlist_current->pages[page], 0, 0, NULL, NULL);
    history_insert(hotlist_current->pages[page], FALSE);
  }
}


char**
hotlist_mkstr(int *count) {
  static char **hotlist_str, *hotlist_pgstr;
  int entry;
  
  *count = hotlist_current->count;
  if (hotlist_str) {
    hotlist_str = srealloc(hotlist_str, (*count + 1) * sizeof(char*));
    /* Kluge to avoid getting a NULL pointer when alloc'ing 0 bytes: */
    hotlist_pgstr = srealloc(hotlist_pgstr, (*count > 0 ? *count : 1) * 4 * sizeof(char));
  } else {
    hotlist_str = smalloc((*count + 1) * sizeof(char*));
    /* Kluge to avoid getting a NULL pointer when alloc'ing 0 bytes: */
    hotlist_pgstr = smalloc((*count > 0 ? *count : 1) * 4 * sizeof(char));
  }
  for (entry = 0; entry < *count; entry++) {
    hotlist_str[entry] = hotlist_pgstr + 4 * entry * sizeof(char);
    sprintf(hotlist_str[entry], "%3X", hotlist_current->pages[entry]);
  }
  hotlist_str[*count] = NULL;
  return hotlist_str;
}


char*
hotlist_get_name(int entry) {
  return hotlist[entry].name;
}


static int
station_cmp(const byte_t *header, const byte_t *name) {
  int count, offset, len;
  const byte_t *header_chr, *name_chr;
  
  len = strlen(name);
  for (offset = 0; offset <= 24 - len; offset++) {
    header_chr = header + offset;
    name_chr = name;
    for (count = 0; count <= 23; count++) {
      if (!*name_chr)
        return TRUE;
      if (*header_chr && *header_chr != *name_chr)
        break;
      header_chr++;
      name_chr++;
    }
    if (!*name_chr) {
      return TRUE;
    }
  }
  return FALSE;
}


int
hotlist_search(const byte_t *vtx_name, int hint) {
  char name[24];
  int pos, station;
  
  for (pos = 0; pos <= 23; pos++) {
    name[pos] = vtx_name[pos];
    if (!vtx_chkparity(&name[pos])) {
      name[pos] = '\0';
    }
    /* Ignore chars from national charsets when comparing station names, because the charset is
     * unknown until a page was found
     */
    switch (name[pos]) {
      case 0x23:
      case 0x24:
      case 0x40:
      case 0x5b:
      case 0x5c:
      case 0x5d:
      case 0x5e:
      case 0x5f:
      case 0x60:
      case 0x7b:
      case 0x7c:
      case 0x7d:
      case 0x7e:
        name[pos] = '\0';
      break;
    }
  }

  if (hint && station_cmp(name, hotlist[hint].name)) {
    return hint;
  }
  for (station = 1; station < hotlist_count; station++) {
    if (station != hint && station_cmp(name, hotlist[station].name)) {
      return station;
    }
  }
  return 0;
}


void
hotlist_new_name(const char *name, int station) {
  int old_station;

  if (!station) {	/* When changing 'Unknown', create entry for new station */
    old_station = hotlist_current - hotlist;
    hotlist = srealloc(hotlist, ++hotlist_count * sizeof(hotlist_t));
    hotlist[hotlist_count - 1].pages = hotlist[0].pages;
    hotlist[hotlist_count - 1].count = hotlist[0].count;
    hotlist[hotlist_count - 1].page_timeout = TIMEOUT_DEFAULT;
    hotlist[hotlist_count - 1].top_interleave = INTERLEAVE_DEFAULT;
    hotlist[hotlist_count - 1].top_disable = FALSE;
    hotlist[hotlist_count - 1].index_page = 0x100;
    hotlist[hotlist_count - 1].search_level = TT_NORM;
    hotlist[0].pages = smalloc(sizeof(int));
    hotlist[0].count = 0;
    if (old_station) {
      hotlist_current = hotlist + old_station;
    } else {
      hotlist_current = hotlist + hotlist_count - 1;
    }
    strcpy(hotlist[hotlist_count - 1].name, name);
  } else {
    strcpy(hotlist[station].name, name);
  }
  station_changed = TRUE;
}


int
hotlist_station_changed(void) {
  int retval;
  
  retval = station_changed;
  station_changed = FALSE;
  return retval;
}


int
hotlist_get_timeout(void) {
  return hotlist_current->page_timeout;
}


void
hotlist_set_timeout(int timeout) {
  hotlist_current->page_timeout = timeout;
}


int
hotlist_get_interleave(void) {
  return hotlist_current->top_interleave;
}


void
hotlist_set_interleave(int interleave) {
  hotlist_current->top_interleave = interleave;
}


int
hotlist_get_lookahead(void) {
  return hotlist_current->lookahead;
}


void
hotlist_set_lookahead(int lookahead) {
  hotlist_current->lookahead = lookahead;
}


int
hotlist_get_topdisable(void) {
  return hotlist_current->top_disable;
}


void
hotlist_set_topdisable(int disable) {
  hotlist_current->top_disable = disable;
}


int
hotlist_get_index_page(void) {
  return hotlist_current->index_page;
}


void
hotlist_set_index_page(int page) {
  hotlist_current->index_page = page;
}


tt_pageclass_t
hotlist_get_searchlevel(void) {
  return hotlist_current->search_level;
}


void
hotlist_set_searchlevel(tt_pageclass_t level) {
  hotlist_current->search_level = level;
}
