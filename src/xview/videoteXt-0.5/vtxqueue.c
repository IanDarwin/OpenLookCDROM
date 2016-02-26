/*
 * vtxqueue.c: Routines to search for pages (possibly in the background) and to manage the
 *             page-queue, page-cache & page-history
 *
 * Copyright (c) 1994-95 Martin Buck  <martin.buck@student.uni-ulm.de>
 * Read COPYING for more information
 *
 */

#include <time.h>
#include <errno.h>
#include <string.h>
#include <linux/vtx.h>
#include <xview/xview.h>
#include <xview/panel.h>
#include <xview/svrimage.h>
#include "safe_malloc.h"
#include "misc.h"
#include "config.h"
#include "cct.h"
#include "vtxdecode.h"
#include "toptext.h"
#include "hotlist.h"
#include "dialog.h"
#include "vtxtools.h"
#include "xinit.h"
#include "xevents.h"
#include "vtxqueue.h"


typedef struct bgbuf_s {
  byte_t buf[VTX_PAGESIZE - 40];
  vtx_pageinfo_t info;
  struct bgbuf_s *next;
} bgbuf_t;
typedef struct top_queue_s {
  int page, minute, hour;
  struct top_queue_s *next;
} top_queue_t;


const int sizeof_bgbuf_t = sizeof(bgbuf_t);
int vtx_dev_open, dau_status_changed = TRUE;
queue_entry_t *dau_status;

static queue_entry_t *vtx_queue;
static top_queue_t *top_queue;
static int queue_maxlen, queue_end, top_queue_count;
static bgbuf_t *bgbuf[800];


static int check_open_device(void);
static void insert_queue(int priority, int page, int subpage, int insert_front);
static void remove_queue(int page);
static Server_image history_icon(int page, int default_img);



/* This is one of the main functions in VideoteXt. It does all the things necessary to get a
 * page: open the device if it was closed, checking if the page is already in the cache,
 * updating the dau_status, history-list & pagenumber-display for PRI_FGROUND, handling page-
 * lookahead for PRI_FGROUND, throwing out lower priority pages for PRI_TOP or inserting the
 * page into the queue for other priorities.
 * If subpage == -1, search for the next subpage; if subpage == 0, search for all subpages;
 * if subpage > 0, search for given subpage
 * If hour != -1, search only for pages with equal hour-number (only used with PRI_TOP)
 */
int
get_page(int priority, int page, int subpage, int hour, byte_t *buffer, vtxpgwin_t *vtxwin) {
  const char hexchars[] = "0123456789ABCDEF";
  int bufloop;
  
  if (check_open_device() < 0)
    return -1;

  switch (priority) {
    case PRI_FGROUND:
    case PRI_FGROUND_NH: {
      /* Change history-status of currently sought page */
      if (dau_status[0].page) {
        history_set_status(dau_status[0].page, STAT_NEW);
      }
      /* Check if page is already in cache */
      vtxwin->update = HDR_UPDATE;
      if (query_bgbuf(page, (subpage ? subpage : -1), buffer, vtxwin)) {
        buffer[0] = vtx_mkparity(6);
        vtxwin->update = SCR_UPDATE;
        update_pagenumdisp(vtxwin->page->info.pagenum, vtxwin->page->info.minute,
            toptext_numsubpg(vtxwin->page->info.pagenum),
            count_bgbuf_subpg(vtxwin->page->info.pagenum));
        history_set_status(page, STAT_READ);
      } else {
        buffer[0] = vtx_mkparity(1);
      }
      /* Search for page in DAU 0 */
      CCTCHK(cct_searchpage(page, 0, 0, PGMASK_PAGE, 0), "cct_searchpage", return -1);
      CCTCHK(cct_reset_pgfound(0), "cct_reset_pgfound", return -1);
      /* firstsubpg, lastsubpg, priority & time are unused in buffer 0 */
      dau_status[0].page = page;
      dau_status[0].subpage = subpage;
      dau_status[0].page_found = FALSE;
      dau_status[0].keep_running = TRUE;
      /* Remove page from other DAUs */
      for (bufloop = 1; bufloop < vtx_info.numpages; bufloop++) {
        if (dau_status[bufloop].page == page) {
          CCTCHK(cct_stop_dau(bufloop), "cct_stop_dau", return -1);
          dau_status[bufloop].page = 0;
        }
      }
      remove_queue(page);
      dau_status_changed = TRUE;
      /* Update display (insert 'xxx.xx' in upper left corner), reset stop/reveal-buttons */
      buffer[1] = vtx_mkparity(hexchars[page / 0x100]);
      buffer[2] = vtx_mkparity(hexchars[(page / 0x10) & 0xf]);
      buffer[3] = vtx_mkparity(hexchars[page & 0xf]);
      if (subpage > 0) {
        buffer[4] = vtx_mkparity('.');
        if (subpage >= 10) {
          buffer[5] = vtx_mkparity(hexchars[(subpage / 0x10) & 0xf]);
          buffer[6] = vtx_mkparity(hexchars[subpage & 0xf]);
        } else {
          buffer[5] = vtx_mkparity(hexchars[subpage & 0xf]);
          buffer[6] = vtx_mkparity(' ');
        }
      } else {
        buffer[4] = buffer[5] = buffer[6] = vtx_mkparity(' ');
      }
      decode_page(buffer, vtxwin->page, 0, 0);
      vtxwin->pgnum = page;
      vtxwin->subpgnum = subpage;
      xv_set(checkbox, PANEL_VALUE, 0, NULL);
      checkbox_proc();
      if (priority != PRI_FGROUND_NH) {
        history_insert(page, TRUE);
        history_set_status(page, STAT_SEARCH);
      }

      /* Perform page-lookahead */
      if (page != 0x100) {
        int lookahead, tmp_page, nextpage = page;
        
        if (remove_priority(PRI_LOW) < 0)
          return -1;
        for (lookahead = hotlist_get_lookahead(); lookahead > 0; lookahead--) {
          tmp_page = 0;
          if (toptext_ok) {
            tmp_page = toptext_pginc(nextpage);
          }
          if (!tmp_page) {
            tmp_page = inc_vtxpage(nextpage);
          }
          nextpage = tmp_page;
          if (get_page(PRI_LOW, nextpage, -1, 0, NULL, NULL) < 0) {
            return -1;
          }
        }
      }
      pgdesc_update(page);
    }
    break;
    case PRI_TOP: {				/* Search for TOP-Text-pages immediately */
      int pgbuf = 1;
      
      if (dau_status[0].page == page)
        break;
      /* Find idle DAU, or, if none available, DAU searching for page with lowest priority which
       * started searching most recently. Stop DAU, remove currently sought page from DAU &
       * re-insert it into queue.
       */
      for (bufloop = 1; bufloop < vtx_info.numpages; bufloop++) {
        if (!dau_status[bufloop].page || dau_status[bufloop].page == page) {
          pgbuf = bufloop;
          break;
        }
        if (dau_status[bufloop].priority < dau_status[pgbuf].priority ||
            (dau_status[bufloop].priority == dau_status[pgbuf].priority &&
            dau_status[bufloop].time > dau_status[pgbuf].time)) {
          pgbuf = bufloop;
        }
      }
      if (dau_status[pgbuf].page) {
        int tmppg;
        
        tmppg = dau_status[pgbuf].page;
        dau_status[pgbuf].page = 0;
        history_set_status(tmppg, STAT_NEW);
        insert_queue(dau_status[pgbuf].priority, tmppg, dau_status[pgbuf].subpage, TRUE);
      }
      remove_queue(page);
      /* Start searching for page */
      if (hour != -1) {
        CCTCHK(cct_searchpage(page, hour, 0, PGMASK_PAGE | PGMASK_HOUR, pgbuf), "cct_searchpage",
            return -1);
      } else {
        CCTCHK(cct_searchpage(page, 0, 0, PGMASK_PAGE, pgbuf), "cct_searchpage", return -1);
      }
      CCTCHK(cct_reset_pgfound(pgbuf), "cct_reset_pgfound", return -1);
      /* keep_running is unused if buffer != 0 */
      dau_status[pgbuf].page = page;
      dau_status[pgbuf].subpage = subpage;
      dau_status[pgbuf].priority = PRI_TOP;
      dau_status[pgbuf].time = time(NULL);
      dau_status[pgbuf].page_found = FALSE;
      dau_status[pgbuf].firstsubpg = dau_status[pgbuf].lastsubpg = 0;
      dau_status_changed = TRUE;
      history_set_status(dau_status[pgbuf].page, STAT_SEARCH);
    }
    break;
    default:
      if (!query_bgbuf(page, subpage, NULL, NULL) || priority == PRI_HIGH) {
        insert_queue(priority, page, subpage, FALSE);
      }
      if (priority == PRI_HIGH) {
        history_insert(page, FALSE);
      }
    break;
  }
  return 0;
}


static int
check_open_device(void) {
  int err;

  if (!vtx_dev_open) {
    if ((err = cct_open(REQ_MAJOR, REQ_MINOR, &vtx_info)) < 0) {
      if (err == CCTEVERSION) {
        char tmpstr[10];
        
        sprintf(tmpstr, "%d.%d", REQ_MAJOR, REQ_MINOR);
        confirm_notice_v("Can't open videotext-device ", cct_device, ":\nIncompatible driver "
            "version (need ", tmpstr, ").", NULL);
        return -1;
      } else {
        confirm_notice_v("Can't open videotext-device ", cct_device, ":\n", strerror(errno), NULL);
        return -1;
      }
    }
    vtx_dev_open = TRUE;
    if (dau_status) {
      free(dau_status);
    }
    dau_status = scalloc(vtx_info.numpages, sizeof(queue_entry_t));
    CCTCHK(cct_set_display(display_on_tv ? (display_mode + use_interlace * INTERLACE_OFFSET) :
        DISPOFF), "cct_set_display", return -1);
  }
  return 0;
}


void
top_getpage(int page, int hour, int minute) {
  /* We can't call get_page immediately, because this function is called from within queue_itimer,
   * which modifies dau_status. So we just queue up the requests and flush them when we leave
   * queue_itimer.
   */
  top_queue_count++;
  top_queue = srealloc(top_queue, sizeof(top_queue_t) * top_queue_count);
  top_queue[top_queue_count - 1].page = page;
  top_queue[top_queue_count - 1].minute = minute;
  top_queue[top_queue_count - 1].hour = hour;
}


int
top_flush_getpage(void) {
  if (!top_queue_count || !top_queue)
    return 0;

  if (get_page(PRI_TOP, top_queue[top_queue_count - 1].page, top_queue[top_queue_count - 1].minute,
      top_queue[top_queue_count - 1].hour, NULL, NULL) < 0)
    return -1;

  top_queue_count--;
  /* Kluge to avoid getting a NULL-pointer when realloc'ing 0 bytes: */
  top_queue = srealloc(top_queue, (sizeof(top_queue_t) * top_queue_count) + 1);
  return 0;
}


static void
exch_queue_entries(queue_entry_t *entry1, queue_entry_t *entry2) {
  queue_entry_t save;
  
  save = *entry1;
  *entry1 = *entry2;
  *entry2 = save;
}


static void
insert_queue(int priority, int page, int subpage, int insert_front) {
  int bufloop, entry, new_pos;
  
  if (dau_status[0].page == page)
    return;
  for (bufloop = 1; bufloop < vtx_info.numpages; bufloop++) {
    if (dau_status[bufloop].page == page) {
      dau_status[bufloop].time = time(NULL);
      return;
    }
  }
  for (entry = 0; entry < queue_end; entry++) {
    /* If new page already is in queue, set new priority & move it in front of all other
     * pages with same priority
     */
    if (vtx_queue[entry].page == page && vtx_queue[entry].subpage == subpage) {
      if (priority >= vtx_queue[entry].priority) {
        vtx_queue[entry].priority = priority;
        new_pos = entry;
        for (; new_pos > 0 && priority >= vtx_queue[new_pos - 1].priority; new_pos--) {
          exch_queue_entries(&vtx_queue[new_pos], &vtx_queue[new_pos - 1]);
        }
      }
      return;
    }
  }
  if (queue_end >= queue_maxlen) {
    vtx_queue = srealloc(vtx_queue, (queue_maxlen += 10) * sizeof(queue_entry_t));
  }
  
  for (entry = 0; entry < queue_end; entry++) {
    if (vtx_queue[entry].priority < priority + !!insert_front)
      break;
  }
  memmove(vtx_queue + entry + 1, vtx_queue + entry, (queue_end - entry) * sizeof(queue_entry_t));
  queue_end++;
  /* Only page, subpage & priority are used in vtx_queue */
  vtx_queue[entry].page = page;
  vtx_queue[entry].subpage = subpage;
  vtx_queue[entry].priority = priority;
}


static void
remove_queue(int page) {
  int entry;
  
  for (entry = 0; entry < queue_end; entry++) {
    if (vtx_queue[entry].page == page) {
      memmove(vtx_queue + entry, vtx_queue + entry + 1,
          (queue_end - entry - 1) * sizeof(queue_entry_t));
      queue_end--;
      return;
    }
  }
}


static int
queue_get_next(queue_entry_t *entry) {
  if (!queue_end)
    return FALSE;
  entry->page = vtx_queue[0].page;
  entry->subpage = vtx_queue[0].subpage;
  entry->priority = vtx_queue[0].priority;
  remove_queue(entry->page);
  return TRUE;
}


int
remove_priority(int priority) {
  int entry, start;
  
  for (entry = 1; entry < vtx_info.numpages; entry++) {
    if (dau_status[entry].page &&
        (priority == PRI_ALL || dau_status[entry].priority == priority)) {
      if (vtx_dev_open) {
        CCTCHK(cct_stop_dau(entry), "cct_stop_dau", return -1);
      }
      history_set_status(dau_status[entry].page, STAT_NEW);
      dau_status[entry].page = 0;
      dau_status_changed = TRUE;
    }
  }
  if (priority == PRI_ALL) {
    if (!queue_end)
      return 0;
    queue_maxlen = queue_end = 0;
    free(vtx_queue);
    vtx_queue = NULL;
  } else {
    start = -1;
    for (entry = 0; entry < queue_end; entry++) {
      if (vtx_queue[entry].priority == priority && start == -1) {
        start = entry;
      }
      if (vtx_queue[entry].priority > priority) {
        if (start != -1) {
          memmove(vtx_queue + start, vtx_queue + entry,
              (queue_end - entry) * sizeof(queue_entry_t));
          queue_end -= entry - start;
          return 0;
        }
      }
    }
  }
  return 0;
}


int
queue_itimer(void) {
  int bufloop, pgnum, newpage = 0;
  static int lastpage = 100;
  queue_entry_t next_page;

  if (!vtx_dev_open)
    return 0;
  for (bufloop = 1; bufloop < vtx_info.numpages; bufloop++) {
    if (dau_status[bufloop].page) {
      do {
        if (newpage < 0 && report_cct_error(newpage, "cct_checkpage")) {
          cct_close();
          station_status = STATION_CHANGED;
          hotlist_set_current(0);
          vtx_dev_open = FALSE;
          return -1;
        }
      } while ((newpage = cct_checkpage(bufloop)) < 0);
      if (!newpage) {
        if (dau_status[bufloop].page_found) {
          byte_t tmp_buffer[VTX_PAGESIZE - 40];
          vtx_pageinfo_t tmp_pageinfo;
          int toptext_last;

          CCTCHK(cct_getpage(bufloop, 0, 1, 39, 23, tmp_buffer, &tmp_pageinfo), "cct_getpage",
              return -1);
          /* Restart DAU to keep header-line running (for station-change detection)
           */
          CCTCHK(cct_searchpage(dau_status[bufloop].page, 0, 0, PGMASK_PAGE, bufloop),
              "cct_searchpage", return -1);
          CCTCHK(cct_reset_pgfound(bufloop), "cct_reset_pgfound", return -1);
          dau_status[bufloop].page_found = FALSE;
          insert_bgbuf(tmp_buffer, &tmp_pageinfo);
          if (!tmp_pageinfo.minute) {
            lastpage = dau_status[bufloop].page;
          }

          toptext_last = toptext_ok;
          if (!hotlist_get_topdisable() &&
              toptext_newpage(tmp_buffer, &tmp_pageinfo, &top_getpage)) {
            CCTCHK(cct_stop_dau(bufloop), "cct_stop_dau", return -1);
            dau_status[bufloop].page = 0;
            dau_status_changed = TRUE;
            if (toptext_ok != toptext_last) {
              toptext_ok_notify();
            }
            goto bufend;
          }
          
          if (dau_status[bufloop].subpage < 0 || !tmp_pageinfo.minute) {
            /* We were only searching for the next subpage or there are no subpages -> stop
             * searching
             */
            CCTCHK(cct_stop_dau(bufloop), "cct_stop_dau", return -1);
            dau_status[bufloop].page = 0;
            dau_status_changed = TRUE;
            goto bufend;
          }
          
          /* If we are searching for a certain subpage, check if the right one was found */
          if (dau_status[bufloop].subpage && dau_status[bufloop].subpage == tmp_pageinfo.minute) {
            CCTCHK(cct_stop_dau(bufloop), "cct_stop_dau", return -1);
            dau_status[bufloop].page = 0;
            dau_status_changed = TRUE;
            goto bufend;
          }

          /* Check if all subpages for this page were received */
          if (dau_status[bufloop].lastsubpg != tmp_pageinfo.minute) {
            dau_status[bufloop].time = time(NULL);
            dau_status[bufloop].lastsubpg = tmp_pageinfo.minute;
            if (!dau_status[bufloop].firstsubpg) {
              dau_status[bufloop].firstsubpg = tmp_pageinfo.minute;
            } else if (tmp_pageinfo.minute == dau_status[bufloop].firstsubpg ||
                tmp_pageinfo.minute == dau_status[bufloop].firstsubpg - 1) {
              /* All subpages received or subpage doesn't exist */
              CCTCHK(cct_stop_dau(bufloop), "cct_stop_dau", return -1);
              dau_status[bufloop].page = 0;
              dau_status_changed = TRUE;
            }
            goto bufend;
          }
bufend:
          /* If we're still searching for a page, set status to STAT_FOUND again (icon will
           * probably change because now there's already a page in the cache)
           */
          if (dau_status[bufloop].page) {
            history_set_status(dau_status[bufloop].page, STAT_SEARCH);
          } else {
            history_set_status(lastpage, STAT_FOUND);
          }
        } else {
          dau_status[bufloop].page_found = TRUE;
        }
      } else if (time(NULL) - dau_status[bufloop].time >= hotlist_get_timeout()) {
        history_set_status(dau_status[bufloop].page, STAT_TIMEOUT);
        CCTCHK(cct_stop_dau(bufloop), "cct_stop_dau", return -1);
        dau_status[bufloop].page = 0;
        dau_status_changed = TRUE;
      } 
    }

    /* Start searching for a new page if DAU is idle
     */
    if (!dau_status[bufloop].page) {
      if (queue_get_next(&next_page)) {
        CCTCHK(cct_searchpage(next_page.page, 0, 0, PGMASK_PAGE, bufloop), "cct_searchpage",
            return -1);
        CCTCHK(cct_reset_pgfound(bufloop), "cct_reset_pgfound", return -1);
        /* keep_running is unused if buffer != 0 */
        dau_status[bufloop].page = next_page.page;
        dau_status[bufloop].subpage = next_page.subpage;
        dau_status[bufloop].priority = next_page.priority;
        dau_status[bufloop].time = time(NULL);
        dau_status[bufloop].page_found = FALSE;
        dau_status[bufloop].firstsubpg = dau_status[bufloop].lastsubpg = 0;
        dau_status_changed = TRUE;
        history_set_status(dau_status[bufloop].page, STAT_SEARCH);
      } else if (toptext_ok && !hotlist_get_topdisable() && hotlist_get_searchlevel() && 
          (pgnum = toptext_getnext(lastpage, hotlist_get_interleave(), TT_BLOCK,
          hotlist_get_searchlevel())) > 0) {
        int searchbuf;

        for (searchbuf = 0; searchbuf < vtx_info.numpages; searchbuf++) {
          if (dau_status[searchbuf].page == pgnum)
            pgnum = -1;
        }
        if (pgnum == -1)
          continue;
        CCTCHK(cct_searchpage(pgnum, 0, 0, PGMASK_PAGE, bufloop), "cct_searchpage", return -1);
        CCTCHK(cct_reset_pgfound(bufloop), "cct_reset_pgfound", return -1);
        dau_status[bufloop].page = pgnum;
        dau_status[bufloop].subpage = 0;
        dau_status[bufloop].priority = PRI_MED;
        dau_status[bufloop].time = time(NULL);
        dau_status[bufloop].page_found = FALSE;
        dau_status[bufloop].firstsubpg = dau_status[bufloop].lastsubpg = 0;
        dau_status_changed = TRUE;
      }
    }
  }
  return 0;
}


void
insert_bgbuf(const byte_t *buffer, const vtx_pageinfo_t *info) {
  bgbuf_t **buf_ptr, *new;

  if (!vtx_chkpgnum(info->pagenum, FALSE))
    return;

  buf_ptr = bgbuf + vtx_hex2dec(info->pagenum) - 100;
  while (*buf_ptr && (**buf_ptr).info.minute < info->minute) {
    buf_ptr = &(**buf_ptr).next;
  }
  if (!*buf_ptr || (**buf_ptr).info.minute != info->minute) {
    new = smalloc(sizeof(bgbuf_t));
    new->next = *buf_ptr;
    *buf_ptr = new;
  }
  memcpy((**buf_ptr).buf, buffer, (VTX_PAGESIZE - 40) * sizeof(byte_t));
  (**buf_ptr).info = *info;
  return;
}


int
query_bgbuf(int page, int subpage, byte_t *buffer, vtxpgwin_t *vtxwin) {
  bgbuf_t *buf_ptr;
  
  if (!vtx_chkpgnum(page, FALSE))
    return FALSE;

  buf_ptr = bgbuf[vtx_hex2dec(page) - 100];
  while (buf_ptr) {
    if (buf_ptr->info.pagenum == page && (subpage == -1 || buf_ptr->info.minute == subpage)) {
      if (buffer != NULL)
        memcpy(buffer + 40, buf_ptr->buf, VTX_PAGESIZE - 40);
      if (vtxwin != NULL)
        vtxwin->page->info = buf_ptr->info;
      if (buffer != NULL && vtxwin != NULL) {
        decode_page(buffer, vtxwin->page, 1, 23);
      }
      return TRUE;
    }
    buf_ptr = buf_ptr->next;
  }
  return FALSE;
}


int
count_bgbuf_subpg(int page) {
  int subpages = 0;
  bgbuf_t *buf_ptr;
  
  if (!vtx_chkpgnum(page, FALSE))
    return 0;

  buf_ptr = bgbuf[vtx_hex2dec(page) - 100];
  while (buf_ptr) {
    subpages++;
    buf_ptr = buf_ptr->next;
  }
  return subpages;
}


void
flush_bgbuf(void) {
  int entry;
  bgbuf_t *current, *next;
  
  for (entry = 0; entry <= 799; entry++) {
    current = bgbuf[entry];
    bgbuf[entry] = NULL;
    while (current) {
      next = current->next;
      free(current);
      current = next;
    }
  }
}


static int history_end = -1;

void
history_insert(int page, int do_select) {
  int pos;
  char tmpstr[4];
  Server_image new_img;

  if (do_select &&
      (pos = (int)xv_get(popups[POPUP_HISTORY].list, PANEL_LIST_FIRST_SELECTED)) != -1) {
    xv_set(popups[POPUP_HISTORY].list, PANEL_LIST_SELECT, pos, FALSE, NULL);
  }
  for (pos = 0; pos <= history_end; pos++) {
    if ((int)xv_get(popups[POPUP_HISTORY].list, PANEL_LIST_CLIENT_DATA, pos) == page) {
      if (do_select) {
        xv_set(popups[POPUP_HISTORY].list, PANEL_LIST_SELECT, pos, TRUE, NULL);
      }
      return;
    }
  }

  new_img = history_icon(page, STAT_NEW);
  history_end++;
  sprintf(tmpstr, "%3X", page);
  xv_set(popups[POPUP_HISTORY].list,
      PANEL_LIST_INSERT, history_end,
      PANEL_LIST_STRING, history_end, tmpstr,
      PANEL_LIST_GLYPH, history_end, new_img,
      PANEL_LIST_MASK_GLYPH, history_end, new_img,
      PANEL_LIST_CLIENT_DATA, history_end, page,
      NULL);
  if (do_select) {
    xv_set(popups[POPUP_HISTORY].list, PANEL_LIST_SELECT, history_end, TRUE, NULL);
  }
}


void
history_flush(void) {
  xv_set(popups[POPUP_HISTORY].list, PANEL_LIST_DELETE_ROWS, 0, history_end + 1, NULL);
  history_end = -1;
}


static Server_image
history_icon(int page, int default_img) {
  int count;
  Server_image img;
  
  if ((count = count_bgbuf_subpg(page))) {
    img = hist_images[count > 1 ? STAT_FOUND_MP : STAT_FOUND];
  } else {
    img = hist_images[default_img];
  }
  return img;
}


void
history_set_status(int page, int status) {
  int pos;
  Server_image current, new;
  
  for (pos = 0; pos <= history_end; pos++) {
    if ((int)xv_get(popups[POPUP_HISTORY].list, PANEL_LIST_CLIENT_DATA, pos) == page) {
      new = current = (Server_image)xv_get(popups[POPUP_HISTORY].list, PANEL_LIST_GLYPH, pos);
      if (current != hist_images[STAT_READ] && current != hist_images[STAT_READ_MP]) {
        switch (status) {
          case STAT_FOUND:
          case STAT_NEW:
            new = history_icon(page, STAT_NEW);
          break;
          case STAT_SEARCH:
            if (query_bgbuf(page, -1, NULL, NULL)) {
              new = hist_images[STAT_SEARCH_MP];
            } else {
              new = hist_images[STAT_SEARCH];
            }
          break;
          case STAT_TIMEOUT:
            new = history_icon(page, STAT_TIMEOUT);
          break;
          case STAT_READ:
            if (count_bgbuf_subpg(page) > 1) {
              new = hist_images[STAT_READ_MP];
            } else {
              new = hist_images[STAT_READ];
            }
          break;
        }
      } else if (status == STAT_READ) {
        if (count_bgbuf_subpg(page) > 1) {
          new = hist_images[STAT_READ_MP];
        } else {
          new = hist_images[STAT_READ];
        }
      }
      if (new != current) {
        xv_set(popups[POPUP_HISTORY].list,
            PANEL_LIST_GLYPH, pos, new,
            PANEL_LIST_MASK_GLYPH, pos, new,
            NULL);
      }
    }
  }
}
