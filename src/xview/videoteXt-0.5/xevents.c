/*
 * xevents.c: Callbacks for XView-objects. The main work of the XView-version is done here.
 *
 * Copyright (c) 1994-95 Martin Buck  <martin.buck@student.uni-ulm.de>
 * Read COPYING for more information
 *
 */

#include <unistd.h>
#include <ctype.h>
#include <string.h>
#include <signal.h>
#include <sys/time.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/keysym.h>
#include <xview/xview.h>
#include <xview/frame.h>
#include <xview/panel.h>
#include <xview/notify.h>
#include <xview/notice.h>
#include "vtx_assert.h"
#include "safe_malloc.h"
#include "misc.h"
#include "cct.h"
#include "vtxtools.h"
#include "vtxdecode.h"
#include "vtxqueue.h"
#include "toptext.h"
#include "config.h"
#include "dialog.h"
#include "hotlist.h"
#include "fileio.h"
#include "xevents.h"
#include "xinit.h"

#include "titlepage.h"


vtxpgwin_t vtxwin;
vtxpage_t page;
byte_t vtx_buffer[VTX_PAGESIZE];
int station_status = STATION_UNKNOWN, update_entry = 1, do_dither, auto_reset = TRUE;

static int update_count;
static volatile int reload_intv_file = TRUE;
static vtxpage_t curr_disp;



void
xquit(void) {
  xv_destroy_safe(frame);
}


int
report_cct_error(int err, const char *err_func_str) {
  Xv_notice notice;
  char *catmsg;
  int retval;
  
  catmsg = sstrdup("Error: Videotext-driver returned\n");
  switch (err) {
    case CCTERR:
      catmsg = sstrapp(catmsg, "I/O-error");
    break;
    case CCTEINVAL:
      catmsg = sstrapp(catmsg, "Invalid argument");
    break;
    default:
      catmsg = sstrapp(catmsg, "Unknown error");
    break;
  }
  catmsg = sstrapp(catmsg, " in function ");
  catmsg = sstrapp(catmsg, err_func_str);
  catmsg = sstrapp(catmsg, "\nfor device ");
  catmsg = sstrapp(catmsg, cct_device);
  notice = (Xv_notice)xv_create(frame, NOTICE,
      NOTICE_MESSAGE_STRING, catmsg,
      NOTICE_BUTTON_YES, "Retry",
      NOTICE_BUTTON_NO, "Close device",
      XV_SHOW, TRUE,
      NULL);
  switch ((int)xv_get(notice, NOTICE_STATUS)) {
    case NOTICE_YES:
      retval = FALSE;
    break;
    case NOTICE_NO:
      retval = TRUE;
    break;
    default:
      assert(0);
    break;
  }
  xv_destroy_safe(notice);
  free(catmsg);
  return retval;
}


void
sigusr1_handler(int dummy) {
  reload_intv_file = TRUE;
  signal(SIGUSR1, sigusr1_handler);
}


void
font_menu_proc(Menu menu, Menu_item menu_item) {
  int font;
  
  font = (int)xv_get(menu_item, MENU_CLIENT_DATA);
  xv_set(frame,
      XV_WIDTH, 41 * vtxfonts[font].width + (intv_fname ? 0 : RPANEL_WIDTH) +
          4 * WIN_DEFAULT_BORDER_WIDTH,
      XV_HEIGHT, 26 * vtxfonts[font].height + CANVAS_OFFSET,
      NULL);
}


void
update_menu_proc(Menu menu, Menu_item menu_item) {
  update_count = 0;
  update_entry = (int)xv_get(menu_item, MENU_CLIENT_DATA);
}


void
checkbox_proc(void) {
  int val;

  val = (int)xv_get(checkbox, PANEL_VALUE);
  vtxwin.stopped = ((val & 1) && vtxwin.stopped == 2) ? 2 : (val & 1);
  if (vtxwin.hidden != !(val & 2)) {
    vtxwin.hidden = !(val & 2);
    vtxwin.update = SCR_UPDATE;
  }
  xv_set(intv_reveal_toggle, PANEL_VALUE, !vtxwin.hidden, NULL);
}


void
intv_reveal_proc(void) {
  xv_set(checkbox, PANEL_VALUE, (int)xv_get(checkbox, PANEL_VALUE) ^ 2, NULL);
  checkbox_proc();
}


int
new_station(int station) {
  hotlist_set_current(station);
  station_status = (station ? STATION_RECOGNIZED : STATION_CHANGED);
  station_selected = FALSE;
  remove_priority(PRI_ALL);
  flush_bgbuf();
  toptext_reset();
  toptext_ok_notify();
  history_flush();
  update_pagenumdisp(-1, -1, -1, -1);
  return get_page(PRI_FGROUND, hotlist_get_index_page(), 0, 0, vtx_buffer, &vtxwin);
}


void
reset_proc(void) {
  if (vtx_dev_open) {
    cct_close();
    vtx_dev_open = FALSE;
  }
  new_station(0);
}


void
pginc_proc(void) {
  int nextpage;
  
  nextpage = inc_vtxpage(vtxwin.pgnum);
  get_page(PRI_FGROUND, nextpage, 0, 0, vtx_buffer, &vtxwin);
}


void
pgdec_proc(void) {
  int prevpage;
  
  prevpage = dec_vtxpage(vtxwin.pgnum);
  get_page(PRI_FGROUND, prevpage, 0, 0, vtx_buffer, &vtxwin);
}


void
subpginc_proc(void) {
  int nextsubpage, subpages;
  
  nextsubpage = inc_vtxpage(vtxwin.subpgnum + 0x100) - 0x100;
  subpages = toptext_numsubpg(vtxwin.pgnum);
  if (subpages == 1) {
    nextsubpage = 0;
  } else if (subpages > 1 && nextsubpage > subpages) {
    nextsubpage = 1;
  }
  if (nextsubpage > 0x79) {
    nextsubpage = 1;
  }
  get_page(PRI_FGROUND_NH, vtxwin.pgnum, nextsubpage, 0, vtx_buffer,
      &vtxwin);
}


void
subpgdec_proc(void) {
  int prevsubpage, subpages;
  
  prevsubpage = dec_vtxpage(vtxwin.subpgnum + 0x200) - 0x200;
  subpages = toptext_numsubpg(vtxwin.pgnum);
  if (subpages == 1) {
    prevsubpage = 0;
  } else if (prevsubpage < 1) {
    if (subpages > 1) {
      prevsubpage = subpages;
    } else {
      prevsubpage = 0x79;
    }
  }
  get_page(PRI_FGROUND_NH, vtxwin.pgnum, prevsubpage, 0, vtx_buffer,
      &vtxwin);
}


void
subpgcont_proc(void) {
  get_page(PRI_FGROUND_NH, vtxwin.pgnum, 0, 0, vtx_buffer, &vtxwin);
}


static void
check_get_pgnum(int priority) {
  char *page_str, *subpage_str;
  int page, subpage;

  page_str = (char*)xv_get(pgnum_item, PANEL_VALUE);
  page = (strcmp(page_str, "") ? strtol(page_str, NULL, 16) : vtxwin.pgnum);
  subpage_str = (char*)xv_get(subpgnum_item, PANEL_VALUE);
  subpage = strtol(subpage_str, NULL, 16);

  if (vtx_chkpgnum(page, TRUE) && subpage >= 0 && subpage <= 0x7f) {
    get_page(priority, page, subpage, 0, vtx_buffer, &vtxwin);
    xv_set(pgnum_item, PANEL_VALUE, "", NULL);
    xv_set(subpgnum_item, PANEL_VALUE, "", NULL);
  } else {
    confirm_notice("Invalid page number. Valid range\nis 100-8FF/1-7F (hexadecimal).");
  }
}


Panel_setting
pgnum_proc(void) {
  check_get_pgnum(PRI_FGROUND);
  return PANEL_NONE;
}


Panel_setting
pgnum_bg_proc(void) {
  check_get_pgnum(PRI_HIGH);
  return PANEL_NONE;
}


static void
display_tv(int x1, int y1, int x2, int y2, int hidden, const byte_t *buffer,
    const vtx_pageinfo_t *info) {
  byte_t tmp_buf[VTX_PAGESIZE];
  int pos;

  if (!vtx_dev_open || !display_on_tv)
    return;
  if (hidden) {
    for (pos = x1 + y1 * 40; pos <= x2 + y2 * 40; pos++) {
      tmp_buf[pos - x1 - y1 * 40] = buffer[pos] & 0x7f;
    }
  } else {
    for (pos = x1 + y1 * 40; pos <= x2 + y2 * 40; pos++) {
      tmp_buf[pos - x1 - y1 * 40] = (((buffer[pos] & 0x7f) == 0x18) ? ' ' : buffer[pos] & 0x7f);
    }
  }
  CCTCHK(cct_putpage(x1, y1, x2, y2, tmp_buf, info), "cct_putpage", return);
}


void
update_pagenumdisp(int page, int subpage, int numsubpages, int cachecount) {
  char tmpstr[20];
  
  if (page < 0 || subpage < 0) {
    xv_set(pgmsg_item, PANEL_LABEL_STRING, "Page: ?", NULL);
  } else {
    if (subpage) {
      sprintf(tmpstr, "Page: %X/%X", page, subpage);
    } else {
      sprintf(tmpstr, "Page: %X", page);
    }
    xv_set(pgmsg_item, PANEL_LABEL_STRING, tmpstr, NULL);
  }
  if (numsubpages < 0) {
    xv_set(subpgmsg_item, PANEL_LABEL_STRING, "Subpgs.: ?", NULL);
  } else {
    sprintf(tmpstr, "Subpgs.: %d", numsubpages);
    xv_set(subpgmsg_item, PANEL_LABEL_STRING, tmpstr, NULL);
  }
  if (page < 0 || subpage < 0) {
    xv_set(cachemsg_item, PANEL_LABEL_STRING, "Cached: ?", NULL);
  } else {
    sprintf(tmpstr, "Cached: %d", cachecount);
    xv_set(cachemsg_item, PANEL_LABEL_STRING, tmpstr, NULL);
  }
}


static int
check_station_name(void) {
  static int last_buf, cycle_count, last_recognized;

  /* Try once a second to recognize the current station-name
   */
  if (vtx_dev_open && cycle_count++ >= 4) {
    cycle_count = 0;
    if (station_status == STATION_UNKNOWN || station_status == STATION_CHANGED ||
        (auto_reset && !station_selected)) {
      char station_name[24];
      int station, buf;
      vtx_pageinfo_t info;
        
      /* Read the current header-line from a running DAU
       */
      if (!last_buf || !dau_status[last_buf].page) {
        for (buf = 1; buf < vtx_info.numpages; buf++) {
          if (dau_status[buf].page) {
            if (last_buf && !dau_status[last_buf].page) {
              cct_stop_dau(last_buf);
            }
            last_buf = buf;
            goto get_header;
          }
        }
        /* No DAU is searching for a page. Start the last one and search for any page to get the
         * current header-line
         */
        last_buf = buf - 1;
        CCTCHK(cct_searchpage(0x8ff, 0x3f, 0x7f, PGMASK_PAGE | PGMASK_HOUR | PGMASK_MINUTE,
            last_buf), "cct_searchpage", return -1);
        CCTCHK(cct_reset_pgfound(last_buf), "cct_reset_pgfound", return -1);
      }
get_header:
      CCTCHK(cct_getpage(last_buf, 8, 0, 31, 0, station_name, &info), "cct_getpage", return -1);

      /* Find out station-name if it is unknown up to now
       */
      if (station_status == STATION_UNKNOWN) {
        if ((station = hotlist_search(station_name, last_recognized))) {
          hotlist_set_current(station);
          station_status = STATION_RECOGNIZED;
          if (!hotlist_get_topdisable() && get_page(PRI_TOP, 0x1f0, -1, -1, NULL, NULL) < 0) {
            return -1;
          }
          hotlist_put_queue();
        }
      } else if (station_status == STATION_CHANGED) {
        station_status = STATION_UNKNOWN;
        update_pagenumdisp(-1, -1, -1, -1);
      }
      /* If auto_reset is set, try to find out if the station has changed
       */
      if (auto_reset) {
        if ((station = hotlist_search(station_name, hotlist_get_current())) !=
            hotlist_get_current() && station) {
          /* A station must be recognized at least twice in a row to trigger an auto-reset
           */
          if (station == last_recognized) {
            if (new_station(station) < 0 ||
                (!hotlist_get_topdisable() && get_page(PRI_TOP, 0x1f0, -1, -1, NULL, NULL) < 0)) {
              return -1;
            }
            hotlist_put_queue();
            last_recognized = 0;
          } else {
            last_recognized = station;
          }
        } else {
          last_recognized = 0;
        }
      }
    } else {
      /* No station-name required. Stop DAU if it is still running.
       */
      if (last_buf && !dau_status[last_buf].page) {
        CCTCHK(cct_stop_dau(last_buf), "cct_stop_dau", return -1);
        last_buf = 0;
      }
    }
  }
  return 0;
}


Notify_value
update_proc(void) {
  int do_update = 0, st_changed;
  static int flash_count = 0, lastfont = -1, last_dev_status = -1;
  byte_t tmp_buffer[VTX_PAGESIZE - 40];
  vtx_pageinfo_t tmp_info;
  
  /* Load & display page in INtv-mode. This will be executed on startup and whenever VideoteXt
   * receives SIGUSR1
   */
  if (intv_fname && reload_intv_file) {
    FILE *file;
    
    if (!((file = fopen(intv_fname, "r")))) {
      confirm_notice_v("Can't open ", intv_fname, " for reading:\n", strerror(errno), NULL);
      xquit();
    }
    load_vtx(file);
    if (ferror(file) || fclose(file) < 0) {
      confirm_notice_v("Error while reading ", intv_fname, ":\n", strerror(errno), NULL);
      fclose(file);
      xquit();
    }
    reload_intv_file = FALSE;
  }

  /* Check if a new page was received in DAU 0 & prepare to update the screen if necessary
   */
  if (vtx_dev_open && vtxwin.stopped != 2 && (!update_count || dau_status[0].page_found)) {
    if (dau_status[0].page_found) {
      dau_status[0].page_found = FALSE;
      CCTCHK(cct_getpage(0, 0, 1, 39, 23, tmp_buffer, &tmp_info), "cct_getpage",
          return NOTIFY_DONE);
      insert_bgbuf(tmp_buffer, &tmp_info);

      if (!hotlist_get_topdisable()) {
        int toptext_last = toptext_ok;
        
        toptext_newpage(tmp_buffer, &tmp_info, &top_getpage);
        if (toptext_ok != toptext_last) {
          toptext_ok_notify();
        }
      }

      /* Update screen only, if the right subpage was found (or if we didn't search for a certain
       * subpage
       */
      if (dau_status[0].subpage <= 0 || dau_status[0].subpage == tmp_info.minute) {
        memcpy(vtx_buffer + 40, tmp_buffer, VTX_PAGESIZE - 40);
        page.info = tmp_info;
        CCTCHK(cct_getpage(0, 7, 0, 39, 0, vtx_buffer + 7, NULL), "cct_getpage",
            return NOTIFY_DONE);
        CCTCHK(cct_reset_pgfound(0), "cct_reset_pgfound", return NOTIFY_DONE);
        vtx_buffer[0] = vtx_mkparity(7);
        decode_page(vtx_buffer, &page, 0, 23);
        history_set_status(dau_status[0].page, STAT_READ);
        update_pagenumdisp(tmp_info.pagenum, tmp_info.minute, toptext_numsubpg(tmp_info.pagenum),
            count_bgbuf_subpg(tmp_info.pagenum));
        dau_status[0].keep_running = FALSE;
        do_update = SCR_UPDATE;
      } else if (dau_status[0].keep_running) {
        /* Restart DAU so that the header-line keeps on running */
        CCTCHK(cct_searchpage(dau_status[0].page, 0, 0, PGMASK_PAGE, 0), "cct_searchpage",
            return NOTIFY_DONE);
        CCTCHK(cct_reset_pgfound(0), "cct_reset_pgfound", return NOTIFY_DONE);
      }
    } else {
      if (!vtxwin.stopped) {
        int newpage = 0;
      
        do {
          if (newpage < 0 && report_cct_error(newpage, "cct_checkpage")) {
            cct_close();
            station_status = STATION_CHANGED;
            hotlist_set_current(0);
            vtx_dev_open = FALSE;
            return -1;
          }
        } while ((newpage = cct_checkpage(0)) < 0);
        if (!newpage)
          dau_status[0].page_found = TRUE;
      }
      /* If it's time to update the screen, check if a new page was received. If there's a new
       * page, delay the update, because we don't know yet if it really was the page we were
       * searching for, so we don't want to stop the running header-line now.
       */
      if (update_entry && !dau_status[0].page_found) {
        CCTCHK(cct_getpage(0, 7, 0, 39, 0, vtx_buffer + 7, NULL), "cct_getpage",
            return NOTIFY_DONE);
        decode_page(vtx_buffer, &page, 0, 0);
        do_update = HDR_UPDATE;
      }
    }
  }
  /* Update the screen if one of the update-flags is set or if it's time to enter a new flash-cycle
   */
  if (--update_count < 0) {
    update_count = update_table[update_entry].update_interval;
  }
  switch (MAX(do_update, vtxwin.update)) {
    case HDR_UPDATE:
      x_vtx_redraw(&vtxwin, 0, 0, 39, 0, FALSE);
      display_tv(0, 0, 39, 0, vtxwin.hidden, vtx_buffer, &page.info);
    break;
    case SCR_UPDATE:
      x_vtx_redraw(&vtxwin, 0, 0, 39, 23, FALSE);
      display_tv(0, 0, 39, 23, vtxwin.hidden, vtx_buffer, &page.info);
    break;
  }
  if (!flash_count--) {
    flash_count = (vtxwin.flash_state = !vtxwin.flash_state) ? 3 : 6;
    x_vtx_redraw(&vtxwin, 0, 0, 39, 23, FALSE);
  }
  vtxwin.update = 0;

  if (queue_itimer() < 0)
    return NOTIFY_DONE;
  if (top_flush_getpage() < 0) {
    return NOTIFY_DONE;
  }

  /* Display ccc-status on left footer
   */
  if (dau_status_changed || vtx_dev_open != last_dev_status) {
    int pgbuf;
    char tmpstr[256] = "CCT Status:";
    
    if (intv_fname) {
      xv_set(frame,
          FRAME_LEFT_FOOTER, "Display-only mode",
          FRAME_RIGHT_FOOTER, intv_fname,
          NULL);
    } else {
      if (vtx_dev_open) {
        for (pgbuf = 0; pgbuf < vtx_info.numpages; pgbuf++) {
          sprintf(tmpstr + strlen(tmpstr), " %d:", pgbuf);
          if (dau_status[pgbuf].page) {
            sprintf(tmpstr + strlen(tmpstr), "%X", dau_status[pgbuf].page);
          } else {
            sprintf(tmpstr + strlen(tmpstr), "Idle");
          }
        }
      } else {
        sprintf(tmpstr + strlen(tmpstr), " Not connected");
      }
      xv_set(frame, FRAME_LEFT_FOOTER, tmpstr, NULL);
    }
    dau_status_changed = FALSE;
  }

  if (check_station_name() < 0)
    return NOTIFY_DONE;

  /* Display station-name & charset-info on right footer if station has changed
   */
  if (!intv_fname &&
      ((st_changed = hotlist_station_changed()) || vtxwin.page->info.charset != lastfont)) {
    char *tmp_name;
    
    tmp_name = sstrdup(hotlist_get_name(hotlist_get_current()));
    if (vtxwin.page->info.charset == 3 || vtxwin.page->info.charset == 7) {
      tmp_name = sstrapp(tmp_name, "  (Unknown charset)");
    }
    xv_set(frame, FRAME_RIGHT_FOOTER, tmp_name, NULL);
    free(tmp_name);
    if (st_changed) {
      sel_st_update(TRUE);
      hot_update();
      station_opt_update();
    }
  }
  lastfont = vtxwin.page->info.charset;
  last_dev_status = vtx_dev_open;
  return NOTIFY_DONE;
}


void
canvas_repaint(Canvas canvas, Xv_window paint_window, const Rectlist *area) {
  x_vtx_redraw(&vtxwin, area->rl_bound.r_left / vtxfonts[vtxwin.font].width,
      area->rl_bound.r_top / vtxfonts[vtxwin.font].height,
      (area->rl_bound.r_width  + area->rl_bound.r_left) / vtxfonts[vtxwin.font].width,
      (area->rl_bound.r_height + area->rl_bound.r_top) / vtxfonts[vtxwin.font].height, TRUE);
}


void
canvas_proc(Xv_window window, const Event *event) {
  int pgnum;

  switch (event_action(event)) {
    case ACTION_SELECT:
      if (!intv_fname && event_is_down(event) && (pgnum = x_extract_pgnum(&vtxwin, event_x(event) /
          vtxfonts[vtxwin.font].width, event_y(event) / vtxfonts[vtxwin.font].height))) {
        get_page(PRI_FGROUND, pgnum, 0, 0, vtx_buffer, &vtxwin);
      }
    break;
    case ACTION_MENU:
      if (!intv_fname && event_is_down(event) && (pgnum = x_extract_pgnum(&vtxwin, event_x(event) /
          vtxfonts[vtxwin.font].width, event_y(event) / vtxfonts[vtxwin.font].height))) {
        get_page(PRI_HIGH, pgnum, 0, 0, vtx_buffer, &vtxwin);
      }
    break;
    case KBD_USE:
      xv_set(right_panel, WIN_SET_FOCUS, NULL);
    break;
  }
}


void
frame_proc(Xv_window window, const Event *event) {
  switch (event_action(event)) {
    case WIN_RESIZE: {
      int font, foundfont = FALSE;

      xv_set((Menu_item)xv_get(font_menu, MENU_NTH_ITEM, vtxwin.font + 1),
          MENU_SELECTED, FALSE, NULL);

      vtxwin.font = 0;		/* Find the biggest font that fits into the canvas */
      for (font = 0; font <= vtxmaxfont; font++) {
        if (vtxfonts[vtxwin.font].width <= vtxfonts[font].width &&
            vtxfonts[vtxwin.font].height <= vtxfonts[font].height &&
            vtxfonts[font].width * 41 <= (int)xv_get(frame, XV_WIDTH) -
            (intv_fname ? 0 : RPANEL_WIDTH) - 4 * WIN_DEFAULT_BORDER_WIDTH &&
            vtxfonts[font].height * 26 <= (int)xv_get(frame, XV_HEIGHT) - CANVAS_OFFSET) {
          vtxwin.font = font;
          foundfont = TRUE;
        }
      }
      if (!foundfont) {		/* If no font fits, find the smallest one */
        for (font = 0; font <= vtxmaxfont; font++) {
          if (vtxfonts[font].width <= vtxfonts[vtxwin.font].width &&
              vtxfonts[font].height <= vtxfonts[vtxwin.font].height)
            vtxwin.font = font;
        }
      }

      xv_set((Menu_item)xv_get(font_menu, MENU_NTH_ITEM, vtxwin.font + 1),
          MENU_SELECTED, TRUE, NULL);
      xv_set(canvas_bg,
          XV_WIDTH, 41 * vtxfonts[vtxwin.font].width + 4 * WIN_DEFAULT_BORDER_WIDTH,
          XV_HEIGHT, 26 * vtxfonts[vtxwin.font].height,
          NULL);
      xv_set(canvas,
          XV_Y, vtxfonts[vtxwin.font].height,
          XV_WIDTH, 40 * vtxfonts[vtxwin.font].width + 2 * WIN_DEFAULT_BORDER_WIDTH,
          XV_HEIGHT, 24 * vtxfonts[vtxwin.font].height + 2 * WIN_DEFAULT_BORDER_WIDTH,
          CANVAS_WIDTH, 40 * vtxfonts[vtxwin.font].width + 2 * WIN_DEFAULT_BORDER_WIDTH,
          CANVAS_HEIGHT, 24 * vtxfonts[vtxwin.font].height + 2 * WIN_DEFAULT_BORDER_WIDTH,
          NULL);
      /* Need to set XV_WIDTH to work around bug in XView (otherwise parts exposed after moving
       * don't get redrawn)
       */
      xv_set(right_panel,
          XV_X, (int)xv_get(frame, XV_WIDTH) - RPANEL_WIDTH,
          XV_WIDTH, RPANEL_WIDTH,
          NULL);
      xv_set(intv_reveal_toggle, XV_X, (int)xv_get(frame, XV_WIDTH) - 65, NULL);
      position_popups();
    }
    break;
    case WIN_REPARENT_NOTIFY: {
      static int popup_init;

      /* Creating the popups here makes sure that we can find out the size of the wm's decoration
       * since our frame-window has already been reparented by the wm. Unfortunately, this doesn't
       * work if the window manager doesn't reparent our window (or if no wm is running at all),
       * but since the popups are not vital, I ignore this.
       */
      if (!popup_init && !intv_fname) {
        popup_init = TRUE;
        if (popups[POPUP_HISTORY].startup)
          history_proc();
        if (popups[POPUP_TOPTEXT].startup)
          toptext_proc();
        if (popups[POPUP_HOTLIST].startup)
          hotlist_proc();
        if (popups[POPUP_SELST].startup)
          sel_station_proc();
      }
    }
    break;
    case WIN_PROPERTY_NOTIFY: {
      static int laststate = -1;
      
      if ((int)xv_get(frame, FRAME_CLOSED) != laststate) {
        laststate = (int)xv_get(frame, FRAME_CLOSED);
        iconify_popups(laststate);
      }
    }
    break;
  }
}


/* This function checks if a hotkey was pressed. Every non-hex-key can be pressed either with
 * or without meta (except for 'x' and 'r'). Hex-digits must be pressed with Meta to be
 * interpreted as a hotkey, otherwise they will be interpreted as a part of the page-number
 */
Notify_value
panel_interposer(Panel panel, const Event *event, Notify_arg arg, Notify_event_type type) {
  XKeyEvent *xev = (XKeyEvent*)event->ie_xevent;
  KeySym keysym;
  static XComposeStatus cs;
  char buf[3];

  if (xev->type == KeyPress) {
    buf[XLookupString(xev, buf, sizeof buf, &keysym, &cs)] = '\0';
    if (strlen(buf) == 1 && ((!isxdigit(buf[0]) && !iscntrl(buf[0])) || xev->state & Mod1Mask)) {
      switch (tolower(buf[0])) {
        case 'e':
          xv_set(checkbox, PANEL_VALUE, (int)xv_get(checkbox, PANEL_VALUE) ^ 2, NULL);
          checkbox_proc();
          return NOTIFY_DONE;
        case 'x':
          if (xev->state & Mod1Mask) {
            xquit();
            return NOTIFY_DONE;
          }
        break;
      }
      if (!intv_fname) {
        switch (tolower(buf[0])) {
          case '\r':
          case '\n':
            pgnum_bg_proc();
            return NOTIFY_DONE;
          case '+':
            pginc_proc();
            return NOTIFY_DONE;
          case '-':
            pgdec_proc();
            return NOTIFY_DONE;
          case 's':
            xv_set(checkbox, PANEL_VALUE, (int)xv_get(checkbox, PANEL_VALUE) ^ 1, NULL);
            checkbox_proc();
            return NOTIFY_DONE;
          case 'r':
            if (xev->state & Mod1Mask) {
              reset_proc();
              return NOTIFY_DONE;
            }
          break;
          case 'n':
            history_move(+1);
            return NOTIFY_DONE;
          case 'p':
            history_move(-1);
            return NOTIFY_DONE;
          case 'b':
            toptext_go_nextblk();
            return NOTIFY_DONE;
          case 'g':
            toptext_go_nextgrp();
            return NOTIFY_DONE;
          case 'i':
            get_page(PRI_FGROUND, hotlist_get_index_page(), 0, 0, vtx_buffer, &vtxwin);
            return NOTIFY_DONE;
          case 'c':
            subpgcont_proc();
            return NOTIFY_DONE;
        }
      }
    }
    if (!intv_fname) {
      switch (keysym) {
        case XK_Up:
          subpginc_proc();
          return NOTIFY_DONE;
        case XK_Down:
          subpgdec_proc();
          return NOTIFY_DONE;
        case XK_Prior:
          toptext_go_nextpage();
          return NOTIFY_DONE;
        case XK_Next:
          toptext_go_prevpage();
          return NOTIFY_DONE;
      }
    }
  }
  return notify_next_event_func(panel, (Notify_event)event, arg, type);
}


void
x_main_loop(void) {
  int pos;
  struct itimerval itimer;

  for (pos = 0; pos <= 959; pos++) {
    vtx_buffer[pos] = ' ';
    page.chr[pos] = titlepage_chr[pos];
    page.attrib[pos] = titlepage_attrib[pos];
    curr_disp.chr[pos] = ' ';
    curr_disp.attrib[pos] = 65535;
  }

  vtxwin.winid = (Window)xv_get(canvas_paint_window(canvas), XV_XID);
  vtxwin.gc = vtxgc;
  vtxwin.draw_bm = vtxwin.flash_state = vtxwin.update = 0;
  vtxwin.page = &page;
  vtxwin.curr_disp = &curr_disp;
  vtxwin.pgnum = 0x100;
  vtxwin.subpgnum = 0;
  checkbox_proc();

  xv_set(font_menu, MENU_VALUE, vtxwin.font, NULL);
  xv_set(frame,
      XV_WIDTH, 41 * vtxfonts[vtxwin.font].width + (intv_fname ? 0 : RPANEL_WIDTH) +
          4 * WIN_DEFAULT_BORDER_WIDTH,
      XV_HEIGHT, 26 * vtxfonts[vtxwin.font].height + CANVAS_OFFSET,
      NULL);
  xv_set(right_panel, XV_X, 41 * vtxfonts[vtxwin.font].width + 4 * WIN_DEFAULT_BORDER_WIDTH, NULL);
  xv_set(intv_reveal_toggle, XV_X, 41 * vtxfonts[vtxwin.font].width + 2 * WIN_DEFAULT_BORDER_WIDTH
      - 65, NULL);
  xv_set(scr_menu, MENU_VALUE, update_entry, NULL);
  hotlist_init();

  itimer.it_interval.tv_sec = 0; itimer.it_interval.tv_usec = 200000;
  itimer.it_value.tv_sec = 0; itimer.it_value.tv_usec = 200000;
  notify_set_itimer_func(frame, update_proc, ITIMER_REAL, &itimer, NULL);

  xv_main_loop(frame);
  
  if (!intv_fname)
    hotlist_exit();
}


void
xv_sleep(int sec, int usec) {
  struct timeval tv;
  sigset_t mask, oldmask;

  tv.tv_sec = sec;
  tv.tv_usec = usec;
  sigemptyset(&mask);
  sigaddset(&mask, SIGIO);
  sigaddset(&mask, SIGALRM);
  sigprocmask(SIG_BLOCK, &mask, &oldmask);
  select(0, NULL, NULL, NULL, &tv);
  sigprocmask(SIG_SETMASK, &oldmask, NULL);
}


/* Read 3-digit number at pos x,y in vtxwin, also do some *blinkenlights*
 * Return 0 if no number found, hex-value otherwise
 */
int
x_extract_pgnum(const vtxpgwin_t *vtxwin, int x, int y) {
  int numstart, numend, pos, pgnum;
  chr_t *chr, num[4];
  
  x = MAX(0, MIN(39, x));
  y = MAX(0, MIN(23, y));
  chr = vtxwin->page->chr;
  if (vtxwin->page->attrib[y * 40 + x] & VTX_HIDDEN && vtxwin->hidden)
    return 0;
  if (vtxwin->page->attrib[y * 40 + x] & VTX_DOUBLE1)
    y++;
  for (numstart = x; numstart >= 0; numstart--)
    if (!isdigit(chr[y * 40 + numstart]))
      break;
  numstart++;
  for (numend = x; numend <= 39; numend++)
    if (!isdigit(chr[y * 40 + numend]))
      break;
  numend--;
  if (numend - numstart != 2)
    return 0;
  memcpy(num, chr + y * 40 + numstart, 3);
  num[3] = 0;
  pgnum = strtol(num, NULL, 16);
  if (vtx_chkpgnum(pgnum, FALSE)) {
    for (pos = numstart + y * 40; pos <= numend + y * 40; pos++) {
      vtxwin->page->attrib[pos] = (vtxwin->page->attrib[pos] & ~(VTX_COLMASK | VTX_BGMASK)) |
          ((vtxwin->page->attrib[pos] & VTX_COLMASK) << 3) |
          ((vtxwin->page->attrib[pos] & VTX_BGMASK) >> 3);
    }
    x_vtx_redraw(vtxwin, numstart, y, numend, y, FALSE);
    XSync(dpy, False);
    xv_sleep(0, 200000);
    for (pos = numstart + y * 40; pos <= numend + y * 40; pos++) {
      vtxwin->page->attrib[pos] = (vtxwin->page->attrib[pos] & ~(VTX_COLMASK | VTX_BGMASK)) |
          ((vtxwin->page->attrib[pos] & VTX_COLMASK) << 3) |
          ((vtxwin->page->attrib[pos] & VTX_BGMASK) >> 3);
    }
    x_vtx_redraw(vtxwin, numstart, y, numend, y, FALSE);
    XSync(dpy, False);
    return pgnum;
  } else return 0;
}


/* The next two routines are responsible for (re)drawing a VTX-page. flush_string_buffer()
 * should be called only by x_vtx_redraw() which tries to do as few GC-switches as possible and
 * also to print as long strings as possible to reduce X-protocol-overhead.
 * vtxwin is the structure that holds all necessary information about a VTX-page.
 * If expose is true, the complete requested region gets redrawn; otherwise only parts that have
 * changed since the last call get updated.
 * x1/y1 & x2/y2 (in character-positions, _not_ pixels!) define the rectangle that will be drawn
 * Returns TRUE if OK, FALSE if rectangle is invalid
 */
typedef struct {
  Window winid;
  GC gc;
  int xpos, ypos, count, xinc, yinc, draw_bm, flash_state;
  unsigned long fg, bg;
  attrib_t attr;
  chr_t *chr;
} stringbuf_t;

static void
flush_string_buffer(stringbuf_t *buf) {
  static chr_t tmp_str[40];
  int a;
     
  if (buf->count > 0) {
    if (buf->attr & VTX_HIDDEN || (buf->attr & VTX_FLASH && buf->flash_state)) {
      memset(tmp_str, ' ', buf->count);
    } else {
      memcpy(tmp_str, buf->chr, buf->count);
    }
    if (!do_dither || buf->draw_bm) {
      XDrawImageString(dpy, buf->winid, buf->gc, buf->xpos, buf->ypos, tmp_str, buf->count);
    } else {
      XSetStipple(dpy, buf->gc, dith_bm[buf->bg]);
      XFillRectangle(dpy, buf->winid, buf->gc, buf->xpos, buf->ypos - buf->yinc,
          buf->xinc * buf->count, buf->yinc);
      XSetStipple(dpy, buf->gc, dith_bm[buf->fg]);
      XDrawString(dpy, buf->winid, buf->gc, buf->xpos, buf->ypos, tmp_str, buf->count);
    }
    if (buf->attr & VTX_GRSEP) {
      for (a = 0; a < buf->count; a++)
        tmp_str[a] = (buf->chr[a] >= 0x80 && buf->chr[a] <= 0xbf) ? 255 : ' ';
      if (buf->draw_bm || !do_dither) {
        XSetForeground(dpy, buf->gc, buf->draw_bm ? 0 : buf->bg);
        XDrawString(dpy, buf->winid, buf->gc, buf->xpos, buf->ypos, tmp_str, buf->count);  
        XSetForeground(dpy, buf->gc, buf->draw_bm ? 1 : buf->fg);
      } else {
        XSetStipple(dpy, buf->gc, dith_bm[buf->bg]);
        XDrawString(dpy, buf->winid, buf->gc, buf->xpos, buf->ypos, tmp_str, buf->count);  
      }
    }
  }
  buf->chr += buf->count;
  buf->xpos += buf->count * buf->xinc;
  buf->count = 0;
}

int
x_vtx_redraw(const vtxpgwin_t *vtxwin, int x1, int y1, int x2, int y2, int expose) {
  int line, col, doubleht = 0;
  stringbuf_t buf;
  attrib_t *currattr, *currdispattr;
  chr_t *currchr, *currdispchr, tmp_chr;
  const attrib_t *lastattr, default_attr = 7;

  x1 = MAX(0, x1);
  y1 = MAX(0, y1);
  x2 = MIN(39, x2);
  y2 = MIN(23, y2);
  if (y1 * 40 + x1 > y2 * 40 + x2)
    return FALSE;
  buf.xinc = vtxfonts[vtxwin->font].width;
  buf.yinc = vtxfonts[vtxwin->font].height;
  buf.winid = vtxwin->winid;
  buf.attr = default_attr;
  buf.gc = vtxwin->gc;
  buf.draw_bm = vtxwin->draw_bm;
  buf.flash_state = vtxwin->flash_state;
  lastattr = &default_attr;
  if (vtxwin->draw_bm) {
    XSetForeground(dpy, vtxwin->gc, 1);
    XSetBackground(dpy, vtxwin->gc, 0);
    XSetFillStyle(dpy, vtxwin->gc, FillSolid);
  } else {
    XSetForeground(dpy, vtxwin->gc, buf.fg = VTXCWHITE);
    XSetBackground(dpy, vtxwin->gc, buf.bg = VTXCBLACK);
    if (!do_dither) {
      XSetFillStyle(dpy, vtxwin->gc, FillSolid);
    } else {
      XSetFillStyle(dpy, vtxwin->gc, FillOpaqueStippled);
      buf.fg = 7;
      buf.bg = 0;
    }
  }
  XSetFont(dpy, vtxwin->gc, vtxfonts[vtxwin->font].normal->fid);
  for (line = y1; line <= y2 || doubleht; line++) {
    buf.count = 0;
    buf.xpos = x1 * buf.xinc;
    buf.ypos = (line + 1) * buf.yinc;
    buf.chr = currchr = vtxwin->page->chr + line * 40 + x1;
    currattr = vtxwin->page->attrib + line * 40 + x1;
    currdispchr = vtxwin->curr_disp->chr + line * 40 + x1;
    currdispattr = vtxwin->curr_disp->attrib + line * 40 + x1;
    doubleht = 0;
    for (col = x1; col <= x2; col++) {
      if (*currattr & VTX_DOUBLE1)
        doubleht = 1;
      tmp_chr = ((*currattr & VTX_HIDDEN && vtxwin->hidden) ||
          (*currattr & VTX_FLASH && vtxwin->flash_state)) ? ' ' : *currchr;
      if (expose || *currdispchr != tmp_chr || *currdispattr != *currattr) {
        *currdispchr = tmp_chr;
        *currdispattr = *currattr;
        if ((*lastattr & VTX_COLMASK) != (*currattr & VTX_COLMASK)) {
          flush_string_buffer(&buf);
          if (!vtxwin->draw_bm) {
            if (!do_dither) {
              XSetForeground(dpy, vtxwin->gc, buf.fg = vtxcolors[*currattr & VTX_COLMASK]);
            } else {
              buf.fg = *currattr & VTX_COLMASK;
            }
          }
        }
        if ((*lastattr & VTX_BGMASK) != (*currattr & VTX_BGMASK)) {
          flush_string_buffer(&buf);
          if (!vtxwin->draw_bm) {
            if (!do_dither) {
              XSetBackground(dpy, vtxwin->gc, buf.bg = vtxcolors[(*currattr >> 3) & VTX_COLMASK]);
            } else {
              buf.bg = (*currattr >> 3) & VTX_COLMASK;
            }
          }
        }
        if ((*lastattr & VTX_DOUBLE2) != (*currattr & VTX_DOUBLE2)) {
          flush_string_buffer(&buf);
          XSetFont(dpy, vtxwin->gc, (*currattr & VTX_DOUBLE2 ?
              vtxfonts[vtxwin->font].doubleht->fid : vtxfonts[vtxwin->font].normal->fid));
        }
        if ((*lastattr & VTX_GRSEP) != (*currattr & VTX_GRSEP)) {
          flush_string_buffer(&buf);
          buf.attr = (buf.attr & ~VTX_GRSEP) | (*currattr & VTX_GRSEP);
        }
        if ((*lastattr & VTX_HIDDEN) != (*currattr & VTX_HIDDEN)) {
          flush_string_buffer(&buf);
          buf.attr = (buf.attr & ~VTX_HIDDEN) | (*currattr & VTX_HIDDEN) * vtxwin->hidden;
        }
        if ((*lastattr & VTX_FLASH) != (*currattr & VTX_FLASH)) {
          flush_string_buffer(&buf);
          buf.attr = (buf.attr & ~VTX_FLASH) | (*currattr & VTX_FLASH);
        }
        lastattr = currattr;
        buf.count++;
      } else {
        flush_string_buffer(&buf);
        buf.xpos += buf.xinc;
        buf.chr++;
      }
      currchr++;
      currattr++;
      currdispchr++;
      currdispattr++;
    }
    flush_string_buffer(&buf);
  }
  return TRUE;
}
