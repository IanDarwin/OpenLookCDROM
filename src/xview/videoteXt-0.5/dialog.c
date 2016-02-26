/*
 * dialog.c: Event-handlers & initialization for XView-popups
 *
 * Copyright (c) 1994-95 Martin Buck  <martin.buck@student.uni-ulm.de>
 * Read COPYING for more information
 *
 */

#include <stdio.h>
#include <stdarg.h>
#include <time.h>
#include <math.h>
#include <sys/stat.h>
#include <xview/xview.h>
#include <xview/panel.h>
#include <xview/notice.h>
#include <xview/file_chsr.h>
#include <X11/Xatom.h>
#include "vtx_assert.h"
#include "safe_malloc.h"
#include "misc.h"
#include "vtxtools.h"
#include "config.h"
#include "cct.h"
#include "fileio.h"
#include "postscript.h"
#include "vtxdecode.h"
#include "vtxqueue.h"
#include "toptext.h"
#include "hotlist.h"
#include "xinit.h"
#include "xevents.h"
#include "dialog.h"


enum { APPLY_BUTTON = 1, CLOSE_BUTTON, CHGNAME_BUTTON, RDHEADER_BUTTON, UP_BUTTON, DOWN_BUTTON,
    ADD_BUTTON, REMOVE_BUTTON, GOTO_BUTTON, PREV_BUTTON, NEXT_BUTTON, INDEX_BUTTON, NEXTGRP_BUTTON,
    NEXTBLK_BUTTON };

#define BORDER_DISTANCE 20


popups_t popups[POPUP_COUNT];
int toptext_when_active = FALSE, display_on_tv = FALSE, use_interlace = TRUE;
int display_mode = DISPOFF, station_selected = FALSE;
int win_adj_x, win_adj_y, win_lb, win_rb, win_tb, win_bb, win_adjusted;
unsigned char default_cps_prn[100] = "|lpr", default_ps_prn[100] = "|lpr";
unsigned char default_ascii_prn[100] = "|lpr", default_bm_prn[100] = "|lpr";

static int win_adj_orig_x, win_adj_orig_y, ignore_config;
static time_t win_orig_set;
static Panel selst_panel;
static Panel_item station_text, st_buttons[3], tt_buttons[4], ov_menubutton, hier_menubutton;
static Panel_item top_msgs[5];
static Frame st_frame;
static Menu ov_menu, hier_menu;
static ttdesc_t *tt_desctable = NULL;
static int tt_desccount;


static void stname_cb_proc(Panel_item item, const Event *event);
static Panel_setting set_station_name(void);
static void sel_st_apply(int selected);
static void sel_st_cb_proc(Panel_item item, const Event *event);
static void hotlist_goto(int selected);
static void hot_cb_proc(Panel_item item, const Event *event);
static void history_goto(int selected);
static void hist_cb_proc(Panel_item item, const Event *event);
static void toptext_cb_proc(Panel_item item, const Event *event);
static void topmenu_update(const ttdesc_t *desctable, int desccount);
static void toptext_menu_cb_proc(Menu menu, Menu_item menu_item);
static void layout_cb_proc(Panel_item item, const Event *event);
static void tvopt_cb_proc(Panel_item item, const Event *event);
static void miscopt_cb_proc(Panel_item item, const Event *event);
static void so_cb_proc(Panel_item item, const Event *event);
static void station_opt_activate(void);



static void
center_frame(Frame parent, Frame new_frame) {
  int x, y, width, height, popup;
  Window dummy;

  if (!win_orig_set && !win_adjusted) {
    for (popup = 0; popup < POPUP_COUNT; popup++) {
      if (popups[popup].frame == new_frame) {
        ignore_config++;
      }
    }
  }
  width = (int)xv_get(new_frame, XV_WIDTH);
  height = (int)xv_get(new_frame, XV_HEIGHT);
  XTranslateCoordinates(dpy, (Window)xv_get(parent, XV_XID), rootid, 0, 0, &x, &y, &dummy);
  x += ((int)xv_get(parent, XV_WIDTH) - width) / 2;
  y += ((int)xv_get(parent, XV_HEIGHT) - height) / 2;

  if (x + width + BORDER_DISTANCE > DisplayWidth(dpy, screen_num))
    x = DisplayWidth(dpy, screen_num) - width - BORDER_DISTANCE ;
  if (x < BORDER_DISTANCE)
    x = BORDER_DISTANCE;
  if (y + height + BORDER_DISTANCE > DisplayHeight(dpy, screen_num))
    y = DisplayHeight(dpy, screen_num) - height - BORDER_DISTANCE;
  if (y < BORDER_DISTANCE)
    y = BORDER_DISTANCE;
#ifdef DEBUG_WM
  printf("center_frame: x: %d y: %d\n", x, y);
#endif
  xv_set(new_frame, 
      XV_X, x,
      XV_Y, y,
      NULL);
}


/* Find out the size of the decoration the window manager puts around our window & store it in
 * global variables. I assume that the wm's decoration window is a child of the (virtual) root
 * window (If you have a virtual root window, the __SWM_ROOT-property of the top-level window must
 * be set to the virtual root window's ID)!
 */
static void
get_decoration_size(void) {
  Window wm_win, root, vroot, parent, *children, wdummy;
  Atom vroot_atom;
  int nchildren, xf, yf, wf, hf, wm_xf, wm_yf, wm_wf, wm_hf, wm_border, idummy;
  
  vroot = None;
  if ((vroot_atom = XInternAtom(dpy, "__SWM_ROOT", True)) != None) {
    unsigned char *property;
    Atom type;
    int format;
    unsigned long nitems, after;

    if (XGetWindowProperty(dpy, frameid, vroot_atom, 0, 1, False, XA_WINDOW, &type, &format,
        &nitems, &after, &property) == Success) {
      if (type == XA_WINDOW && format == 32 && nitems == 1 && after == 0) {
        vroot = *((Window*)property);
#ifdef DEBUG_WM
        printf("__SWM_ROOT atom found, virtual root is 0x%lx\n", (unsigned long)vroot);
#endif
      }
      XFree(property);
    }
  }
  wm_win = frameid;
  while (XQueryTree(dpy, wm_win, &root, &parent, &children, &nchildren)) {
    XFree(children);
    if (parent == root || (vroot != None && parent == vroot))
      break;
    wm_win = parent;
  }
  XGetGeometry(dpy, wm_win, &wdummy, &idummy, &idummy, &wm_wf, &wm_hf, &wm_border, &idummy);
  XTranslateCoordinates(dpy, wm_win, rootid, 0, 0, &wm_xf, &wm_yf, &wdummy);
  XGetGeometry(dpy, frameid, &wdummy, &idummy, &idummy, &wf, &hf, &idummy, &idummy);
  XTranslateCoordinates(dpy, frameid, rootid, 0, 0, &xf, &yf, &wdummy);
  win_lb = xf - wm_xf + wm_border;
  win_tb = yf - wm_yf + wm_border;
  win_rb = wm_wf - wf - win_lb + 2 * wm_border;
  win_bb = wm_hf - hf - win_tb + 2 * wm_border;

#ifdef DEBUG_WM
  printf("frame_id: 0x%lx wm_id: 0x%lx\n", (unsigned long)frameid, (unsigned long)wm_win);
  printf("frame_size: %dx%d wm_size: %dx%d\n", wf, hf, wm_wf, wm_hf);
  printf("wm_decoration=%d,%d,%d,%d\n", win_lb, win_rb, win_tb, win_bb);
#endif

  /* Some sanity checks...
   */
  if (win_lb < 0 || win_lb > 50 || win_rb < 0 || win_rb > 50 || 
      win_tb < 0 || win_tb > 50 || win_bb < 0 || win_bb > 50) {
    confirm_notice("Warning: Can't determine size\nof window manager's decoration.");
    win_lb = win_rb = win_bb = 10;
    win_tb = 30;
  }
}


void
position_popups(void) {
  Window wdummy;
  int popup, x = 0, y = 0, xf, yf, wf, hf, idummy, newpos = FALSE;
  int lypos, rypos, txpos, bxpos;
  
  XGetGeometry(dpy, frameid, &wdummy, &idummy, &idummy, &wf, &hf, &idummy, &idummy);
  XTranslateCoordinates(dpy, frameid, rootid, 0, 0, &xf, &yf, &wdummy);
  lypos = rypos = yf - win_tb;
  txpos = bxpos = xf - win_lb;
  
  for (popup = 0; popup < POPUP_COUNT; popup++) {
    if (popups[popup].frame && (int)xv_get(popups[popup].frame, XV_SHOW) &&
        !(int)xv_get(popups[popup].frame, FRAME_CLOSED)) {
      switch (popups[popup].placement) {
        case PLACEMENT_LEFT:
          x = xf - win_lb - win_rb - (int)xv_get(popups[popup].frame, XV_WIDTH) + win_adj_x;
          y = lypos + win_tb + win_adj_y;
          lypos += (int)xv_get(popups[popup].frame, XV_HEIGHT) + win_tb + win_bb;
          newpos = TRUE;
        break;
        case PLACEMENT_RIGHT:
          x = xf + wf + win_rb + win_lb + win_adj_x;
          y = rypos + win_tb + win_adj_y;
          rypos += (int)xv_get(popups[popup].frame, XV_HEIGHT) + win_tb + win_bb;
          newpos = TRUE;
        break;
        case PLACEMENT_TOP:
          x = txpos + win_lb + win_adj_x;
          y = yf + - win_tb - win_bb - (int)xv_get(popups[popup].frame, XV_HEIGHT) + win_adj_y;
          txpos += (int)xv_get(popups[popup].frame, XV_WIDTH) + win_lb + win_rb;
          newpos = TRUE;
        break;
        case PLACEMENT_BOTTOM:
          x = bxpos + win_lb + win_adj_x;
          y = yf + hf + win_bb + win_tb + win_adj_y;
          bxpos += (int)xv_get(popups[popup].frame, XV_WIDTH) + win_lb + win_rb;
          newpos = TRUE;
        break;
        case PLACEMENT_CENTER:
        break;
        default:
          assert(0);
        break;
      }
      if (newpos) {
#ifdef DEBUG_WM
        printf("position_popup: x: %d y: %d\n", x, y);
#endif
        XMoveWindow(dpy, (Window)xv_get(popups[popup].frame, XV_XID), x, y);
        if (!win_orig_set) {
          win_adj_orig_x = x;
          win_adj_orig_y = y;
          win_orig_set = time(NULL);
        }
        newpos = FALSE;
      }
    }
  }
}
  

void
iconify_popups(int state) {
  int popup;
  
  for (popup = 0; popup < POPUP_COUNT; popup++) {
    if (popups[popup].frame) {
      xv_set(popups[popup].frame, FRAME_CLOSED, state, NULL);
    }
  }
}


static void
popup_frame_proc(Xv_window window, const Event *event) {
  int popup, rows;

  switch (event_action(event)) {
    case WIN_STRUCTURE_NOTIFY:
      /* Find out necessary offset for window positions */
      if (!win_adjusted && event_xevent(event)->xconfigure.send_event) {
        if (ignore_config) {
          ignore_config--;
        } else if (win_orig_set) {
          win_adj_x = win_adj_orig_x - event_xevent(event)->xconfigure.x;
          win_adj_y = win_adj_orig_y - event_xevent(event)->xconfigure.y;
          win_adjusted = TRUE;

#ifdef DEBUG_WM
          printf("wm_offset=%d,%d\n", win_adj_x, win_adj_y);
#endif

          /* Some sanity checks to avoid trouble with stupid window managers and other things that 
           * can go wrong (really many things can go wrong, since this whole autodetection is a
           * _BIG_KLUGE_ !!!)
           */
          if (difftime(time(NULL), win_orig_set) > 10 ||
              win_adj_x < -50 || win_adj_x > 50 || win_adj_y < -50 || win_adj_y > 50) {
            win_adj_x = 30;
            win_adj_y = 10;
            confirm_notice("Warning: Can't determine\noffset for window positions.");
          }
          get_decoration_size();
          position_popups();
        }
      }
    
      /* Check if the size of one of the popups containing a PANEL_LIST has changed & adjust number
       * of rows for PANEL_LIST if necessary.
       */
      for (popup = 0; popup < POPUP_COUNT; popup++) {
        if (window == popups[popup].frame)
          break;
      }
      assert(popup != POPUP_COUNT);

      if (!popups[popup].list)
        return;
      rows = (event_xevent(event)->xconfigure.height - popups[popup].borderht) / popups[popup].rowht;

      /* Check number of rows in case some stupid window manager ignores our size hints */
      rows = (rows >= 3) ? rows : 3;

      if (rows != (int)xv_get(popups[popup].list, PANEL_LIST_DISPLAY_ROWS)) {
        xv_set(popups[popup].list, PANEL_LIST_DISPLAY_ROWS, rows, NULL);
        popups[popup].default_rows = rows;

        if (popup == POPUP_SELST) {
          int button;

          for (button = 0; button <= 2; button++) {
            xv_set(st_buttons[button],
                XV_Y, rows * popups[POPUP_SELST].rowht + popups[POPUP_SELST].borderht - 32,
                NULL);
          }
          panel_paint(selst_panel, PANEL_CLEAR);
        }
        position_popups();
      }
    break;    
    case WIN_PROPERTY_NOTIFY:
      position_popups();
    break;
  }
}


static void
popup_frame_done(Frame frame) {
  xv_set(frame, XV_SHOW, FALSE, NULL);
  position_popups();
}


static void
disable_frame_resize(Frame frame, int allow_ygrow) {
  xv_set(frame,
      FRAME_MIN_SIZE, (int)xv_get(frame, XV_WIDTH), (int)xv_get(frame, XV_HEIGHT),
      FRAME_MAX_SIZE, (int)xv_get(frame, XV_WIDTH),
          (allow_ygrow ? 100000 : (int)xv_get(frame, XV_HEIGHT)),
      NULL);
}


/* Print an alert-box containing a single string
 */
void
confirm_notice(const char *msg) {
  confirm_notice_v(msg, NULL);
}


/* Print an alert-box containing multiple strings; arg-list must be NULL-terminated.
 */
void
confirm_notice_v(const char *msg, ...) {
  Xv_notice notice;
  char *catmsg, *nextmsg;
  va_list argp;
  
  va_start(argp, msg);
  catmsg = sstrdup(msg);
  while ((nextmsg = va_arg(argp, char*))) {
    catmsg = sstrapp(catmsg, nextmsg);
  }
  va_end(argp);
  notice = (Xv_notice)xv_create(frame, NOTICE,
      NOTICE_MESSAGE_STRING, catmsg,
      XV_SHOW, TRUE,
      NULL);
  xv_destroy_safe(notice);
  free(catmsg);
}


/* Ask the user for confirmation to overwrite a file. If can_append is true, also ask if
 * the user wants to append to an existing file.
 */
int
chk_conf_overwrite(const char *path, int can_append) {
  Xv_notice notice;
  char *catmsg;
  int retval;
  struct stat st;
  
  if (stat(path, &st) < 0)
    return 0;
  catmsg = sstrdup("The file \"");
  catmsg = sstrapp(catmsg, path);
  catmsg = sstrapp(catmsg, "\" already exists.\n");
  if (can_append) {
    catmsg = sstrapp(catmsg, "Do you want to overwrite it\nor append the new data to it?");
    notice = (Xv_notice)xv_create(frame, NOTICE,
        NOTICE_MESSAGE_STRING, catmsg,
        NOTICE_BUTTON_YES, "Overwrite",
        NOTICE_BUTTON, "Append", 2,
        NOTICE_BUTTON_NO, "Cancel",
        XV_SHOW, TRUE,
        NULL);
  } else {
    catmsg = sstrapp(catmsg, "Do you want to overwrite it?");
    notice = (Xv_notice)xv_create(frame, NOTICE,
        NOTICE_MESSAGE_STRING, catmsg,
        NOTICE_BUTTON_YES, "Overwrite",
        NOTICE_BUTTON_NO, "Cancel",
        XV_SHOW, TRUE,
        NULL);
  }
  switch ((int)xv_get(notice, NOTICE_STATUS)) {
    case NOTICE_YES:
      retval = 0;
    break;
    case NOTICE_NO:
      retval = -1;
    break;
    case 2:
      retval = 1;
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
about_proc(void) {
  Xv_notice notice;
  
  notice = (Xv_notice)xv_create(frame, NOTICE,
      NOTICE_MESSAGE_STRING,
          VTXWINNAME " for Linux, Version " VTXVERSION "\n"
          "© 1994-95 Martin Buck <martin.buck@student.uni-ulm.de>\n\n"
          "Look for new releases on ftp://ftp.gwdg.de/pub/linux/misc\n"
          "or check my WWW-page: http://www.uni-ulm.de/~s_buck/\n\n"
          "This program is free software and is distributed under\n"
          "the terms of the GNU General Public License.",
      NOTICE_NO_BEEPING, TRUE,
      XV_SHOW, TRUE,
      NULL);
  xv_destroy_safe(notice);
}


void
stat_proc(void) {
  Xv_notice notice;
  int norm_count, multi_count, subpg_count, subpages, page;
  char *msg;
  char tmpstr[20];
  
  msg = sstrdup("Statistics for station ");
  msg = sstrapp(msg, hotlist_get_name(hotlist_get_current()));
  norm_count = multi_count = 0;
  if (toptext_ok) {
    for (page = 100; page <= 899; page++) {
      switch (toptext_numsubpg(vtx_dec2hex(page))) {
        case 0:
        break;
        case 1:
          norm_count++;
        break;
        default:
          multi_count++;
        break;
      }
    }
  }
  msg = sstrapp(msg, ":\n\nNumber of normal pages in TOP-Table: ");
  sprintf(tmpstr, "%d", norm_count);
  msg = sstrapp(msg, tmpstr);
  msg = sstrapp(msg, "\nNumber of multi-pages in TOP-Table: ");
  sprintf(tmpstr, "%d", multi_count);
  msg = sstrapp(msg, tmpstr);
  norm_count = multi_count = subpg_count = 0;
  for (page = 100; page <= 899; page++) {
    if (query_bgbuf(vtx_dec2hex(page), 0, NULL, NULL)) {
      norm_count++;
    } else if ((subpages = count_bgbuf_subpg(vtx_dec2hex(page)))) {
      multi_count++;
      subpg_count += subpages;
    }
  }
  msg = sstrapp(msg, "\nNumber of normal pages in cache: ");
  sprintf(tmpstr, "%d", norm_count);
  msg = sstrapp(msg, tmpstr);
  msg = sstrapp(msg, "\nNumber of multi-pages in cache: ");
  sprintf(tmpstr, "%d", multi_count);
  msg = sstrapp(msg, tmpstr);
  msg = sstrapp(msg, "\nTotal number of pages in cache: ");
  sprintf(tmpstr, "%d", subpg_count + norm_count);
  msg = sstrapp(msg, tmpstr);
  msg = sstrapp(msg, "\nMemory used by cache: ");
  sprintf(tmpstr, "%d", ((subpg_count + norm_count) * sizeof_bgbuf_t) / 1024);
  msg = sstrapp(msg, tmpstr);
  msg = sstrapp(msg, " kB");
  notice = (Xv_notice)xv_create(frame, NOTICE,
      NOTICE_MESSAGE_STRING, msg,
      NOTICE_NO_BEEPING, TRUE,
      XV_SHOW, TRUE,
      NULL);
  xv_destroy_safe(notice);
  free(msg);
}


static int
list_dbl_click(Panel_item item, const char *entry_name, Xv_opaque client_data, Panel_list_op op,
    const Event *event, int row) {
  int selected;

  if (op == PANEL_LIST_OP_DBL_CLICK) {
    selected = (int)xv_get(item, PANEL_LIST_FIRST_SELECTED);
    if (item == popups[POPUP_HISTORY].list) {
      history_goto(selected);
    } else if (item == popups[POPUP_HOTLIST].list) {
      hotlist_goto(selected);
    } else if (item == popups[POPUP_SELST].list) {
      sel_st_apply(selected);
    } else {
      assert(0);
    }
  }
  return XV_OK;
}


void
station_name_proc(int station) {
  Panel st_panel;

  if (!st_frame) {
    st_frame = (Frame)xv_create(frame, FRAME,
        XV_LABEL, "Set station name",
        NULL);
    st_panel = (Panel)xv_create(st_frame, PANEL, NULL);
    xv_create(st_panel, PANEL_MESSAGE,
        XV_X, 30,
        XV_Y, 15,
        PANEL_LABEL_STRING, "Please enter the TV station's name. NOTE: The",
        NULL);
    xv_create(st_panel, PANEL_MESSAGE,
        XV_X, 15,
        XV_Y, 30,
        PANEL_LABEL_STRING, "name must be a constant substring of the station's",
        NULL);
    xv_create(st_panel, PANEL_MESSAGE,
        XV_X, 45,
        XV_Y, 45,
        PANEL_LABEL_STRING, "headline. If you use 'Read header', please",
        NULL);
    xv_create(st_panel, PANEL_MESSAGE,
        XV_X, 40,
        XV_Y, 60,
        PANEL_LABEL_STRING, "remove everything but the station's name.",
        NULL);
    station_text = (Panel_item)xv_create(st_panel, PANEL_TEXT,
        XV_X, 30,
        XV_Y, 90,
        PANEL_LABEL_STRING, "Station name:",
        PANEL_VALUE_STORED_LENGTH, 24,
        PANEL_VALUE_DISPLAY_WIDTH, 180,
        PANEL_NOTIFY_PROC, set_station_name,
        NULL);
    xv_create(st_panel, PANEL_BUTTON,
        XV_X, 65,
        XV_Y, 130,
        PANEL_LABEL_STRING, "Apply",
        PANEL_NOTIFY_PROC, stname_cb_proc,
        PANEL_CLIENT_DATA, APPLY_BUTTON,
        NULL);
    xv_create(st_panel, PANEL_BUTTON,
        XV_X, 125,
        XV_Y, 130,
        PANEL_LABEL_STRING, "Read header",
        PANEL_NOTIFY_PROC, stname_cb_proc,
        PANEL_CLIENT_DATA, RDHEADER_BUTTON,
        NULL);
    xv_create(st_panel, PANEL_BUTTON,
        XV_X, 225,
        XV_Y, 130,
        PANEL_LABEL_STRING, "Close",
        PANEL_NOTIFY_PROC, stname_cb_proc,
        PANEL_CLIENT_DATA, CLOSE_BUTTON,
        NULL);
    window_fit(st_panel);
    window_fit(st_frame);
    disable_frame_resize(st_frame, FALSE);
  }
  if (!(int)xv_get(st_frame, XV_SHOW))
    center_frame(popups[POPUP_SELST].frame, st_frame);
  xv_set(station_text,
      PANEL_VALUE, hotlist_get_name(station),
      PANEL_CLIENT_DATA, station,
      NULL);
  xv_set(st_frame, XV_SHOW, TRUE, NULL);
}


static void
stname_cb_proc(Panel_item item, const Event *event) {
  switch ((int)xv_get(item, PANEL_CLIENT_DATA)) {
    case APPLY_BUTTON:
      set_station_name();
    break;
    case RDHEADER_BUTTON: {
      char header_str[25];
      int pos, strpos;

      strpos = 0;
      for (pos = 0; pos <= 23; pos++) {
        if (vtx2iso_table[vtxwin.page->chr[pos + 8]] != ' ' || strpos) {
          header_str[strpos++] = vtx2iso_table[vtxwin.page->chr[pos + 8]];
        }
      }
      header_str[strpos] = '\0';
      for (pos = strpos - 1; pos >= 0; pos--) {
        if (header_str[pos] == ' ') {
          header_str[pos] = '\0';
        } else {
          break;
        }
      }
      xv_set(station_text, PANEL_VALUE, header_str, NULL);
    }
    break;
    case CLOSE_BUTTON:
      xv_set(st_frame, WIN_SHOW, FALSE, NULL);
    break;
    default:
      assert(0);
    break;
  }
}


static Panel_setting
set_station_name(void) {
  char name[25], *tmp_name;
  int pos;
  
  tmp_name = (char*)xv_get(station_text, PANEL_VALUE);
  strcpy(name, tmp_name + strspn(tmp_name, " "));
  for (pos = strlen(name) - 1; pos >= 0; pos--) {
    if (name[pos] == ' ') {
      name[pos] = '\0';
    } else {
      break;
    }
  }
  hotlist_new_name(name, (int)xv_get(station_text, PANEL_CLIENT_DATA));
  sel_st_update(FALSE);
  return PANEL_NONE;
}


void
sel_station_proc(void) {
  popups_t *cpop = popups + POPUP_SELST;
    
  if (!cpop->frame) {
    cpop->frame = (Frame)xv_create(frame, FRAME,
        XV_LABEL, "Select station",
        WIN_CONSUME_EVENTS,
          WIN_STRUCTURE_NOTIFY, WIN_PROPERTY_NOTIFY, NULL,
        WIN_EVENT_PROC, popup_frame_proc,
        FRAME_DONE_PROC, popup_frame_done,
        NULL);
    selst_panel = (Panel)xv_create(cpop->frame, PANEL, NULL);
    cpop->list = (Panel_item)xv_create(selst_panel, PANEL_LIST,
        XV_X, 15,
        XV_Y, 15,
        PANEL_LIST_TITLE, "Stations available",
        PANEL_LIST_WIDTH, 215,
        PANEL_LIST_DISPLAY_ROWS, cpop->default_rows,
        PANEL_READ_ONLY, TRUE,
        PANEL_LIST_DO_DBL_CLICK, TRUE,
        PANEL_NOTIFY_PROC, list_dbl_click,
        NULL);
    st_buttons[0] = xv_create(selst_panel, PANEL_BUTTON,
        XV_X, 20,
        XV_Y, 165,
        PANEL_LABEL_STRING, "Apply",
        PANEL_NOTIFY_PROC, sel_st_cb_proc,
        PANEL_CLIENT_DATA, APPLY_BUTTON,
        NULL);
    st_buttons[1] = xv_create(selst_panel, PANEL_BUTTON,
        XV_X, 80,
        XV_Y, 165,
        PANEL_LABEL_STRING, "Change Name",
        PANEL_NOTIFY_PROC, sel_st_cb_proc,
        PANEL_CLIENT_DATA, CHGNAME_BUTTON,
        NULL);
    st_buttons[2] = xv_create(selst_panel, PANEL_BUTTON,
        XV_X, 190,
        XV_Y, 165,
        PANEL_LABEL_STRING, "Close",
        PANEL_NOTIFY_PROC, sel_st_cb_proc,
        PANEL_CLIENT_DATA, CLOSE_BUTTON,
        NULL);
    window_fit(selst_panel);
    window_fit(cpop->frame);
    cpop->rowht = (int)xv_get(cpop->list, PANEL_LIST_ROW_HEIGHT);
    cpop->borderht = (int)xv_get(cpop->frame, XV_HEIGHT) - cpop->default_rows * cpop->rowht;
    disable_frame_resize(cpop->frame, TRUE);
  }
  if (!(int)xv_get(cpop->frame, XV_SHOW))
    center_frame(frame, cpop->frame);
  sel_st_update(TRUE);
  xv_set(cpop->list, PANEL_LIST_SELECT, hotlist_get_current(), TRUE, NULL);
  xv_set(cpop->frame, XV_SHOW, TRUE, NULL);
  position_popups();
}


static void
sel_st_apply(int selected) {
  if (selected != -1) {
    new_station(selected);
    hotlist_put_queue();
    station_selected = TRUE;
  }
}


static void
sel_st_cb_proc(Panel_item item, const Event *event) {
  int selected;
  
  selected = (int)xv_get(popups[POPUP_SELST].list, PANEL_LIST_FIRST_SELECTED);
  switch ((int)xv_get(item, PANEL_CLIENT_DATA)) {
    case APPLY_BUTTON:
      sel_st_apply(selected);
    break;
    case CHGNAME_BUTTON:
      if (selected != -1) {
        station_name_proc(selected);
      }
    break;
    case CLOSE_BUTTON:
      popup_frame_done(popups[POPUP_SELST].frame);
    break;
    default:
      assert(0);
    break;
  }
}


void
sel_st_update(int update_sel) {
  static char **station_list = NULL;
  static int station_count = 0;
  int entry, selected = -1;
  
  if (!popups[POPUP_SELST].frame)
    return;
  if (station_list) {
    selected = (int)xv_get(popups[POPUP_SELST].list, PANEL_LIST_FIRST_SELECTED);
    xv_set(popups[POPUP_SELST].list, PANEL_LIST_DELETE_ROWS, 0, station_count, NULL);
    free(station_list);
    station_list = NULL;
  }
  station_count = hotlist_get_stcount();
  station_list = smalloc((station_count + 1) * sizeof(char*));
  station_list[station_count] = NULL;
  for (entry = 0; entry < station_count; entry++) {
    station_list[entry] = hotlist_get_name(entry);
  }
  xv_set(popups[POPUP_SELST].list, PANEL_LIST_INSERT_STRINGS, 0, station_list, NULL);
  if (update_sel) {
    xv_set(popups[POPUP_SELST].list, PANEL_LIST_SELECT, hotlist_get_current(), TRUE, NULL);
  } else if (selected != -1) {
    xv_set(popups[POPUP_SELST].list, PANEL_LIST_SELECT, selected, TRUE, NULL);
  }
}


void
hotlist_proc(void) {
  Panel hot_panel;
  popups_t *cpop = popups + POPUP_HOTLIST;
  
  if (!cpop->frame) {
    cpop->frame = (Frame)xv_create(frame, FRAME,
        XV_LABEL, "Hotlist",
        WIN_CONSUME_EVENTS,
          WIN_STRUCTURE_NOTIFY, WIN_PROPERTY_NOTIFY, NULL,
        WIN_EVENT_PROC, popup_frame_proc,
        FRAME_DONE_PROC, popup_frame_done,
        NULL);
    hot_panel = (Panel)xv_create(cpop->frame, PANEL, NULL);
    cpop->list = (Panel_item)xv_create(hot_panel, PANEL_LIST,
        XV_X, 15,
        XV_Y, 15,
        PANEL_LIST_WIDTH, 55,
        PANEL_LIST_DISPLAY_ROWS, cpop->default_rows,
        PANEL_READ_ONLY, TRUE,
        PANEL_LIST_DO_DBL_CLICK, TRUE,
        PANEL_NOTIFY_PROC, list_dbl_click,
        NULL);
    xv_create(hot_panel, PANEL_BUTTON,
        XV_X, 110,
        XV_Y, 15,
        PANEL_LABEL_STRING, "     Go to",
        PANEL_LABEL_WIDTH, 80,
        PANEL_NOTIFY_PROC, hot_cb_proc,
        PANEL_CLIENT_DATA, GOTO_BUTTON,
        NULL);
    xv_create(hot_panel, PANEL_BUTTON,
        XV_X, 110,
        XV_Y, 50,
        PANEL_LABEL_STRING, "   Move up",
        PANEL_LABEL_WIDTH, 80,
        PANEL_NOTIFY_PROC, hot_cb_proc,
        PANEL_CLIENT_DATA, UP_BUTTON,
        NULL);
    xv_create(hot_panel, PANEL_BUTTON,
        XV_X, 110,
        XV_Y, 80,
        PANEL_LABEL_STRING, " Move down",
        PANEL_LABEL_WIDTH, 80,
        PANEL_NOTIFY_PROC, hot_cb_proc,
        PANEL_CLIENT_DATA, DOWN_BUTTON,
        NULL);
    xv_create(hot_panel, PANEL_BUTTON,
        XV_X, 110,
        XV_Y, 110,
        PANEL_LABEL_STRING, "Add current",
        PANEL_LABEL_WIDTH, 80,
        PANEL_NOTIFY_PROC, hot_cb_proc,
        PANEL_CLIENT_DATA, ADD_BUTTON,
        NULL);
    xv_create(hot_panel, PANEL_BUTTON,
        XV_X, 110,
        XV_Y, 140,
        PANEL_LABEL_STRING, "    Remove",
        PANEL_LABEL_WIDTH, 80,
        PANEL_NOTIFY_PROC, hot_cb_proc,
        PANEL_CLIENT_DATA, REMOVE_BUTTON,
        NULL);
    xv_create(hot_panel, PANEL_BUTTON,
        XV_X, 110,
        XV_Y, 175,
        PANEL_LABEL_STRING, "      Close",
        PANEL_LABEL_WIDTH, 80,
        PANEL_NOTIFY_PROC, hot_cb_proc,
        PANEL_CLIENT_DATA, CLOSE_BUTTON,
        NULL);
    window_fit(hot_panel);
    window_fit(cpop->frame);
    cpop->rowht = (int)xv_get(cpop->list, PANEL_LIST_ROW_HEIGHT);
    cpop->borderht = (int)xv_get(cpop->frame, XV_HEIGHT) - cpop->default_rows * cpop->rowht;
    disable_frame_resize(cpop->frame, TRUE);
  }
  if (!(int)xv_get(cpop->frame, XV_SHOW))
    center_frame(frame, cpop->frame);
  hot_update();
  xv_set(cpop->frame, XV_SHOW, TRUE, NULL);
  position_popups();
}


static void
hotlist_goto(int selected) {
  if (selected != -1) {
    get_page(PRI_FGROUND, hotlist_get(selected), 0, 0, vtx_buffer, &vtxwin);
  }
}


static void
hot_cb_proc(Panel_item item, const Event *event) {
  int selected;
  
  selected = (int)xv_get(popups[POPUP_HOTLIST].list, PANEL_LIST_FIRST_SELECTED);
  switch ((int)xv_get(item, PANEL_CLIENT_DATA)) {
    case GOTO_BUTTON:
      hotlist_goto(selected);
    break;
    case UP_BUTTON:
      if (selected > 0) {
        xv_set(popups[POPUP_HOTLIST].list, PANEL_LIST_SELECT, selected, FALSE, NULL);
        hotlist_add(hotlist_remove(selected), selected - 1);
        xv_set(popups[POPUP_HOTLIST].list, PANEL_LIST_SELECT, selected - 1, TRUE, NULL);
        hot_update();
      }
    break;
    case DOWN_BUTTON:
      if (selected != -1) {
        xv_set(popups[POPUP_HOTLIST].list, PANEL_LIST_SELECT, selected, FALSE, NULL);
        hotlist_add(hotlist_remove(selected), selected + 1);
        xv_set(popups[POPUP_HOTLIST].list, PANEL_LIST_SELECT, selected + 1, TRUE, NULL);
        hot_update();
      }
    break;
    case ADD_BUTTON:
      if (selected == -1)
        selected = 0;
      xv_set(popups[POPUP_HOTLIST].list, PANEL_LIST_SELECT, selected, FALSE, NULL);
      hotlist_add(vtxwin.pgnum, selected + 1);
      hot_update();
      xv_set(popups[POPUP_HOTLIST].list, PANEL_LIST_SELECT, selected + 1, TRUE, NULL);
    break;
    case REMOVE_BUTTON:
      if (selected != -1) {
        hotlist_remove(selected);
        if (selected >= hotlist_get_count(hotlist_get_current()))
          xv_set(popups[POPUP_HOTLIST].list, PANEL_LIST_SELECT, selected - 1, TRUE, NULL);
        hot_update();
      }
    break;
    case CLOSE_BUTTON:
      popup_frame_done(popups[POPUP_HOTLIST].frame);
    break;
    default:
      assert(0);
    break;
  }
}


void
hot_update(void) {
  static int hot_list_count;
  int selected = -1;
  
  if (!popups[POPUP_HOTLIST].list)
    return;
  if (hot_list_count) {
    selected = (int)xv_get(popups[POPUP_HOTLIST].list, PANEL_LIST_FIRST_SELECTED);
    xv_set(popups[POPUP_HOTLIST].list, PANEL_LIST_DELETE_ROWS, 0, hot_list_count, NULL);
  }
  xv_set(popups[POPUP_HOTLIST].list,
      PANEL_LIST_INSERT_STRINGS, 0, hotlist_mkstr(&hot_list_count), NULL);
  if (selected != -1)
    xv_set(popups[POPUP_HOTLIST].list, PANEL_LIST_SELECT, selected, TRUE, NULL);
}


void
history_proc(void) {
  static int init_done;
  popups_t *cpop = popups + POPUP_HISTORY;

  if (!init_done) {
    init_done = TRUE;
    xv_set(cpop->frame,
        WIN_CONSUME_EVENTS,
          WIN_STRUCTURE_NOTIFY, WIN_PROPERTY_NOTIFY, NULL,
        WIN_EVENT_PROC, popup_frame_proc,
        FRAME_DONE_PROC, popup_frame_done,
        NULL);
    xv_set(cpop->list,
        XV_X, 15,
        XV_Y, 15,
        PANEL_LIST_WIDTH, 75,
        PANEL_LIST_DISPLAY_ROWS, cpop->default_rows,
        PANEL_LIST_DO_DBL_CLICK, TRUE,
        PANEL_NOTIFY_PROC, list_dbl_click,
        NULL);
    xv_create(hist_panel, PANEL_BUTTON,
        XV_X, 130,
        XV_Y, 15,
        PANEL_LABEL_STRING, "  Go to",
        PANEL_LABEL_WIDTH, 55,
        PANEL_NOTIFY_PROC, hist_cb_proc,
        PANEL_CLIENT_DATA, GOTO_BUTTON,
        NULL);
    xv_create(hist_panel, PANEL_BUTTON,
        XV_X, 130,
        XV_Y, 45,
        PANEL_LABEL_STRING, "Previous",
        PANEL_LABEL_WIDTH, 55,
        PANEL_NOTIFY_PROC, hist_cb_proc,
        PANEL_CLIENT_DATA, PREV_BUTTON,
        NULL);
    xv_create(hist_panel, PANEL_BUTTON,
        XV_X, 130,
        XV_Y, 75,
        PANEL_LABEL_STRING, "   Next",
        PANEL_LABEL_WIDTH, 55,
        PANEL_NOTIFY_PROC, hist_cb_proc,
        PANEL_CLIENT_DATA, NEXT_BUTTON,
        NULL);
    xv_create(hist_panel, PANEL_BUTTON,
        XV_X, 130,
        XV_Y, 115,
        PANEL_LABEL_STRING, "Index pg",
        PANEL_LABEL_WIDTH, 55,
        PANEL_NOTIFY_PROC, hist_cb_proc,
        PANEL_CLIENT_DATA, INDEX_BUTTON,
        NULL);
    xv_create(hist_panel, PANEL_BUTTON,
        XV_X, 130,
        XV_Y, 175,
        PANEL_LABEL_STRING, "  Close",
        PANEL_LABEL_WIDTH, 55,
        PANEL_NOTIFY_PROC, hist_cb_proc,
        PANEL_CLIENT_DATA, CLOSE_BUTTON,
        NULL);
    window_fit(hist_panel);
    window_fit(cpop->frame);
    cpop->rowht = (int)xv_get(cpop->list, PANEL_LIST_ROW_HEIGHT);
    cpop->borderht = (int)xv_get(cpop->frame, XV_HEIGHT) - cpop->default_rows * cpop->rowht;
    disable_frame_resize(cpop->frame, TRUE);
  }
  if (!(int)xv_get(cpop->frame, XV_SHOW))
    center_frame(frame, cpop->frame);
  xv_set(cpop->frame, XV_SHOW, TRUE, NULL);
  position_popups();
}


void
history_move(int direction) {
  int selected;
  
  if (!popups[POPUP_HISTORY].frame)
    return;
  if ((selected = (int)xv_get(popups[POPUP_HISTORY].list, PANEL_LIST_FIRST_SELECTED)) != -1) {
    xv_set(popups[POPUP_HISTORY].list, PANEL_LIST_SELECT, selected, FALSE, NULL);
    selected += direction;
    xv_set(popups[POPUP_HISTORY].list, PANEL_LIST_SELECT, selected, TRUE, NULL);
    selected = (int)xv_get(popups[POPUP_HISTORY].list, PANEL_LIST_FIRST_SELECTED);
    if (selected != -1) {
      get_page(PRI_FGROUND_NH, strtol((char*)xv_get(popups[POPUP_HISTORY].list,
          PANEL_LIST_STRING, selected), NULL, 16), 0, 0, vtx_buffer, &vtxwin);
    }
  }
}


static void
history_goto(int selected) {
  if (selected != -1) {
    get_page(PRI_FGROUND_NH,
        strtol((char*)xv_get(popups[POPUP_HISTORY].list, PANEL_LIST_STRING, selected),
        NULL, 16), 0, 0, vtx_buffer, &vtxwin);
  }
}


static void
hist_cb_proc(Panel_item item, const Event *event) {
  switch ((int)xv_get(item, PANEL_CLIENT_DATA)) {
    case GOTO_BUTTON:
      history_goto((int)xv_get(popups[POPUP_HISTORY].list, PANEL_LIST_FIRST_SELECTED));
    break;
    case PREV_BUTTON:
      history_move(-1);
    break;
    case NEXT_BUTTON:
      history_move(+1);
    break;
    case INDEX_BUTTON:
      get_page(PRI_FGROUND, hotlist_get_index_page(), 0, 0, vtx_buffer, &vtxwin);
    break;
    case CLOSE_BUTTON:
      popup_frame_done(popups[POPUP_HISTORY].frame);
    break;
    default:
      assert(0);
    break;
  }
}


void
toptext_proc(void) {
  popups_t *cpop = popups + POPUP_TOPTEXT;
  Panel toptext_panel;
    
  if (!cpop->frame) {
    ov_menu = (Menu)xv_create(XVNULL, MENU,
        MENU_GEN_PIN_WINDOW, frame, "TOP-Text overview",
        MENU_NOTIFY_PROC, toptext_menu_cb_proc,
        NULL);
    hier_menu = (Menu)xv_create(XVNULL, MENU,
        MENU_GEN_PIN_WINDOW, frame, "TOP-Text hierarchy",
        MENU_NOTIFY_PROC, toptext_menu_cb_proc,
        NULL);
    cpop->frame = (Frame)xv_create(frame, FRAME,
        XV_LABEL, "TOP-Text",
        WIN_CONSUME_EVENTS,
          WIN_STRUCTURE_NOTIFY, WIN_PROPERTY_NOTIFY, NULL,
        WIN_EVENT_PROC, popup_frame_proc,
        FRAME_DONE_PROC, popup_frame_done,
        NULL);
    toptext_panel = (Panel)xv_create(cpop->frame, PANEL,
        XV_WIDTH, 350,
        XV_HEIGHT, 185,
        NULL);
    ov_menubutton = (Panel_item)xv_create(toptext_panel, PANEL_BUTTON,
        XV_X, 15,
        XV_Y, 15,
        PANEL_LABEL_STRING, "Overview",
        PANEL_ITEM_MENU, ov_menu,
        NULL);
    hier_menubutton = (Panel_item)xv_create(toptext_panel, PANEL_BUTTON,
        XV_X, 125,
        XV_Y, 15,
        PANEL_LABEL_STRING, "Hierarchy",
        PANEL_ITEM_MENU, hier_menu,
        NULL);
    xv_create(toptext_panel, PANEL_BUTTON,
        XV_X, 285,
        XV_Y, 15,
        PANEL_LABEL_STRING, "Close",
        PANEL_NOTIFY_PROC, toptext_cb_proc,
        PANEL_CLIENT_DATA, CLOSE_BUTTON,
        NULL);
    xv_create(toptext_panel, PANEL_MESSAGE,
        XV_X, 15,
        XV_Y, 50,
        PANEL_LABEL_BOLD, TRUE,
        PANEL_LABEL_STRING, "Current page:",
        NULL);
    tt_buttons[0] = (Panel_item)xv_create(toptext_panel, PANEL_BUTTON,
        XV_X, 15,
        XV_Y, 72,
        PANEL_LABEL_STRING, "Next page:",
        PANEL_NOTIFY_PROC, toptext_cb_proc,
        PANEL_CLIENT_DATA, NEXT_BUTTON,
        NULL);
    tt_buttons[1] = (Panel_item)xv_create(toptext_panel, PANEL_BUTTON,
        XV_X, 15,
        XV_Y, 97,
        PANEL_LABEL_STRING, "Prev. page:",
        PANEL_NOTIFY_PROC, toptext_cb_proc,
        PANEL_CLIENT_DATA, PREV_BUTTON,
        NULL);
    tt_buttons[2] = (Panel_item)xv_create(toptext_panel, PANEL_BUTTON,
        XV_X, 15,
        XV_Y, 122,
        PANEL_LABEL_STRING, "Next group:",
        PANEL_NOTIFY_PROC, toptext_cb_proc,
        PANEL_CLIENT_DATA, NEXTGRP_BUTTON,
        NULL);
    tt_buttons[3] = (Panel_item)xv_create(toptext_panel, PANEL_BUTTON,
        XV_X, 15,
        XV_Y, 147,
        PANEL_LABEL_STRING, "Next block:",
        PANEL_NOTIFY_PROC, toptext_cb_proc,
        PANEL_CLIENT_DATA, NEXTBLK_BUTTON,
        NULL);
    top_msgs[0] = (Panel_item)xv_create(toptext_panel, PANEL_MESSAGE,
        XV_X, 120,
        XV_Y, 50,
        PANEL_LABEL_STRING, "",
        NULL);
    top_msgs[1] = (Panel_item)xv_create(toptext_panel, PANEL_MESSAGE,
        XV_X, 120,
        XV_Y, 75,
        PANEL_LABEL_STRING, "",
        PANEL_CLIENT_DATA, 0,
        NULL);
    top_msgs[2] = (Panel_item)xv_create(toptext_panel, PANEL_MESSAGE,
        XV_X, 120,
        XV_Y, 100,
        PANEL_LABEL_STRING, "",
        PANEL_CLIENT_DATA, 0,
        NULL);
    top_msgs[3] = (Panel_item)xv_create(toptext_panel, PANEL_MESSAGE,
        XV_X, 120,
        XV_Y, 125,
        PANEL_LABEL_STRING, "",
        PANEL_CLIENT_DATA, 0,
        NULL);
    top_msgs[4] = (Panel_item)xv_create(toptext_panel, PANEL_MESSAGE,
        XV_X, 120,
        XV_Y, 150,
        PANEL_LABEL_STRING, "",
        PANEL_CLIENT_DATA, 0,
        NULL);
    window_fit(cpop->frame);
    disable_frame_resize(cpop->frame, FALSE);
  }
  if (!(int)xv_get(cpop->frame, XV_SHOW))
    center_frame(frame, cpop->frame);
  pgdesc_update(-1);
  topmenu_update(tt_desctable, tt_desccount);
  xv_set(cpop->frame, XV_SHOW, TRUE, NULL);
  position_popups();
}


void
toptext_go_nextpage(void) {
  int page;
  
  if (!popups[POPUP_TOPTEXT].frame)
    return;
  if ((page = (int)xv_get(top_msgs[1], PANEL_CLIENT_DATA))) {
    get_page(PRI_FGROUND_NH, page, 0, 0, vtx_buffer, &vtxwin);
  }
}


void
toptext_go_prevpage(void) {
  int page;
  
  if (!popups[POPUP_TOPTEXT].frame)
    return;
  if ((page = (int)xv_get(top_msgs[2], PANEL_CLIENT_DATA))) {
    get_page(PRI_FGROUND_NH, page, 0, 0, vtx_buffer, &vtxwin);
  }
}


void
toptext_go_nextgrp(void) {
  int page;
  
  if (!popups[POPUP_TOPTEXT].frame)
    return;
  if ((page = (int)xv_get(top_msgs[3], PANEL_CLIENT_DATA))) {
    get_page(PRI_FGROUND_NH, page, 0, 0, vtx_buffer, &vtxwin);
  }
}


void
toptext_go_nextblk(void) {
  int page;
  
  if (!popups[POPUP_TOPTEXT].frame)
    return;
  if ((page = (int)xv_get(top_msgs[4], PANEL_CLIENT_DATA))) {
    get_page(PRI_FGROUND_NH, page, 0, 0, vtx_buffer, &vtxwin);
  }
}


static void
toptext_cb_proc(Panel_item item, const Event *event) {
  switch ((int)xv_get(item, PANEL_CLIENT_DATA)) {
    case NEXT_BUTTON:
      toptext_go_nextpage();
    break;
    case PREV_BUTTON:
      toptext_go_prevpage();
    break;
    case NEXTGRP_BUTTON:
      toptext_go_nextgrp();
    break;
    case NEXTBLK_BUTTON:
      toptext_go_nextblk();
    break;
    case CLOSE_BUTTON:
      popup_frame_done(popups[POPUP_TOPTEXT].frame);
    break;
    default:
      assert(0);
    break;
  }
}


static void
toptext_menu_cb_proc(Menu menu, Menu_item menu_item) {
  int page;

  if ((page = (int)xv_get(menu_item, MENU_CLIENT_DATA))) {
    get_page(PRI_FGROUND, page, 0, 0, vtx_buffer, &vtxwin);
  }
}


void
toptext_ok_notify(void) {
  ttdesc_t *next;

  if (popups[POPUP_TOPTEXT].frame) {
    int item;
    Frame pin_frame;
    Menu menu;

    /* We have to remove the menu from the PANEL_BUTTON before destroying it, because XView is way
     * too clever to allow us to destroy a menu that is still attached to something. Of course,
     * this isn't mentioned anywhere in the O'Reilly manuals. Also, this sort of "consistency-
     * checking" is complete nonsense and won't prevent you from shooting yourself into the foot,
     * but now I know at least why XView is that bloated and awfully slow. Guess how much I
     * *love* XView and SunSoft Inc.
     */
    xv_set(ov_menubutton, PANEL_ITEM_MENU, NULL, NULL);
    pin_frame = (Frame)xv_get(ov_menu, MENU_PIN_WINDOW);
    assert(!xv_destroy(ov_menu));
    if (pin_frame) {
      assert(!xv_destroy(pin_frame));
    }
    ov_menu = (Menu)xv_create(XVNULL, MENU,
        MENU_GEN_PIN_WINDOW, frame, "TOP-Text overview",
        MENU_NOTIFY_PROC, toptext_menu_cb_proc,
        NULL);
    xv_set(ov_menubutton, PANEL_ITEM_MENU, ov_menu, NULL);

    xv_set(hier_menubutton, PANEL_ITEM_MENU, NULL, NULL);
    pin_frame = (Frame)xv_get(hier_menu, MENU_PIN_WINDOW);
    for (item = (int)xv_get(hier_menu, MENU_NITEMS); item >= 1; item--) {
      if ((menu =
          (Menu)xv_get((Menu_item)xv_get(hier_menu, MENU_NTH_ITEM, item), MENU_PULLRIGHT))) {
        xv_set((Menu_item)xv_get(hier_menu, MENU_NTH_ITEM, item), MENU_PULLRIGHT, NULL, NULL);
        assert(!xv_destroy(menu));
      }
    }
    assert(!xv_destroy(hier_menu));
    if (pin_frame) {
      assert(!xv_destroy(pin_frame));
    }
    hier_menu = (Menu)xv_create(XVNULL, MENU,
        MENU_GEN_PIN_WINDOW, frame, "TOP-Text hierarchy",
        MENU_NOTIFY_PROC, toptext_menu_cb_proc,
        NULL);
    xv_set(hier_menubutton, PANEL_ITEM_MENU, hier_menu, NULL);
  }
  while (tt_desctable) {
    next = tt_desctable->next;
    free(tt_desctable);
    tt_desctable = next;
  }
  if ((tt_desctable = toptext_mkdesctable(&tt_desccount))) {
    if (popups[POPUP_TOPTEXT].frame) {
      topmenu_update(tt_desctable, tt_desccount);
    }
  }
  if (!toptext_when_active)
    return;
  if (toptext_ok) {
    if (!popups[POPUP_TOPTEXT].frame) {
      toptext_proc();
    } else {
      xv_set(popups[POPUP_TOPTEXT].frame, XV_SHOW, TRUE, NULL);
      position_popups();
    }
  } else if (popups[POPUP_TOPTEXT].frame) {
    popup_frame_done(popups[POPUP_TOPTEXT].frame);
  }
  pgdesc_update(-1);
}


void
pgdesc_update(int page) {
  char *text;
  static int lastpage = 0x100;
  int msg, button;
  
  if (page < 0) {
    page = lastpage;
  }
  lastpage = page;
  if (!popups[POPUP_TOPTEXT].frame)
    return;
  xv_set(ov_menubutton, PANEL_INACTIVE, !toptext_ok, NULL);
  xv_set(hier_menubutton, PANEL_INACTIVE, !toptext_ok, NULL);
  for (button = 0; button <= 3; button++) {
    xv_set(tt_buttons[button], PANEL_INACTIVE, !toptext_ok, NULL);
  }
  if ((text = toptext_pgdesc(page))) {
    xv_set(top_msgs[0], PANEL_LABEL_STRING, text, NULL);
  } else {
    xv_set(top_msgs[0], PANEL_LABEL_STRING, "No TOP-Text active\n", NULL);
    for (msg = 1; msg <= 4; msg++) {
      xv_set(top_msgs[msg], 
          PANEL_LABEL_STRING, "",
          PANEL_CLIENT_DATA, 0,
          NULL);
    }
    return;
  }
  if (!(page = toptext_pginc(lastpage))) {
    page = lastpage;
  }
  xv_set(top_msgs[1],
      PANEL_LABEL_STRING, toptext_pgdesc(page),
      PANEL_CLIENT_DATA, page,
      NULL);
  if (!(page = toptext_pgdec(lastpage))) {
    page = lastpage;
  }
  xv_set(top_msgs[2],
      PANEL_LABEL_STRING, toptext_pgdesc(page),
      PANEL_CLIENT_DATA, page,
      NULL);
  if (!(page = toptext_nextgrppg(lastpage))) {
    page = lastpage;
  }
  xv_set(top_msgs[3],
      PANEL_LABEL_STRING, toptext_pgdesc(page),
      PANEL_CLIENT_DATA, page,
      NULL);
  if (!(page = toptext_nextblkpg(lastpage))) {
    page = lastpage;
  }
  xv_set(top_msgs[4],
      PANEL_LABEL_STRING, toptext_pgdesc(page),
      PANEL_CLIENT_DATA, page,
      NULL);
}


static void
topmenu_update(const ttdesc_t *desctable, int desccount) {
  const ttdesc_t *desc;
  
  if (desctable) {
    /* Create overview-/hierarchy-menu
     */
    for (desc = desctable; desc; desc = desc->next) {
      xv_set(ov_menu, MENU_APPEND_ITEM,
          (Menu_item)xv_create(XVNULL, MENUITEM,
              MENU_STRING, desc->fullname,
              MENU_CLIENT_DATA, desc->page,
              MENU_RELEASE,
              NULL),
          NULL);
      if (desc->is_block) {
        if (desc->next && !desc->next->is_block) {
          /* If page is a blockpage followed by other non-blockpages, create pullright-menu & add
           * blockpage as first entry
           */
          xv_set(hier_menu, MENU_APPEND_ITEM,
              (Menu_item)xv_create(XVNULL, MENUITEM,
                  MENU_STRING, desc->pagename,
                  MENU_PULLRIGHT, (Menu)xv_create(XVNULL, MENU,
                      MENU_NOTIFY_PROC, toptext_menu_cb_proc,
                      MENU_ITEM,
                          MENU_STRING, desc->fullname,
                          MENU_CLIENT_DATA, desc->page,
                          MENU_RELEASE,
                          NULL,
                      NULL),
                  MENU_RELEASE,
                  NULL),
              NULL);
        } else {
          /* If page is a blockpage followed by another blockpage, there's no need for a pullright-
           * menu, so just insert it into the toplevel-menu
           */
          xv_set(hier_menu, MENU_APPEND_ITEM,
              (Menu_item)xv_create(XVNULL, MENUITEM,
                  MENU_STRING, desc->fullname,
                  MENU_CLIENT_DATA, desc->page,
                  MENU_RELEASE,
                  NULL),
              NULL);
        }
      } else {
        /* If page is a non-blockpage, insert it into the last pullright-menu or into the toplevel-
         * menu, if no pullright-menu has been created up to now
         */
        Menu_item item;
        
        item = (Menu_item)xv_get(hier_menu, MENU_NTH_ITEM, (int)xv_get(hier_menu, MENU_NITEMS));
        if ((Menu)xv_get(item, MENU_PULLRIGHT)) {
          xv_set((Menu)xv_get(item, MENU_PULLRIGHT), MENU_APPEND_ITEM,
              (Menu_item)xv_create(XVNULL, MENUITEM,
                  MENU_STRING, desc->pagename,
                  MENU_CLIENT_DATA, desc->page,
                  MENU_RELEASE,
                  NULL),
              NULL);
        } else {
          xv_set(hier_menu, MENU_APPEND_ITEM,
              (Menu_item)xv_create(XVNULL, MENUITEM,
                  MENU_STRING, desc->pagename,
                  MENU_CLIENT_DATA, desc->page,
                  MENU_RELEASE,
                  NULL),
              NULL);
        }
      }
    }
    /* Try to determine the number of rows/columns so that the height of the popup-menu equals
     * approxmiately its width. The widht and height of a single entry are about 250 and 20 pixels.
     */
    xv_set(ov_menu, MENU_NCOLS, (int)ceil(sqrt(20.0 / 250.0 * desccount)), NULL);
  } else {
    xv_set(ov_menu, MENU_APPEND_ITEM,
        (Menu_item)xv_create(XVNULL, MENUITEM,
            MENU_STRING, "No pages available",
            MENU_CLIENT_DATA, 0,
            MENU_RELEASE,
            NULL),
        NULL);
    xv_set(hier_menu, MENU_APPEND_ITEM,
        (Menu_item)xv_create(XVNULL, MENUITEM,
            MENU_STRING, "No pages available",
            MENU_CLIENT_DATA, 0,
            MENU_RELEASE,
            NULL),
        NULL);
  }
}


static Frame layout_frame;
static Panel_item lay_hot_choice, lay_top_choice, lay_hist_choice, lay_stat_choice,
    lay_visible_toggle, lay_top_checkmark;

void
layout_proc(void) {
  int popup, val = 0;
  Panel layout_panel;

  if (!layout_frame) {
    layout_frame = (Frame)xv_create(frame, FRAME,
        XV_LABEL, "Window layout",
        NULL);
    layout_panel = (Panel)xv_create(layout_frame, PANEL, NULL);
    xv_create(layout_panel, PANEL_MESSAGE,
        XV_X, 170,
        XV_Y, 15,
        PANEL_LABEL_STRING, "Popup-window placement",
        PANEL_LABEL_BOLD, TRUE,
        NULL);
    lay_hist_choice = (Panel_item)xv_create(layout_panel, PANEL_CHOICE,
        XV_X, 75,
        XV_Y, 40,
        PANEL_LABEL_STRING, "Page history",
        PANEL_CHOICE_STRINGS, "Centered", "Left", "Right", "Top", "Bottom", NULL,
        NULL);
    lay_top_choice = (Panel_item)xv_create(layout_panel, PANEL_CHOICE,
        XV_X, 95,
        XV_Y, 80,
        PANEL_LABEL_STRING, "TOP-Text",
        PANEL_CHOICE_STRINGS, "Centered", "Left", "Right", "Top", "Bottom", NULL,
        NULL);
    lay_hot_choice = (Panel_item)xv_create(layout_panel, PANEL_CHOICE,
        XV_X, 114,
        XV_Y, 120,
        PANEL_LABEL_STRING, "Hotlist",
        PANEL_CHOICE_STRINGS, "Centered", "Left", "Right", "Top", "Bottom", NULL,
        NULL);
    lay_stat_choice = (Panel_item)xv_create(layout_panel, PANEL_CHOICE,
        XV_X, 85,
        XV_Y, 160,
        PANEL_LABEL_STRING, "Station list",
        PANEL_CHOICE_STRINGS, "Centered", "Left", "Right", "Top", "Bottom", NULL,
        NULL);
    lay_visible_toggle = (Panel_item)xv_create(layout_panel, PANEL_TOGGLE,
        XV_X, 15,
        XV_Y, 210,
        PANEL_LABEL_STRING, "Visible on startup",
        PANEL_CHOICE_STRINGS, "Page history", "TOP-Text", "Hotlist", "Station list", NULL,
        NULL);
    lay_top_checkmark = (Panel_item)xv_create(layout_panel, PANEL_CHECK_BOX,
        XV_X, 147,
        XV_Y, 245,
        PANEL_CHOICE_STRINGS, "Show TOP-Text when active", NULL,
        NULL);
    xv_create(layout_panel, PANEL_BUTTON,
        XV_X, 200,
        XV_Y, 295,
        PANEL_LABEL_STRING, "Apply",
        PANEL_NOTIFY_PROC, layout_cb_proc,
        PANEL_CLIENT_DATA, APPLY_BUTTON,
        NULL);
    xv_create(layout_panel, PANEL_BUTTON,
        XV_X, 260,
        XV_Y, 295,
        PANEL_LABEL_STRING, "Close",
        PANEL_NOTIFY_PROC, layout_cb_proc,
        PANEL_CLIENT_DATA, CLOSE_BUTTON,
        NULL);
    window_fit(layout_panel);
    window_fit(layout_frame);
    disable_frame_resize(layout_frame, FALSE);
  }
  if (!(int)xv_get(layout_frame, XV_SHOW))
    center_frame(frame, layout_frame);
  xv_set(lay_hot_choice, PANEL_VALUE, popups[POPUP_HOTLIST].placement, NULL);
  xv_set(lay_top_choice, PANEL_VALUE, popups[POPUP_TOPTEXT].placement, NULL);
  xv_set(lay_hist_choice, PANEL_VALUE, popups[POPUP_HISTORY].placement, NULL);
  xv_set(lay_stat_choice, PANEL_VALUE, popups[POPUP_SELST].placement, NULL);
  for (popup = 0; popup < POPUP_COUNT; popup++) {
    val |= (popups[popup].startup ? (1 << popup) : 0);
  }
  xv_set(lay_visible_toggle, PANEL_VALUE, val, NULL);
  xv_set(lay_top_checkmark, PANEL_VALUE, toptext_when_active, NULL);
  xv_set(layout_frame, XV_SHOW, TRUE, NULL);
}


static void
layout_cb_proc(Panel_item item, const Event *event) {
  int popup;

  switch ((int)xv_get(item, PANEL_CLIENT_DATA)) {
    case APPLY_BUTTON:
      popups[POPUP_HOTLIST].placement = (int)xv_get(lay_hot_choice, PANEL_VALUE);
      popups[POPUP_TOPTEXT].placement = (int)xv_get(lay_top_choice, PANEL_VALUE);
      popups[POPUP_HISTORY].placement = (int)xv_get(lay_hist_choice, PANEL_VALUE);
      popups[POPUP_SELST].placement = (int)xv_get(lay_stat_choice, PANEL_VALUE);
      for (popup = 0; popup < POPUP_COUNT; popup++) {
        popups[popup].startup = (((int)xv_get(lay_visible_toggle, PANEL_VALUE) & (1 << popup)) ?
            TRUE : FALSE);
      }
      toptext_when_active = (int)xv_get(lay_top_checkmark, PANEL_VALUE) & 1;
      toptext_ok_notify();
      position_popups();
    break;
    case CLOSE_BUTTON:
      xv_set(layout_frame, XV_SHOW, FALSE, NULL);
    break;
    default:
      assert(0);
    break;
  }
}


static Frame tvopt_frame;
static Panel_item tvopt_disp_checkbox, tvopt_int_checkbox, tvopt_mode_choice;

static void
tvopt_activate(void) {
  xv_set(tvopt_int_checkbox, PANEL_INACTIVE, !(int)xv_get(tvopt_disp_checkbox, PANEL_VALUE), NULL);
  xv_set(tvopt_mode_choice, PANEL_INACTIVE, !(int)xv_get(tvopt_disp_checkbox, PANEL_VALUE), NULL);
}


void
tvopt_proc(void) {
  Panel tvopt_panel;
  
  if (!tvopt_frame) {
    tvopt_frame = (Frame)xv_create(frame, FRAME,
        XV_LABEL, "TV options",
        NULL);
    tvopt_panel = (Panel)xv_create(tvopt_frame, PANEL, NULL);
    tvopt_disp_checkbox = (Panel_item)xv_create(tvopt_panel, PANEL_CHECK_BOX,
        XV_X, 55,
        XV_Y, 20,
        PANEL_CHOICE_STRINGS, "Display videotext-pages on TV", NULL,
        PANEL_NOTIFY_PROC, tvopt_activate,
        NULL);
    tvopt_int_checkbox = (Panel_item)xv_create(tvopt_panel, PANEL_CHECK_BOX,
        XV_X, 55,
        XV_Y, 55,
        PANEL_CHOICE_STRINGS, "Always use interlace", NULL,
        NULL);
    tvopt_mode_choice = (Panel_item)xv_create(tvopt_panel, PANEL_CHOICE,
        XV_X, 15,
        XV_Y, 95,
        PANEL_LABEL_STRING, "Display mode",
		/* The order of these strings must be the same as in enum vtxdisp_t (vtx.h) */
        PANEL_CHOICE_STRINGS, "Normal", "Transparent", "Insert", NULL,
        NULL);
    xv_create(tvopt_panel, PANEL_BUTTON,
        XV_X, 115,
        XV_Y, 145,
        PANEL_LABEL_STRING, "Apply",
        PANEL_NOTIFY_PROC, tvopt_cb_proc,
        PANEL_CLIENT_DATA, APPLY_BUTTON,
        NULL);
    xv_create(tvopt_panel, PANEL_BUTTON,
        XV_X, 175,
        XV_Y, 145,
        PANEL_LABEL_STRING, "Close",
        PANEL_NOTIFY_PROC, tvopt_cb_proc,
        PANEL_CLIENT_DATA, CLOSE_BUTTON,
        NULL);
    window_fit(tvopt_panel);
    window_fit(tvopt_frame);
    disable_frame_resize(tvopt_frame, FALSE);
  }
  if (!(int)xv_get(tvopt_frame, XV_SHOW))
    center_frame(frame, tvopt_frame);
  xv_set(tvopt_disp_checkbox, PANEL_VALUE, display_on_tv, NULL);
  xv_set(tvopt_int_checkbox, PANEL_VALUE, use_interlace, NULL);
  xv_set(tvopt_mode_choice, PANEL_VALUE, display_mode - 1, NULL);
  tvopt_activate();
  xv_set(tvopt_frame, XV_SHOW, TRUE, NULL);
}


static void
tvopt_cb_proc(Panel_item item, const Event *event) {
  switch ((int)xv_get(item, PANEL_CLIENT_DATA)) {
    case APPLY_BUTTON:
      display_on_tv = (int)xv_get(tvopt_disp_checkbox, PANEL_VALUE);
      use_interlace = (int)xv_get(tvopt_int_checkbox, PANEL_VALUE);
      display_mode = (int)xv_get(tvopt_mode_choice, PANEL_VALUE) + 1;
      if (vtx_dev_open) {
        CCTCHK(cct_set_display(display_on_tv ? (display_mode + use_interlace * INTERLACE_OFFSET) :
            DISPOFF), "cct_set_display", return);
      }
    break;
    case CLOSE_BUTTON:
      xv_set(tvopt_frame, XV_SHOW, FALSE, NULL);
    break;
    default:
      assert(0);
    break;
  }
}


static Frame miscopt_frame;
static Panel_item miscopt_auto_checkbox, miscopt_cps_text, miscopt_ps_text, miscopt_ascii_text;
static Panel_item miscopt_bm_text;

void
miscopt_proc(void) {
  Panel miscopt_panel;
  
  if (!miscopt_frame) {
    miscopt_frame = (Frame)xv_create(frame, FRAME,
        XV_LABEL, "Micellaneous options",
        NULL);
    miscopt_panel = (Panel)xv_create(miscopt_frame, PANEL, NULL);
    miscopt_auto_checkbox = (Panel_item)xv_create(miscopt_panel, PANEL_CHECK_BOX,
        XV_X, 30,
        XV_Y, 20,
        PANEL_CHOICE_STRINGS, "Auto-reset on station-change", NULL,
        NULL);
    xv_create(miscopt_panel, PANEL_MESSAGE,
        XV_X, 70,
        XV_Y, 60,
        PANEL_LABEL_STRING, "Printing commands:",
        PANEL_LABEL_BOLD, TRUE,
        NULL);
    miscopt_cps_text = (Panel_item)xv_create(miscopt_panel, PANEL_TEXT,
        XV_X, 10,
        XV_Y, 80,
        PANEL_LABEL_STRING, "Color-PostScript",
        PANEL_VALUE_DISPLAY_WIDTH, 120,
        PANEL_VALUE_STORED_LENGTH, 99,
        NULL);
    miscopt_ps_text = (Panel_item)xv_create(miscopt_panel, PANEL_TEXT,
        XV_X, 18,
        XV_Y, 100,
        PANEL_LABEL_STRING, "B/W-PostScript",
        PANEL_VALUE_DISPLAY_WIDTH, 120,
        PANEL_VALUE_STORED_LENGTH, 99,
        NULL);
    miscopt_ascii_text = (Panel_item)xv_create(miscopt_panel, PANEL_TEXT,
        XV_X, 87,
        XV_Y, 120,
        PANEL_LABEL_STRING, "ASCII",
        PANEL_VALUE_DISPLAY_WIDTH, 120,
        PANEL_VALUE_STORED_LENGTH, 99,
        NULL);
    miscopt_bm_text = (Panel_item)xv_create(miscopt_panel, PANEL_TEXT,
        XV_X, 74,
        XV_Y, 140,
        PANEL_LABEL_STRING, "Bitmap",
        PANEL_VALUE_DISPLAY_WIDTH, 120,
        PANEL_VALUE_STORED_LENGTH, 99,
        NULL);
    xv_create(miscopt_panel, PANEL_BUTTON,
        XV_X, 75,
        XV_Y, 175,
        PANEL_LABEL_STRING, "Apply",
        PANEL_NOTIFY_PROC, miscopt_cb_proc,
        PANEL_CLIENT_DATA, APPLY_BUTTON,
        NULL);
    xv_create(miscopt_panel, PANEL_BUTTON,
        XV_X, 135,
        XV_Y, 175,
        PANEL_LABEL_STRING, "Close",
        PANEL_NOTIFY_PROC, miscopt_cb_proc,
        PANEL_CLIENT_DATA, CLOSE_BUTTON,
        NULL);
    window_fit(miscopt_panel);
    window_fit(miscopt_frame);
    disable_frame_resize(miscopt_frame, FALSE);
  }
  if (!(int)xv_get(miscopt_frame, XV_SHOW))
    center_frame(frame, miscopt_frame);
  xv_set(miscopt_auto_checkbox, PANEL_VALUE, auto_reset, NULL);
  xv_set(miscopt_cps_text, PANEL_VALUE, default_cps_prn, NULL);
  xv_set(miscopt_ps_text, PANEL_VALUE, default_ps_prn, NULL);
  xv_set(miscopt_ascii_text, PANEL_VALUE, default_ascii_prn, NULL);
  xv_set(miscopt_bm_text, PANEL_VALUE, default_bm_prn, NULL);
  xv_set(miscopt_frame, XV_SHOW, TRUE, NULL);
}


static void
miscopt_cb_proc(Panel_item item, const Event *event) {
  switch ((int)xv_get(item, PANEL_CLIENT_DATA)) {
    case APPLY_BUTTON:
      auto_reset = (int)xv_get(miscopt_auto_checkbox, PANEL_VALUE);
      strncpy(default_cps_prn, (char *)xv_get(miscopt_cps_text, PANEL_VALUE), 99);
      strncpy(default_ps_prn, (char *)xv_get(miscopt_ps_text, PANEL_VALUE), 99);
      strncpy(default_ascii_prn, (char *)xv_get(miscopt_ascii_text, PANEL_VALUE), 99);
      strncpy(default_bm_prn, (char *)xv_get(miscopt_bm_text, PANEL_VALUE), 99);
    break;
    case CLOSE_BUTTON:
      xv_set(miscopt_frame, XV_SHOW, FALSE, NULL);
    break;
    default:
      assert(0);
    break;
  }
}


static Frame so_frame;
static Panel_item so_to_slider, so_il_slider, so_la_num, so_ip_num, so_td_checkbox, so_as_choice;

void
station_opt_proc(void) {
  Panel so_panel;
  
  if (!so_frame) {
    so_frame = (Frame)xv_create(frame, FRAME,
        XV_LABEL, "Station options",
        NULL);
    so_panel = (Panel)xv_create(so_frame, PANEL, NULL);
    so_to_slider = (Panel_item)xv_create(so_panel, PANEL_SLIDER,
        XV_X, 33,
        XV_Y, 20,
        PANEL_LABEL_STRING, "Page-Timeout (sec.)",
        PANEL_MIN_VALUE, 10,
        PANEL_MAX_VALUE, 180,
        PANEL_SLIDER_WIDTH, 170,
        PANEL_TICKS, 18,
        PANEL_SLIDER_END_BOXES, TRUE,
        NULL);
    so_il_slider = (Panel_item)xv_create(so_panel, PANEL_SLIDER,
        XV_X, 15,
        XV_Y, 60,
        PANEL_LABEL_STRING, "TOP-Text interleave",
        PANEL_MIN_VALUE, -100,
        PANEL_MAX_VALUE, 100,
        PANEL_SLIDER_WIDTH, 170,
        PANEL_TICKS, 11,
        PANEL_SLIDER_END_BOXES, TRUE,
        NULL);
    so_td_checkbox = (Panel_item)xv_create(so_panel, PANEL_CHECK_BOX,
        XV_X, 145,
        XV_Y, 95,
        PANEL_CHOICE_STRINGS, "Don't search for TOP-Text", NULL,
        PANEL_NOTIFY_PROC, station_opt_activate,
        NULL);
    so_as_choice = (Panel_item)xv_create(so_panel, PANEL_CHOICE,
        XV_X, 25,
        XV_Y, 125,
        PANEL_LABEL_STRING, "Auto-search pages",
        PANEL_CHOICE_STRINGS, "None", "Blocks", "Groups", "Normal", "Subpages", NULL,
        NULL);
    so_la_num = (Panel_item)xv_create(so_panel, PANEL_NUMERIC_TEXT,
        XV_X, 75,
        XV_Y, 175,
        PANEL_LABEL_STRING, "Page-lookahead",
        PANEL_MIN_VALUE, 0,
        PANEL_MAX_VALUE, 5,
        PANEL_VALUE_DISPLAY_WIDTH, 20,
        PANEL_VALUE_STORED_LENGTH, 1,
        NULL);
    so_ip_num = (Panel_item)xv_create(so_panel, PANEL_TEXT,
        XV_X, 290,
        XV_Y, 175,
        PANEL_LABEL_STRING, "Index page",
        PANEL_VALUE_DISPLAY_WIDTH, 35,
        PANEL_VALUE_STORED_LENGTH, 3,
        NULL);
    xv_create(so_panel, PANEL_BUTTON,
        XV_X, 180,
        XV_Y, 220,
        PANEL_LABEL_STRING, "Apply",
        PANEL_NOTIFY_PROC, so_cb_proc,
        PANEL_CLIENT_DATA, APPLY_BUTTON,
        NULL);
    xv_create(so_panel, PANEL_BUTTON,
        XV_X, 240,
        XV_Y, 220,
        PANEL_LABEL_STRING, "Close",
        PANEL_NOTIFY_PROC, so_cb_proc,
        PANEL_CLIENT_DATA, CLOSE_BUTTON,
        NULL);
    window_fit(so_panel);
    window_fit(so_frame);
    disable_frame_resize(so_frame, FALSE);
  }
  if (!(int)xv_get(so_frame, XV_SHOW))
    center_frame(frame, so_frame);
  station_opt_update();
  xv_set(so_frame, XV_SHOW, TRUE, NULL);
}


static void
so_cb_proc(Panel_item item, const Event *event) {
  int index_page;

  switch ((int)xv_get(item, PANEL_CLIENT_DATA)) {
    case APPLY_BUTTON:
      index_page = strtol((char*)xv_get(so_ip_num, PANEL_VALUE), NULL, 16);
      if (!vtx_chkpgnum(index_page, TRUE)) {
        confirm_notice("Invalid index-page number.\nValid range is 100-8FF (hexadecimal).");
      } else {
        hotlist_set_timeout((int)xv_get(so_to_slider, PANEL_VALUE));
        hotlist_set_interleave((int)xv_get(so_il_slider, PANEL_VALUE));
        hotlist_set_topdisable((int)xv_get(so_td_checkbox, PANEL_VALUE));
        hotlist_set_searchlevel((tt_pageclass_t)xv_get(so_as_choice, PANEL_VALUE));
        hotlist_set_lookahead((int)xv_get(so_la_num, PANEL_VALUE));
        hotlist_set_index_page(index_page);
      }
    break;
    case CLOSE_BUTTON:
      xv_set(so_frame, WIN_SHOW, FALSE, NULL);
    break;
    default:
      assert(0);
    break;
  }
}


static void
station_opt_activate(void) {
  xv_set(so_as_choice, PANEL_INACTIVE, (int)xv_get(so_td_checkbox, PANEL_VALUE), NULL);
}


void
station_opt_update(void) {
  char page_str[4];

  if (!so_frame)
    return;
  xv_set(so_to_slider, PANEL_VALUE, hotlist_get_timeout(), NULL);
  xv_set(so_il_slider, PANEL_VALUE, hotlist_get_interleave(), NULL);
  xv_set(so_td_checkbox, PANEL_VALUE, hotlist_get_topdisable(), NULL);
  xv_set(so_as_choice, PANEL_VALUE, (int)hotlist_get_searchlevel(), NULL);
  station_opt_activate();
  xv_set(so_la_num, PANEL_VALUE, hotlist_get_lookahead(), NULL);
  sprintf(page_str, "%X", hotlist_get_index_page());
  xv_set(so_ip_num, PANEL_VALUE, page_str, NULL);
}


/*****************************************************************************
 * File_choosers & file-I/O
 */

static File_chooser load_fc, save_fc, export_fc;
static Panel_item export_ftype_item;

static const char*
check_pipe(const char *path) {
  const char *path_loop, *fname;
  
  for (path_loop = fname = path; *path_loop; path_loop++) {
    if (*path_loop == '/') {
      fname = path_loop + 1;
    } else if (*path_loop == ' ') {
      break;
    }
  }
  if (*fname == '|') {
    return fname + 1;
  }
  return NULL;
}


static FILE*
fpopen(const char *path, int do_write, int can_append) {
  FILE *file;
  const char *fname;
  int do_overwrite;
  
  if ((fname = check_pipe(path))) {
    if (!((file = popen(fname, (do_write ? "w" : "r"))))) {
      confirm_notice_v("Can't exec ", fname, ":\n", strerror(errno), NULL);
      return NULL;
    }
  } else {
    if (do_write) {
      if ((do_overwrite = chk_conf_overwrite(path, can_append)) < 0)
        return NULL;
      if (do_overwrite == 1) {
        if (!((file = fopen(path, "a")))) {
          confirm_notice_v("Can't open ", path, " for writing:\n", strerror(errno), NULL);
          return NULL;
        }
      } else {
        if (!((file = fopen(path, "w")))) {
          confirm_notice_v("Can't open ", path, " for writing:\n", strerror(errno), NULL);
          return NULL;
        }
      }
    } else {
      if (!((file = fopen(path, "r")))) {
        confirm_notice_v("Can't open ", path, " for reading:\n", strerror(errno), NULL);
        return NULL;
      }
    }
  }
  return file;
}


static int
fpclose(const char *path, FILE *file, int do_write, int print_msg) {
  const char *fname;

  if ((fname = check_pipe(path))) {
    if (ferror(file)) {
      if (print_msg) {
        if (do_write) {
          confirm_notice_v("Error while writing to ", fname, ":\n", strerror(errno), NULL);
        } else {
          confirm_notice_v("Error while reading from ", fname, ":\n", strerror(errno), NULL);
        }
      }
      pclose(file);
      return -1;
    } else if (pclose(file)) {
      if (print_msg) {
        confirm_notice_v("Warning: ", fname, " returned\nnon-zero exit-status.", NULL);
      }
      return -1;
    }
  } else {
    if (ferror(file) || fclose(file) < 0) {
      if (print_msg) {
        if (do_write) {
          confirm_notice_v("Error while writing ", path, ":\n", strerror(errno), NULL);
        } else {
          confirm_notice_v("Error while reading ", path, ":\n", strerror(errno), NULL);
        }
      }
      fclose(file);
      return -1;
    }
  }
  return 0;
}


static void
create_filename(char *fname, int page, int subpage, const char *suffix) {
  int pos;

  if (!intv_fname) {
    strcpy(fname, hotlist_get_name(hotlist_get_current()));
    for (pos = strlen(fname); pos >= 0; pos--) {
      if (fname[pos] == '/') {
        fname[pos] = '_';
      }
    }
    sprintf(fname + strlen(fname), ":%X_%X.%s", page, subpage, suffix);
  } else {
    sprintf(fname, "%X.%s", page, suffix);
  }
}


static void
change_ext(File_chooser fc, const char *newext) {
  int len, extpos = -1;
  char *newname;

  newname = sstrdup((char*)xv_get(fc, FILE_CHOOSER_DOC_NAME));
  len = strlen(newname);
  if (len >= 4 &&
      (!strcmp(".txt", newname + len - 4) || !strcmp(".gif", newname + len - 4) ||
      !strcmp(".ppm", newname + len - 4))) {
    extpos = len - 3;
  }
  if (len >= 3 && !strcmp(".ps", newname + len - 3)) {
    extpos = len - 2;
  }
  if (extpos < 0) {
    free(newname);
    return;
  }
  newname = sstrapp(newname, newext);	/* Make sure there's enough space for the new extension */
  strcpy(newname + extpos, newext);
  xv_set(fc, FILE_CHOOSER_DOC_NAME, newname, NULL);
  free(newname);
}


static int
common_file_proc(Frame fc, const char *path) {
  FILE *file = NULL;
  int do_write = TRUE, status, ftype;

  if (fc == load_fc) {				/* The fpopen is currently useless, because it's */
    if (!(file = fpopen(path, FALSE, FALSE)))	/* impossible to type in a file to load. Thus */
      return TRUE;				/* you're unable to tell VideoteXt which pipe to */
    do_write = FALSE;				/* read from. Thanks, SunSoft Inc. */
    status = load_vtx(file);
    if (status < 0) {
      const char *fname, *errmsg = "File is corrupt";

      fname = check_pipe(path);
      if (!feof(file)) {
        errmsg = strerror(errno);
      }
      if (fname) {
        confirm_notice_v("Error while reading from ", fname, ":\n", errmsg, NULL);
      } else {
        confirm_notice_v("Error while reading ", path, ":\n", errmsg, NULL);
      }
    }
    if (status) {
      fpclose(path, file, do_write, FALSE);
      return TRUE;
    }
  } else if (fc == save_fc) {
    if (!(file = fpopen(path, TRUE, FALSE)))
      return TRUE;
    save_vtx(file);
  } else if (fc == export_fc) {
    switch (ftype = (int)xv_get(export_ftype_item, PANEL_VALUE)) {
      case 0: /* ASCII */
        if (!(file = fpopen(path, TRUE, TRUE)))
          return TRUE;
        export_ascii(file);
      break;
      case 1: /* GIF */
        if (!(file = fpopen(path, TRUE, FALSE)))
          return TRUE;
        export_gif(file);
      break;
      case 2: /* PPM */
        if (!(file = fpopen(path, TRUE, FALSE)))
          return TRUE;
        export_ppm(file);
      break;
      case 3: /* Color EPSF */
      case 4: /* B&W EPSF */
      case 5: /* B&W EPSF (inverted) */
        if (!(file = fpopen(path, TRUE, FALSE)))
          return TRUE;
        write_postscript(file, vtxwin.page, hotlist_get_name(hotlist_get_current()),
            ftype == 3, ftype == 5, !vtxwin.hidden);
      break;
      default:
        assert(0);
      break;
    }
  } else {
    assert(0);
  }
  if (fpclose(path, file, do_write, TRUE) < 0) {
    return TRUE;
  }
  return FALSE;
}


void
load_proc(void) {
  if (!load_fc) {
    load_fc = (File_chooser)xv_create(frame, FILE_CHOOSER_OPEN_DIALOG,
        FRAME_LABEL, "Load VTX File",
        FRAME_SHOW_LABEL, TRUE,
        FILE_CHOOSER_NOTIFY_FUNC, common_file_proc,
        FILE_CHOOSER_NO_CONFIRM, TRUE,
        NULL);
  }
  if (!(int)xv_get(load_fc, XV_SHOW))
    center_frame(frame, load_fc);
  xv_set(load_fc, 
      FILE_CHOOSER_UPDATE,
      FRAME_CMD_PIN_STATE, FRAME_CMD_PIN_IN,
      XV_SHOW, TRUE,
      NULL);
}


void
save_proc(void) {
  char fname[36];		/* 24 chars for name, 3 for page, 2 for subpage, */
				/* 2 for delimiters, 4 for extension, 1 for '\0' */
  if (!save_fc) {
    save_fc = (File_chooser)xv_create(frame, FILE_CHOOSER_SAVEAS_DIALOG,
        FRAME_LABEL, "Save VTX File",
        FRAME_SHOW_LABEL, TRUE,
        FILE_CHOOSER_NOTIFY_FUNC, common_file_proc,
        FILE_CHOOSER_NO_CONFIRM, TRUE,
        NULL);
  }
  if (!(int)xv_get(save_fc, XV_SHOW))
    center_frame(frame, save_fc);
  create_filename(fname, vtxwin.page->info.pagenum, vtxwin.page->info.minute, "vtx");
  xv_set(save_fc, 
      FILE_CHOOSER_UPDATE,
      FRAME_CMD_PIN_STATE, FRAME_CMD_PIN_IN,
      FILE_CHOOSER_DOC_NAME, fname,
      XV_SHOW, TRUE,
      NULL);
}


static Panel_item export_prn_button;
static const char *export_ext[] = { "txt", "gif", "ppm", "ps", "ps", "ps" };

static void
export_ftype_proc(Panel_item item, int value, const Event *event) {
  change_ext(export_fc, export_ext[value]);
}


static void
export_cb_proc(Panel_item item, const Event *event) {
  char *prn_name;

  switch ((int)xv_get(export_ftype_item, PANEL_VALUE)) {
    case 0:
      prn_name = default_ascii_prn;
    break;
    case 1:
    case 2:
      prn_name = default_bm_prn;
    break;
    case 3:
      prn_name = default_cps_prn;
    break;
    case 4:
    case 5:
      prn_name = default_ps_prn;
    break;
    default:
      assert(0);
    break;
  }
  xv_set(export_fc, FILE_CHOOSER_DOC_NAME, prn_name, NULL);
}


static int
export_ext_func(File_chooser fc, const Rect *frame_rect, const Rect *exten_rect, int left_edge,
    int right_edge, int max_height) {
  xv_set(export_ftype_item, XV_Y, exten_rect->r_top - 10, NULL);
  xv_set(export_prn_button,
      XV_X, (frame_rect->r_width - (int)xv_get(export_prn_button, XV_WIDTH)) / 2,
      XV_Y, exten_rect->r_top + 25,
      NULL);
  return -1;
}


void
export_proc(void) {
  char fname[36];		/* 24 chars for name, 3 for page, 2 for subpage, */
  Panel fc_panel;		/* 2 for delimiters, 4 for extension, 1 for '\0' */

  if (!export_fc) {
    export_fc = (File_chooser)xv_create(frame, FILE_CHOOSER_SAVEAS_DIALOG,
        FRAME_LABEL, "Export File",
        FRAME_SHOW_LABEL, TRUE,
        FILE_CHOOSER_NOTIFY_FUNC, common_file_proc,
        FILE_CHOOSER_NO_CONFIRM, TRUE,
        NULL);
    fc_panel = (Panel)xv_get(export_fc, FRAME_CMD_PANEL);
    export_ftype_item = (Panel_item)xv_create(fc_panel, PANEL_CHOICE_STACK,
        XV_X, 15,
        PANEL_LABEL_STRING, "Export format",
        PANEL_CHOICE_STRINGS, "ASCII", "GIF", "PPM", "Color EPSF", "B&W EPSF",
            "B&W EPSF (inv.)", NULL,
	PANEL_CHOICE_NCOLS, 2,
	PANEL_CHOICE_NROWS, 3,
	PANEL_LAYOUT, PANEL_HORIZONTAL,
        PANEL_NOTIFY_PROC, export_ftype_proc,
        NULL);
    export_prn_button = (Panel_item)xv_create(fc_panel, PANEL_BUTTON,
        PANEL_LABEL_STRING, "Use default printer",
        PANEL_NOTIFY_PROC, export_cb_proc,
        NULL);
    xv_set(export_fc,
        XV_HEIGHT, (int)xv_get(export_fc, XV_HEIGHT) + 10,
        XV_WIDTH, MAX((int)xv_get(export_fc, XV_WIDTH), 250),
        FILE_CHOOSER_EXTEN_HEIGHT, 10,
        FILE_CHOOSER_EXTEN_FUNC, export_ext_func,
        NULL);
  }
  if (!(int)xv_get(export_fc, XV_SHOW))
    center_frame(frame, export_fc);
  create_filename(fname, vtxwin.page->info.pagenum, vtxwin.page->info.minute,
      export_ext[(int)xv_get(export_ftype_item, PANEL_VALUE)]);
  xv_set(export_fc, 
      FILE_CHOOSER_UPDATE,
      FRAME_CMD_PIN_STATE, FRAME_CMD_PIN_IN,
      FILE_CHOOSER_DOC_NAME, fname,
      XV_SHOW, TRUE,
      NULL);
}
