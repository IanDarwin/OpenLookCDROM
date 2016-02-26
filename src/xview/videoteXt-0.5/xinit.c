/*
 * xinit.c: Initialize XView, create XView-objects 
 *
 * Copyright (c) 1994-95 Martin Buck  <martin.buck@student.uni-ulm.de>
 * Read COPYING for more information
 *
 */

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <errno.h>
#include <sys/time.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <xview/xview.h>
#include <xview/frame.h>
#include <xview/cms.h>
#include <xview/panel.h>
#include <xview/canvas.h>
#include <xview/svrimage.h>
#include <xview/icon.h>
#include "safe_malloc.h"
#include "misc.h"
#include "cct.h"
#include "vtxdecode.h"
#include "vtxqueue.h"
#include "config.h"
#include "dialog.h"
#include "xevents.h"
#include "xinit.h"

#include "vtx.xbm"
#include "empty.xbm"
#include "eyes.xbm"
#include "sheet.xbm"
#include "checkmark_sheet.xbm"
#include "checkmark_sheets.xbm"
#include "clock.xbm"
#include "eyes_sheet.xbm"
#include "sheets.xbm"

#define VTXRC_FNAME "vtxrc"
#define MAXFONTS 8

const scr_update_t update_table[] = {
  { "None", 3 },
  { "Slow", 2 },
  { "Fast", 0 }
};

char *intv_fname;
Display *dpy;
Window rootid, frameid;
int screen_num;
unsigned long vtxcolors[8];
GC vtxgc;
Pixmap dith_bm[8];
vtxfont_t vtxfonts[MAXFONTS];
int vtxmaxfont;
Frame frame;
Canvas canvas, canvas_bg;
Panel right_panel, main_panel, hist_panel;
Panel_item pgnum_item, subpgnum_item, checkbox, intv_reveal_toggle;
Panel_item pgmsg_item, subpgmsg_item, cachemsg_item;
Server_image hist_images[STAT_COUNT];
Menu scr_menu, font_menu;
vtx_info_t vtx_info;



static void
save_user_config(void) {
  char *home, *rcfile;
  
  if ((home = getenv("HOME"))) {
    rcfile = sstrdup(home);
    rcfile = sstrapp(rcfile, "/." VTXRC_FNAME);
    if (config_save(rcfile)) {
      confirm_notice_v("Error: Can't save options to ", rcfile, ":\n", strerror(errno), NULL);
    }
    free(rcfile);
  } else {
    confirm_notice("Error: $HOME is unset --\ncan't save your options.");
  }
}


int
main(int argc, char *argv[]) {
  char **fontlist, *home, *rcfile, tmpstr[256];
  int optchr, font, fontnum, item;
  Menu file_menu, gadg_menu, opt_menu;
  Cms cms;
  Server_image icon_image;
  Icon icon;
  const Xv_singlecolor rgb_val[8] = {
    { 255, 255, 255 },	/* white */
    {   0, 255, 255 },	/* cyan */
    { 255,   0, 255 },	/* magenta */
    {   0,   0, 255 },	/* blue */
    { 255, 255,   0 },	/* yellow */
    {   0, 255,   0 },	/* green */
    { 255,   0,   0 },	/* red */
    {   0,   0,   0 },	/* black */
  };
  const char dith_data[8][8] = {
    { 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 },	/* black */
    { 0x00, 0x33, 0x00, 0x33, 0x00, 0x33, 0x00, 0x33 },	/* red */
    { 0x55, 0xaa, 0x55, 0xaa, 0x55, 0xaa, 0x55, 0xaa },	/* green */
    { 0x66, 0xff, 0x66, 0xff, 0x66, 0xff, 0x66, 0xff },	/* yellow */
    { 0x55, 0x00, 0x55, 0x00, 0x55, 0x00, 0x55, 0x00 },	/* blue */
    { 0x33, 0xcc, 0x33, 0xcc, 0x33, 0xcc, 0x33, 0xcc }, /* magenta */
    { 0xff, 0x33, 0xff, 0x33, 0xff, 0x33, 0xff, 0x33 },	/* cyan */
    { 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff }	/* white */
  };

  if ((smalloc_progname = strrchr(argv[0], '/'))) {
    smalloc_progname++;
  } else {
    smalloc_progname = argv[0];
  }

  xv_init(XV_INIT_ARGC_PTR_ARGV, &argc, argv, NULL);
  
  frame = (Frame)xv_create(XVNULL, FRAME,
      WIN_EVENT_PROC, frame_proc,
      WIN_CONSUME_EVENTS,
        WIN_STRUCTURE_NOTIFY, WIN_PROPERTY_NOTIFY, NULL,
      FRAME_LABEL, VTXWINNAME " V" VTXVERSION,
      FRAME_SHOW_FOOTER, TRUE,
      NULL);

  dpy = (Display*)xv_get(frame, XV_DISPLAY);
  rootid = (Window)xv_get((Xv_object)xv_get(frame, XV_ROOT), XV_XID);
  screen_num = (int)xv_get((Xv_screen)xv_get(frame, XV_SCREEN), SCREEN_NUMBER);
  frameid = (Window)xv_get(frame, XV_XID);

  cms = (Cms)xv_create(XVNULL, CMS,
      CMS_SIZE, 8,
      CMS_COLORS, rgb_val,
      NULL);
  if ((Colormap)xv_get(cms, XV_XID) != DefaultColormap(dpy, screen_num)) {
    cms = (Cms)xv_create(XVNULL, CMS,
        CMS_SIZE, 8,
        CMS_NAMED_COLORS, "white", "black", "black", "black", "black", "black", "black", "black", NULL,
        NULL);
    do_dither = TRUE;
  } else if ((int)xv_get(frame, WIN_DEPTH) < 3) {
    do_dither = TRUE;
  }

  for (item = 0; item <= 7; item++)
    vtxcolors[7 - item] = (unsigned long)xv_get(cms, CMS_PIXEL, item);

  vtxgc = XCreateGC(dpy, rootid, 0, NULL);
  for (item = 0; item <= 7; item++) {
    dith_bm[item] = XCreateBitmapFromData(dpy, rootid, dith_data[item], 8, 8);
  }
			/* Find all fonts that match the following pattern & get their font-info */
  fontlist = XListFonts(dpy, "-misc-videotext-medium-r-normal--*-*-75-75-c-*-misc-fontspecific",
      MAXFONTS + 1, &vtxmaxfont);
  fontnum = 0;
  for (font = 0; fontlist && font < vtxmaxfont && fontnum < MAXFONTS; font++) {
    if (strncmp(fontlist[font], "-misc-videotext-medium-r-normal--0-", 35)) {
      if ((vtxfonts[fontnum].normal = XLoadQueryFont(dpy, fontlist[font]))) {
        vtxfonts[fontnum].height = vtxfonts[fontnum].normal->max_bounds.ascent;

        sprintf(tmpstr, "-misc-videotext-medium-r-half--%d-*-75-75-c-*-misc-fontspecific",
            vtxfonts[fontnum].height * 2);
        if ((vtxfonts[fontnum].doubleht = XLoadQueryFont(dpy, tmpstr))) {
          vtxfonts[fontnum].width = vtxfonts[fontnum].normal->max_bounds.width;
          sprintf(vtxfonts[fontnum].fontname, "%dx%d", (unsigned char)vtxfonts[fontnum].width,
              (unsigned char)vtxfonts[fontnum].height);
          fontnum++;
        } else {
          fprintf(stderr, "Warning: can't load double-height-font (%s)\n", tmpstr);
        }
      }
    }
  }
  if (!fontnum) {
    fprintf(stderr, "Error: No VideoteXt-fonts found "
        "(-misc-videotext-medium-r-normal--*-*-75-75-c-*-misc-fontspecific)\n");
    exit(1);
  }
  vtxmaxfont = fontnum - 1;
  XFreeFontNames(fontlist);

  popups[POPUP_SELST].default_rows = 5;
  popups[POPUP_HOTLIST].default_rows = popups[POPUP_HISTORY].default_rows = 9;

  /* We can't load the config-files earlier, since the font-names must be already known */
  if (config_load(VTX_CONFDIR "/" VTXRC_FNAME) && errno != ENOENT) {
    fprintf(stderr, "%s: " VTX_CONFDIR "/" VTXRC_FNAME ": %s\n", smalloc_progname, strerror(errno));
    exit(1);
  }
  if ((home = getenv("HOME"))) {
    rcfile = sstrdup(home);
    rcfile = sstrapp(rcfile, "/." VTXRC_FNAME);
    if (config_load(rcfile) && errno != ENOENT) {
      fprintf(stderr, "%s: %s: %s\n", smalloc_progname, rcfile, strerror(errno));
      exit(1);
    }
    free(rcfile);
  } else {
    fprintf(stderr, "%s: Warning: $HOME is unset -- can't load user-defaults\n", smalloc_progname);
  }

  if (argc > 1) {
    while ((optchr = getopt(argc, argv, "d:f:?")) != -1) {
      switch (optchr) {
        case 'd':
          cct_device = optarg;
        break;
        case 'f':
          intv_fname = optarg;
        break;
        case '?':
          printf("Usage: %s [standard XView-options] [options]\n"
                 "Options:\n"
                 "  -d <device>  Set device to use (default: /dev/vtx)\n"
                 "  -f <file>    Load file & enter display-only mode\n"
                 "  -\\?          Show this help\n"
                 "  -help        Show XView-options\n", smalloc_progname);
          exit(1);
        break;
      }
    }
  }

  icon_image = (Server_image)xv_create(XVNULL, SERVER_IMAGE,
      XV_WIDTH, vtx_width,
      XV_HEIGHT, vtx_height,
      SERVER_IMAGE_X_BITS, vtx_bits,
      NULL);
  icon = (Icon)xv_create(frame, ICON,
      ICON_IMAGE, icon_image,
      NULL);
  hist_images[STAT_NEW] = (Server_image)xv_create(XVNULL, SERVER_IMAGE,
      XV_WIDTH, empty_width,
      XV_HEIGHT, empty_height,
      SERVER_IMAGE_X_BITS, empty_bits,
      NULL);
  hist_images[STAT_SEARCH] = (Server_image)xv_create(XVNULL, SERVER_IMAGE,
      XV_WIDTH, eyes_width,
      XV_HEIGHT, eyes_height,
      SERVER_IMAGE_X_BITS, eyes_bits,
      NULL);
  hist_images[STAT_FOUND] = (Server_image)xv_create(XVNULL, SERVER_IMAGE,
      XV_WIDTH, sheet_width,
      XV_HEIGHT, sheet_height,
      SERVER_IMAGE_X_BITS, sheet_bits,
      NULL);
  hist_images[STAT_READ] = (Server_image)xv_create(XVNULL, SERVER_IMAGE,
      XV_WIDTH, checkmark_sheet_width,
      XV_HEIGHT, checkmark_sheet_height,
      SERVER_IMAGE_X_BITS, checkmark_sheet_bits,
      NULL);
  hist_images[STAT_READ_MP] = (Server_image)xv_create(XVNULL, SERVER_IMAGE,
      XV_WIDTH, checkmark_sheets_width,
      XV_HEIGHT, checkmark_sheets_height,
      SERVER_IMAGE_X_BITS, checkmark_sheets_bits,
      NULL);
  hist_images[STAT_TIMEOUT] = (Server_image)xv_create(XVNULL, SERVER_IMAGE,
      XV_WIDTH, clock_width,
      XV_HEIGHT, clock_height,
      SERVER_IMAGE_X_BITS, clock_bits,
      NULL);
  hist_images[STAT_FOUND_MP] = (Server_image)xv_create(XVNULL, SERVER_IMAGE,
      XV_WIDTH, sheets_width,
      XV_HEIGHT, sheets_height,
      SERVER_IMAGE_X_BITS, sheets_bits,
      NULL);
  hist_images[STAT_SEARCH_MP] = (Server_image)xv_create(XVNULL, SERVER_IMAGE,
      XV_WIDTH, eyes_sheet_width,
      XV_HEIGHT, eyes_sheet_height,
      SERVER_IMAGE_X_BITS, eyes_sheet_bits,
      NULL);

  xv_set(frame,
      FRAME_MIN_SIZE, 41 * vtxfonts[0].width + (intv_fname ? 0 : RPANEL_WIDTH) +
          4 * WIN_DEFAULT_BORDER_WIDTH, 26 * vtxfonts[0].height + CANVAS_OFFSET,
      FRAME_MAX_SIZE, 41 * vtxfonts[vtxmaxfont].width + (intv_fname ? 0 : RPANEL_WIDTH) +
          4 * WIN_DEFAULT_BORDER_WIDTH, 26 * vtxfonts[vtxmaxfont].height + CANVAS_OFFSET,
      FRAME_ICON, icon,
      NULL);

  main_panel = (Panel)xv_create(frame, PANEL,
      XV_X, 0,
      XV_Y, 0,
      NULL);

  file_menu = (Menu)xv_create(XVNULL, MENU,
      MENU_GEN_PIN_WINDOW, frame, "File",
      MENU_ACTION_ITEM, "Load VTX...", load_proc,
      MENU_ACTION_ITEM, "Save VTX...", save_proc,
      MENU_ACTION_ITEM, "Export...", export_proc,
      MENU_ITEM, MENU_STRING, "", MENU_INACTIVE, TRUE, NULL,
      NULL);
  if (!intv_fname) {
    xv_set(file_menu,
        MENU_ACTION_ITEM, "Reset", reset_proc,
        MENU_ITEM, MENU_STRING, "", MENU_INACTIVE, TRUE, NULL,
        MENU_ACTION_ITEM, "Statistics...", stat_proc,
        NULL);
  }
  xv_set(file_menu,
    MENU_ACTION_ITEM, "About...", about_proc,
    MENU_ITEM, MENU_STRING, "", MENU_INACTIVE, TRUE, NULL,
    MENU_ACTION_ITEM, "Exit", xquit,
    NULL);
  xv_create(main_panel, PANEL_BUTTON,
      PANEL_LABEL_STRING, "File",
      PANEL_ITEM_MENU, file_menu,
      XV_X, 10,
      XV_Y, 10,
      NULL);

  gadg_menu = (Menu)xv_create(XVNULL, MENU,
      MENU_GEN_PIN_WINDOW, frame, "Gadgets",
      MENU_ACTION_ITEM, "Page history...", history_proc,
      MENU_ACTION_ITEM, "TOP-Text...", toptext_proc,
      MENU_ACTION_ITEM, "Hotlist...", hotlist_proc,
      MENU_ACTION_ITEM, "Select station...", sel_station_proc,
      NULL);
  if (!intv_fname) {
    xv_create(main_panel, PANEL_BUTTON,
        PANEL_LABEL_STRING, "Gadgets",
        PANEL_ITEM_MENU, gadg_menu,
        XV_X, 80,
        XV_Y, 10,
        NULL);
  }

  scr_menu = (Menu)xv_create(XVNULL, MENU_CHOICE_MENU, NULL);
  for (item = 0; item < sizeof(update_table) / sizeof(scr_update_t); item++) {
    xv_set(scr_menu, MENU_APPEND_ITEM,
        (Menu_item)xv_create(XVNULL, MENUITEM,
            MENU_STRING, update_table[item].menu_string,
            MENU_NOTIFY_PROC, update_menu_proc,
            MENU_CLIENT_DATA, item,
            MENU_SELECTED, (item == 1),
            MENU_RELEASE,
            NULL),
        NULL);
  }
  font_menu = (Menu)xv_create(XVNULL, MENU_CHOICE_MENU, NULL);
  for (item = 0; item <= vtxmaxfont; item++) {
    xv_set(font_menu, MENU_APPEND_ITEM,
        (Menu_item)xv_create(XVNULL, MENUITEM,
            MENU_STRING, vtxfonts[item].fontname,
            MENU_NOTIFY_PROC, font_menu_proc,
            MENU_CLIENT_DATA, item,
            MENU_RELEASE,
            NULL),
        NULL);
  }
  opt_menu = (Menu)xv_create(XVNULL, MENU,
      MENU_GEN_PIN_WINDOW, frame, "Options",
      MENU_PULLRIGHT_ITEM, "Font", font_menu,
      MENU_PULLRIGHT_ITEM, "Screen updates", scr_menu,
      MENU_ACTION_ITEM, "Window layout...", layout_proc,
      MENU_ACTION_ITEM, "TV options...", tvopt_proc,
      MENU_ACTION_ITEM, "Misc. options...", miscopt_proc,
      MENU_ITEM, MENU_STRING, "", MENU_INACTIVE, TRUE, NULL,
      MENU_ACTION_ITEM, "Save global options", save_user_config,
      MENU_ITEM, MENU_STRING, "", MENU_INACTIVE, TRUE, NULL,
      MENU_ACTION_ITEM, "Station options...", station_opt_proc,
      NULL);
  xv_create(main_panel, PANEL_BUTTON,
      PANEL_LABEL_STRING, intv_fname ? "Fonts" : "Options",
      PANEL_ITEM_MENU, intv_fname ? font_menu : opt_menu,
      XV_X, intv_fname ? 80 : 180,
      XV_Y, 10,
      NULL);
  intv_reveal_toggle = (Panel_item)xv_create(main_panel, PANEL_TOGGLE,
      XV_Y, 8,
      PANEL_CHOICE_STRINGS, "Reveal", NULL,
      PANEL_NOTIFY_PROC, intv_reveal_proc,
      XV_SHOW, !!intv_fname,
      NULL);

  right_panel = (Panel)xv_create(frame, PANEL,
      XV_Y, CANVAS_OFFSET,
      XV_X, 41 * vtxfonts[0].width + 4 * WIN_DEFAULT_BORDER_WIDTH,
      PANEL_ACCEPT_KEYSTROKE, TRUE,
      NULL);
  pgmsg_item = (Panel_item)xv_create(right_panel, PANEL_MESSAGE,
      XV_X, 10,
      XV_Y, 0,
      PANEL_LABEL_STRING, "Page: ?",
      NULL);
  subpgmsg_item = (Panel_item)xv_create(right_panel, PANEL_MESSAGE,
      XV_X, 10,
      XV_Y, 15,
      PANEL_LABEL_STRING, "Subpgs.: ?",
      NULL);
  cachemsg_item = (Panel_item)xv_create(right_panel, PANEL_MESSAGE,
      XV_X, 10,
      XV_Y, 30,
      PANEL_LABEL_STRING, "Cached: ?",
      NULL);
  pgnum_item = (Panel_item)xv_create(right_panel, PANEL_TEXT,
      XV_X, 10,
      XV_Y, 55,
      PANEL_LABEL_STRING, "Page:",
      PANEL_VALUE_DISPLAY_WIDTH, 35,
      PANEL_VALUE_STORED_LENGTH, 3,
      PANEL_NOTIFY_STRING, "\n\r",
      PANEL_NOTIFY_PROC, pgnum_proc,
      XV_SHOW, !intv_fname,
      NULL);
  xv_create(right_panel, PANEL_BUTTON,
      XV_X, 10,
      XV_Y, 80,
      PANEL_LABEL_STRING, " +",
      PANEL_LABEL_WIDTH, 20,
      PANEL_NOTIFY_PROC, pginc_proc,
      XV_SHOW, !intv_fname,
      NULL);
  xv_create(right_panel, PANEL_BUTTON,
      XV_X, 50,
      XV_Y, 80,
      PANEL_LABEL_STRING, " -",
      PANEL_LABEL_WIDTH, 20,
      PANEL_NOTIFY_PROC, pgdec_proc,
      XV_SHOW, !intv_fname,
      NULL);
  subpgnum_item = (Panel_item)xv_create(right_panel, PANEL_TEXT,
      XV_X, 10,
      XV_Y, 110,
      PANEL_LABEL_STRING, "Subpg.:",
      PANEL_VALUE_DISPLAY_WIDTH, 23,
      PANEL_VALUE_STORED_LENGTH, 2,
      PANEL_NOTIFY_STRING, "\n\r",
      PANEL_NOTIFY_PROC, pgnum_proc,
      XV_SHOW, !intv_fname,
      NULL);
  xv_create(right_panel, PANEL_BUTTON,
      XV_X, 10,
      XV_Y, 135,
      PANEL_LABEL_STRING, " +",
      PANEL_LABEL_WIDTH, 20,
      PANEL_NOTIFY_PROC, subpginc_proc,
      XV_SHOW, !intv_fname,
      NULL);
  xv_create(right_panel, PANEL_BUTTON,
      XV_X, 50,
      XV_Y, 135,
      PANEL_LABEL_STRING, " -",
      PANEL_LABEL_WIDTH, 20,
      PANEL_NOTIFY_PROC, subpgdec_proc,
      XV_SHOW, !intv_fname,
      NULL);
  xv_create(right_panel, PANEL_BUTTON,
      XV_X, 10,
      XV_Y, 165,
      PANEL_LABEL_STRING, "   Cont.",
      PANEL_LABEL_WIDTH, 60,
      PANEL_NOTIFY_PROC, subpgcont_proc,
      XV_SHOW, !intv_fname,
      NULL);
  xv_create(right_panel, PANEL_BUTTON,
      PANEL_LABEL_STRING, "  Search",
      XV_X, 10,
      XV_Y, 200,
      PANEL_LABEL_WIDTH, 60,
      PANEL_NOTIFY_PROC, pgnum_proc,
      XV_SHOW, !intv_fname,
      NULL);
  xv_create(right_panel, PANEL_BUTTON,
      PANEL_LABEL_STRING, "Search bg",
      XV_X, 10,
      XV_Y, 230,
      PANEL_LABEL_WIDTH, 60,
      PANEL_NOTIFY_PROC, pgnum_bg_proc,
      XV_SHOW, !intv_fname,
      NULL);
  checkbox = (Panel_item)xv_create(right_panel, PANEL_CHECK_BOX,
      XV_X, 10,
      XV_Y, 260,
      PANEL_LAYOUT, PANEL_VERTICAL,
      PANEL_CHOICE_STRINGS, "Stop", "Reveal", NULL,
      PANEL_NOTIFY_PROC, checkbox_proc,
      XV_SHOW, !intv_fname,
      NULL);

  canvas_bg = (Canvas)xv_create(frame, CANVAS,
      XV_X, 0,
      XV_Y, CANVAS_OFFSET,
      CANVAS_AUTO_SHRINK, FALSE,
      CANVAS_AUTO_EXPAND, FALSE,
      WIN_CMS, cms,
      WIN_BACKGROUND_COLOR, CMSCBLACK,
      WIN_FOREGROUND_COLOR, CMSCWHITE,
      NULL);
  canvas = (Canvas)xv_create(canvas_bg, CANVAS,
      XV_X, 1,
      XV_Y, vtxfonts[0].height - 2 * WIN_DEFAULT_BORDER_WIDTH,
      CANVAS_AUTO_SHRINK, FALSE,
      CANVAS_AUTO_EXPAND, FALSE,
      CANVAS_REPAINT_PROC, canvas_repaint,
      CANVAS_X_PAINT_WINDOW, FALSE,
      CANVAS_FIXED_IMAGE, FALSE,
      WIN_CMS, cms,
      WIN_BACKGROUND_COLOR, CMSCBLACK,
      WIN_FOREGROUND_COLOR, CMSCBLACK,
      WIN_EVENT_PROC, canvas_proc,
      WIN_CONSUME_EVENTS,
        WIN_MOUSE_BUTTONS, KBD_USE, NULL,
      NULL);
  xv_set(canvas_paint_window(canvas),
      WIN_EVENT_PROC, canvas_proc,
      WIN_CONSUME_EVENTS,
        WIN_MOUSE_BUTTONS, NULL,
      NULL);

  popups[POPUP_HISTORY].frame = (Frame)xv_create(frame, FRAME,
      XV_LABEL, "Page history",
      NULL);
  hist_panel = (Panel)xv_create(popups[POPUP_HISTORY].frame, PANEL, NULL);
  popups[POPUP_HISTORY].list = (Panel_item)xv_create(hist_panel, PANEL_LIST,
      PANEL_READ_ONLY, TRUE,
      NULL);

  notify_interpose_event_func(right_panel, panel_interposer, NOTIFY_SAFE);
  signal(SIGPIPE, SIG_IGN);			/* required for popen() in print_ps */
  if (intv_fname) {
    signal(SIGUSR1, sigusr1_handler);
  }
  x_main_loop();
  exit(0);
}
