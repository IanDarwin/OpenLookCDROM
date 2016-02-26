#ifndef _XEVENTS_H
#define _XEVENTS_H

/* Copyright (c) 1994-95 Martin Buck  <martin.buck@student.uni-ulm.de>
 * Read COPYING for more information
 */


#include <xview/xview.h>
#include <xview/frame.h>
#include <xview/panel.h>
#include <xview/canvas.h>
#include <xview/notify.h>
#include "vtxdecode.h"


#define NO_UPDATE  0		/* Don't change these defines !!! */
#define HDR_UPDATE 1
#define SCR_UPDATE 2

#define STATION_UNKNOWN    0
#define STATION_RECOGNIZED 1
#define STATION_CHANGED    2

/* Execute CCT-function `cmd' & check status; if operation failed, display function-name
 * `func_str' & ask whether to retry or cancel the operation; if user selects cancel, close
 * device & execute command `fail'
 */
#define CCTCHK(cmd, func_str, fail) \
        do { \
          int cctretval = 0; \
          do { \
            if (cctretval < 0 && report_cct_error(cctretval, func_str)) { \
              cct_close(); \
              station_status = STATION_CHANGED; \
              hotlist_set_current(0); \
              vtx_dev_open = FALSE; \
              fail; \
            } \
          } while ((cctretval = cmd) < 0); \
        } while (0)


typedef struct {
  Window winid;
  GC gc;
  vtxpage_t *page, *curr_disp;
  int pgnum, subpgnum, font, hidden, flash_state, stopped, update, draw_bm;
} vtxpgwin_t;


extern vtxpgwin_t vtxwin;
extern byte_t vtx_buffer[];
extern int station_status, update_entry, do_dither, display_on_tv, auto_reset;


void xquit(void);
int report_cct_error(int err, const char *err_func_str);
void sigusr1_handler(int dummy);
void frame_proc(Xv_window window, const Event *event);
void font_menu_proc(Menu menu, Menu_item menu_item);
void update_menu_proc(Menu menu, Menu_item menu_item);
void checkbox_proc(void);
void intv_reveal_proc(void);
int new_station(int station);
void reset_proc(void);
void pginc_proc(void);
void pgdec_proc(void);
void subpginc_proc(void);
void subpgdec_proc(void);
void subpgcont_proc(void);
Panel_setting pgnum_proc(void);
Panel_setting pgnum_bg_proc(void);
void update_pagenumdisp(int page, int subpage, int numsubpages, int cachecount);
Notify_value update_proc(void);
void canvas_repaint(Canvas canvas, Xv_window window, const Rectlist *area);
void canvas_proc(Xv_window window, const Event *event);
Notify_value panel_interposer(Panel panel, const Event *event, Notify_arg arg,
    Notify_event_type type);
void x_main_loop(void);
void xv_sleep(int sec, int usec);
int x_extract_pgnum(const vtxpgwin_t *vtxwin, int x, int y);
int x_vtx_redraw(const vtxpgwin_t *vtxwin, int x1, int y1, int x2, int y2, int expose);

#endif /* _XEVENTS_H */
