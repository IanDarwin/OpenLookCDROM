#ifndef _DIALOG_H
#define _DIALOG_H

/* Copyright (c) 1994-95 Martin Buck  <martin.buck@student.uni-ulm.de>
 * Read COPYING for more information
 */


#include <xview/xview.h>
#include <xview/frame.h>
#include <xview/panel.h>


/* POPUP_ALL must be first, POPUP_COUNT last */
enum { POPUP_ALL = -1, POPUP_HISTORY, POPUP_TOPTEXT, POPUP_HOTLIST, POPUP_SELST, POPUP_COUNT };

enum { PLACEMENT_CENTER, PLACEMENT_LEFT, PLACEMENT_RIGHT, PLACEMENT_TOP, PLACEMENT_BOTTOM };


typedef struct {
  Frame frame;
  Panel_item list;
  int placement, startup, borderht, rowht, default_rows;
} popups_t;


extern popups_t popups[];
extern int toptext_when_active, display_on_tv, use_interlace, display_mode;
extern int win_adj_x, win_adj_y, win_lb, win_rb, win_tb, win_bb, win_adjusted, station_selected;
extern unsigned char default_cps_prn[], default_ps_prn[], default_ascii_prn[], default_bm_prn[];


void position_popups(void);
void iconify_popups(int state);
void confirm_notice(const char *msg);
void confirm_notice_v(const char *msg, ...);
int chk_conf_overwrite(const char *path, int can_append);
void about_proc(void);
void stat_proc(void);
void sel_station_proc(void);
void sel_st_update(int update_sel);
void hotlist_proc(void);
void hot_update(void);
void history_proc(void);
void history_move(int direction);
void toptext_proc(void);
void toptext_go_nextpage(void);
void toptext_go_prevpage(void);
void toptext_go_nextgrp(void);
void toptext_go_nextblk(void);
void toptext_ok_notify(void);
void pgdesc_update(int page);
void layout_proc(void);
void tvopt_proc(void);
void miscopt_proc(void);
void station_opt_proc(void);
void station_opt_update(void);
void load_proc(void);
void save_proc(void);
void export_proc(void);

#endif /* _DIALOG_H */
