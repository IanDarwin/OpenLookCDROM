#ifndef _XINIT_H
#define _XINIT_H

/* Copyright (c) 1994-95 Martin Buck  <martin.buck@student.uni-ulm.de>
 * Read COPYING for more information
 */


#include <linux/vtx.h>
#include <X11/Xlib.h>
#include <xview/xview.h>
#include <xview/frame.h>
#include <xview/panel.h>
#include <xview/canvas.h>
#include <xview/svrimage.h>


#define CMSCWHITE   0
#define CMSCCYAN    1
#define CMSCMAGENTA 2
#define CMSCBLUE    3
#define CMSCYELLOW  4
#define CMSCGREEN   5
#define CMSCRED     6
#define CMSCBLACK   7
#define VTXCBLACK   vtxcolors[0]
#define VTXCRED     vtxcolors[1]
#define VTXCGREEN   vtxcolors[2]
#define VTXCYELLOW  vtxcolors[3]
#define VTXCBLUE    vtxcolors[4]
#define VTXCMAGENTA vtxcolors[5]
#define VTXCCYAN    vtxcolors[6]
#define VTXCWHITE   vtxcolors[7]

#define RPANEL_WIDTH 95
#define CANVAS_OFFSET 40

#define XVNULL ((Xv_opaque)0)


typedef struct {
  XFontStruct *normal, *doubleht;
  int width, height;
  char fontname[8];
} vtxfont_t;

typedef struct {
  char *menu_string;
  int update_interval;
} scr_update_t;


extern char *intv_fname;
extern Display *dpy;
extern Window rootid, frameid;
extern int screen_num;
extern unsigned long vtxcolors[];
extern GC vtxgc;
extern Pixmap dith_bm[];
extern vtxfont_t vtxfonts[];
extern const scr_update_t update_table[];
extern int vtxmaxfont;
extern vtx_info_t vtx_info;


extern Frame frame;
extern Canvas canvas, canvas_bg;
extern Panel right_panel, main_panel, hist_panel;
extern Panel_item pgnum_item, subpgnum_item, checkbox, intv_reveal_toggle;
extern Panel_item pgmsg_item, subpgmsg_item, cachemsg_item;
extern Server_image hist_images[];
extern Menu scr_menu, font_menu;

#endif /* _XINIT_H */
