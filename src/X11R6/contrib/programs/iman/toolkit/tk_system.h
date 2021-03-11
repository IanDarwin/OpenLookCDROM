/*
 *
 * 	tk_system.h 
 * 	structure du toolkit
 *
 * 	Modification :  23/04/94
 *
 *	Copyright (c) 1993, 1994 Bruno RIVAS
 *	All Rights Reserved
 *
 *
 * Permission to use, copy, modify, distribute and sell this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of Bruno RIVAS not be used in advertising
 * or publicity pertaining to distribution of the software without specific,
 * written prior permission.  Bruno RIVAS makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * Bruno RIVAS disclaims all warranties with regard to this software,
 * including all implied warranties of merchantability and fitness,
 * in no event shall Bruno RIVAS be liable for any special,
 * indirect or consequential damages or any damages
 * whatsoever resulting from loss of use, data or profits,
 * whether in an action of contract, negligence or other tortious
 * action, arising out of or in connection with the use 
 * or performance of this software.
 *
 *
 *	Bruno RIVAS 
 *      IMAN Development Toolkit version 1.2
 *
 *	Internet:       rivas@email.teaser.com
 *	Surface Mail:   Bruno RIVAS,
 *			30 avenue Marie
 *			93250 Villemomble -- FRANCE
 *	Voice phone:    (33) (1) 49.35.97.27
 *	Fax: 		(33) (1) 48.94.27.91
 *
 */



#ifndef TK_SYSTEM_H
#define TK_SYSTEM_H



typedef struct {
		int type;

		Window window;
		ButtonID button;
		ScrollbarID scroll;
		EditID  edit;
		ListID  list;
		ComboID combo;
		MenuID  menu;
		
		int window_number, window_type;
		int start_x, start_y, start_width, start_height;
		int x, y, width, height;
		int gap_x, gap_y, gap_width, gap_height;

		short debug;

		}ActionStruct;



#define NoAction		0
#define MenuSelectingAction 	1
#define ComboSelectingAction	2




typedef struct {
		Atom _IMAN_WINDOW_MANAGER;
		Atom _IMAN_HELP_SERVER;

		Atom _IMAN_WM_TYPE;
		Atom _IMAN_WM_DATA;
		Atom _IMAN_WM_FOCUS;
		Atom _IMAN_WM_MDW;
		Atom _IMAN_WM_MESSAGES;

		Atom _IMAN_DROP_SITES;
		Atom _IMAN_DROP_TARGETS;
		Atom _IMAN_DROP_ACTION;
		Atom _IMAN_DROP_MESSAGES;

		Atom _IMAN_HS_DATABOOK;
		Atom _IMAN_HS_TOPIC;
		Atom _IMAN_HS_MESSAGES;	
		
		Atom WM_STATE;
		Atom WM_COLORMAP_WINDOWS;
		Atom WM_PROTOCOLS;
		Atom WM_CHANGE_STATE;
		Atom WM_TAKE_FOCUS;
		Atom WM_SAVE_YOURSELF;
		Atom WM_DELETE_WINDOW;
		Atom WM_TOP_LEVEL;
		} TkAtoms;


typedef struct {
		XFontStruct  *ega;
		XFontStruct  *vga;
		XFontStruct  *f8_13;
		XFontStruct  *f6_10;
		XFontStruct  *fixed;
		XFontStruct  *helvetica12;
		XFontStruct  *times12;
		XFontStruct  *f5_7;
		unsigned int flag;
		} TkFonts;


typedef struct {
		Pixmap iconify, iconifyM, iconifyMmask, iconifyMgrayed, iconifyMmaskgrayed;
		Pixmap zoom, zoomM, zoomMmask, zoomMgrayed, zoomMmaskgrayed;
		Pixmap close, closeM, closeMmask, closeMgrayed,closeMmaskgrayed;
		Pixmap up, upM, upMgrayed, upMmask,upMmaskgrayed;
		Pixmap down, downM, downMgrayed, downMmask,downMmaskgrayed;
		Pixmap left, leftM, leftMgrayed, leftMmask, leftMmaskgrayed;
		Pixmap right, rightM, rightMgrayed, rightMmask, rightMmaskgrayed; 
		Pixmap thumb1, thumb2, thumbM, thumbMmask;
		Pixmap radio1, radio2, radio3,radio4;
		Pixmap popup1, popup2, popup3, popup4;
		Pixmap combo1, combo2, comboM, comboMmask,comboMgrayed, comboMmaskgrayed;
		Pixmap graytile;
		
		} TkPixmaps;


typedef struct {
		Cursor normal;		/* XC_left_ptr 		*/
		Cursor textedit;	/* XC_xterm 		*/
		Cursor top_left;	/* XC_top_left_corner 	*/
		Cursor top_right;	/* XC_top_right_corner 	*/
		Cursor bottom_left;	/* XC_bottom_left_corner */
		Cursor bottom_right;	/* XC_bottom_right_corner */
		Cursor up_down;		/* XC_sb_v_double_arrow */
		Cursor left_right;	/* XC_sb_h_double_arrow */
		Cursor sb_up;		/* XC_sb_left_arrow 	*/
		Cursor sb_left;		/* XC_sb_up_arrow 	*/
		} TkCursors;


typedef struct {
		char *vendor;
		int version;
		int release;
		char *comment;
		}TkInfos;


typedef struct {
		Window main_window;
		Bool active;
		int version;
		int release;
		char *comment;
		}WmInfos;


typedef struct {
		Window main_window;
		Bool active;
		int version;
		int release;
		char *comment;
		}HsInfos;



typedef struct {
		Display *display;
		int screen;
		int depth;

		int argc;
		char **argv;
		
		TkInfos infos;
		WmInfos wm;
		HsInfos hs;

		TkAtoms    atoms;		
		TkFonts    fonts;
		TkPixmaps  pixmaps;
		TkCursors  cursors;

		WidgetColors   bn_colors;
		WidgetColors   sb_colors;
		WidgetColors   ed_colors;
		WidgetColors   ls_colors;
		WidgetColors   cb_colors;
		WidgetColors   mn_colors;
		WindowColors  win_colors;
		WindowColors  dlg_colors;
		IconColors icn_colors;

		ActionStruct action;
		WidgetStruct *widgets;
		unsigned int numwidgets;
		unsigned int maxwidgets;

		unsigned long widget_double_click;

		} TkDisplay;


	

#endif 		/* TK_SYSTEM_H */



