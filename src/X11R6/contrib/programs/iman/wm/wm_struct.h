/*
 *
 * 	wm_struct.h
 * 	structure des fenetres
 *
 * 	Modification :  26/12/93
 *
 *	Copyright (c) 1993 Bruno RIVAS
 *	All Rights Reserved
 *
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appears in all copies and that both that
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
 *      IMAN Window Manager version 1.2
 *
 *	Internet:       rivas@email.teaser.com
 *	Surface Mail:   Bruno RIVAS,
 *			30 avenue Marie
 *			93250 Villemomble -- FRANCE
 *	Voice phone:    (33) (1) 49.35.97.27
 *	Fax: 		(33) (1) 48.94.27.91
 *
 */



#ifndef WM_STRUCT_H
#define WM_STRUCT_H



#define NormalWindow	0
#define ShapeWindow	1
#define LeftBar		2
#define RightBar	3
#define TopBar		4





typedef struct {

	int class;
	int type;
	int attributes;

	struct
	{
	  Bool isFrozen;
	  Bool isZoomed;
	  Bool isIconic;
	  Bool isWithdrawn;

	  Bool isMapped;
	  Bool isOnTop;
	  
	  int initialstate;
	  Bool isInitialized;
	}state;

	struct
	{
	  Bool input;
	  Bool shape;
	  Atom *protocols;
	  int numprotocols;
	  int maxprotocols;
	  Colormap colormap;
	}identity;

	struct
	{
	  Window leader;
	  int number;
	  unsigned int *members;
	  int nummembers, maxmembers;
	}group;

	struct
	{
	  Window leader; 
	  int number;
	  Bool isTransient;
	}transient;

	struct
	{
	  Window leader; 
	  int number;
	}freezing;


	Window mainwindow, clientwindow;

	Window titlebar, leftbar, rightbar, topbar, bottombar;
	Window topleftbox, toprightbox, bottomleftbox, bottomrightbox;
	ButtonID bn_close, bn_zoom, bn_iconify;
	char *title_name;


	struct
	{
	  int x, y;
	  int width, height,border;

	  int client_x, client_y;
	  int client_width, client_height;

	  int max_width, max_height;
	  int min_width, min_height;
	  int add_top, add_bottom, add_leftside, add_rightside;

	}normal;

	struct
	{
	  int x, y;
	  int width, height,border;

	  int client_x, client_y;
	  int client_width, client_height;

	  int max_width, max_height;
	  int min_width, min_height;
	  int add_top, add_bottom, add_leftside, add_rightside;
	}zoom;

	struct
	{
	  Window window, title, draw_area, clientwindow;
	  int x, y;	
	  int width, height;

	  Pixmap pixmap, mask;
 	  int depth;
	  int pix_x, pix_y;	
	  int pix_width, pix_height;
	  char *name;	  

	  Bool hasIconWindow;
	  Bool hasStoredIconName;
	}icon;

	Bool hasStoredName;
	Bool isUsed;

	} WindowStruct;




#define LIGHTDECORATION		1
#define FULLDECORATION		2

#define TITLEBARHEIGHT	22
#define BORDERSIZE	4
#define RESIZEBARHEIGHT 4


#define ICONWIDTH	68
#define ICONHEIGHT	82


#ifdef WM_MAIN_C

int MINWIDTH=80;
int MINHEIGHT=55;
int MAXWIDTH=790;
int MAXHEIGHT=590;
int ZOOMMAXWIDTH=800;
int ZOOMMAXHEIGHT=600;

#else

extern int MINWIDTH;
extern int MINHEIGHT;
extern int MAXWIDTH;
extern int MAXHEIGHT;
extern int ZOOMMAXWIDTH;
extern int ZOOMMAXHEIGHT;

#endif


typedef struct {
		int type;
		Window window;
		
		int number;
		int start_x, start_y, start_width, start_height;
		int x, y, width, height;
		int gap_x, gap_y, gap_width, gap_height;

		short debug;

		}WmAction;



typedef struct {
		int screen_depth, screen_width, screen_height;
		int remote;

		int wm_style;
		int wm_version;
		int wm_release;
		int wm_date;

		Bool set_decoration;
		Bool set_groups;
		Bool set_icons;
		Bool set_icontitle;
		Bool set_helpactive;
		Bool set_debug; 
		Bool set_objectserver;
		Bool set_printmanager;
		Bool set_mdwextension;

		unsigned long desk_bg, desk_fg;
		Pixmap desk_pixmap;
		int pix_depth, pix_width, pix_height;

		int dsk_motif;
		int dsk_defaulticon;
		int dsk_screensaver;
		int dsk_screensaver_time;
		int dsk_paper;
		int dsk_paper_drawing;

		} WmInfo;




typedef struct{
		Pixmap question, question_mask;
		Pixmap warning, warning_mask;
		Pixmap killprocess, killprocess_mask;
	  	Pixmap clare, clare_mask;
		}WmPixmap;




typedef struct {
		Atom title;
		Atom wmhints;
		Atom type;
		Atom state;
		Atom group;
		Atom transient;

		}WmAtoms;



typedef struct {
		XFontStruct *times_big;
		XFontStruct *venice_28;
		}WmFonts;


typedef struct {
		Bool isOwner;
		Window owner;
		Atom selection;
		Atom target;
		Time time;
		Pixmap pixmap;
		int depth;
		char *text;
		int write_number;
		}ClipInfo;


#define InitAction		100
#define MoveAction		101
#define ResizeByTopLeftAction	102
#define ResizeByTopRightAction	103
#define ResizeByBottomLeftAction  104
#define ResizeByBottomRightAction 105
#define ResizeByTopAction	106
#define ResizeByBottomAction	107
#define ResizeByLeftAction	108
#define ResizeByRightAction	109
#define IconifyAction		110
#define ZoomAction		111
#define UniconifyAction 	112
#define MoveIconAction		113
#define MoveZoomAction		114
#define KillProcessAction	115
#define ReparentingAction	116
#define EndOfAllAction		117



#define NoError			0
#define AnotherWMError		1
#define NoMemoryError		2
#define KillProcessError	3


#define ICON_DRAW_AREA_WIDTH	64
#define ICON_DRAW_AREA_HEIGHT	64

#define ERROR_BOX_WIDTH		410
#define ERROR_BOX_HEIGHT	190
#define PROCESS_BOX_WIDTH	343
#define PROCESS_BOX_HEIGHT	140
#define END_BOX_WIDTH		365
#define END_BOX_HEIGHT		260
#define DESKTOP_BOX_WIDTH	410	
#define DESKTOP_BOX_HEIGHT	350




typedef struct{
		int current_color;
		int current_widget;
		int current_item;
		WidgetColors	bn_colors;
		WidgetColors	sb_colors;
		WidgetColors	ed_colors;
		WidgetColors	ls_colors;
		WidgetColors	cb_colors;
		WidgetColors	mn_colors;
		WindowColors   	win_colors;
		WindowColors 	dlg_colors;
		IconColors	icn_colors;
		unsigned int desk_bg, desk_fg;
		
	      }WmColorsIndex;




typedef struct{
		XColor color;
		int current_color;
		int current_widget;
		int current_item;

		WidgetColors	bn_colors;
		WidgetColors	sb_colors;
		WidgetColors	ed_colors;
		WidgetColors	ls_colors;
		WidgetColors	cb_colors;
		WidgetColors	mn_colors;
		WindowColors   	win_colors;
		WindowColors 	dlg_colors;
		IconColors	icn_colors;
		unsigned long desk_bg, desk_fg;
		
	      }ColorManagement;






#endif


