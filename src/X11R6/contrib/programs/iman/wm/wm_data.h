/*
 *
 * 	wm_data.h
 * 	donnees du WM
 *
 * 	Modification :  27/01/94
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



#ifndef WM_DATA_H
#define WM_DATA_H


#ifdef WM_MAIN_C


 TkDisplay *tk_display;
 TkEvent  tk_event;
 WmInfo   wm_info;
 WindowStruct *windows;
 WmAction wm_action;
 WmPixmap wm_pixmap;
 WmFonts  wm_fonts;
 WmAtoms  wm_atoms;
 WmColorsIndex cl_index;
 ColorManagement cl_manager;
 ClipInfo clip_info;

 Colormap default_colormap;
 GC grab_gc, title_gc, border_gc, error_gc, kill_gc, icon_gc, resize_gc, desktop_gc;
 GC process_gc, about_gc, setup_gc, colors_gc, clipboard_gc, end_gc;

 Window wm_main_window,wm_error_window, wm_error_title, wm_resize_window;
 Window wm_kill_window, wm_desktop_window;
 Window desktop_motif_window, desktop_screensaver_window, desktop_paper_window,desktop_defaulticon_window;
 Window wm_colors_window;
 Window wm_process_window;
 Window wm_about_window, wm_setup_window;
 Window wm_clipboard_window;
 Window wm_end_window, wm_warning_window;
 Window color_button, color_edit, color_scroll, color_list, color_menu, color_window, color_icon,color_desktop, color_rgb,color_dialog;

 int wm_desktop_index=-1, wm_colors_index=-1, wm_kill_index=-1;
 int wm_process_index=-1, wm_about_index=-1, wm_setup_index=-1;
 int wm_clipboard_index=-1, wm_end_index=-1;
 int wm_warning_index=-1;

 ButtonID bn_end_ok, bn_end_cancel;
 ButtonID bn_error_ok, bn_error_cancel;
 ButtonID bn_kill_ok, bn_kill_cancel;
 ButtonID bn_desktop_ok, bn_desktop_cancel, bn_desktop_help, bn_desktop_colors, bn_desktop_keyboard,bn_desktop_about;
 ButtonID bn_desktop_motif, bn_desktop_screensaver_test, bn_desktop_screensaver_install,bn_desktop_clipboard;
 ButtonID bn_paper_center, bn_paper_mos;
 ButtonID bn_colors_ok, bn_colors_cancel, bn_colors_map, bn_colors_help;
 ButtonID bn_process_ok, bn_process_cancel;
 ButtonID bn_process_pid, bn_process_win, bn_process_kill, bn_process_close;
 ButtonID bn_process_focus, bn_process_iconify, bn_process_zoom;
 ButtonID bn_about_ok;
 ButtonID bn_setup_ok, bn_setup_cancel;
 ButtonID bn_setup_decoration, bn_setup_groups, bn_setup_icons, bn_setup_helpactive, bn_setup_debug,bn_setup_icontitle;
 ButtonID bn_colors_widgets[12];
 ButtonID bn_warning_ok, bn_warning_cancel;
 
 ScrollbarID sb_red, sb_green, sb_blue;
 ComboID  cb_desktop_motif, cb_desktop_screensaver, cb_desktop_paper;
 ComboID  cb_desktop_defaulticon;
 EditID   ed_desktop_screensaver; 
 ListID   ls_process_pid, ls_process_win, ls_colors_items, ls_colors_rgb_names;

 unsigned int numwindows, maxwindows;
 unsigned int numpixmotifs, numscreensavers, numpixicons, numpixpapers;

 Pixmap pix_focus, pix_close, pix_kill, pix_zoom, pix_iconify;
 Pixmap pix_motifs[100];
/* Pixmap pix_clipboard;*/

#ifdef NEED_XRM_RESOURCES
 XrmDatabase xrdb;
 XrmValue xrmvalue;
#endif

 Atom XA_CLIPBOARD;
 Atom XA_TARGETS;
 Atom XA_OWNER_OS;
 Atom XA_FILE_NAME;
 Atom XA_HOST_NAME;
 Atom XA_USER;
 Atom XA_PROCESS;
 Atom XA_TASK;
 Atom XA_CLIENT_WINDOW;


 char *xrm_file;
 unsigned int LANGUAGE;

#else

 extern TkDisplay *tk_display;
 extern TkEvent  tk_event;
 extern WmInfo   wm_info;
 extern WindowStruct *windows;
 extern WmAction wm_action;
 extern WmPixmap wm_pixmap;
 extern WmFonts  wm_fonts;
 extern WmAtoms  wm_atoms;
 extern WmColorsIndex cl_index;
 extern ColorManagement cl_manager;
 extern ClipInfo clip_info;

 extern Colormap default_colormap;
 extern GC grab_gc, title_gc, border_gc, error_gc, kill_gc, icon_gc, resize_gc, desktop_gc;
 extern GC process_gc, about_gc, setup_gc, colors_gc, clipboard_gc, end_gc;
 
 extern Window wm_main_window,wm_error_window, wm_error_title, wm_resize_window;
 extern Window wm_kill_window, wm_desktop_window;
 extern Window desktop_motif_window, desktop_screensaver_window, desktop_paper_window,desktop_defaulticon_window;
 extern Window wm_colors_window, wm_end_window;
 extern Window wm_process_window, wm_about_window, wm_setup_window;
 extern Window wm_clipboard_window, wm_warning_window;
extern Window color_button, color_edit, color_scroll, color_list, color_menu, color_window, color_icon, color_desktop, color_rgb,color_dialog;
 
 extern int wm_desktop_index, wm_colors_index, wm_kill_index;
 extern int wm_process_index, wm_about_index, wm_setup_index;
 extern wm_clipboard_index, wm_end_index;
 extern int wm_warning_index;

 extern ScrollbarID sb_red, sb_green, sb_blue;
 extern ButtonID bn_end_ok, bn_end_cancel;
 extern ButtonID bn_error_ok, bn_error_cancel;
 extern ButtonID bn_kill_ok, bn_kill_cancel;
 extern ButtonID bn_desktop_ok, bn_desktop_cancel, bn_desktop_help, bn_desktop_colors, bn_desktop_keyboard,bn_desktop_about;
 extern ButtonID bn_desktop_motif, bn_desktop_screensaver_test, bn_desktop_screensaver_install,bn_desktop_clipboard;
 extern ButtonID bn_paper_center, bn_paper_mos;
 extern ButtonID bn_colors_ok, bn_colors_cancel, bn_colors_map, bn_colors_help;
 extern ButtonID bn_process_ok, bn_process_cancel;
 extern ButtonID bn_process_pid, bn_process_win, bn_process_kill, bn_process_close;
 extern ButtonID bn_process_focus, bn_process_iconify, bn_process_zoom;
 extern ButtonID bn_about_ok;
 extern ButtonID bn_setup_ok, bn_setup_cancel;
 extern ButtonID bn_setup_decoration, bn_setup_groups,bn_setup_icons, bn_setup_debug, bn_setup_helpactive,bn_setup_icontitle;
 extern ButtonID bn_colors_widgets[12];
 extern ButtonID bn_warning_ok, bn_warning_cancel;

 extern ComboID  cb_desktop_motif, cb_desktop_screensaver, cb_desktop_paper;
 extern ComboID  cb_desktop_defaulticon;
 extern EditID   ed_desktop_screensaver; 
 extern ListID   ls_process_pid, ls_process_win, ls_colors_items, ls_colors_rgb_names;

 extern unsigned int numwindows, maxwindows;
 extern unsigned int numpixmotifs, numscreensavers, numpixicons, numpixpapers;

 extern Pixmap pix_focus, pix_close, pix_kill, pix_zoom, pix_iconify;
 extern Pixmap pix_motifs[100];
/* extern Pixmap pix_clipboard;*/

#ifdef NEED_XRM_RESOURCES
 extern XrmDatabase xrdb;
 extern XrmValue xrmvalue;
#endif

 extern Atom XA_CLIPBOARD;
 extern Atom XA_TARGETS;
 extern Atom XA_OWNER_OS;
 extern Atom XA_FILE_NAME;
 extern Atom XA_HOST_NAME;
 extern Atom XA_USER;
 extern Atom XA_PROCESS;
 extern Atom XA_TASK;
 extern Atom XA_CLIENT_WINDOW;


 extern char *xrm_file;
 extern unsigned int LANGUAGE;
 

#endif


#define NUMLANGUAGES 2


#ifdef WM_DRAW_C


#endif



#endif

