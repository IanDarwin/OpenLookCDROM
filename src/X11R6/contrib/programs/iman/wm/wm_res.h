/*
 *
 * 	wm_res.h
 * 	ressources du WM
 *
 * 	Modification :  02/05/94
 *
 *	Copyright (c) 1993,1994 Bruno RIVAS
 *	All Rights Reserved
 *
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
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



#ifndef WM_RESOURCES_H
#define WM_RESOURCES_H

#ifdef DSK_INIT_C

char *resource_text[]={
	"wm.colors.button.light",
	"wm.colors.button.shadow",
	"wm.colors.button.bg",
	"wm.colors.button.text",
	"wm.colors.button.focus",
	"wm.colors.button.textgrayed",
	"wm.colors.button.cross",
	"wm.colors.button.check",
	"wm.colors.button.radio",
	"wm.colors.button.radiolight",
	"wm.colors.scrollbar.highlight",
	"wm.colors.scrollbar.bg",
	"wm.colors.edit.bg",
	"wm.colors.edit.text",
	"wm.colors.edit.bgselected",
	"wm.colors.edit.textselected",
	"wm.colors.edit.textgrayed",
	"wm.colors.edit.cursor",
	"wm.colors.edit.focus",
	"wm.colors.list.bg",
	"wm.colors.list.cursor_bar",
	"wm.colors.list.text",
	"wm.colors.list.text_bar",
	"wm.colors.list.textgrayed",
	"wm.colors.list.text_bargrayed",
	"wm.colors.list.text_bar_nofocus",
	"wm.colors.list.cursor_bar_nofocus",
	"wm.colors.list.text_bargrayed_nofocus",
	"wm.colors.list.focus",
	"wm.colors.menu.bg",
	"wm.colors.menu.cursor_bar",
	"wm.colors.menu.text",
	"wm.colors.menu.text_bar",
	"wm.colors.menu.textgrayed",
	"wm.colors.menu.text_bargrayed",
	"wm.colors.menu.text_bar_nofocus",
	"wm.colors.menu.cursor_bar_nofocus",
	"wm.colors.menu.text_bargrayed_nofocus",
	"wm.colors.menu.focus",
	"wm.colors.window.title_bgactive",
	"wm.colors.window.title_bgunactive",
	"wm.colors.window.title_textactive",
	"wm.colors.window.title_textunactive",
	"wm.colors.window.border_active",
	"wm.colors.window.border_unactive",
	"wm.colors.window.win_light",
	"wm.colors.window.win_bg",
	"wm.colors.window.win_shadow",
	"wm.colors.window.win_text",
	"wm.colors.window.win_text_grayed",
	"wm.colors.window.dialog_light",
	"wm.colors.window.dialog_bg",
	"wm.colors.window.dialog_shadow",
	"wm.colors.window.dialog_text",
	"wm.colors.window.dialog_text_grayed",
	"wm.colors.icon.icn_bg",
	"wm.colors.icon.icn_light",
	"wm.colors.icon.icn_shadow",
	"wm.colors.icon.icn_draw",
	"wm.colors.icon.icn_draw_bg",
	"wm.colors.icon.title_bg_active",
	"wm.colors.icon.title_text_active",
	"wm.colors.icon.title_bg_unactive",
	"wm.colors.icon.title_bg_textunactive",
	"wm.colors.desktop.bg",
	"wm.colors.desktop.fg"
	};



char *options_text[]={
	"wm.options.decoration",
	"wm.options.groups",
	"wm.options.icons",
	"wm.options.icontitle",
	"wm.options.helpactive",
	"wm.options.debug"
	};


char *desktop_text[]={
	"wm.desktop.motif",
	"wm.desktop.defaulticon",
	"wm.desktop.screensaver",
	"wm.desktop.screensaver_time",
	"wm.desktop.paper",
	"wm.desktop.paper_drawing",
	"wm.desktop.language"
	};



char *fr_widget_colors_text[]={
	"Fond",
	"Avant-plan",
	"Eclairage",
	"Ombre",
	"Texte",
	"Texte grise",
	"Selection",
	"Selection inactive",
	"Texte selecte",
	"Texte grise selecte",
	"Texte selecte sans focus",
	"Texte grise selecte sans focus",
	"Curseur",
	"Focus",
	"Absence de focus",
	"Croix",
	"Check",
	"Fond du radio",
	"Eclairage du radio"
	};

char *fr_window_colors_text[]={
	"Fond",
	"Eclairage",
	"Ombre",
	"Texte",
	"Texte grise",
	"Bordure active",
	"Bordure inactive",
	"Barre de titre active",
	"Barre de titre inactive",
	"Texte du titre actif",
	"Texte du titre inactif"
	};


char *us_widget_colors_text[]={
	"Background",
	"Foreground",
	"Light",
	"Shadow",
	"Text",
	"Text grayed",
	"Selection",
	"Selection no focus",
	"Text selected",
	"Text grayed selected",
	"Text selected no focus",
	"Text grayed selected no focus",
	"Cursor",
	"Focus",
	"No focus",
	"Cross",
	"Check",
	"Radio background",
	"Radio lighting"
	};

char *us_window_colors_text[]={
	"Background",
	"Light",
	"Shadow",
	"Text",
	"Text grayed",
	"Border",
	"Border no focus",
	"Title bar",
	"Title bar no focus",
	"Title text",
	"Title text no focus"
	};



char *colors_desktop_text[]={
	"Foreground",
	"Background",
	"Barre de titre active",
	"Barre de titre inactive",
	"Titre actif",
	"Titre inactif",
	"Bordure active",
	"Bordure inactive",
	"Eclairage des fenetres",
	"Fond des fenetres",
	"Ombre des fenetres",
	"Texte dans les fenetres",
	"Texte grise dans les fenetres",
	"Eclairage des boites",
	"Fond des boites",
	"Ombre des boites",
	"Texte dans les boites",
	"Texte grise dans les boites"
	};


char *colors_button_text[]={
	"Fond",
	"Eclairage",
	"Ombre",
	"Texte",
	"Texte grise",
	"Focus",
	"Couleur de la croix",
/*	"Fond du bouton check",*/
	"Fond du bouton radio",
	"Eclairage du bouton radio"
	};


char *us_icon_colors_text[]={
	"Background",
	"Light",
	"Shadow",
	"Image foreground",
	"Image background",
	"Title bar",
	"Title text"
/*	"Title no focus",
	"Title text no focus"*/
	};


char *fr_icon_colors_text[]={
	"Fond",
	"Eclairage",
	"Ombrage",
	"Couleur de l'image",
	"Fond de l'image",
	"Barre de titre",
	"Texte du titre"
/*	"Titre inactif",
	"Texte du titre inactif"*/
	};



char *colors_scroll_text[]={
	"Fond",
	"Surlignage"
	};


char *colors_edit_text[]={
	"Fond",
	"Texte",
	"Fond selectionne",
	"Texte selectionne",
	"Texte grise",
	"Curseur",
	"Focus"
	};


char *colors_list_text[]={
	"Fond",
	"Texte",
	"Barre de curseur",
	"Texte de la barre",
	"Texte grise",
	"Texte grise dans la barre",
	"Barre de curseur inactive",
	"Texte dans la barre inactive",
	"Texte grise dans la barre inactive",
	"Focus"
	};



#else

extern char *resource_text[66];
extern char *options_text[6];
extern char *desktop_text[7];
extern char *colors_desktop_text[18];
extern char *colors_button_text[9];
extern char *colors_scroll_text[2];
extern char *colors_edit_text[7];
extern char *colors_list_text[10];
extern char *fr_widget_colors_text[19];
extern char *fr_window_colors_text[11];
extern char *fr_icon_colors_text[7];
extern char *us_widget_colors_text[19];
extern char *us_window_colors_text[11];
extern char *us_icon_colors_text[7];


#endif



#endif



