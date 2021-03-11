/* -*-C-*-
********************************************************************************
*
* File:         w_funtab.c
* RCS:          $Header: /users/npm/src/winterp/src-server/RCS/w_funtab.c,v 2.9 1994/06/06 15:40:59 npm Exp $
* Description:  Function table for winterp (replaces xlisp/xlftab.c).
*		NOTE: IF YOU WANT TO ADD NEW C-IMPLEMENTED FUNCTIONS TO
*		YOUR OWN LOCAL VERSION OF WINTERP, SEE FILES app_fundecl.h,
*		app_funidx.h, and app_funextn.h. EVENTUALLY, YOU MAY WANT TO
*		MIGRATE ESTABLISHED FUNCTIONALITY TO w_funtab.{c,h}... NOTE
*		HOWEVER, THAT EACH CHANGE TO w_funtab.h WILL ALSO FORCE A
*		MASSIVE RECOMPILE OF w_funtab.c AND wc_*.c tic_*.c -- ALL THOSE
*		SOURCES DEPEND ON VALUES FROM w_funtab.h THAT MAY HAVE CHANGED!!
* Author:       Niels Mayer
* Created:      Tue Jul 18 00:12:30 1989
* Modified:     Sun Jun  5 14:46:38 1994 (Niels Mayer) npm@indeed
* Language:     C
* Package:      N/A
* Status:       X11r6 contrib release
*
* Copyright (C) 1994, Enterprise Integration Technologies Corp. and Niels Mayer.
* WINTERP 1.15-1.99, Copyright (c) 1993, Niels P. Mayer.
* WINTERP 1.0-1.14, Copyright (c) 1989-1992 Hewlett-Packard Co. and Niels Mayer.
* 
* Permission to use, copy, modify, distribute, and sell this software and its
* documentation for any purpose is hereby granted without fee, provided that
* the above copyright notice appear in all copies and that both that
* copyright notice and this permission notice appear in supporting
* documentation, and that the name of Enterprise Integration Technologies,
* Hewlett-Packard Company, or Niels Mayer not be used in advertising or
* publicity pertaining to distribution of the software without specific,
* written prior permission. Enterprise Integration Technologies, Hewlett-Packard
* Company, and Niels Mayer makes no representations about the suitability of
* this software for any purpose.  It is provided "as is" without express or
* implied warranty.
* 
* ENTERPRISE INTEGRATION TECHNOLOGIES, HEWLETT-PACKARD COMPANY AND NIELS MAYER
* DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL IMPLIED
* WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL ENTERPRISE
* INTEGRATION TECHNOLOGIES, HEWLETT-PACKARD COMPANY OR NIELS MAYER BE LIABLE
* FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER
* RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF
* CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
* CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
*
********************************************************************************
*/
static char rcs_identity[] = "@(#)$Header: /users/npm/src/winterp/src-server/RCS/w_funtab.c,v 2.9 1994/06/06 15:40:59 npm Exp $";

/*
*------------------------------------------------------------------------------
* See ./winterp/COPYRIGHT for information on contacting the authors.
* Please e-mail comments, modifications, questions, improvements and
* bugfixes to the WINTERP mailing list winterp@netcom.com. Please send 
* mailing list subscribe/unsubscribe notices to winterp-request@netcom.com .
* Post XLISP-specific questions/information to the USENET newsgroup
* comp.lang.lisp.x.
*------------------------------------------------------------------------------
*/

#include <stdio.h>
#include <Xm/Xm.h>
#include "winterp.h"
#include "w_funtab.h"

/*
 * To embed additional C-implemented primitives in WINTERP, you must
 * place application function extern declarations in file app_funextn.h. 
 * See comments preceding funtab[] declaration below for more
 * info on the files app_fundecl.h, app_funextn.h, and app_funidx.h .
 *
 * If you want to place your app_funextn.h file outside of 
 * the WINTERP source tree, just modify INCLUDES in the Makefile
 * such that the directory containing app_funextn.h precedes '-I.'
 */
#include <app_funextn.h>


/* SUBR/FSUBR indicator */
#define S	SUBR
#define F	FSUBR

/*
 * note that xlisp prims are extern'd in xlisp/xlftab.h; the file
 * also defined cpp sym 'V' which is either 'void' or '' if not ANSI...
 */
#include "xlisp/xlftab.h"

/****************** BEGIN: WINTERP PRIMITIVES (Motif >= 1.0)  **************/    
extern LVAL Application_Shell_Widget_Class_Method_GET_ARGV(V);
extern LVAL Application_Shell_Widget_Class_Method_ISNEW(V);
extern LVAL Application_Shell_Widget_Class_Method_SET_ARGV(V);
extern LVAL Popup_Shell_Widget_Class_Method_ISNEW(V);
extern LVAL Popup_Shell_Widget_Class_Method_POPDOWN(V);
extern LVAL Popup_Shell_Widget_Class_Method_POPUP(V);
extern LVAL Shell_Widget_Class_Method_ISNEW(V);
extern LVAL Shell_Widget_Class_Method_IS_MOTIF_WM_RUNNING(V);
extern LVAL Shell_Widget_Class_Method_LOWER_WINDOW(V);
extern LVAL Shell_Widget_Class_Method_MANAGE(V);
extern LVAL Shell_Widget_Class_Method_MAP_RAISED(V);
extern LVAL Shell_Widget_Class_Method_RAISE_WINDOW(V);
extern LVAL Shell_Widget_Class_Method_REALIZE(V);
extern LVAL Shell_Widget_Class_Method_UNMANAGE(V);
extern LVAL Shell_Widget_Class_Method_UNREALIZE(V);
extern LVAL Widget_Class_Method_ADD_CALLBACK(V);
extern LVAL Widget_Class_Method_ADD_EVENT_HANDLER(V);
extern LVAL Widget_Class_Method_ADD_GRAB(V);
extern LVAL Widget_Class_Method_ADD_TAB_GROUP(V);
extern LVAL Widget_Class_Method_AUGMENT_TRANSLATIONS(V);
extern LVAL Widget_Class_Method_BUILD_EVENT_MASK(V);
extern LVAL Widget_Class_Method_DESTROY(V);
extern LVAL Widget_Class_Method_EXISTS_P(V);
extern LVAL Widget_Class_Method_GET_VALUES(V);
extern LVAL Widget_Class_Method_HAS_CALLBACKS(V);
extern LVAL Widget_Class_Method_INSTALL_ACCELERATORS(V);
extern LVAL Widget_Class_Method_INSTALL_ALL_ACCELERATORS(V);
extern LVAL Widget_Class_Method_ISNEW(V);
extern LVAL Widget_Class_Method_IS_COMPOSITE(V);
extern LVAL Widget_Class_Method_IS_CONSTRAINT(V);
extern LVAL Widget_Class_Method_IS_GADGET(V);
extern LVAL Widget_Class_Method_IS_MANAGED(V);
extern LVAL Widget_Class_Method_IS_MANAGER(V);
extern LVAL Widget_Class_Method_IS_PRIMITIVE(V);
extern LVAL Widget_Class_Method_IS_REALIZED(V);
extern LVAL Widget_Class_Method_IS_SENSITIVE(V);
extern LVAL Widget_Class_Method_IS_SHELL(V);
extern LVAL Widget_Class_Method_MANAGE(V);
extern LVAL Widget_Class_Method_MAP(V);
extern LVAL Widget_Class_Method_OVERRIDE_TRANSLATIONS(V);
extern LVAL Widget_Class_Method_PARENT(V);
extern LVAL Widget_Class_Method_PRIN1(V);
extern LVAL Widget_Class_Method_REMOVE_ALL_CALLBACKS(V);
extern LVAL Widget_Class_Method_REMOVE_GRAB(V);
extern LVAL Widget_Class_Method_REMOVE_TAB_GROUP(V);
extern LVAL Widget_Class_Method_SET_CALLBACK(V);
extern LVAL Widget_Class_Method_SET_EVENT_HANDLER(V);
extern LVAL Widget_Class_Method_SET_MAPPED_WHEN_MANAGED(V);
extern LVAL Widget_Class_Method_SET_SENSITIVE(V);
extern LVAL Widget_Class_Method_SET_VALUES(V);
extern LVAL Widget_Class_Method_UNINSTALL_TRANSLATIONS(V);
extern LVAL Widget_Class_Method_UNMANAGE(V);
extern LVAL Widget_Class_Method_UNMAP(V);
extern LVAL Widget_Class_Method_UPDATE_DISPLAY(V);
extern LVAL Widget_Class_Method_WINDOW(V);
extern LVAL Xm_Bulletin_Board_Widget_Class_Method_ISNEW(V);
extern LVAL Xm_Cascade_Button_Widget_Class_Method_HIGHLIGHT(V);
extern LVAL Xm_Command_Widget_Class_Method_ADD_CALLBACK(V);
extern LVAL Xm_Command_Widget_Class_Method_APPEND_VALUE(V);
extern LVAL Xm_Command_Widget_Class_Method_ERROR(V);
extern LVAL Xm_Command_Widget_Class_Method_GET_CHILD(V);
extern LVAL Xm_Command_Widget_Class_Method_GET_HISTORY_ITEMS(V);
extern LVAL Xm_Command_Widget_Class_Method_ISNEW(V);
extern LVAL Xm_Command_Widget_Class_Method_SET_CALLBACK(V);
extern LVAL Xm_Command_Widget_Class_Method_SET_VALUE(V);
extern LVAL Xm_Drawing_Area_Widget_Class_Method_ADD_CALLBACK(V);
extern LVAL Xm_Drawing_Area_Widget_Class_Method_SET_CALLBACK(V);
extern LVAL Xm_Drawn_Button_Widget_Class_Method_ADD_CALLBACK(V);
extern LVAL Xm_Drawn_Button_Widget_Class_Method_SET_CALLBACK(V);
extern LVAL Xm_File_Selection_Box_Widget_Class_Method_ADD_CALLBACK(V);
extern LVAL Xm_File_Selection_Box_Widget_Class_Method_DO_SEARCH(V);
extern LVAL Xm_File_Selection_Box_Widget_Class_Method_GET_CHILD(V);
extern LVAL Xm_File_Selection_Box_Widget_Class_Method_ISNEW(V);
extern LVAL Xm_File_Selection_Box_Widget_Class_Method_SET_CALLBACK(V);
extern LVAL Xm_Form_Widget_Class_Method_ISNEW(V);
extern LVAL Xm_List_Widget_Class_Method_ADD_CALLBACK(V);
extern LVAL Xm_List_Widget_Class_Method_ADD_ITEM(V);
extern LVAL Xm_List_Widget_Class_Method_ADD_ITEM_UNSELECTED(V);
extern LVAL Xm_List_Widget_Class_Method_DELETE_ITEM(V);
extern LVAL Xm_List_Widget_Class_Method_DELETE_POS(V);
extern LVAL Xm_List_Widget_Class_Method_DESELECT_ALL_ITEMS(V);
extern LVAL Xm_List_Widget_Class_Method_DESELECT_ITEM(V);
extern LVAL Xm_List_Widget_Class_Method_DESELECT_POS(V);
extern LVAL Xm_List_Widget_Class_Method_GET_ITEMS(V);
extern LVAL Xm_List_Widget_Class_Method_GET_SELECTED_ITEMS(V);
extern LVAL Xm_List_Widget_Class_Method_ISNEW(V);
extern LVAL Xm_List_Widget_Class_Method_ITEM_EXISTS(V);
extern LVAL Xm_List_Widget_Class_Method_SELECT_ITEM(V);
extern LVAL Xm_List_Widget_Class_Method_SELECT_POS(V);
extern LVAL Xm_List_Widget_Class_Method_SET_BOTTOM_ITEM(V);
extern LVAL Xm_List_Widget_Class_Method_SET_BOTTOM_POS(V);
extern LVAL Xm_List_Widget_Class_Method_SET_CALLBACK(V);
extern LVAL Xm_List_Widget_Class_Method_SET_HORIZ_POS(V);
extern LVAL Xm_List_Widget_Class_Method_SET_ITEM(V);
extern LVAL Xm_List_Widget_Class_Method_SET_POS(V);
extern LVAL Xm_Main_Window_Widget_Class_Method_SEP1(V);
extern LVAL Xm_Main_Window_Widget_Class_Method_SEP2(V);
extern LVAL Xm_Main_Window_Widget_Class_Method_SET_AREAS(V);
extern LVAL Xm_Message_Box_Widget_Class_Method_GET_CHILD(V);
extern LVAL Xm_Message_Box_Widget_Class_Method_ISNEW(V);
extern LVAL Xm_Row_Column_Widget_Class_Method_ADD_CALLBACK(V);
extern LVAL Xm_Row_Column_Widget_Class_Method_ISNEW(V);
extern LVAL Xm_Row_Column_Widget_Class_Method_MENU_POSITION(V);
extern LVAL Xm_Row_Column_Widget_Class_Method_OPTION_BUTTON_GADGET(V);
extern LVAL Xm_Row_Column_Widget_Class_Method_OPTION_LABEL_GADGET(V);
extern LVAL Xm_Row_Column_Widget_Class_Method_SET_CALLBACK(V);
extern LVAL Xm_Scale_Widget_Class_Method_ADD_CALLBACK(V);
extern LVAL Xm_Scale_Widget_Class_Method_GET_VALUE(V);
extern LVAL Xm_Scale_Widget_Class_Method_SET_CALLBACK(V);
extern LVAL Xm_Scale_Widget_Class_Method_SET_VALUE(V);
extern LVAL Xm_Scroll_Bar_Widget_Class_Method_ADD_CALLBACK(V);
extern LVAL Xm_Scroll_Bar_Widget_Class_Method_GET_VALUE(V);
extern LVAL Xm_Scroll_Bar_Widget_Class_Method_SET_CALLBACK(V);
extern LVAL Xm_Scroll_Bar_Widget_Class_Method_SET_VALUE(V);
extern LVAL Xm_Scrolled_Window_Widget_Class_Method_SET_AREAS(V);
extern LVAL Xm_Selection_Box_Widget_Class_Method_ADD_CALLBACK(V);
extern LVAL Xm_Selection_Box_Widget_Class_Method_GET_CHILD(V);
extern LVAL Xm_Selection_Box_Widget_Class_Method_GET_LIST_ITEMS(V);
extern LVAL Xm_Selection_Box_Widget_Class_Method_ISNEW(V);
extern LVAL Xm_Selection_Box_Widget_Class_Method_SET_CALLBACK(V);
extern LVAL Xm_Text_Widget_Class_Method_ADD_CALLBACK(V);
extern LVAL Xm_Text_Widget_Class_Method_CLEAR_SELECTION(V);
extern LVAL Xm_Text_Widget_Class_Method_DISABLE_REDISPLAY(V);
extern LVAL Xm_Text_Widget_Class_Method_ENABLE_REDISPLAY(V);
extern LVAL Xm_Text_Widget_Class_Method_GET_EDITABLE(V);
extern LVAL Xm_Text_Widget_Class_Method_GET_INSERTION_POSITION(V);
extern LVAL Xm_Text_Widget_Class_Method_GET_LAST_POSITION(V);
extern LVAL Xm_Text_Widget_Class_Method_GET_MAX_LENGTH(V);
extern LVAL Xm_Text_Widget_Class_Method_GET_SELECTION(V);
extern LVAL Xm_Text_Widget_Class_Method_GET_SELECTION_POSITION(V);
extern LVAL Xm_Text_Widget_Class_Method_GET_STRING(V);
extern LVAL Xm_Text_Widget_Class_Method_GET_TOP_CHARACTER(V);
extern LVAL Xm_Text_Widget_Class_Method_GOTO_LINE(V);
extern LVAL Xm_Text_Widget_Class_Method_ISNEW(V);
extern LVAL Xm_Text_Widget_Class_Method_POS_TO_XY(V);
extern LVAL Xm_Text_Widget_Class_Method_READ_FILE(V);
extern LVAL Xm_Text_Widget_Class_Method_READ_FILE_GOTO_LINE(V);
extern LVAL Xm_Text_Widget_Class_Method_REPLACE(V);
extern LVAL Xm_Text_Widget_Class_Method_SCROLL(V);
extern LVAL Xm_Text_Widget_Class_Method_SEARCH(V);
extern LVAL Xm_Text_Widget_Class_Method_SET_CALLBACK(V);
extern LVAL Xm_Text_Widget_Class_Method_SET_EDITABLE(V);
extern LVAL Xm_Text_Widget_Class_Method_SET_INSERTION_POSITION(V);
extern LVAL Xm_Text_Widget_Class_Method_SET_MAX_LENGTH(V);
extern LVAL Xm_Text_Widget_Class_Method_SET_SELECTION(V);
extern LVAL Xm_Text_Widget_Class_Method_SET_STRING(V);
extern LVAL Xm_Text_Widget_Class_Method_SET_TOP_CHARACTER(V);
extern LVAL Xm_Text_Widget_Class_Method_SHOW_POSITION(V);
extern LVAL Xm_Text_Widget_Class_Method_WRITE_FILE(V);
extern LVAL Xm_Text_Widget_Class_Method_XY_TO_POS(V);
extern LVAL Xm_Toggle_Button_Gadget_Class_Method_GET_STATE(V);
extern LVAL Xm_Toggle_Button_Gadget_Class_Method_SET_STATE(V);
extern LVAL Xm_Toggle_Button_Widget_Class_Method_ADD_CALLBACK(V);
extern LVAL Xm_Toggle_Button_Widget_Class_Method_GET_STATE(V);
extern LVAL Xm_Toggle_Button_Widget_Class_Method_SET_CALLBACK(V);
extern LVAL Xm_Toggle_Button_Widget_Class_Method_SET_STATE(V);
extern LVAL Widget_Class_Method_SHOW(V);

extern LVAL Prim_FFLUSH(V);	/* NPM: xlisp/unixstuf.c */
extern LVAL Prim_FSCANF_FIXNUM(V); /* NPM: xlisp/unixstuf.c */
extern LVAL Prim_FSCANF_FLONUM(V); /* NPM: xlisp/unixstuf.c */
extern LVAL Prim_FSCANF_STRING(V); /* NPM: xlisp/unixstuf.c */
extern LVAL Prim_PCLOSE(V);	/* NPM: xlisp/unixstuf.c */
extern LVAL Prim_POPEN(V);	/* NPM: xlisp/unixstuf.c */
extern LVAL Prim_PROVIDE(V);
extern LVAL Prim_READ_EVAL_PRINT(V);
extern LVAL Prim_REDIRECT_STDERR(V);
extern LVAL Prim_REDIRECT_STDOUT(V);
extern LVAL Prim_SYSTEM(V);	/* NPM: xlisp/unixstuf.c */
extern LVAL Wcb_Prim_XT_REMOVE_CALLBACK(V);
extern LVAL Wcls_Prim_WIDGETOBJP(V);
extern LVAL Weh_Prim_XT_REMOVE_EVENT_HANDLER(V);
extern LVAL Prim_REQUIRE(V);
extern LVAL Wicb_Prim_INPUT_ACTIVE_P(V);
extern LVAL Wicb_Prim_XT_ADD_INPUT(V);
extern LVAL Wicb_Prim_XT_REMOVE_INPUT(V);
extern LVAL Wpm_Prim_XM_GET_IMAGE_FROM_FILE(V);
extern LVAL Wpm_Prim_XM_GET_PIXMAP(V);
extern LVAL Wpm_Prim_XM_INSTALL_IMAGE(V);
extern LVAL Wpm_Prim_XM_UNINSTALL_IMAGE(V);
extern LVAL Wshl_Prim_WINTERP_SHOW_BUSY(V);
extern LVAL Wto_Prim_TIMEOUT_ACTIVE_P(V);
extern LVAL Wto_Prim_XT_ADD_TIMEOUT(V);
extern LVAL Wto_Prim_XT_REMOVE_TIMEOUT(V);
extern LVAL Wtx_Prim_XT_PARSE_ACCELERATOR_TABLE(V);
extern LVAL Wtx_Prim_XT_PARSE_TRANSLATION_TABLE(V);
extern LVAL Wut_Prim_GET_EVENT_COORDS(V);
extern LVAL Wut_Prim_GET_MOUSED_WIDGET(V);
extern LVAL Wut_Prim_GET_MOUSE_LOCATION(V); /* uunet!cimshop!rhess */
extern LVAL Wut_Prim_X_ALLOC_COLOR(V);
extern LVAL Wut_Prim_X_ALLOC_N_COLOR_CELLS_NO_PLANES(V);
extern LVAL Wut_Prim_X_STORE_COLOR(V);
extern LVAL Wut_Prim_X_BELL(V);
extern LVAL Wxm_Prim_XM_SET_MENU_CURSOR(V);
extern LVAL Wxms_Prim_XM_STRING_BYTE_COMPARE(V);
extern LVAL Wxms_Prim_XM_STRING_COMPARE(V);
extern LVAL Wxms_Prim_XM_STRING_CONCAT(V);
extern LVAL Wxms_Prim_XM_STRING_COPY(V);
extern LVAL Wxms_Prim_XM_STRING_CREATE(V);
extern LVAL Wxms_Prim_XM_STRING_CREATE_L_TO_R(V);
extern LVAL Wxms_Prim_XM_STRING_DIRECTION_CREATE(V);
extern LVAL Wxms_Prim_XM_STRING_EMPTY(V);
extern LVAL Wxms_Prim_XM_STRING_GET_L_TO_R(V);
extern LVAL Wxms_Prim_XM_STRING_LENGTH(V);
extern LVAL Wxms_Prim_XM_STRING_LINE_COUNT(V);
extern LVAL Wxms_Prim_XM_STRING_SEGMENT_CREATE(V);
extern LVAL Wxms_Prim_XM_STRING_SEPARATOR_CREATE(V);
extern LVAL Wxt_Prim_XT_MANAGE_CHILDREN(V);
extern LVAL Wxt_Prim_XT_UNMANAGE_CHILDREN(V);
/****************** END: WINTERP PRIMITIVES (Motif >= 1.0)  **************/    

/****************** BEGIN: WINTERP PRIMITIVES (Motif >= 1.1) *************/
#ifdef WINTERP_MOTIF_11
extern LVAL Widget_Class_Method_CALL_ACTION_PROC(V);
extern LVAL Widget_Class_Method_FORCED_EXPOSE_UPDATE(V);
extern LVAL Widget_Class_Method_GET_CHILDREN(V);
extern LVAL Widget_Class_Method_NAME(V);
extern LVAL Widget_Class_Method_PROCESS_TRAVERSAL(V);
extern LVAL Xm_Arrow_Button_Widget_Class_Method_ADD_CALLBACK(V);
extern LVAL Xm_Arrow_Button_Widget_Class_Method_SET_CALLBACK(V);
extern LVAL Xm_Cascade_Button_Gadget_Class_Method_HIGHLIGHT(V);
extern LVAL Xm_File_Selection_Box_Widget_Class_Method_GET_DIR_LIST_ITEMS(V);
extern LVAL Xm_File_Selection_Box_Widget_Class_Method_GET_FILE_LIST_ITEMS(V);
extern LVAL Xm_List_Widget_Class_Method_ADD_ITEMS(V);
extern LVAL Xm_List_Widget_Class_Method_DELETE_ALL_ITEMS(V);
extern LVAL Xm_List_Widget_Class_Method_DELETE_ITEMS(V);
extern LVAL Xm_List_Widget_Class_Method_DELETE_ITEMS_POS(V);
extern LVAL Xm_List_Widget_Class_Method_GET_MATCH_POS(V);
extern LVAL Xm_List_Widget_Class_Method_GET_SELECTED_POS(V);
extern LVAL Xm_List_Widget_Class_Method_ITEM_POS(V);
extern LVAL Xm_List_Widget_Class_Method_PARENT(V);
extern LVAL Xm_List_Widget_Class_Method_REPLACE_ITEMS(V);
extern LVAL Xm_List_Widget_Class_Method_REPLACE_ITEMS_POS(V);
extern LVAL Xm_List_Widget_Class_Method_SET_ADD_MODE(V);
extern LVAL Xm_Main_Window_Widget_Class_Method_SEP3(V);
extern LVAL Xm_Push_Button_Widget_Class_Method_ADD_CALLBACK(V);
extern LVAL Xm_Push_Button_Widget_Class_Method_SET_CALLBACK(V);
extern LVAL Xm_Row_Column_Widget_Class_Method_GET_POSTED_FROM_WIDGET(V);
extern LVAL Xm_Row_Column_Widget_Class_Method_GET_SUB_MENU_WIDGET(V);
extern LVAL Xm_Text_Field_Widget_Class_Method_CLEAR_SELECTION(V);
extern LVAL Xm_Text_Field_Widget_Class_Method_COPY(V);
extern LVAL Xm_Text_Field_Widget_Class_Method_CUT(V);
extern LVAL Xm_Text_Field_Widget_Class_Method_GET_ADD_MODE(V);
extern LVAL Xm_Text_Field_Widget_Class_Method_GET_BASELINE(V);
extern LVAL Xm_Text_Field_Widget_Class_Method_GET_CURSOR_POSITION(V);
extern LVAL Xm_Text_Field_Widget_Class_Method_GET_EDITABLE(V);
extern LVAL Xm_Text_Field_Widget_Class_Method_GET_INSERTION_POSITION(V);
extern LVAL Xm_Text_Field_Widget_Class_Method_GET_LAST_POSITION(V);
extern LVAL Xm_Text_Field_Widget_Class_Method_GET_MAX_LENGTH(V);
extern LVAL Xm_Text_Field_Widget_Class_Method_GET_SELECTION(V);
extern LVAL Xm_Text_Field_Widget_Class_Method_GET_SELECTION_POSITION(V);
extern LVAL Xm_Text_Field_Widget_Class_Method_GET_STRING(V);
extern LVAL Xm_Text_Field_Widget_Class_Method_INSERT(V);
extern LVAL Xm_Text_Field_Widget_Class_Method_PASTE(V);
extern LVAL Xm_Text_Field_Widget_Class_Method_POS_TO_XY(V);
extern LVAL Xm_Text_Field_Widget_Class_Method_REMOVE(V);
extern LVAL Xm_Text_Field_Widget_Class_Method_REPLACE(V);
extern LVAL Xm_Text_Field_Widget_Class_Method_SET_ADD_MODE(V);
extern LVAL Xm_Text_Field_Widget_Class_Method_SET_CURSOR_POSITION(V);
extern LVAL Xm_Text_Field_Widget_Class_Method_SET_EDITABLE(V);
extern LVAL Xm_Text_Field_Widget_Class_Method_SET_HIGHLIGHT(V);
extern LVAL Xm_Text_Field_Widget_Class_Method_SET_INSERTION_POSITION(V);
extern LVAL Xm_Text_Field_Widget_Class_Method_SET_MAX_LENGTH(V);
extern LVAL Xm_Text_Field_Widget_Class_Method_SET_SELECTION(V);
extern LVAL Xm_Text_Field_Widget_Class_Method_SET_STRING(V);
extern LVAL Xm_Text_Field_Widget_Class_Method_SHOW_POSITION(V);
extern LVAL Xm_Text_Field_Widget_Class_Method_XY_TO_POS(V);
extern LVAL Xm_Text_Widget_Class_Method_COPY(V);
extern LVAL Xm_Text_Widget_Class_Method_CUT(V);
extern LVAL Xm_Text_Widget_Class_Method_GET_ADD_MODE(V);
extern LVAL Xm_Text_Widget_Class_Method_GET_BASELINE(V);
extern LVAL Xm_Text_Widget_Class_Method_GET_CURSOR_POSITION(V);
extern LVAL Xm_Text_Widget_Class_Method_INSERT(V);
extern LVAL Xm_Text_Widget_Class_Method_PARENT(V);
extern LVAL Xm_Text_Widget_Class_Method_PASTE(V);
extern LVAL Xm_Text_Widget_Class_Method_REMOVE(V);
extern LVAL Xm_Text_Widget_Class_Method_SET_ADD_MODE(V);
extern LVAL Xm_Text_Widget_Class_Method_SET_CURSOR_POSITION(V);
extern LVAL Xm_Text_Widget_Class_Method_SET_HIGHLIGHT(V);

extern LVAL Wxm_Prim_XM_CONVERT_UNITS(V);
extern LVAL Wxm_Prim_XM_GET_COLORS(V);
extern LVAL Wxm_Prim_XM_SET_FONT_UNITS(V);
extern LVAL Wxm_Prim_XM_TRACKING_LOCATE(V);
extern LVAL Wxms_Prim_XM_CVT_CT_TO_XM_STRING(V);
extern LVAL Wxms_Prim_XM_CVT_XM_STRING_TO_CT(V);
extern LVAL Wxms_Prim_XM_STRING_HAS_SUBSTRING(V);
extern LVAL Wxt_Prim_XT_RESOLVE_PATHNAME(V);
#endif				/* WINTERP_MOTIF_11 */
/****************** END: WINTERP PRIMITIVES (Motif >= 1.1) *************/

/****************** BEGIN: WINTERP PRIMITIVES (Motif >= 1.2) *************/
#ifdef WINTERP_MOTIF_12
extern LVAL Widget_Class_Method_CHANGE_COLOR(V);
extern LVAL Wxm_Prim_XM_GET_DESTINATION(V);
extern LVAL Xm_Scrolled_Window_Widget_Class_Method_SCROLL_VISIBLE(V);
#endif /* WINTERP_MOTIF_12 */
/****************** END: WINTERP PRIMITIVES (Motif >= 1.2) *************/

/****************** BEGIN: WINTERP XmGraph PRIMITIVES (Motif >= 1.1) ***/
#ifdef HP_GRAPH_WIDGET		/* if HP_GRAPH_WIDGET defined */
extern LVAL Xm_Arc_Widget_Class_Method_ISNEW(V);
extern LVAL Xm_Graph_Widget_Class_Method_ADD_CALLBACK(V);
extern LVAL Xm_Graph_Widget_Class_Method_CENTER_AROUND_WIDGET(V);
extern LVAL Xm_Graph_Widget_Class_Method_DESTROY_ALL_ARCS(V);
extern LVAL Xm_Graph_Widget_Class_Method_DESTROY_ALL_NODES(V);
extern LVAL Xm_Graph_Widget_Class_Method_DESTROY_SELECTED_ARCS_OR_NODES(V);
extern LVAL Xm_Graph_Widget_Class_Method_GET_ARCS(V);
extern LVAL Xm_Graph_Widget_Class_Method_GET_ARCS_BETWEEN_NODES(V);
extern LVAL Xm_Graph_Widget_Class_Method_GET_ARC_NODES(V);
extern LVAL Xm_Graph_Widget_Class_Method_GET_NODES(V);
extern LVAL Xm_Graph_Widget_Class_Method_GET_NODE_ARCS(V);
extern LVAL Xm_Graph_Widget_Class_Method_GET_ROOTS(V);
extern LVAL Xm_Graph_Widget_Class_Method_GET_SELECTED_ARCS(V);
extern LVAL Xm_Graph_Widget_Class_Method_GET_SELECTED_NODES(V);
extern LVAL Xm_Graph_Widget_Class_Method_INPUT_OVER_ARC(V);
extern LVAL Xm_Graph_Widget_Class_Method_INSERT_ROOTS(V);
extern LVAL Xm_Graph_Widget_Class_Method_ISNEW(V);
extern LVAL Xm_Graph_Widget_Class_Method_IS_POINT_IN_ARC(V);
extern LVAL Xm_Graph_Widget_Class_Method_IS_SELECTED_ARC(V);
extern LVAL Xm_Graph_Widget_Class_Method_IS_SELECTED_NODE(V);
extern LVAL Xm_Graph_Widget_Class_Method_LAYOUT(V);
extern LVAL Xm_Graph_Widget_Class_Method_MOVE_ALL(V);
extern LVAL Xm_Graph_Widget_Class_Method_MOVE_ARC(V);
extern LVAL Xm_Graph_Widget_Class_Method_MOVE_NODE(V);
extern LVAL Xm_Graph_Widget_Class_Method_NUM_ARCS(V);
extern LVAL Xm_Graph_Widget_Class_Method_NUM_NODES(V);
extern LVAL Xm_Graph_Widget_Class_Method_NUM_NODE_ARCS(V);
extern LVAL Xm_Graph_Widget_Class_Method_NUM_ROOTS(V);
extern LVAL Xm_Graph_Widget_Class_Method_NUM_SELECTED_ARCS(V);
extern LVAL Xm_Graph_Widget_Class_Method_NUM_SELECTED_NODES(V);
extern LVAL Xm_Graph_Widget_Class_Method_RELAY_SUBGRAPH(V);
extern LVAL Xm_Graph_Widget_Class_Method_REMOVE_ARC_BETWEEN_NODES(V);
extern LVAL Xm_Graph_Widget_Class_Method_REMOVE_ROOTS(V);
extern LVAL Xm_Graph_Widget_Class_Method_SELECT_ARC(V);
extern LVAL Xm_Graph_Widget_Class_Method_SELECT_ARCS(V);
extern LVAL Xm_Graph_Widget_Class_Method_SELECT_NODE(V);
extern LVAL Xm_Graph_Widget_Class_Method_SELECT_NODES(V);
extern LVAL Xm_Graph_Widget_Class_Method_SET_CALLBACK(V);
extern LVAL Xm_Graph_Widget_Class_Method_UNSELECT_ARC(V);
extern LVAL Xm_Graph_Widget_Class_Method_UNSELECT_ARCS(V);
extern LVAL Xm_Graph_Widget_Class_Method_UNSELECT_NODE(V);
extern LVAL Xm_Graph_Widget_Class_Method_UNSELECT_NODES(V);
#endif				/* HP_GRAPH_WIDGET */
/****************** END: WINTERP XmGraph PRIMITIVES  *****************/

/****************** BEGIN: WINTERP Xtango PRIMITIVES  ****************/
#ifdef WINTERP_XTANGO_WIDGET
extern LVAL Tango_Bitmap_Image_Class_Method_ISNEW(V);
extern LVAL Tango_Bitmap_Image_Class_Method_STOREON(V);
extern LVAL Tango_Bitmap_Image_Class_Method_TX_SHUFFLE(V);
extern LVAL Tango_Circle_Image_Class_Method_ISNEW(V);
extern LVAL Tango_Circle_Image_Class_Method_STOREON(V);
extern LVAL Tango_Composite_Image_Class_Method_ISNEW(V);
extern LVAL Tango_Composite_Image_Class_Method_STOREON(V);
extern LVAL Tango_Ellipse_Image_Class_Method_ISNEW(V);
extern LVAL Tango_Ellipse_Image_Class_Method_STOREON(V);
extern LVAL Tango_Image_Class_Method_EXISTS_P(V);
extern LVAL Tango_Image_Class_Method_IMAGE_COPY(V);
extern LVAL Tango_Image_Class_Method_ISNEW(V);
extern LVAL Tango_Image_Class_Method_PRIN1(V);
extern LVAL Tango_Image_Class_Method_TAP_COLOR(V);
extern LVAL Tango_Image_Class_Method_TAP_FILL(V);
extern LVAL Tango_Image_Class_Method_TAP_FLASH(V);
extern LVAL Tango_Image_Class_Method_TAP_JUMP(V);
extern LVAL Tango_Image_Class_Method_TAP_MOVE(V);
extern LVAL Tango_Image_Class_Method_TAP_SHOW(V);
extern LVAL Tango_Image_Class_Method_TAP_TRAVERSE(V);
extern LVAL Tango_Image_Class_Method_TAP_VIS_TOGGLE(V);
extern LVAL Tango_Image_Class_Method_TX_COLOR(V);
extern LVAL Tango_Image_Class_Method_TX_DELAY(V);
extern LVAL Tango_Image_Class_Method_TX_DELETE(V);
extern LVAL Tango_Image_Class_Method_TX_FILL(V);
extern LVAL Tango_Image_Class_Method_TX_LOWER(V);
extern LVAL Tango_Image_Class_Method_TX_MOVE(V);
extern LVAL Tango_Image_Class_Method_TX_RAISE(V);
extern LVAL Tango_Image_Class_Method_TX_REFRESH(V);
extern LVAL Tango_Image_Class_Method_TX_VISIBLE(V);
extern LVAL Tango_Image_Class_Method_TX_ZOOM(V);
extern LVAL Tango_Line_Image_Class_Method_ISNEW(V);
extern LVAL Tango_Line_Image_Class_Method_STOREON(V);
extern LVAL Tango_Non_Poly_Image_Class_Method_IMAGE_LOC(V);
extern LVAL Tango_Non_Poly_Image_Class_Method_TX_RESIZE(V);
extern LVAL Tango_Poly_Image_Class_Method_TX_GRAB1(V);
extern LVAL Tango_Poly_Image_Class_Method_TX_GRAB2(V);
extern LVAL Tango_Poly_Image_Class_Method_TX_GRAB3(V);
extern LVAL Tango_Poly_Image_Class_Method_TX_GRAB4(V);
extern LVAL Tango_Poly_Image_Class_Method_TX_GRAB5(V);
extern LVAL Tango_Poly_Image_Class_Method_TX_GRAB6(V);
extern LVAL Tango_Poly_Image_Class_Method_TX_GRAB7(V);
extern LVAL Tango_Poly_Image_Class_Method_TX_RESIZE1(V);
extern LVAL Tango_Poly_Image_Class_Method_TX_RESIZE2(V);
extern LVAL Tango_Poly_Image_Class_Method_TX_RESIZE3(V);
extern LVAL Tango_Poly_Image_Class_Method_TX_RESIZE4(V);
extern LVAL Tango_Poly_Image_Class_Method_TX_RESIZE5(V);
extern LVAL Tango_Poly_Image_Class_Method_TX_RESIZE6(V);
extern LVAL Tango_Poly_Image_Class_Method_TX_RESIZE7(V);
extern LVAL Tango_Polygon_Image_Class_Method_IMAGE_LOC(V);
extern LVAL Tango_Polygon_Image_Class_Method_ISNEW(V);
extern LVAL Tango_Polygon_Image_Class_Method_STOREON(V);
extern LVAL Tango_Polyline_Image_Class_Method_IMAGE_LOC(V);
extern LVAL Tango_Polyline_Image_Class_Method_ISNEW(V);
extern LVAL Tango_Polyline_Image_Class_Method_STOREON(V);
extern LVAL Tango_Rectangle_Image_Class_Method_ISNEW(V);
extern LVAL Tango_Rectangle_Image_Class_Method_STOREON(V);
extern LVAL Tango_Spline_Image_Class_Method_IMAGE_LOC(V);
extern LVAL Tango_Spline_Image_Class_Method_ISNEW(V);
extern LVAL Tango_Spline_Image_Class_Method_STOREON(V);
extern LVAL Tango_Text_Image_Class_Method_ISNEW(V);
extern LVAL Tango_Text_Image_Class_Method_STOREON(V);
extern LVAL Xtango_Widget_Class_Method_ADD_CALLBACK(V);
extern LVAL Xtango_Widget_Class_Method_BEGIN_DRAWING(V);
extern LVAL Xtango_Widget_Class_Method_COLORS_STOREON(V);
extern LVAL Xtango_Widget_Class_Method_COPY_TO_2D_BITMAP_ARRAY(V);
extern LVAL Xtango_Widget_Class_Method_GET_EVENT_COORD(V);
extern LVAL Xtango_Widget_Class_Method_GET_EVENT_IMAGE(V);
extern LVAL Xtango_Widget_Class_Method_GET_IMAGES(V);
extern LVAL Xtango_Widget_Class_Method_INPUT_COORD(V);
extern LVAL Xtango_Widget_Class_Method_INPUT_IMAGE(V);
extern LVAL Xtango_Widget_Class_Method_INQ_COORD(V);
extern LVAL Xtango_Widget_Class_Method_ISNEW(V);
extern LVAL Xtango_Widget_Class_Method_LOAD_COLOR(V);
extern LVAL Xtango_Widget_Class_Method_MONO_PATTERN_REPRESENTATION(V);
extern LVAL Xtango_Widget_Class_Method_PAN(V);
extern LVAL Xtango_Widget_Class_Method_REFRESH(V);
extern LVAL Xtango_Widget_Class_Method_SET_ANIMATION_EVENT_PROCESSING(V);
extern LVAL Xtango_Widget_Class_Method_SET_BGCOLOR(V);
extern LVAL Xtango_Widget_Class_Method_SET_CALLBACK(V);
extern LVAL Xtango_Widget_Class_Method_SET_COORD(V);
extern LVAL Xtango_Widget_Class_Method_SET_DEBUG(V);
extern LVAL Xtango_Widget_Class_Method_SET_DELAY(V);
extern LVAL Xtango_Widget_Class_Method_ZOOM(V);
extern LVAL Tango_GIF_Image_Class_Method_ISNEW(V);
extern LVAL Tango_Image_Class_Method_SHOW(V);

extern LVAL Tcls_Prim_TANGOIMAGEOBJP(V);
extern LVAL Xtango_Prim_TANGO_PATH_ADD_HEAD(V);
extern LVAL Xtango_Prim_TANGO_PATH_ADD_TAIL(V);
extern LVAL Xtango_Prim_TANGO_PATH_COLOR(V);
extern LVAL Xtango_Prim_TANGO_PATH_COMPOSE(V);
extern LVAL Xtango_Prim_TANGO_PATH_CONCATENATE(V);
extern LVAL Xtango_Prim_TANGO_PATH_COPY(V);
extern LVAL Xtango_Prim_TANGO_PATH_CREATE(V);
extern LVAL Xtango_Prim_TANGO_PATH_DELETE_HEAD(V);
extern LVAL Xtango_Prim_TANGO_PATH_DELETE_TAIL(V);
extern LVAL Xtango_Prim_TANGO_PATH_DISTANCE(V);
extern LVAL Xtango_Prim_TANGO_PATH_DX(V);
extern LVAL Xtango_Prim_TANGO_PATH_DY(V);
extern LVAL Xtango_Prim_TANGO_PATH_EXAMPLE(V);
extern LVAL Xtango_Prim_TANGO_PATH_EXTEND(V);
extern LVAL Xtango_Prim_TANGO_PATH_FREE(V);
extern LVAL Xtango_Prim_TANGO_PATH_INTERPOLATE(V);
extern LVAL Xtango_Prim_TANGO_PATH_ITERATE(V);
extern LVAL Xtango_Prim_TANGO_PATH_LENGTH(V);
extern LVAL Xtango_Prim_TANGO_PATH_MOTION(V);
extern LVAL Xtango_Prim_TANGO_PATH_REVERSE(V);
extern LVAL Xtango_Prim_TANGO_PATH_ROTATE(V);
extern LVAL Xtango_Prim_TANGO_PATH_SCALE(V);
extern LVAL Xtango_Prim_TANGO_PATH_SMOOTH(V);
extern LVAL Xtango_Prim_TANGO_PATH_TYPE(V);
extern LVAL Xtango_Prim_TANGO_TAP_EXCHANGE(V);
extern LVAL Xtango_Prim_TANGO_TAP_SWITCH(V);
extern LVAL Xtango_Prim_TANGO_TX_COMPOSE(V);
extern LVAL Xtango_Prim_TANGO_TX_CONCATENATE(V);
extern LVAL Xtango_Prim_TANGO_TX_FREE(V);
extern LVAL Xtango_Prim_TANGO_TX_ITERATE(V);
extern LVAL Xtango_Prim_TANGO_TX_PERFORM(V);

extern LVAL Wpm_Prim_GIF_TO_PIXMAP(V);

#endif /* WINTERP_XTANGO_WIDGET */
/****************** END: WINTERP Xtango PRIMITIVES ***********************/

/****************** BEGIN: WINTERP Table PRIMITIVES **********************/
#ifdef WINTERP_TABLE_WIDGET
extern LVAL Prim_XT_TBL_CONFIG(V);
extern LVAL Prim_XT_TBL_OPTIONS(V);
extern LVAL Prim_XT_TBL_POSITION(V);
extern LVAL Prim_XT_TBL_RESIZE(V);
#endif /* WINTERP_TABLE_WIDGET */
/****************** END: WINTERP Table PRIMITIVES ************************/

/****************** BEGIN: WINTERP Expect FUNCTIONS **********************/
#ifdef WINTERP_EXPECT_SUBPROCESS
extern LVAL Prim_EXP_GET_PID(V);
extern LVAL Prim_EXP_POPEN(V);
extern LVAL Prim_EXP_SPAWN(V);
extern LVAL Prim_EXP_STTY_INIT(V);
extern LVAL Prim_EXP_WAIT(V);
extern LVAL Prim_EXP_KILL(V);
#endif /* WINTERP_EXPECT_SUBPROCESS */
/****************** END: WINTERP Expect FUNCTIONS *************************/    

/****************** BEGIN: WINTERP SgDropPocketWidget PRIMITIVES **********/
#ifdef SGI_DROP_POCKET_WIDGET	/* only for Irix 5.1 and IndigoMagic desktop */
extern LVAL Sg_Drop_Pocket_Widget_Class_Method_ADD_CALLBACK(V);
extern LVAL Sg_Drop_Pocket_Widget_Class_Method_SET_CALLBACK(V);
extern LVAL Sg_Finder_Widget_Class_Method_ADD_HISTORY_ITEM(V);
extern LVAL Sg_Finder_Widget_Class_Method_CLEAR_HISTORY(V);
extern LVAL Sg_Finder_Widget_Class_Method_SET_STRING(V);
extern LVAL Sg_Finder_Widget_Class_Method_GET_STRING(V);
extern LVAL Sg_Finder_Widget_Class_Method_GET_CHILD(V);
#endif /* SGI_DROP_POCKET_WIDGET */
/****************** END: WINTERP SgDropPocketWidget PRIMITIVES ************/

/* xnotimp - function table entries that are currently not implemented */
LVAL xnotimp()
{
    xlfail("function not implemented");
}


/* see #define INDEX_OF_LAST_FUNTAB_ENTRY_USED_BY_libWinterp below... */
#define SIZE_OF_FUNTAB_SEGMENT_0 0
#define SIZE_OF_FUNTAB_SEGMENT_1 0
#define SIZE_OF_FUNTAB_SEGMENT_2 0
#define SIZE_OF_FUNTAB_SEGMENT_3 0
#define SIZE_OF_FUNTAB_SEGMENT_4 0
#define SIZE_OF_FUNTAB_SEGMENT_5 0
#define SIZE_OF_FUNTAB_SEGMENT_6 0
#define SIZE_OF_FUNTAB_SEGMENT_7 0
#define SIZE_OF_FUNTAB_SEGMENT_8 0
#define SIZE_OF_FUNTAB_SEGMENT_9 0

/*
 * The function table -- funtab[].
 *
 * NOTE:
 * The order and number of METHOD entries in funtab[] must correspond to the
 * number and order of the enumerations (method-functiontable indexes) created
 * in w_funtab.h. FUNCTIONS need not be entered in w_funtab.h.
 *
 * When adding METHODS to funtab[] below, make sure to update the #define
 * SIZE_OF_FUNTAB_SEGMENT_i (for the appropriate value of i) to the number
 * of entries added by your new set of methods. Also, for each method in
 * funtab[], a corresponding entry must be added to w_funtab.h, with the
 * last method-entry being used to update the value of cpp-variable
 * LAST_FUNTAB_POINTER_USED_BY_libWinterp.
 * (You will also need to declare the function "extern" above in w_funtab.c)
 *
 * Function Wfu_Funtab_Sanity_Check() is called from main() and checks to make
 * sure that the number of entries in funtab[] correspond to the number of
 * indexes in w_funtab.h. This test catches the majority of errors in keeping
 * the funtab[] table up to date with the pointers in w_funtab.h. Note however
 * that the test will only work if you keep
 * LAST_FUNTAB_POINTER_USED_BY_libWinterp and
 * INDEX_OF_LAST_FUNTAB_ENTRY_USED_BY_libWinterp up to date.
 *
 * In order to decouple your c-language extensions to WINTERP from changes in
 * future releases of WINTERP, you should not add new entries to w_funtab.c
 * and w_funtab.h. To add a function or method <function> to WINTERP, you should
 * (1) declare the new function as "extern" by adding a line of the form
 * "extern LVAL <function>();" in app_funextn.h;
 * (2) create a symbolic index for the function or method by adding a line
 * "FTAB_<function>," to app_funidx.h;
 * (3) associate a lisp symbol name <FUNCTION-NAME> (must be all uppercase)
 * to the C function <function> by adding the following line to app_fundecl.h:
 * "{"<FUNCTION-NAME>", S, <function>},"
 *
 * If you are adding a method, rather than a function, do the same in
 * steps (1) and (2), but for (3) you should add a line like the following to
 * app_fundecl.h: "{NULL, S, <function>},".
 * To associate a message keyword <:MESSAGE> to a method on <widgetclass>,
 * you must call xladdmsg() in the portion of a file that initializes
 * a class. An example of such a call is:
 * xladdmsg(<widgetclass>, "<:MESSAGE>", FTAB_<function>).
 * The file defining a class and it's methods should #include w_funtab.h in
 * order to get FTAB_<function>.
 * See files wc_*.c for examples of defining methods/messages.
 */
FUNDEF funtab[] = {
/* DO NOT ALTER ENTRIES UNTIL AFTER OBPRIN1 */
  /*
   * NOTE-FROM-NPM: if you re-order any entries between index 0-15, you must
   * also change the #defines in xlisp/xlisp.h accordingly:
   * #define FT_RMHASH   0
   * #define FT_RMQUOTE	 1
   * #define FT_RMDQUOTE 2
   * #define FT_RMBQUOTE 3
   * #define FT_RMCOMMA  4
   * #define FT_RMLPAR   5
   * #define FT_RMRPAR   6
   * #define FT_RMSEMI   7
   * -- #define xxxxxx   8 --
   * -- #define yyyyyy   9 --
   * #define FT_CLNEW	 10
   * #define FT_CLISNEW	 11
   * #define FT_CLANSWER 12
   * #define FT_OBISNEW	 13
   * #define FT_OBCLASS	 14
   * #define FT_OBSHOW	 15
   * #ifdef OBJPRNT
   * #define FT_OBPRIN1	16
   * #endif
   */

  /* read macro functions */
  {	NULL,				S, rmhash		}, /*   0 */
  {	NULL,				S, rmquote		}, /*   1 */
  {	NULL,				S, rmdquote		}, /*   2 */
  {	NULL,				S, rmbquote		}, /*   3 */
  {	NULL,				S, rmcomma		}, /*   4 */
  {	NULL,				S, rmlpar		}, /*   5 */
  {	NULL,				S, rmrpar		}, /*   6 */
  {	NULL,				S, rmsemi		}, /*   7 */
  {	NULL,				S, xnotimp		}, /*   8 */
  {	NULL,				S, xnotimp		}, /*   9 */

  /* methods */
  {	NULL,				S, clnew		}, /*  10 */
  {	NULL,				S, clisnew		}, /*  11 */
  {	NULL,				S, clanswer		}, /*  12 */
  {	NULL,				S, obisnew		}, /*  13 */
  {	NULL,				S, obclass		}, /*  14 */
  {	NULL,				S, obshow		}, /*  15 */

#ifdef OBJPRNT
  {	NULL,				S, obprin1		}, /*  16 */
#else
  {	NULL,				S, xnotimp		}, /*  16 */
#endif /* OBJPRNT */

  {	NULL,				S, xnotimp		}, /*  17 */
  {	NULL,				S, xnotimp		}, /*  18 */
  {	NULL,				S, xnotimp		}, /*  19 */

/******************************************************************************/
/************************ BEGIN: WINTERP PRIMITIVES ***************************/
/******************************************************************************/

/****************** BEGIN: WINTERP METHODS (Motif >= 1.0) *********************/
  {NULL, S, Application_Shell_Widget_Class_Method_GET_ARGV}, /* 20 */
  {NULL, S, Application_Shell_Widget_Class_Method_ISNEW}, /* 21 */
  {NULL, S, Application_Shell_Widget_Class_Method_SET_ARGV}, /* 22 */
  {NULL, S, Popup_Shell_Widget_Class_Method_ISNEW}, /* 23 */
  {NULL, S, Popup_Shell_Widget_Class_Method_POPDOWN}, /* 24 */
  {NULL, S, Popup_Shell_Widget_Class_Method_POPUP}, /* 25 */
  {NULL, S, Shell_Widget_Class_Method_ISNEW}, /* 26 */
  {NULL, S, Shell_Widget_Class_Method_IS_MOTIF_WM_RUNNING}, /* 27 */
  {NULL, S, Shell_Widget_Class_Method_LOWER_WINDOW}, /* 28 */
  {NULL, S, Shell_Widget_Class_Method_MANAGE}, /* 29 */
  {NULL, S, Shell_Widget_Class_Method_MAP_RAISED}, /* 30 */
  {NULL, S, Shell_Widget_Class_Method_RAISE_WINDOW}, /* 31 */
  {NULL, S, Shell_Widget_Class_Method_REALIZE},	/* 32 */
  {NULL, S, Shell_Widget_Class_Method_UNMANAGE}, /* 33 */
  {NULL, S, Shell_Widget_Class_Method_UNREALIZE}, /* 34 */
  {NULL, S, Widget_Class_Method_ADD_CALLBACK}, /* 35 */
  {NULL, S, Widget_Class_Method_ADD_EVENT_HANDLER}, /* 36 */
  {NULL, S, Widget_Class_Method_ADD_GRAB}, /* 37 */
  {NULL, S, Widget_Class_Method_ADD_TAB_GROUP},	/* 38 */
  {NULL, S, Widget_Class_Method_AUGMENT_TRANSLATIONS}, /* 39 */
  {NULL, S, Widget_Class_Method_BUILD_EVENT_MASK}, /* 40 */
  {NULL, S, Widget_Class_Method_DESTROY}, /* 41 */
  {NULL, S, Widget_Class_Method_EXISTS_P}, /* 42 */
  {NULL, S, Widget_Class_Method_GET_VALUES}, /* 43 */
  {NULL, S, Widget_Class_Method_HAS_CALLBACKS},	/* 44 */
  {NULL, S, Widget_Class_Method_INSTALL_ACCELERATORS}, /* 45 */
  {NULL, S, Widget_Class_Method_INSTALL_ALL_ACCELERATORS}, /* 46 */
  {NULL, S, Widget_Class_Method_ISNEW},	/* 47 */
  {NULL, S, Widget_Class_Method_IS_COMPOSITE}, /* 48 */
  {NULL, S, Widget_Class_Method_IS_CONSTRAINT},	/* 49 */
  {NULL, S, Widget_Class_Method_IS_GADGET}, /* 50 */
  {NULL, S, Widget_Class_Method_IS_MANAGED}, /* 51 */
  {NULL, S, Widget_Class_Method_IS_MANAGER}, /* 52 */
  {NULL, S, Widget_Class_Method_IS_PRIMITIVE}, /* 53 */
  {NULL, S, Widget_Class_Method_IS_REALIZED}, /* 54 */
  {NULL, S, Widget_Class_Method_IS_SENSITIVE}, /* 55 */
  {NULL, S, Widget_Class_Method_IS_SHELL}, /* 56 */
  {NULL, S, Widget_Class_Method_MANAGE}, /* 57 */
  {NULL, S, Widget_Class_Method_MAP}, /* 58 */
  {NULL, S, Widget_Class_Method_OVERRIDE_TRANSLATIONS},	/* 59 */
  {NULL, S, Widget_Class_Method_PARENT}, /* 60 */
  {NULL, S, Widget_Class_Method_PRIN1}, /* 61 */
  {NULL, S, Widget_Class_Method_REMOVE_ALL_CALLBACKS}, /* 62 */
  {NULL, S, Widget_Class_Method_REMOVE_GRAB}, /* 63 */
  {NULL, S, Widget_Class_Method_REMOVE_TAB_GROUP}, /* 64 */
  {NULL, S, Widget_Class_Method_SET_CALLBACK}, /* 65 */
  {NULL, S, Widget_Class_Method_SET_EVENT_HANDLER}, /* 66 */
  {NULL, S, Widget_Class_Method_SET_MAPPED_WHEN_MANAGED}, /* 67 */
  {NULL, S, Widget_Class_Method_SET_SENSITIVE},	/* 68 */
  {NULL, S, Widget_Class_Method_SET_VALUES}, /* 69 */
  {NULL, S, Widget_Class_Method_UNINSTALL_TRANSLATIONS}, /* 70 */
  {NULL, S, Widget_Class_Method_UNMANAGE}, /* 71 */
  {NULL, S, Widget_Class_Method_UNMAP},	/* 72 */
  {NULL, S, Widget_Class_Method_UPDATE_DISPLAY}, /* 73 */
  {NULL, S, Widget_Class_Method_WINDOW}, /* 74 */
  {NULL, S, Xm_Bulletin_Board_Widget_Class_Method_ISNEW}, /* 75 */
  {NULL, S, Xm_Cascade_Button_Widget_Class_Method_HIGHLIGHT}, /* 76 */
  {NULL, S, Xm_Command_Widget_Class_Method_ADD_CALLBACK}, /* 77 */
  {NULL, S, Xm_Command_Widget_Class_Method_APPEND_VALUE}, /* 78 */
  {NULL, S, Xm_Command_Widget_Class_Method_ERROR}, /* 79 */
  {NULL, S, Xm_Command_Widget_Class_Method_GET_CHILD}, /* 80 */
  {NULL, S, Xm_Command_Widget_Class_Method_GET_HISTORY_ITEMS}, /* 81 */
  {NULL, S, Xm_Command_Widget_Class_Method_ISNEW}, /* 82 */
  {NULL, S, Xm_Command_Widget_Class_Method_SET_CALLBACK}, /* 83 */
  {NULL, S, Xm_Command_Widget_Class_Method_SET_VALUE}, /* 84 */
  {NULL, S, Xm_Drawing_Area_Widget_Class_Method_ADD_CALLBACK}, /* 85 */
  {NULL, S, Xm_Drawing_Area_Widget_Class_Method_SET_CALLBACK}, /* 86 */
  {NULL, S, Xm_Drawn_Button_Widget_Class_Method_ADD_CALLBACK}, /* 87 */
  {NULL, S, Xm_Drawn_Button_Widget_Class_Method_SET_CALLBACK}, /* 88 */
  {NULL, S, Xm_File_Selection_Box_Widget_Class_Method_ADD_CALLBACK}, /* 89 */
  {NULL, S, Xm_File_Selection_Box_Widget_Class_Method_DO_SEARCH}, /* 90 */
  {NULL, S, Xm_File_Selection_Box_Widget_Class_Method_GET_CHILD}, /* 91 */
  {NULL, S, Xm_File_Selection_Box_Widget_Class_Method_ISNEW}, /* 92 */
  {NULL, S, Xm_File_Selection_Box_Widget_Class_Method_SET_CALLBACK}, /* 93 */
  {NULL, S, Xm_Form_Widget_Class_Method_ISNEW},	/* 94 */
  {NULL, S, Xm_List_Widget_Class_Method_ADD_CALLBACK}, /* 95 */
  {NULL, S, Xm_List_Widget_Class_Method_ADD_ITEM_UNSELECTED}, /* 96 */
  {NULL, S, Xm_List_Widget_Class_Method_ADD_ITEM}, /* 97 */
  {NULL, S, Xm_List_Widget_Class_Method_DELETE_ITEM}, /* 98 */
  {NULL, S, Xm_List_Widget_Class_Method_DELETE_POS}, /* 99 */
  {NULL, S, Xm_List_Widget_Class_Method_DESELECT_ALL_ITEMS}, /* 100 */
  {NULL, S, Xm_List_Widget_Class_Method_DESELECT_ITEM},	/* 101 */
  {NULL, S, Xm_List_Widget_Class_Method_DESELECT_POS}, /* 102 */
  {NULL, S, Xm_List_Widget_Class_Method_GET_ITEMS}, /* 103 */
  {NULL, S, Xm_List_Widget_Class_Method_GET_SELECTED_ITEMS}, /* 104 */
  {NULL, S, Xm_List_Widget_Class_Method_ISNEW},	/* 105 */
  {NULL, S, Xm_List_Widget_Class_Method_ITEM_EXISTS}, /* 106 */
  {NULL, S, Xm_List_Widget_Class_Method_SELECT_ITEM}, /* 107 */
  {NULL, S, Xm_List_Widget_Class_Method_SELECT_POS}, /* 108 */
  {NULL, S, Xm_List_Widget_Class_Method_SET_BOTTOM_ITEM}, /* 109 */
  {NULL, S, Xm_List_Widget_Class_Method_SET_BOTTOM_POS}, /* 110 */
  {NULL, S, Xm_List_Widget_Class_Method_SET_CALLBACK}, /* 111 */
  {NULL, S, Xm_List_Widget_Class_Method_SET_HORIZ_POS},	/* 112 */
  {NULL, S, Xm_List_Widget_Class_Method_SET_ITEM}, /* 113 */
  {NULL, S, Xm_List_Widget_Class_Method_SET_POS}, /* 114 */
  {NULL, S, Xm_Main_Window_Widget_Class_Method_SEP1}, /* 115 */
  {NULL, S, Xm_Main_Window_Widget_Class_Method_SEP2}, /* 116 */
  {NULL, S, Xm_Main_Window_Widget_Class_Method_SET_AREAS}, /* 117 */
  {NULL, S, Xm_Message_Box_Widget_Class_Method_GET_CHILD}, /* 118 */
  {NULL, S, Xm_Message_Box_Widget_Class_Method_ISNEW}, /* 119 */
  {NULL, S, Xm_Row_Column_Widget_Class_Method_ADD_CALLBACK}, /* 120 */
  {NULL, S, Xm_Row_Column_Widget_Class_Method_ISNEW}, /* 121 */
  {NULL, S, Xm_Row_Column_Widget_Class_Method_MENU_POSITION}, /* 122 */
  {NULL, S, Xm_Row_Column_Widget_Class_Method_OPTION_BUTTON_GADGET}, /* 123 */
  {NULL, S, Xm_Row_Column_Widget_Class_Method_OPTION_LABEL_GADGET}, /* 124 */
  {NULL, S, Xm_Row_Column_Widget_Class_Method_SET_CALLBACK}, /* 125 */
  {NULL, S, Xm_Scale_Widget_Class_Method_ADD_CALLBACK},	/* 126 */
  {NULL, S, Xm_Scale_Widget_Class_Method_GET_VALUE}, /* 127 */
  {NULL, S, Xm_Scale_Widget_Class_Method_SET_CALLBACK},	/* 128 */
  {NULL, S, Xm_Scale_Widget_Class_Method_SET_VALUE}, /* 129 */
  {NULL, S, Xm_Scroll_Bar_Widget_Class_Method_ADD_CALLBACK}, /* 130 */
  {NULL, S, Xm_Scroll_Bar_Widget_Class_Method_GET_VALUE}, /* 131 */
  {NULL, S, Xm_Scroll_Bar_Widget_Class_Method_SET_CALLBACK}, /* 132 */
  {NULL, S, Xm_Scroll_Bar_Widget_Class_Method_SET_VALUE}, /* 133 */
  {NULL, S, Xm_Scrolled_Window_Widget_Class_Method_SET_AREAS}, /* 134 */
  {NULL, S, Xm_Selection_Box_Widget_Class_Method_ADD_CALLBACK},	/* 135 */
  {NULL, S, Xm_Selection_Box_Widget_Class_Method_GET_CHILD}, /* 136 */
  {NULL, S, Xm_Selection_Box_Widget_Class_Method_GET_LIST_ITEMS}, /* 137 */
  {NULL, S, Xm_Selection_Box_Widget_Class_Method_ISNEW}, /* 138 */
  {NULL, S, Xm_Selection_Box_Widget_Class_Method_SET_CALLBACK},	/* 139 */
  {NULL, S, Xm_Text_Widget_Class_Method_ADD_CALLBACK}, /* 140 */
  {NULL, S, Xm_Text_Widget_Class_Method_CLEAR_SELECTION}, /* 141 */
  {NULL, S, Xm_Text_Widget_Class_Method_DISABLE_REDISPLAY}, /* 142 */
  {NULL, S, Xm_Text_Widget_Class_Method_ENABLE_REDISPLAY}, /* 143 */
  {NULL, S, Xm_Text_Widget_Class_Method_GET_EDITABLE}, /* 144 */
  {NULL, S, Xm_Text_Widget_Class_Method_GET_INSERTION_POSITION}, /* 145 */
  {NULL, S, Xm_Text_Widget_Class_Method_GET_LAST_POSITION}, /* 146 */
  {NULL, S, Xm_Text_Widget_Class_Method_GET_MAX_LENGTH}, /* 147 */
  {NULL, S, Xm_Text_Widget_Class_Method_GET_SELECTION_POSITION}, /* 148 */
  {NULL, S, Xm_Text_Widget_Class_Method_GET_SELECTION},	/* 149 */
  {NULL, S, Xm_Text_Widget_Class_Method_GET_STRING}, /* 150 */
  {NULL, S, Xm_Text_Widget_Class_Method_GET_TOP_CHARACTER}, /* 151 */
  {NULL, S, Xm_Text_Widget_Class_Method_GOTO_LINE}, /* 152 */
  {NULL, S, Xm_Text_Widget_Class_Method_ISNEW},	/* 153 */
  {NULL, S, Xm_Text_Widget_Class_Method_POS_TO_XY}, /* 154 */
  {NULL, S, Xm_Text_Widget_Class_Method_READ_FILE_GOTO_LINE}, /* 155 */
  {NULL, S, Xm_Text_Widget_Class_Method_READ_FILE}, /* 156 */
  {NULL, S, Xm_Text_Widget_Class_Method_REPLACE}, /* 157 */
  {NULL, S, Xm_Text_Widget_Class_Method_SCROLL}, /* 158 */
  {NULL, S, Xm_Text_Widget_Class_Method_SEARCH}, /* 159 */
  {NULL, S, Xm_Text_Widget_Class_Method_SET_CALLBACK}, /* 160 */
  {NULL, S, Xm_Text_Widget_Class_Method_SET_EDITABLE}, /* 161 */
  {NULL, S, Xm_Text_Widget_Class_Method_SET_INSERTION_POSITION}, /* 162 */
  {NULL, S, Xm_Text_Widget_Class_Method_SET_MAX_LENGTH}, /* 163 */
  {NULL, S, Xm_Text_Widget_Class_Method_SET_SELECTION},	/* 164 */
  {NULL, S, Xm_Text_Widget_Class_Method_SET_STRING}, /* 165 */
  {NULL, S, Xm_Text_Widget_Class_Method_SET_TOP_CHARACTER}, /* 166 */
  {NULL, S, Xm_Text_Widget_Class_Method_SHOW_POSITION},	/* 167 */
  {NULL, S, Xm_Text_Widget_Class_Method_WRITE_FILE}, /* 168 */
  {NULL, S, Xm_Text_Widget_Class_Method_XY_TO_POS}, /* 169 */
  {NULL, S, Xm_Toggle_Button_Gadget_Class_Method_GET_STATE}, /* 170 */
  {NULL, S, Xm_Toggle_Button_Gadget_Class_Method_SET_STATE}, /* 171 */
  {NULL, S, Xm_Toggle_Button_Widget_Class_Method_ADD_CALLBACK},	/* 172 */
  {NULL, S, Xm_Toggle_Button_Widget_Class_Method_GET_STATE}, /* 173 */
  {NULL, S, Xm_Toggle_Button_Widget_Class_Method_SET_CALLBACK},	/* 174 */
  {NULL, S, Xm_Toggle_Button_Widget_Class_Method_SET_STATE}, /* 175 */
  {NULL, S, Widget_Class_Method_SHOW}, /* 176 */

#undef SIZE_OF_FUNTAB_SEGMENT_0
#define SIZE_OF_FUNTAB_SEGMENT_0 176
/****************** END: WINTERP METHODS (Motif >= 1.0) *********************/

/****************** BEGIN: WINTERP METHODS (Motif >= 1.1) *********************/
#ifdef WINTERP_MOTIF_11
  {NULL, S, Widget_Class_Method_CALL_ACTION_PROC}, /* 1 */
  {NULL, S, Widget_Class_Method_FORCED_EXPOSE_UPDATE}, /* 2 */
  {NULL, S, Widget_Class_Method_GET_CHILDREN}, /* 3 */
  {NULL, S, Widget_Class_Method_NAME}, /* 4 */
  {NULL, S, Widget_Class_Method_PROCESS_TRAVERSAL}, /* 5 */
  {NULL, S, Xm_Arrow_Button_Widget_Class_Method_ADD_CALLBACK}, /* 6 */
  {NULL, S, Xm_Arrow_Button_Widget_Class_Method_SET_CALLBACK}, /* 7 */
  {NULL, S, Xm_Cascade_Button_Gadget_Class_Method_HIGHLIGHT}, /* 8 */
  {NULL, S, Xm_File_Selection_Box_Widget_Class_Method_GET_DIR_LIST_ITEMS}, /* 9 */
  {NULL, S, Xm_File_Selection_Box_Widget_Class_Method_GET_FILE_LIST_ITEMS}, /* 10 */
  {NULL, S, Xm_List_Widget_Class_Method_ADD_ITEMS}, /* 11 */
  {NULL, S, Xm_List_Widget_Class_Method_DELETE_ALL_ITEMS}, /* 12 */
  {NULL, S, Xm_List_Widget_Class_Method_DELETE_ITEMS_POS}, /* 13 */
  {NULL, S, Xm_List_Widget_Class_Method_DELETE_ITEMS}, /* 14 */
  {NULL, S, Xm_List_Widget_Class_Method_GET_MATCH_POS},	/* 15 */
  {NULL, S, Xm_List_Widget_Class_Method_GET_SELECTED_POS}, /* 16 */
  {NULL, S, Xm_List_Widget_Class_Method_ITEM_POS}, /* 17 */
  {NULL, S, Xm_List_Widget_Class_Method_PARENT}, /* 18 */
  {NULL, S, Xm_List_Widget_Class_Method_REPLACE_ITEMS_POS}, /* 19 */
  {NULL, S, Xm_List_Widget_Class_Method_REPLACE_ITEMS},	/* 20 */
  {NULL, S, Xm_List_Widget_Class_Method_SET_ADD_MODE}, /* 21 */
  {NULL, S, Xm_Main_Window_Widget_Class_Method_SEP3}, /* 22 */
  {NULL, S, Xm_Push_Button_Widget_Class_Method_ADD_CALLBACK}, /* 23 */
  {NULL, S, Xm_Push_Button_Widget_Class_Method_SET_CALLBACK}, /* 24 */
  {NULL, S, Xm_Row_Column_Widget_Class_Method_GET_POSTED_FROM_WIDGET}, /* 25 */
  {NULL, S, Xm_Row_Column_Widget_Class_Method_GET_SUB_MENU_WIDGET}, /* 26 */
  {NULL, S, Xm_Text_Field_Widget_Class_Method_CLEAR_SELECTION},	/* 27 */
  {NULL, S, Xm_Text_Field_Widget_Class_Method_COPY}, /* 28 */
  {NULL, S, Xm_Text_Field_Widget_Class_Method_CUT}, /* 29 */
  {NULL, S, Xm_Text_Field_Widget_Class_Method_GET_ADD_MODE}, /* 30 */
  {NULL, S, Xm_Text_Field_Widget_Class_Method_GET_BASELINE}, /* 31 */
  {NULL, S, Xm_Text_Field_Widget_Class_Method_GET_CURSOR_POSITION}, /* 32 */
  {NULL, S, Xm_Text_Field_Widget_Class_Method_GET_EDITABLE}, /* 33 */
  {NULL, S, Xm_Text_Field_Widget_Class_Method_GET_INSERTION_POSITION}, /* 34 */
  {NULL, S, Xm_Text_Field_Widget_Class_Method_GET_LAST_POSITION}, /* 35 */
  {NULL, S, Xm_Text_Field_Widget_Class_Method_GET_MAX_LENGTH}, /* 36 */
  {NULL, S, Xm_Text_Field_Widget_Class_Method_GET_SELECTION_POSITION}, /* 37 */
  {NULL, S, Xm_Text_Field_Widget_Class_Method_GET_SELECTION}, /* 38 */
  {NULL, S, Xm_Text_Field_Widget_Class_Method_GET_STRING}, /* 39 */
  {NULL, S, Xm_Text_Field_Widget_Class_Method_INSERT}, /* 40 */
  {NULL, S, Xm_Text_Field_Widget_Class_Method_PASTE}, /* 41 */
  {NULL, S, Xm_Text_Field_Widget_Class_Method_POS_TO_XY}, /* 42 */
  {NULL, S, Xm_Text_Field_Widget_Class_Method_REMOVE}, /* 43 */
  {NULL, S, Xm_Text_Field_Widget_Class_Method_REPLACE},	/* 44 */
  {NULL, S, Xm_Text_Field_Widget_Class_Method_SET_ADD_MODE}, /* 45 */
  {NULL, S, Xm_Text_Field_Widget_Class_Method_SET_CURSOR_POSITION}, /* 46 */
  {NULL, S, Xm_Text_Field_Widget_Class_Method_SET_EDITABLE}, /* 47 */
  {NULL, S, Xm_Text_Field_Widget_Class_Method_SET_HIGHLIGHT}, /* 48 */
  {NULL, S, Xm_Text_Field_Widget_Class_Method_SET_INSERTION_POSITION}, /* 49 */
  {NULL, S, Xm_Text_Field_Widget_Class_Method_SET_MAX_LENGTH}, /* 50 */
  {NULL, S, Xm_Text_Field_Widget_Class_Method_SET_SELECTION}, /* 51 */
  {NULL, S, Xm_Text_Field_Widget_Class_Method_SET_STRING}, /* 52 */
  {NULL, S, Xm_Text_Field_Widget_Class_Method_SHOW_POSITION}, /* 53 */
  {NULL, S, Xm_Text_Field_Widget_Class_Method_XY_TO_POS}, /* 54 */
  {NULL, S, Xm_Text_Widget_Class_Method_COPY}, /* 55 */
  {NULL, S, Xm_Text_Widget_Class_Method_CUT}, /* 56 */
  {NULL, S, Xm_Text_Widget_Class_Method_GET_ADD_MODE}, /* 57 */
  {NULL, S, Xm_Text_Widget_Class_Method_GET_BASELINE}, /* 58 */
  {NULL, S, Xm_Text_Widget_Class_Method_GET_CURSOR_POSITION}, /* 59 */
  {NULL, S, Xm_Text_Widget_Class_Method_INSERT}, /* 60 */
  {NULL, S, Xm_Text_Widget_Class_Method_PARENT}, /* 61 */
  {NULL, S, Xm_Text_Widget_Class_Method_PASTE},	/* 62 */
  {NULL, S, Xm_Text_Widget_Class_Method_REMOVE}, /* 63 */
  {NULL, S, Xm_Text_Widget_Class_Method_SET_ADD_MODE}, /* 64 */
  {NULL, S, Xm_Text_Widget_Class_Method_SET_CURSOR_POSITION}, /* 65 */
  {NULL, S, Xm_Text_Widget_Class_Method_SET_HIGHLIGHT},	/* 66 */

#undef SIZE_OF_FUNTAB_SEGMENT_1
#define SIZE_OF_FUNTAB_SEGMENT_1 66
#endif				/* WINTERP_MOTIF_11 */
/****************** END: WINTERP METHODS (Motif >= 1.1) ***********************/  

/****************** BEGIN: WINTERP METHODS (Motif >= 1.2) *********************/
#ifdef WINTERP_MOTIF_12
  {NULL, S, Widget_Class_Method_CHANGE_COLOR}, /* 1 */
  {NULL, S, Xm_Scrolled_Window_Widget_Class_Method_SCROLL_VISIBLE}, /* 2 */
#undef SIZE_OF_FUNTAB_SEGMENT_2
#define SIZE_OF_FUNTAB_SEGMENT_2 2
#endif /* WINTERP_MOTIF_12 */
/****************** END: WINTERP METHODS (Motif >= 1.2) **********************/

/****************** BEGIN: WINTERP XmGraph METHODS (Motif >= 1.1) ***********/
#ifdef HP_GRAPH_WIDGET		/* if HP_GRAPH_WIDGET defined */
  {NULL, S, Xm_Arc_Widget_Class_Method_ISNEW}, /* 1 */
  {NULL, S, Xm_Graph_Widget_Class_Method_ADD_CALLBACK}, /* 2 */
  {NULL, S, Xm_Graph_Widget_Class_Method_CENTER_AROUND_WIDGET}, /* 3 */
  {NULL, S, Xm_Graph_Widget_Class_Method_DESTROY_ALL_ARCS}, /* 4 */
  {NULL, S, Xm_Graph_Widget_Class_Method_DESTROY_ALL_NODES}, /* 5 */
  {NULL, S, Xm_Graph_Widget_Class_Method_DESTROY_SELECTED_ARCS_OR_NODES}, /* 6 */
  {NULL, S, Xm_Graph_Widget_Class_Method_GET_ARCS}, /* 7 */
  {NULL, S, Xm_Graph_Widget_Class_Method_GET_ARCS_BETWEEN_NODES}, /* 8 */
  {NULL, S, Xm_Graph_Widget_Class_Method_GET_ARC_NODES}, /* 9 */
  {NULL, S, Xm_Graph_Widget_Class_Method_GET_NODES}, /* 10 */
  {NULL, S, Xm_Graph_Widget_Class_Method_GET_NODE_ARCS}, /* 11 */
  {NULL, S, Xm_Graph_Widget_Class_Method_GET_ROOTS}, /* 12 */
  {NULL, S, Xm_Graph_Widget_Class_Method_GET_SELECTED_ARCS}, /* 13 */
  {NULL, S, Xm_Graph_Widget_Class_Method_GET_SELECTED_NODES}, /* 14 */
  {NULL, S, Xm_Graph_Widget_Class_Method_INPUT_OVER_ARC}, /* 15 */
  {NULL, S, Xm_Graph_Widget_Class_Method_INSERT_ROOTS}, /* 16 */
  {NULL, S, Xm_Graph_Widget_Class_Method_ISNEW}, /* 17 */
  {NULL, S, Xm_Graph_Widget_Class_Method_IS_POINT_IN_ARC}, /* 18 */
  {NULL, S, Xm_Graph_Widget_Class_Method_IS_SELECTED_ARC}, /* 19 */
  {NULL, S, Xm_Graph_Widget_Class_Method_IS_SELECTED_NODE}, /* 20 */
  {NULL, S, Xm_Graph_Widget_Class_Method_LAYOUT}, /* 21 */
  {NULL, S, Xm_Graph_Widget_Class_Method_MOVE_ALL}, /* 22 */
  {NULL, S, Xm_Graph_Widget_Class_Method_MOVE_ARC}, /* 23 */
  {NULL, S, Xm_Graph_Widget_Class_Method_MOVE_NODE}, /* 24 */
  {NULL, S, Xm_Graph_Widget_Class_Method_NUM_ARCS}, /* 25 */
  {NULL, S, Xm_Graph_Widget_Class_Method_NUM_NODES}, /* 26 */
  {NULL, S, Xm_Graph_Widget_Class_Method_NUM_NODE_ARCS}, /* 27 */
  {NULL, S, Xm_Graph_Widget_Class_Method_NUM_ROOTS}, /* 28 */
  {NULL, S, Xm_Graph_Widget_Class_Method_NUM_SELECTED_ARCS}, /* 29 */
  {NULL, S, Xm_Graph_Widget_Class_Method_NUM_SELECTED_NODES}, /* 30 */
  {NULL, S, Xm_Graph_Widget_Class_Method_RELAY_SUBGRAPH}, /* 31 */
  {NULL, S, Xm_Graph_Widget_Class_Method_REMOVE_ARC_BETWEEN_NODES}, /* 32 */
  {NULL, S, Xm_Graph_Widget_Class_Method_REMOVE_ROOTS}, /* 33 */
  {NULL, S, Xm_Graph_Widget_Class_Method_SELECT_ARC}, /* 34 */
  {NULL, S, Xm_Graph_Widget_Class_Method_SELECT_ARCS}, /* 35 */
  {NULL, S, Xm_Graph_Widget_Class_Method_SELECT_NODE}, /* 36 */
  {NULL, S, Xm_Graph_Widget_Class_Method_SELECT_NODES}, /* 37 */
  {NULL, S, Xm_Graph_Widget_Class_Method_SET_CALLBACK}, /* 38 */
  {NULL, S, Xm_Graph_Widget_Class_Method_UNSELECT_ARC}, /* 39 */
  {NULL, S, Xm_Graph_Widget_Class_Method_UNSELECT_ARCS}, /* 40 */
  {NULL, S, Xm_Graph_Widget_Class_Method_UNSELECT_NODE}, /* 41 */
  {NULL, S, Xm_Graph_Widget_Class_Method_UNSELECT_NODES}, /* 42 */
	                                                                          
#undef SIZE_OF_FUNTAB_SEGMENT_3
#define SIZE_OF_FUNTAB_SEGMENT_3 42
#endif				/* HP_GRAPH_WIDGET */
/****************** END: WINTERP XmGraph METHODS ****************************/

/****************** BEGIN: WINTERP Xtango METHODS ***************************/
#ifdef WINTERP_XTANGO_WIDGET
  {NULL, S, Tango_Bitmap_Image_Class_Method_ISNEW}, /* 1 */
  {NULL, S, Tango_Bitmap_Image_Class_Method_STOREON}, /* 2 */
  {NULL, S, Tango_Bitmap_Image_Class_Method_TX_SHUFFLE}, /* 3 */
  {NULL, S, Tango_Circle_Image_Class_Method_ISNEW}, /* 4 */
  {NULL, S, Tango_Circle_Image_Class_Method_STOREON}, /* 5 */
  {NULL, S, Tango_Composite_Image_Class_Method_ISNEW}, /* 6 */
  {NULL, S, Tango_Composite_Image_Class_Method_STOREON}, /* 7 */
  {NULL, S, Tango_Ellipse_Image_Class_Method_ISNEW}, /* 8 */
  {NULL, S, Tango_Ellipse_Image_Class_Method_STOREON}, /* 9 */
  {NULL, S, Tango_Image_Class_Method_EXISTS_P}, /* 10 */
  {NULL, S, Tango_Image_Class_Method_IMAGE_COPY}, /* 11 */
  {NULL, S, Tango_Image_Class_Method_ISNEW}, /* 12 */
  {NULL, S, Tango_Image_Class_Method_PRIN1}, /* 13 */
  {NULL, S, Tango_Image_Class_Method_TAP_COLOR}, /* 14 */
  {NULL, S, Tango_Image_Class_Method_TAP_FILL}, /* 15 */
  {NULL, S, Tango_Image_Class_Method_TAP_FLASH}, /* 16 */
  {NULL, S, Tango_Image_Class_Method_TAP_JUMP}, /* 17 */
  {NULL, S, Tango_Image_Class_Method_TAP_MOVE}, /* 18 */
  {NULL, S, Tango_Image_Class_Method_TAP_SHOW}, /* 19 */
  {NULL, S, Tango_Image_Class_Method_TAP_TRAVERSE}, /* 20 */
  {NULL, S, Tango_Image_Class_Method_TAP_VIS_TOGGLE}, /* 21 */
  {NULL, S, Tango_Image_Class_Method_TX_COLOR}, /* 22 */
  {NULL, S, Tango_Image_Class_Method_TX_DELAY}, /* 23 */
  {NULL, S, Tango_Image_Class_Method_TX_DELETE}, /* 24 */
  {NULL, S, Tango_Image_Class_Method_TX_FILL}, /* 25 */
  {NULL, S, Tango_Image_Class_Method_TX_LOWER}, /* 26 */
  {NULL, S, Tango_Image_Class_Method_TX_MOVE}, /* 27 */
  {NULL, S, Tango_Image_Class_Method_TX_RAISE}, /* 28 */
  {NULL, S, Tango_Image_Class_Method_TX_REFRESH}, /* 29 */
  {NULL, S, Tango_Image_Class_Method_TX_VISIBLE}, /* 30 */
  {NULL, S, Tango_Image_Class_Method_TX_ZOOM}, /* 31 */
  {NULL, S, Tango_Line_Image_Class_Method_ISNEW}, /* 32 */
  {NULL, S, Tango_Line_Image_Class_Method_STOREON}, /* 33 */
  {NULL, S, Tango_Non_Poly_Image_Class_Method_IMAGE_LOC}, /* 34 */
  {NULL, S, Tango_Non_Poly_Image_Class_Method_TX_RESIZE}, /* 35 */
  {NULL, S, Tango_Poly_Image_Class_Method_TX_GRAB1}, /* 36 */
  {NULL, S, Tango_Poly_Image_Class_Method_TX_GRAB2}, /* 37 */
  {NULL, S, Tango_Poly_Image_Class_Method_TX_GRAB3}, /* 38 */
  {NULL, S, Tango_Poly_Image_Class_Method_TX_GRAB4}, /* 39 */
  {NULL, S, Tango_Poly_Image_Class_Method_TX_GRAB5}, /* 40 */
  {NULL, S, Tango_Poly_Image_Class_Method_TX_GRAB6}, /* 41 */
  {NULL, S, Tango_Poly_Image_Class_Method_TX_GRAB7}, /* 42 */
  {NULL, S, Tango_Poly_Image_Class_Method_TX_RESIZE1}, /* 43 */
  {NULL, S, Tango_Poly_Image_Class_Method_TX_RESIZE2}, /* 44 */
  {NULL, S, Tango_Poly_Image_Class_Method_TX_RESIZE3}, /* 45 */
  {NULL, S, Tango_Poly_Image_Class_Method_TX_RESIZE4}, /* 46 */
  {NULL, S, Tango_Poly_Image_Class_Method_TX_RESIZE5}, /* 47 */
  {NULL, S, Tango_Poly_Image_Class_Method_TX_RESIZE6}, /* 48 */
  {NULL, S, Tango_Poly_Image_Class_Method_TX_RESIZE7}, /* 49 */
  {NULL, S, Tango_Polygon_Image_Class_Method_IMAGE_LOC}, /* 50 */
  {NULL, S, Tango_Polygon_Image_Class_Method_ISNEW}, /* 51 */
  {NULL, S, Tango_Polygon_Image_Class_Method_STOREON}, /* 52 */
  {NULL, S, Tango_Polyline_Image_Class_Method_IMAGE_LOC}, /* 53 */
  {NULL, S, Tango_Polyline_Image_Class_Method_ISNEW}, /* 54 */
  {NULL, S, Tango_Polyline_Image_Class_Method_STOREON}, /* 55 */
  {NULL, S, Tango_Rectangle_Image_Class_Method_ISNEW}, /* 56 */
  {NULL, S, Tango_Rectangle_Image_Class_Method_STOREON}, /* 57 */
  {NULL, S, Tango_Spline_Image_Class_Method_IMAGE_LOC}, /* 58 */
  {NULL, S, Tango_Spline_Image_Class_Method_ISNEW}, /* 59 */
  {NULL, S, Tango_Spline_Image_Class_Method_STOREON}, /* 60 */
  {NULL, S, Tango_Text_Image_Class_Method_ISNEW}, /* 61 */
  {NULL, S, Tango_Text_Image_Class_Method_STOREON}, /* 62 */
  {NULL, S, Xtango_Widget_Class_Method_ADD_CALLBACK}, /* 63 */
  {NULL, S, Xtango_Widget_Class_Method_BEGIN_DRAWING}, /* 64 */
  {NULL, S, Xtango_Widget_Class_Method_COLORS_STOREON}, /* 65 */
  {NULL, S, Xtango_Widget_Class_Method_COPY_TO_2D_BITMAP_ARRAY}, /* 66 */
  {NULL, S, Xtango_Widget_Class_Method_GET_EVENT_COORD}, /* 67 */
  {NULL, S, Xtango_Widget_Class_Method_GET_EVENT_IMAGE}, /* 68 */
  {NULL, S, Xtango_Widget_Class_Method_GET_IMAGES}, /* 69 */
  {NULL, S, Xtango_Widget_Class_Method_INPUT_COORD}, /* 70 */
  {NULL, S, Xtango_Widget_Class_Method_INPUT_IMAGE}, /* 71 */
  {NULL, S, Xtango_Widget_Class_Method_INQ_COORD}, /* 72 */
  {NULL, S, Xtango_Widget_Class_Method_ISNEW}, /* 73 */
  {NULL, S, Xtango_Widget_Class_Method_LOAD_COLOR}, /* 74 */
  {NULL, S, Xtango_Widget_Class_Method_MONO_PATTERN_REPRESENTATION}, /* 75 */
  {NULL, S, Xtango_Widget_Class_Method_PAN}, /* 76 */
  {NULL, S, Xtango_Widget_Class_Method_REFRESH}, /* 77 */
  {NULL, S, Xtango_Widget_Class_Method_SET_ANIMATION_EVENT_PROCESSING}, /* 78 */
  {NULL, S, Xtango_Widget_Class_Method_SET_BGCOLOR}, /* 79 */
  {NULL, S, Xtango_Widget_Class_Method_SET_CALLBACK}, /* 80 */
  {NULL, S, Xtango_Widget_Class_Method_SET_COORD}, /* 81 */
  {NULL, S, Xtango_Widget_Class_Method_SET_DEBUG}, /* 82 */
  {NULL, S, Xtango_Widget_Class_Method_SET_DELAY}, /* 83 */
  {NULL, S, Xtango_Widget_Class_Method_ZOOM}, /* 84 */
  {NULL, S, Tango_GIF_Image_Class_Method_ISNEW}, /* 85 */
  {NULL, S, Tango_Image_Class_Method_SHOW}, /* 86 */

#undef SIZE_OF_FUNTAB_SEGMENT_4
#define SIZE_OF_FUNTAB_SEGMENT_4 86
#endif				/* WINTERP_XTANGO_WIDGET */
/****************** END: WINTERP Xtango METHODS *****************************/

/****************** BEGIN: WINTERP SgDropPocketWidget METHODS **********/
#ifdef SGI_DROP_POCKET_WIDGET	/* only for Irix 5.1 and IndigoMagic desktop */
  {NULL, S, Sg_Drop_Pocket_Widget_Class_Method_ADD_CALLBACK}, /* 1 */
  {NULL, S, Sg_Drop_Pocket_Widget_Class_Method_SET_CALLBACK}, /* 2 */
  {NULL, S, Sg_Finder_Widget_Class_Method_ADD_HISTORY_ITEM}, /* 3 */
  {NULL, S, Sg_Finder_Widget_Class_Method_CLEAR_HISTORY}, /* 4 */
  {NULL, S, Sg_Finder_Widget_Class_Method_SET_STRING}, /* 5 */
  {NULL, S, Sg_Finder_Widget_Class_Method_GET_STRING}, /* 6 */
  {NULL, S, Sg_Finder_Widget_Class_Method_GET_CHILD}, /* 7 */

#undef SIZE_OF_FUNTAB_SEGMENT_5
#define SIZE_OF_FUNTAB_SEGMENT_5 7
#endif /* SGI_DROP_POCKET_WIDGET */
/****************** END: WINTERP SgDropPocketWidget METHODS ************/

  /*
   * To add new conditionally-compilable methods to WINTERP, create
   * "segments" similar to the conditional compilation portions above,
   * (e.g. #ifdef WINTERP_XTANGO_WIDGET ... #endif). Update the
   * appropriate SIZE_OF_FUNTAB_SEGMENT_i with the count of the number
   * of new methods added within the "segment".
   *
   * INDEX_OF_LAST_FUNTAB_ENTRY_USED_BY_libWinterp is a constant needed by
   * Wfu_Funtab_Sanity_Check() -- note how cpp symbol SIZE_OF_FUNTAB_SEGMENT_i
   * gets modified by conditional compilation of features such as
   * -DWINTERP_XTANGO_WIDGET, -DHP_GRAPH_WIDGET, etc
   */
#define INDEX_OF_LAST_FUNTAB_ENTRY_USED_BY_libWinterp SIZE_OF_FUNTAB_SEGMENT_0 + SIZE_OF_FUNTAB_SEGMENT_1 + SIZE_OF_FUNTAB_SEGMENT_2 + SIZE_OF_FUNTAB_SEGMENT_3 + SIZE_OF_FUNTAB_SEGMENT_4 + SIZE_OF_FUNTAB_SEGMENT_5 + SIZE_OF_FUNTAB_SEGMENT_6 + SIZE_OF_FUNTAB_SEGMENT_7 + SIZE_OF_FUNTAB_SEGMENT_8 + SIZE_OF_FUNTAB_SEGMENT_9
 
  /*
   * To embed additional C-implemented primitives in WINTERP, you must
   * place new function table entries in file app_fundecl.h.
   * The indexes of entries in this file must correspond
   * to the indexes computed by CPP in app_funidx.h.
   * See comments preceding funtab[] declaration above for more
   * info on file app_fundecl.h, app_funextn.h, and app_funidx.h.
   *
   * If you want to place your app_fundecl.h file outside of 
   * the WINTERP source tree, just modify INCLUDES in the Makefile
   * such that the directory containing app_fundecl.h precedes '-I.'
   */
#include <app_fundecl.h>

/* Past this point, don't track INDEX_OF_LAST_FUNTAB_ENTRY_USED_BY_libWinterp
   and don't bother updating w_funtab.h with corresponding entries unless you
   are adding methods... */

  /****************** BEGIN: WINTERP FUNCTIONS (Motif >= 1.0)  **************/
  {"FFLUSH",			S, Prim_FFLUSH},
  {"FSCANF-FIXNUM",		S, Prim_FSCANF_FIXNUM},
  {"FSCANF-FLONUM",		S, Prim_FSCANF_FLONUM},
  {"FSCANF-STRING",		S, Prim_FSCANF_STRING},
  {"GET_EVENT_COORDS",		S, Wut_Prim_GET_EVENT_COORDS},
  {"GET_MOUSED_WIDGET",		S, Wut_Prim_GET_MOUSED_WIDGET},	
  {"GET_MOUSE_LOCATION",        S, Wut_Prim_GET_MOUSE_LOCATION},
  {"INPUT_ACTIVE_P",		S, Wicb_Prim_INPUT_ACTIVE_P},
  {"PCLOSE",			S, Prim_PCLOSE},
  {"POPEN",			S, Prim_POPEN},
  {"PROVIDE",			S, Prim_PROVIDE},
  {"READ_EVAL_PRINT",		S, Prim_READ_EVAL_PRINT},
  {"REDIRECT_STDERR",		S, Prim_REDIRECT_STDERR},
  {"REDIRECT_STDOUT",		S, Prim_REDIRECT_STDOUT},
  {"XT_REMOVE_EVENT_HANDLER",	S, Weh_Prim_XT_REMOVE_EVENT_HANDLER},
  {"REQUIRE",			S, Prim_REQUIRE},
  {"SYSTEM",			S, Prim_SYSTEM},
  {"TIMEOUT_ACTIVE_P",          S, Wto_Prim_TIMEOUT_ACTIVE_P},
  {"WIDGETOBJP",		S, Wcls_Prim_WIDGETOBJP},
  {"WINTERP_SHOW_BUSY",		S, Wshl_Prim_WINTERP_SHOW_BUSY},
  {"XM_GET_PIXMAP",		S, Wpm_Prim_XM_GET_PIXMAP},
  {"XM_GET_XIMAGE_FROM_FILE",	S, Wpm_Prim_XM_GET_IMAGE_FROM_FILE},
  {"XM_INSTALL_IMAGE",		S, Wpm_Prim_XM_INSTALL_IMAGE},
  {"XM_SET_MENU_CURSOR",        S, Wxm_Prim_XM_SET_MENU_CURSOR},
  {"XM_STRING_BYTE_COMPARE",	S, Wxms_Prim_XM_STRING_BYTE_COMPARE},
  {"XM_STRING_COMPARE",		S, Wxms_Prim_XM_STRING_COMPARE},
  {"XM_STRING_CONCAT",		S, Wxms_Prim_XM_STRING_CONCAT},	
  {"XM_STRING_COPY",		S, Wxms_Prim_XM_STRING_COPY},
  {"XM_STRING_CREATE",		S, Wxms_Prim_XM_STRING_CREATE},	
  {"XM_STRING_CREATE_L_TO_R",	S, Wxms_Prim_XM_STRING_CREATE_L_TO_R},
  {"XM_STRING_DIRECTION_CREATE",S, Wxms_Prim_XM_STRING_DIRECTION_CREATE},
  {"XM_STRING_EMPTY",		S, Wxms_Prim_XM_STRING_EMPTY},
  {"XM_STRING_GET_L_TO_R",	S, Wxms_Prim_XM_STRING_GET_L_TO_R},
  {"XM_STRING_LENGTH",		S, Wxms_Prim_XM_STRING_LENGTH},	
  {"XM_STRING_LINE_COUNT",	S, Wxms_Prim_XM_STRING_LINE_COUNT},
  {"XM_STRING_SEGMENT_CREATE",	S, Wxms_Prim_XM_STRING_SEGMENT_CREATE},	
  {"XM_STRING_SEPARATOR_CREATE",S, Wxms_Prim_XM_STRING_SEPARATOR_CREATE},
  {"XM_UNINSTALL_IMAGE",	S, Wpm_Prim_XM_UNINSTALL_IMAGE},
  {"XT_ADD_INPUT",		S, Wicb_Prim_XT_ADD_INPUT},
  {"XT_ADD_TIMEOUT",		S, Wto_Prim_XT_ADD_TIMEOUT},
  {"XT_MANAGE_CHILDREN",	S, Wxt_Prim_XT_MANAGE_CHILDREN},
  {"XT_PARSE_ACCELERATOR_TABLE", S, Wtx_Prim_XT_PARSE_ACCELERATOR_TABLE},
  {"XT_PARSE_TRANSLATION_TABLE", S, Wtx_Prim_XT_PARSE_TRANSLATION_TABLE},
  {"XT_REMOVE_CALLBACK",	S, Wcb_Prim_XT_REMOVE_CALLBACK},
  {"XT_REMOVE_INPUT",		S, Wicb_Prim_XT_REMOVE_INPUT},
  {"XT_REMOVE_TIMEOUT",		S, Wto_Prim_XT_REMOVE_TIMEOUT},
  {"XT_UNMANAGE_CHILDREN",	S, Wxt_Prim_XT_UNMANAGE_CHILDREN},
  {"X_ALLOC_COLOR",		S, Wut_Prim_X_ALLOC_COLOR},
  {"X_ALLOC_N_COLOR_CELLS_NO_PLANES", S, Wut_Prim_X_ALLOC_N_COLOR_CELLS_NO_PLANES},
  {"X_STORE_COLOR",		S, Wut_Prim_X_STORE_COLOR},
  {"X_BELL",			S, Wut_Prim_X_BELL},

  /****************** END: WINTERP FUNCTIONS (Motif >= 1.0)  ****************/

  /****************** BEGIN: WINTERP FUNCTIONS (Motif >= 1.1) ***************/
#ifdef WINTERP_MOTIF_11
  {"XM_CONVERT_UNITS",		S, Wxm_Prim_XM_CONVERT_UNITS},
  {"XM_CVT_CT_TO_XM_STRING",	S, Wxms_Prim_XM_CVT_CT_TO_XM_STRING},
  {"XM_CVT_XM_STRING_TO_CT",	S, Wxms_Prim_XM_CVT_XM_STRING_TO_CT},
  {"XM_GET_COLORS",		S, Wxm_Prim_XM_GET_COLORS},
  {"XM_SET_FONT_UNITS",		S, Wxm_Prim_XM_SET_FONT_UNITS},	
  {"XM_STRING_HAS_SUBSTRING",	S, Wxms_Prim_XM_STRING_HAS_SUBSTRING},
  {"XM_TRACKING_LOCATE",	S, Wxm_Prim_XM_TRACKING_LOCATE},
  {"XT_RESOLVE_PATHNAME",	S, Wxt_Prim_XT_RESOLVE_PATHNAME},
#endif /* WINTERP_MOTIF_11 */
  /****************** END: WINTERP FUNCTIONS (Motif >= 1.1) ******************/    
  
  /****************** BEGIN: WINTERP FUNCTIONS (Motif >= 1.2) ***************/
#ifdef WINTERP_MOTIF_12
  {"XM_GET_DESTINATION",	S, Wxm_Prim_XM_GET_DESTINATION},
#endif /* WINTERP_MOTIF_12 */
  /****************** END: WINTERP FUNCTIONS (Motif >= 1.2) ******************/    

  /****************** BEGIN: WINTERP Table FUNCTIONS *************************/    
#ifdef WINTERP_TABLE_WIDGET
  {"XT_TBL_CONFIG",		S, Prim_XT_TBL_CONFIG},
  {"XT_TBL_OPTIONS",		S, Prim_XT_TBL_OPTIONS},
  {"XT_TBL_POSITION",		S, Prim_XT_TBL_POSITION},
  {"XT_TBL_RESIZE",		S, Prim_XT_TBL_RESIZE},
#endif /* WINTERP_TABLE_WIDGET */
  /****************** END: WINTERP Table FUNCTIONS ***************************/    

  /****************** BEGIN: WINTERP Expect FUNCTIONS ************************/    
#ifdef WINTERP_EXPECT_SUBPROCESS
  {"EXP_GET_PID",		S, Prim_EXP_GET_PID},
  {"EXP_POPEN",			S, Prim_EXP_POPEN},
  {"EXP_SPAWN",			S, Prim_EXP_SPAWN},
  {"EXP_STTY_INIT",		S, Prim_EXP_STTY_INIT},
  {"EXP_WAIT",			S, Prim_EXP_WAIT},
  {"EXP_KILL",			S, Prim_EXP_KILL},
#endif /* WINTERP_EXPECT_SUBPROCESS */
  /****************** END: WINTERP Expect FUNCTIONS ***************************/    

  /****************** BEGIN: WINTERP Xtango FUNCTIONS *************************/    
#ifdef WINTERP_XTANGO_WIDGET
  {"TANGO:IMAGEOBJP",		S, Tcls_Prim_TANGOIMAGEOBJP},
  {"TANGO:PATH_ADD_HEAD",	S, Xtango_Prim_TANGO_PATH_ADD_HEAD},
  {"TANGO:PATH_ADD_TAIL",	S, Xtango_Prim_TANGO_PATH_ADD_TAIL},
  {"TANGO:PATH_COLOR",		S, Xtango_Prim_TANGO_PATH_COLOR},
  {"TANGO:PATH_COMPOSE",	S, Xtango_Prim_TANGO_PATH_COMPOSE},
  {"TANGO:PATH_CONCATENATE",	S, Xtango_Prim_TANGO_PATH_CONCATENATE},
  {"TANGO:PATH_COPY",		S, Xtango_Prim_TANGO_PATH_COPY},
  {"TANGO:PATH_CREATE",		S, Xtango_Prim_TANGO_PATH_CREATE},
  {"TANGO:PATH_DELETE_HEAD",	S, Xtango_Prim_TANGO_PATH_DELETE_HEAD},
  {"TANGO:PATH_DELETE_TAIL",	S, Xtango_Prim_TANGO_PATH_DELETE_TAIL},
  {"TANGO:PATH_DISTANCE",	S, Xtango_Prim_TANGO_PATH_DISTANCE},
  {"TANGO:PATH_DX",		S, Xtango_Prim_TANGO_PATH_DX},
  {"TANGO:PATH_DY",		S, Xtango_Prim_TANGO_PATH_DY},
  {"TANGO:PATH_EXAMPLE",	S, Xtango_Prim_TANGO_PATH_EXAMPLE},
  {"TANGO:PATH_EXTEND",		S, Xtango_Prim_TANGO_PATH_EXTEND},
  {"TANGO:PATH_FREE",		S, Xtango_Prim_TANGO_PATH_FREE},
  {"TANGO:PATH_INTERPOLATE",	S, Xtango_Prim_TANGO_PATH_INTERPOLATE},
  {"TANGO:PATH_ITERATE",	S, Xtango_Prim_TANGO_PATH_ITERATE},
  {"TANGO:PATH_LENGTH",		S, Xtango_Prim_TANGO_PATH_LENGTH},
  {"TANGO:PATH_MOTION",		S, Xtango_Prim_TANGO_PATH_MOTION},
  {"TANGO:PATH_REVERSE",	S, Xtango_Prim_TANGO_PATH_REVERSE},
  {"TANGO:PATH_ROTATE",		S, Xtango_Prim_TANGO_PATH_ROTATE},
  {"TANGO:PATH_SCALE",		S, Xtango_Prim_TANGO_PATH_SCALE},
  {"TANGO:PATH_SMOOTH",		S, Xtango_Prim_TANGO_PATH_SMOOTH},
  {"TANGO:PATH_TYPE",		S, Xtango_Prim_TANGO_PATH_TYPE},
  {"TANGO:TAP_EXCHANGE",	S, Xtango_Prim_TANGO_TAP_EXCHANGE},
  {"TANGO:TAP_SWITCH",		S, Xtango_Prim_TANGO_TAP_SWITCH},
  {"TANGO:TX_COMPOSE",		S, Xtango_Prim_TANGO_TX_COMPOSE},
  {"TANGO:TX_CONCATENATE",	S, Xtango_Prim_TANGO_TX_CONCATENATE},
  {"TANGO:TX_FREE",		S, Xtango_Prim_TANGO_TX_FREE},
  {"TANGO:TX_ITERATE",		S, Xtango_Prim_TANGO_TX_ITERATE},
  {"TANGO:TX_PERFORM",		S, Xtango_Prim_TANGO_TX_PERFORM},

  {"GIF_TO_PIXMAP",		S, Wpm_Prim_GIF_TO_PIXMAP},

#endif /* WINTERP_XTANGO_WIDGET */
  /****************** END: WINTERP Xtango FUNCTIONS ***************************/    

/******************************************************************************/
/************************ END: WINTERP PRIMITIVES *****************************/
/******************************************************************************/

/******************************************************************************/
/************** BEGIN: entries [20-end] copied from xlftab.c:funtab[] *********/
/******************************************************************************/

    /* evaluator functions */
{   "EVAL",		S, xeval	},
{   "APPLY",		S, xapply	},
{   "FUNCALL",		S, xfuncall	},
{   "QUOTE",		F, xquote	},
{   "FUNCTION",		F, xfunction	},
{   "BACKQUOTE",	F, xbquote	},
{   "LAMBDA",		F, xlambda	},

    /* symbol functions */
{   "SET",		S, xset		},
{   "SETQ",		F, xsetq	},
{   "SETF",		F, xsetf	},
{   "DEFUN",		F, xdefun	},
{   "DEFMACRO",		F, xdefmacro	},
{   "GENSYM",		S, xgensym	},
{   "MAKE-SYMBOL",	S, xmakesymbol	},
{   "INTERN",		S, xintern	},
{   "SYMBOL-NAME",	S, xsymname	},
{   "SYMBOL-VALUE",	S, xsymvalue	},
{   "SYMBOL-PLIST",	S, xsymplist	},
{   "GET",		S, xget		},
{   "PUTPROP",		S, xputprop	},
{   "REMPROP",		S, xremprop	},
{   "HASH",		S, xhash	},

    /* array functions */
{   "MAKE-ARRAY",	S, xmkarray	},
{   "AREF",		S, xaref	},

    /* list functions */
{   "CAR",		S, xcar		},
{   "CDR",		S, xcdr		},

{   "CAAR",		S, xcaar	},
{   "CADR",		S, xcadr	},
{   "CDAR",		S, xcdar	},
{   "CDDR",		S, xcddr	},

{   "CAAAR",		S, xcaaar	},
{   "CAADR",		S, xcaadr	},
{   "CADAR",		S, xcadar	},
{   "CADDR",		S, xcaddr	},
{   "CDAAR",		S, xcdaar	},
{   "CDADR",		S, xcdadr	},
{   "CDDAR",		S, xcddar	},
{   "CDDDR",		S, xcdddr	},

{   "CAAAAR",		S, xcaaaar	},
{   "CAAADR",		S, xcaaadr	},
{   "CAADAR",		S, xcaadar	},
{   "CAADDR",		S, xcaaddr	},
{   "CADAAR",		S, xcadaar	},
{   "CADADR",		S, xcadadr	},
{   "CADDAR",		S, xcaddar	},
{   "CADDDR",		S, xcadddr	},
{   "CDAAAR",		S, xcdaaar	},
{   "CDAADR",		S, xcdaadr	},
{   "CDADAR",		S, xcdadar	},
{   "CDADDR",		S, xcdaddr	},
{   "CDDAAR",		S, xcddaar	},
{   "CDDADR",		S, xcddadr	},
{   "CDDDAR",		S, xcdddar	},
{   "CDDDDR",		S, xcddddr	},

{   "CONS",		S, xcons	},
{   "LIST",		S, xlist	},
#ifdef COMMONLISPF
{   "LIST*",		S, xliststar	},
#endif
{   "APPEND",		S, xappend	},
{   "REVERSE",		S, xreverse	},
{   "LAST",		S, xlast	},
{   "NTH",		S, xnth		},
{   "NTHCDR",		S, xnthcdr	},
{   "MEMBER",		S, xmember	},
{   "ASSOC",		S, xassoc	},
{   "SUBST",		S, xsubst	},
{   "SUBLIS",		S, xsublis	},
{   "REMOVE",		S, xremove	},
{   "LENGTH",		S, xlength	},
{   "MAPC",		S, xmapc	},
{   "MAPCAR",		S, xmapcar	},
{   "MAPL",		S, xmapl	},
{   "MAPLIST",		S, xmaplist	},

    /* destructive list functions */
{   "RPLACA",		S, xrplca	},
{   "RPLACD",		S, xrplcd	},
{   "NCONC",		S, xnconc	},
{   "DELETE",		S, xdelete	},

    /* predicate functions */
{   "ATOM",		S, xatom	},
{   "SYMBOLP",		S, xsymbolp	},
{   "NUMBERP",		S, xnumberp	},
{   "BOUNDP",		S, xboundp	},
{   "NULL",		S, xnull	},
{   "LISTP",		S, xlistp	},
{   "CONSP",		S, xconsp	},
{   "MINUSP",		S, xminusp	},
{   "ZEROP",		S, xzerop	},
{   "PLUSP",		S, xplusp	},
{   "EVENP",		S, xevenp	},
{   "ODDP",		S, xoddp	},
{   "EQ",		S, xeq		},
{   "EQL",		S, xeql		},
{   "EQUAL",		S, xequal	},

    /* special forms */
{   "COND",		F, xcond	},
{   "CASE",		F, xcase	},
{   "AND",		F, xand		},
{   "OR",		F, xor		},
{   "LET",		F, xlet		},
{   "LET*",		F, xletstar	},
{   "IF",		F, xif		},
{   "PROG",		F, xprog	},
{   "PROG*",		F, xprogstar	},
{   "PROG1",		F, xprog1	},
{   "PROG2",		F, xprog2	},
{   "PROGN",		F, xprogn	},
{   "GO",		F, xgo		},
{   "RETURN",		F, xreturn	},
{   "DO",		F, xdo		},
{   "DO*",		F, xdostar	},
{   "DOLIST",		F, xdolist	},
{   "DOTIMES",		F, xdotimes	},
{   "CATCH",		F, xcatch	},
{   "THROW",		F, xthrow	},

    /* debugging and error handling functions */
{   "ERROR",		S, xerror	},
{   "CERROR",		S, xcerror	},
{   "BREAK",		S, xbreak	},
{   "CLEAN-UP",		S, xcleanup	},
{   "TOP-LEVEL",	S, xtoplevel	},
{   "CONTINUE",		S, xcontinue	},
{   "ERRSET",		F, xerrset	},
{   "BAKTRACE",		S, xbaktrace	},
{   "EVALHOOK",		S, xevalhook	},

    /* arithmetic functions */
{   "TRUNCATE",		S, xfix		},
{   "FLOAT",		S, xfloat	},
{   "+",		S, xadd		},
{   "-",		S, xsub		},
{   "*",		S, xmul		},
{   "/",		S, xdiv		},
{   "1+",		S, xadd1	},
{   "1-",		S, xsub1	},
{   "REM",		S, xrem		},
{   "MIN",		S, xmin		},
{   "MAX",		S, xmax		},
{   "ABS",		S, xabs		},
{   "SIN",		S, xsin		},
{   "COS",		S, xcos		},
{   "TAN",		S, xtan		},
{   "EXPT",		S, xexpt	},
{   "EXP",		S, xexp		},
{   "SQRT",		S, xsqrt	},
{   "RANDOM",		S, xrand	},

    /* bitwise logical functions */
{   "LOGAND",		S, xlogand	},
{   "LOGIOR",		S, xlogior	},
{   "LOGXOR",		S, xlogxor	},
{   "LOGNOT",		S, xlognot	},

    /* numeric comparison functions */
{   "<",		S, xlss		},
{   "<=",		S, xleq		},
{   "=",		S, xequ		},
{   "/=",		S, xneq		},
{   ">=",		S, xgeq		},
{   ">",		S, xgtr		},

    /* string functions */
#ifdef COMMONLISPF
{   "CONCATENATE",	S, xconcatenate },
#else
{   "STRCAT",		S, xstrcat	},
#endif
{   "SUBSEQ",		S, xsubseq	},
{   "STRING",		S, xstring	},
{   "CHAR",		S, xchar	},

    /* I/O functions */
{   "READ",		S, xread	},
{   "PRINT",		S, xprint	},
{   "PRIN1",		S, xprin1	},
{   "PRINC",		S, xprinc	},
{   "TERPRI",		S, xterpri	},
{   "FLATSIZE",		S, xflatsize	},
{   "FLATC",		S, xflatc	},

    /* file I/O functions */
{   "OPEN",		S, xopen	},
{   "FORMAT",		S, xformat	},
{   "CLOSE",		S, xclose	},
{   "READ-CHAR",	S, xrdchar	},
{   "PEEK-CHAR",	S, xpkchar	},
{   "WRITE-CHAR",	S, xwrchar	},
{   "READ-LINE",	S, xreadline	},

    /* system functions */
{   "LOAD",		S, xload	},
{   "DRIBBLE",		S, xtranscript	},

/* functions specific to xldmem.c */
{   "GC",		S, xgc		},
{   "EXPAND",		S, xexpand	},
{   "ALLOC",		S, xalloc	},
{   "ROOM",		S, xmem		},
#ifdef SAVERESTORE
{   "SAVE",		S, xsave	},
{   "RESTORE",		S, xrestore	},
#endif
/* end of functions specific to xldmem.c */

{   "TYPE-OF",		S, xtype	},
{   "EXIT",		S, xexit	},
{   "PEEK",		S, xpeek	},
{   "POKE",		S, xpoke	},
{   "ADDRESS-OF",	S, xaddrs	},

    /* new functions and special forms */
{   "VECTOR",		S, xvector	},
{   "BLOCK",		F, xblock	},
{   "RETURN-FROM",	F, xrtnfrom	},
{   "TAGBODY",		F, xtagbody	},
{   "PSETQ",		F, xpsetq	},
{   "FLET",		F, xflet	},
{   "LABELS",		F, xlabels	},
{   "MACROLET",		F, xmacrolet	},
{   "UNWIND-PROTECT",	F, xunwindprotect},
{   "PPRINT",		S, xpp		},
{   "STRING<",		S, xstrlss	},
{   "STRING<=",		S, xstrleq	},
{   "STRING=",		S, xstreql	},
{   "STRING/=",		S, xstrneq	},
{   "STRING>=",		S, xstrgeq	},
{   "STRING>",		S, xstrgtr	},
{   "STRING-LESSP",	S, xstrilss	},
{   "STRING-NOT-GREATERP",S,xstrileq	},
{   "STRING-EQUAL",	S, xstrieql	},
{   "STRING-NOT-EQUAL", S, xstrineq	},
{   "STRING-NOT-LESSP", S, xstrigeq	},
{   "STRING-GREATERP",	S, xstrigtr	},
{   "INTEGERP",		S, xintegerp	},
{   "FLOATP",		S, xfloatp	},
{   "STRINGP",		S, xstringp	},
{   "ARRAYP",		S, xarrayp	},
{   "STREAMP",		S, xstreamp	},
{   "OBJECTP",		S, xobjectp	},
{   "STRING-UPCASE",	S, xupcase	},
{   "STRING-DOWNCASE",	S, xdowncase	},
{   "NSTRING-UPCASE",	S, xnupcase	},
{   "NSTRING-DOWNCASE", S, xndowncase	},
{   "STRING-TRIM",	S, xtrim	},
{   "STRING-LEFT-TRIM", S, xlefttrim	},
{   "STRING-RIGHT-TRIM",S, xrighttrim	},
{   "WHEN",		F, xwhen	},
{   "UNLESS",		F, xunless	},
{   "LOOP",		F, xloop	},
{   "SYMBOL-FUNCTION",	S, xsymfunction },
{   "FBOUNDP",		S, xfboundp	},
{   "SEND",		S, xsend	},
{   "SEND-SUPER",	S, xsendsuper	},
{   "PROGV",		F, xprogv	},
{   "CHARACTERP",	S, xcharp	},
{   "CHAR-INT",		S, xcharint	},
{   "INT-CHAR",		S, xintchar	},
{   "READ-BYTE",	S, xrdbyte	},
{   "WRITE-BYTE",	S, xwrbyte	},
{   "MAKE-STRING-INPUT-STREAM", S, xmkstrinput	    },
{   "MAKE-STRING-OUTPUT-STREAM",S, xmkstroutput	    },
{   "GET-OUTPUT-STREAM-STRING", S, xgetstroutput    },
{   "GET-OUTPUT-STREAM-LIST",	S, xgetlstoutput    },
{   "GCD",		S, xgcd		},
{   "GET-LAMBDA-EXPRESSION",	S, xgetlambda	    },
{   "MACROEXPAND",	S, xmacroexpand },
{   "MACROEXPAND-1",	S, x1macroexpand},
{   "CHAR<",		S, xchrlss	},
{   "CHAR<=",		S, xchrleq	},
{   "CHAR=",		S, xchreql	},
{   "CHAR/=",		S, xchrneq	},
{   "CHAR>=",		S, xchrgeq	},
{   "CHAR>",		S, xchrgtr	},
{   "CHAR-LESSP",	S, xchrilss	},
{   "CHAR-NOT-GREATERP",S, xchrileq	},
{   "CHAR-EQUAL",	S, xchrieql	},
{   "CHAR-NOT-EQUAL",	S, xchrineq	},
{   "CHAR-NOT-LESSP",	S, xchrigeq	},
{   "CHAR-GREATERP",	S, xchrigtr	},
{   "UPPER-CASE-P",	S, xuppercasep	},
{   "LOWER-CASE-P",	S, xlowercasep	},
{   "BOTH-CASE-P",	S, xbothcasep	},
{   "DIGIT-CHAR-P",	S, xdigitp	},
{   "ALPHANUMERICP",	S, xalphanumericp},
{   "CHAR-UPCASE",	S, xchupcase	},
{   "CHAR-DOWNCASE",	S, xchdowncase	},
{   "DIGIT-CHAR",	S, xdigitchar	},
{   "CHAR-CODE",	S, xcharcode	},
{   "CODE-CHAR",	S, xcodechar	},
{   "ENDP",		S, xendp	},
{   "REMOVE-IF",	S, xremif	},
{   "REMOVE-IF-NOT",	S, xremifnot	},
{   "DELETE-IF",	S, xdelif	},
{   "DELETE-IF-NOT",	S, xdelifnot	},
{   "TRACE",		F, xtrace	},
{   "UNTRACE",		F, xuntrace	},
{   "SORT",		S, xsort	},
#ifdef ADDEDTAA
{   "GENERIC",		S, xgeneric	},
#endif
#ifdef TIMES
{   "TIME",		F, xtime	},
{   "GET-INTERNAL-RUN-TIME",	S, xruntime  },
{   "GET-INTERNAL-REAL-TIME",	S, xrealtime },
#endif
/* extra table entries */
#ifdef COMMONLISPF
#ifdef POSFCNS
{   "COUNT-IF",		S, xcountif	},
{   "FIND-IF",		S, xfindif	},
{   "POSITION-IF",	S, xpositionif	},
#endif
{   "COERCE",		S, xcoerce	},
{   "ELT",		S, xelt		},
#ifdef SRCHFCN
{   "SEARCH",		S, xsearch	},
#endif
#ifdef MAPFCNS
{   "MAP",		S, xmap		},
{   "SOME",		S, xsome	},
{   "EVERY",		S, xevery	},
{   "NOTANY",		S, xnotany	},
{   "NOTEVERY",		S, xnotevery	},
#endif
#endif
#ifdef BETTERIO
{   "FILE-POSITION",	S, xfileposition},
{   "FILE-LENGTH",	S, xfilelength	},
{   "FRESH-LINE",	S, xfreshline	},
{   "OPEN-STREAM-P",	S, xopenstreamp },
{   "INPUT-STREAM-P",	S, xinputstreamp},
{   "OUTPUT-STREAM-P",	S, xoutputstreamp},
#endif
#ifdef STRUCTS
{   "DEFSTRUCT",	F, xdefstruct	},
{   "%STRUCT-TYPE-P",	S, xstrtypep	},
{   "%MAKE-STRUCT",	S, xmkstruct	},
{   "%COPY-STRUCT",	S, xcpystruct	},
{   "%STRUCT-REF",	S, xstrref	},
{   "%STRUCT-SET",	S, xstrset	},
#endif
#if defined(STRUCTS) | defined(COMPLX)
{   "ASIN",		S, xasin	},
{   "ACOS",		S, xacos	},
{   "ATAN",		S, xatan	},
#endif
#ifdef APPLYHOOK
{   "APPLYHOOK",	S, xapplyhook	},
#endif

#ifdef COMMONLISPF
{   "NREVERSE",		S, xnreverse	},
{   "BUTLAST",		S, xbutlast	},
{   "TYPEP",		S, xtypep	},
#ifdef TIERNEY
{   "REDUCE",		S, xreduce	},
#endif
#ifdef REMDUPS
{   "REMOVE-DUPLICATES",S, xremove_duplicates },
#endif
#endif

#ifdef SETS
{   "ADJOIN",		S, xadjoin	    },
{   "UNION",		S, xunion	    },
{   "INTERSECTION",	S, xintersection    },
{   "SET-DIFFERENCE",	S, xset_difference  },
{   "SUBSETP",		S, xsubsetp	    },
#endif

#ifdef HASHFCNS
{   "GETHASH",		S, xgethash	    },
{   "REMHASH",		S, xremhash	    },
{   "MAKE-HASH-TABLE",	S, xmakehash	    },
{   "CLRHASH",		S, xclrhash	    },
{   "MAPHASH",		S, xmaphash	    },
{   "HASH-TABLE-COUNT", S, xhashcount	    },
#endif

#ifdef COMPLX
{   "COMPLEXP",		S, xcomplexp	    },
{   "COMPLEX",		S, xcomplex	    },
{   "CONJUGATE",	S, xconjugate	    },
{   "REALPART",		S, xrealpart	    },
{   "IMAGPART",		S, ximagpart	    },
{   "LOG",		S, xlog		    },
{   "FLOOR",		S, xfloor	    },
{   "CEILING",		S, xceil	    },
{   "ROUND",		S, xround	    },
{   "PHASE",		S, xphase	    },
{   "LCM",		S, xlcm		    },
#endif

#ifdef SPECIALS
{   "DEFCONSTANT",	F, xdefconstant	    },
{   "CONSTANTP",	S, xconstantp	    },
{   "DEFPARAMETER",	F, xdefparameter    },
{   "DEFVAR",		F, xdefvar	    },
{   "MAKUNBOUND",	S, xmakunbound	    },
#endif

#ifdef RANDOM
{   "MAKE-RANDOM-STATE",S, xmakerandom	    },
#endif

/* added by Niels Mayer to file xlbfun.c */
{   "COPY-ARRAY",	S, Prim_COPY_ARRAY	}, /* NPM  */
{   "ARRAY-INSERT-POS",	S, Prim_ARRAY_INSERT_POS}, /* NPM */
{   "ARRAY-DELETE-POS",	S, Prim_ARRAY_DELETE_POS}, /* NPM */

/******************************************************************************/
/************** END: entries [20-end] copied from xlftab.c:funtab[] ***********/
/******************************************************************************/

  {0,0,0}			/* end of table marker */

};

int ftabsize = sizeof(funtab); /* TAA MOD -- added validity check */

/*******************************************************************************
 * A sanity check called from main() which ensures that the number of indexes to
 * function table entries (in w_funtab.h) corresnpond to the actual number of
 * entries in funtab[] in this file.
 *
 * LAST_FUNTAB_POINTER_USED_BY_libWinterp is declared in w_funtab.h, and
 * INDEX_OF_LAST_FUNTAB_ENTRY_USED_BY_libWinterp is declared above.
 *******************************************************************************/
void Wfu_Funtab_Sanity_Check()
{
  extern char* app_name;	/* winterp.c */

  if ((LAST_FUNTAB_POINTER_USED_BY_libWinterp) != (INDEX_OF_LAST_FUNTAB_ENTRY_USED_BY_libWinterp)) {
    (void) fprintf(stderr,"%s: Fatal error: w_funtab.c out of sync with w_funtab.h\n\tLAST_FUNTAB_POINTER_USED_BY_libWinterp=%d\n\tINDEX_OF_LAST_FUNTAB_ENTRY_USED_BY_libWinterp=%d\n\t -- correct errors in those files and recompile!\n",
		   app_name,
		   LAST_FUNTAB_POINTER_USED_BY_libWinterp,
		   INDEX_OF_LAST_FUNTAB_ENTRY_USED_BY_libWinterp);
    exit(1);
  }
}
