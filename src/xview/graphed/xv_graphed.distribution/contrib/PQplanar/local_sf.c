/* (C) Universitaet Passau 1986-1991 */
/*****************************************************************/
/*         FILE : local_sf.c                                     */
/*        AUTOR : Uwe Schnieders                                 */
/*        UPDATE: 08.10.90                                       */
/*****************************************************************/

/*****************************************************************/
/*       INCLUDE                                                 */
/*****************************************************************/

#include <xview/xview.h>
#include <xview/panel.h>
#include <xview/frame.h>

#include <std.h>
#include <sgraph.h>

#include "modula.h"
#include "main.h"
#include "local_sf.h"

/*****************************************************************/
/*       EXTERN                                                  */
/*****************************************************************/

extern	Frame	base_frame;
extern	char	*int_to_ascii ();


/************************************************************************/
/*									*/
/*			GLOBALE FUNKTIONEN				*/
/*									*/
/************************************************************************/
/*									*/
/*	void	show_local_subframe ()		                	*/
/*									*/
/************************************************************************/

static	void	create_local_subframe	();
static		notify_local_buttons ();

static  BOOLEAN         local_subframe_opend = FALSE;

static	Frame		local_subframe;
static	Panel		local_panel;
static	Panel_item	local_do_button,
                        local_group_button,
			local_quit_button,
			local_optimize_object_mode_choice,
                        local_local_optimize_mode_choice,
			local_richtungs_mode_choice,
                        local_knoten_mode_choice   ;

static	char	*local_optimize_object_strings [] = {
			"Optimiere Winkel",	
			"Optimiere Kanten" 
		};
	
static	char	*local_optimize_mode_strings [] = {
			"Maximiere minimalen Wert",	
			"Minimiere maximalen Wert",
                        "Minimiere Varianz" 
		};
	
static	char	*richtungs_mode_strings [] = {
			"Alle Verbesserungen durchfuehren",	
			"Beste Richtung um einen Knoten waehlen",
                        "Besten Knoten waehlen" 
		};

static	char	*local_knoten_mode_strings [] = {
			"Knoten mit kleinstem Wert",	
			"Knoten mit groesstem Wert" 
		};
	
Local_Optimize_Object   local_optimize_object = LOCAL_OPTIMIZE_OBJECT_WINKEL ;

Local_Optimize_Mode   local_optimize_mode = LOCAL_MODE_MAX_MIN ;

Richtungs_Mode richtungs_mode = LOCAL_MODE_ALL ;

Knoten_Mode    knoten_mode    = LOCAL_MODE_SMALEST ;

/*****************************************************************/
/*                                                               */
/*  FUNCTION :                                                   */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/

static	void	create_local_subframe()
{
	Menu	local_subframe_menu;
	int	row_count = 0;
	
	local_subframe = (Frame)xv_create(base_frame, FRAME,
		FRAME_NO_CONFIRM,	TRUE,
		NULL);

	local_panel = (Panel)xv_create(local_subframe, PANEL, NULL);


	/* Entferne "Done" aus dem Menue des Subframes	*/
	local_subframe_menu = xv_get(local_subframe, WIN_MENU);
	menu_set (local_subframe_menu,
		MENU_REMOVE, 1,	/* Done */
		0);
	xv_set(local_subframe, WIN_MENU, local_subframe_menu, NULL);

	local_do_button = xv_create(local_panel, PANEL_BUTTON,
		PANEL_LABEL_STRING,	"Do ALL",
		XV_X,			xv_col(local_panel, 0),
		PANEL_NOTIFY_PROC,	notify_local_buttons,
		NULL);

	local_group_button = xv_create(local_panel, PANEL_BUTTON,
		PANEL_LABEL_STRING,	"Do Group",
		PANEL_NOTIFY_PROC,	notify_local_buttons,
		NULL);

	local_quit_button = xv_create(local_panel, PANEL_BUTTON,
		PANEL_LABEL_STRING,	"Quit",
		PANEL_NOTIFY_PROC,	notify_local_buttons,
		NULL);

	row_count += 2;
	
	local_optimize_object_mode_choice  = xv_create(local_panel, PANEL_CYCLE,
		XV_X,			xv_col(local_panel, 0),
		XV_Y,			xv_row(local_panel, row_count),
		PANEL_CHOICE_STRINGS,	local_optimize_object_strings [(INTEGER)LOCAL_OPTIMIZE_OBJECT_WINKEL],
					local_optimize_object_strings [(INTEGER)LOCAL_OPTIMIZE_OBJECT_KANTEN],
					NULL,
		PANEL_VALUE,		local_optimize_object,
		NULL);
		
        row_count += 2;
	
	local_local_optimize_mode_choice  = xv_create(local_panel, PANEL_CYCLE,
		XV_X,			xv_col(local_panel, 0),
		XV_Y,			xv_row(local_panel, row_count),
		PANEL_CHOICE_STRINGS,	local_optimize_mode_strings [(INTEGER)LOCAL_MODE_MAX_MIN],
					local_optimize_mode_strings [(INTEGER)LOCAL_MODE_MIN_MAX],
					local_optimize_mode_strings [(INTEGER)LOCAL_MODE_MIN_VARIANZ],
					NULL,
		PANEL_VALUE,		local_optimize_mode,
		NULL);
	
	row_count += 2;
	
	local_richtungs_mode_choice  = xv_create(local_panel, PANEL_CYCLE,
		XV_X,			xv_col(local_panel, 0),
		XV_Y,			xv_row(local_panel, row_count),
		PANEL_CHOICE_STRINGS,	richtungs_mode_strings [(INTEGER)LOCAL_MODE_ALL],
					richtungs_mode_strings [(INTEGER)LOCAL_MODE_BEST_DIRECTION],
					richtungs_mode_strings [(INTEGER)LOCAL_MODE_BEST_NODE],
					NULL,
		PANEL_VALUE,		richtungs_mode,
		NULL);

	row_count += 2;
	
	local_knoten_mode_choice  = xv_create(local_panel, PANEL_CYCLE,
		XV_X,			xv_col(local_panel, 0),
		XV_Y,			xv_row(local_panel, row_count),
		PANEL_CHOICE_STRINGS,	local_knoten_mode_strings [(INTEGER)LOCAL_MODE_SMALEST],
					local_knoten_mode_strings [(INTEGER)LOCAL_MODE_BIGGEST],
					NULL,
		PANEL_VALUE,		knoten_mode,
		NULL);
	
	window_fit(local_panel);
	window_fit(local_subframe);
		
ENDPROC


/*****************************************************************/
/*                                                               */
/*  FUNCTION :                                                   */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/

void	show_local_subframe ()
BEGIN
        INTEGER screenwidth,screenheight;
	
        IF NOT(local_subframe_opend) THEN 
          local_subframe_opend = TRUE;
          screenwidth  =  (int)xv_get(base_frame, XV_WIDTH);
          screenheight =  (int)xv_get(base_frame, XV_HEIGHT);

	  create_local_subframe ();
	
	  window_fit(local_panel);
	  window_fit(local_subframe);
	
	  xv_set(local_subframe,
		WIN_X,		0,
		WIN_Y,		screenheight / 2 - (int)xv_get(local_subframe, XV_HEIGHT) / 2,
		WIN_SHOW,	TRUE,
		NULL);
         ENDIF;
ENDPROC

/*****************************************************************/
/*                                                               */
/*  FUNCTION :                                                   */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/

Local void save_local_state()
  BEGIN
          local_optimize_object = 
            (Local_Optimize_Object) xv_get(local_optimize_object_mode_choice, PANEL_VALUE);
          local_optimize_mode = 
            (Local_Optimize_Mode) xv_get(local_local_optimize_mode_choice, PANEL_VALUE);
          richtungs_mode = 
            (Richtungs_Mode) xv_get(local_richtungs_mode_choice, PANEL_VALUE);

          knoten_mode = 
            (Knoten_Mode) xv_get(local_knoten_mode_choice, PANEL_VALUE);
  ENDPROC


/*****************************************************************/
/*                                                               */
/*  FUNCTION :                                                   */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/

static		notify_local_buttons (item, event)
        Panel_item	item;
        Event *		event;
  BEGIN	
	IF item == local_do_button THEN
                save_local_state();
		call_sgraph_proc(planar_local);
        ENDIF;
	IF item == local_group_button THEN
                save_local_state();
		call_sgraph_proc(planar_local_group);
        ENDIF;
        IF item == local_quit_button THEN
		xv_destroy_safe(local_subframe);
                local_subframe_opend = FALSE;
        ENDIF;
  ENDPROC;

/*****************************************************************/
/*                                                               */
/*  FUNCTION :                                                   */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/

Local_Optimize_Object get_optimize_object_mode()
  BEGIN
    RETURN (Local_Optimize_Object) xv_get(local_optimize_object_mode_choice, PANEL_VALUE);
  ENDPROC


/*****************************************************************/
/*                                                               */
/*  FUNCTION :                                                   */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/

Local_Optimize_Mode get_local_optimize_mode()
  BEGIN
    RETURN (Local_Optimize_Mode) xv_get(local_local_optimize_mode_choice, PANEL_VALUE);
  ENDPROC


/*****************************************************************/
/*                                                               */
/*  FUNCTION :                                                   */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/

Richtungs_Mode  get_richtungs_mode()
  BEGIN
    RETURN (Richtungs_Mode) xv_get(local_richtungs_mode_choice, PANEL_VALUE);
  ENDPROC


/*****************************************************************/
/*                                                               */
/*  FUNCTION :                                                   */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/

Knoten_Mode  get_knoten_mode()
  BEGIN
    RETURN (Knoten_Mode) xv_get(local_knoten_mode_choice, PANEL_VALUE);
  ENDPROC

