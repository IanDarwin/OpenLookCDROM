/* (C) Universitaet Passau 1986-1991 */
/*****************************************************************/
/*         FILE : plan_sf.c                                      */
/*        AUTOR : Uwe Schnieders                                 */
/*        UPDATE: 16.08.90                                       */
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
#include "plan_sf.h"

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
/*	void	show_plan_subframe (node_or_edge)			*/
/*									*/
/************************************************************************/

static	void	create_plan_subframe	();
static		notify_plan_buttons ();

static  BOOLEAN         plan_subframe_opend = FALSE;

static	Frame		plan_subframe;
static	Panel		plan_panel;
static	Panel_item	plan_do_button,
			plan_quit_button,
			plan_aussen_button,
			plan_fix_button,
			plan_optimize_mode_choice,
			plan_kantensortierung_mode_choice,
			plan_ausseneck_mode_choice,
			plan_st_number_mode_choice,
			plan_triangulierungs_mode_choice,
			plan_optimierungsrichtung_mode_choice,
                        plan_simplex_mode_choice;

static	char	*optimize_mode_strings [] = {
			"Einfache Optimierung",	
			"Optimierung mit Triangulierung" 
		};
	
static	char	*kantensortierung_mode_strings [] = {
			"Kantensortierung aus planar gezeichneten Graphen",	
			"Kantensortierung durch Planaritaetstest" 
		};
		
static	char	*ausseneck_mode_strings [] = {
			"Maximales Neck als Ausseneck",	
			"Minimales Neck als Ausseneck",	
			"Rechteck       als Ausseneck",	
			"Maximale Ausdehnung als Ausseneck" 
		};
			
static	char	*optimierungsrichtung_mode_strings [] = {
			"Globale Optimierung",	
			"Optimierung von Innen nach Aussen",	
			"Optimierung von Aussen nach Innen"
		};

static	char	*st_number_mode_strings [] = {
			"The Graph is ST-Numbered",
                        "Get ST-Number from Label",
			"Compute ST-Numbers"	
		};
	
static	char	*triangulierungs_mode_strings [] = {
			"Triangulierung mit Tiefensuche",	
			"Strahlenfoermige Triangulierung",	
			"Rand-Triangulierung",
                        "Triangulirung durch Punkteinfuegen (Im Umbau)",
                        "Triangulierung mit Knotensortierung (Im Umbau)"
		};
	
static	char	*simplex_mode_strings [] = {
			"Einfacher Simplexalgorithmus",	
			"MPS - Simplexalgorithmus"
		};
	
Optimize_Mode   optimize_mode = MODE_EASY ;

Kantensortierung_Mode kantensortierung_mode = MODE_AUS_PLANARTEST ;

Ausseneck_Mode ausseneck_mode = MODE_MAXIMALES_NECK ;

Optimierungsrichtung_Mode optimierungsrichtung_mode = MODE_GLOBALE_OPITMIERUNG ;

ST_number_Mode st_number_mode = MODE_COMPUTE_ST_NUMBER;

Triangulierungs_Mode triangulierungs_mode = MODE_TIEFENSUCHE_TRIANGULIERUNG;

Simplex_Mode simplex_mode = MODE_EINFACHER_SIMPLEX;

/*****************************************************************/
/*                                                               */
/*  FUNCTION :                                                   */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/

static	void	create_plan_subframe()
{
	Menu	plan_subframe_menu;
	int	row_count = 0;
	
	plan_subframe = (Frame)xv_create(base_frame, FRAME,
		FRAME_NO_CONFIRM,	TRUE,
		NULL);

	plan_panel = (Panel)xv_create(plan_subframe, PANEL, NULL);


	plan_subframe_menu = xv_get(plan_subframe, WIN_MENU);
	menu_set (plan_subframe_menu,
		MENU_REMOVE, 1,	/* Done */
		0);
	xv_set(plan_subframe, WIN_MENU, plan_subframe_menu, NULL);

	
	plan_do_button = xv_create(plan_panel, PANEL_BUTTON,
		PANEL_LABEL_STRING,	"Draw",
		XV_X,			xv_col(plan_panel, 0),
		PANEL_NOTIFY_PROC,	notify_plan_buttons,
		NULL);

	plan_aussen_button = xv_create(plan_panel, PANEL_BUTTON,
		PANEL_LABEL_STRING,	"Aussen",
		PANEL_NOTIFY_PROC,	notify_plan_buttons,
		NULL);


	plan_fix_button = xv_create(plan_panel, PANEL_BUTTON,
		PANEL_LABEL_STRING,	"Fix",
		PANEL_NOTIFY_PROC,	notify_plan_buttons,
		NULL);


	plan_quit_button = xv_create(plan_panel, PANEL_BUTTON,
		PANEL_LABEL_STRING,	"Quit",
		PANEL_NOTIFY_PROC,	notify_plan_buttons,
		NULL);

	row_count += 2;
	
	plan_st_number_mode_choice = xv_create(plan_panel, PANEL_CYCLE,
		XV_X,			xv_col(plan_panel, 0),
		XV_Y,			xv_row(plan_panel, row_count),
		PANEL_CHOICE_STRINGS,	st_number_mode_strings[(INTEGER)MODE_GRAPH_IS_STNUMBERT],
					st_number_mode_strings[(INTEGER)MODE_STNUMBER_FROM_LABEL],
					st_number_mode_strings[(INTEGER)MODE_COMPUTE_ST_NUMBER],
					NULL,
		PANEL_VALUE,		st_number_mode,
		NULL);

	row_count += 2;
	
	plan_kantensortierung_mode_choice = xv_create(plan_panel, PANEL_CYCLE,
		XV_X,			xv_col(plan_panel, 0),
		XV_Y,			xv_row(plan_panel, row_count),
		PANEL_CHOICE_STRINGS,	kantensortierung_mode_strings [(INTEGER)MODE_AUS_PLANAREN_GRAPH],
					kantensortierung_mode_strings [(INTEGER)MODE_AUS_PLANARTEST],
					NULL,
		PANEL_VALUE,		kantensortierung_mode,
		NULL);
	
	row_count += 2;
	
	plan_ausseneck_mode_choice = xv_create(plan_panel, PANEL_CYCLE,
		XV_X,			xv_col(plan_panel, 0),
		XV_Y,			xv_row(plan_panel, row_count),
		PANEL_CHOICE_STRINGS,	ausseneck_mode_strings [(INTEGER)MODE_MAXIMALES_NECK],
					ausseneck_mode_strings [(INTEGER)MODE_MINIMALES_NECK],
					ausseneck_mode_strings [(INTEGER)MODE_MAXIMALE_AUSDEHNUNG],
					NULL,
		PANEL_VALUE,		ausseneck_mode,
		NULL);
	
	row_count += 2;
	
	plan_optimize_mode_choice = xv_create(plan_panel, PANEL_CYCLE,
		XV_X,			xv_col(plan_panel, 0),
		XV_Y,			xv_row(plan_panel, row_count),
		PANEL_CHOICE_STRINGS,	optimize_mode_strings [(INTEGER)MODE_EASY],
					optimize_mode_strings [(INTEGER)MODE_TRAINGULATION],
					NULL,
		PANEL_VALUE,		optimize_mode,
		NULL);
	
	
	row_count += 2;
	
	plan_triangulierungs_mode_choice = xv_create(plan_panel, PANEL_CYCLE,
		XV_X,			xv_col(plan_panel, 0),
		XV_Y,			xv_row(plan_panel, row_count),
		PANEL_CHOICE_STRINGS,	triangulierungs_mode_strings[(INTEGER)MODE_TIEFENSUCHE_TRIANGULIERUNG],
					triangulierungs_mode_strings[(INTEGER)MODE_STRAHL_TRIANGULIERUNG],
					triangulierungs_mode_strings[(INTEGER)MODE_RAND_TRIANGULIERUNG],
					NULL,
		PANEL_VALUE,		triangulierungs_mode,
		NULL);

	row_count += 2;
	
	plan_simplex_mode_choice = xv_create(plan_panel, PANEL_CYCLE,
		XV_X,			xv_col(plan_panel, 0),
		XV_Y,			xv_row(plan_panel, row_count),
		PANEL_CHOICE_STRINGS,	simplex_mode_strings[(INTEGER)MODE_EINFACHER_SIMPLEX],
					simplex_mode_strings[(INTEGER)MODE_MPS_SIMPLEX],
					NULL,
		PANEL_VALUE,		simplex_mode,
		NULL);

	window_fit(plan_panel);
	window_fit(plan_subframe);
		
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

void	show_plan_subframe ()
BEGIN
        INTEGER screenwidth,screenheight;
	
        IF NOT(plan_subframe_opend) THEN 
          plan_subframe_opend = TRUE;
          screenwidth  =  (int)xv_get(base_frame, XV_WIDTH);
          screenheight =  (int)xv_get(base_frame, XV_HEIGHT);

	  create_plan_subframe ();
	
	  window_fit(plan_panel);
	  window_fit(plan_subframe);
	
	  xv_set(plan_subframe,
		WIN_X,		screenwidth  / 2 - (int)xv_get(plan_subframe, XV_WIDTH) / 2,
		WIN_Y,		screenheight / 2 - (int)xv_get(plan_subframe, XV_HEIGHT) / 2,
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

Local void save_plan_state()
  BEGIN
    optimize_mode = 
     (Optimize_Mode) xv_get(plan_optimize_mode_choice, PANEL_VALUE);
    kantensortierung_mode = 
     (Kantensortierung_Mode) xv_get(plan_kantensortierung_mode_choice, PANEL_VALUE);
    ausseneck_mode = 
     (Ausseneck_Mode) xv_get(plan_ausseneck_mode_choice);
    optimierungsrichtung_mode = 
     (Optimierungsrichtung_Mode) xv_get(plan_optimierungsrichtung_mode_choice, PANEL_VALUE);
    st_number_mode = 
     (ST_number_Mode) xv_get(plan_st_number_mode_choice, PANEL_VALUE);
    triangulierungs_mode = 
     (Triangulierungs_Mode) xv_get(plan_triangulierungs_mode_choice, PANEL_VALUE);
    simplex_mode = 
     (Simplex_Mode) xv_get(plan_simplex_mode_choice, PANEL_VALUE);    
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

static		notify_plan_buttons (item, event)
        Panel_item	item;
        Event *		event;
  BEGIN	
	IF item == plan_do_button THEN
                save_plan_state();
		xv_destroy_safe(plan_subframe);
                plan_subframe_opend = FALSE;
		call_sgraph_proc(planar_draw);
                remove_Work_sgraph();
    
        ENDIF;
        IF item == plan_quit_button THEN
	        save_plan_state();
                xv_destroy_safe(plan_subframe);
                plan_subframe_opend = FALSE;
        ENDIF;
        IF item == plan_aussen_button THEN
		call_sgraph_proc(planar_aussen);
	ENDIF;
        IF item == plan_fix_button THEN
		call_sgraph_proc(planar_fix);
	ENDIF;
	force_repainting ();
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

Optimize_Mode get_optimize_mode()
  BEGIN
    RETURN (Optimize_Mode) xv_get(plan_optimize_mode_choice, PANEL_VALUE);
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

Kantensortierung_Mode  get_kantensortierung_mode()
  BEGIN
    RETURN (Kantensortierung_Mode) xv_get(plan_kantensortierung_mode_choice, PANEL_VALUE);
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

Ausseneck_Mode get_ausseneck_mode()
  BEGIN
    RETURN (Ausseneck_Mode) xv_get(plan_ausseneck_mode_choice, PANEL_VALUE);
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

Optimierungsrichtung_Mode get_optimierungsrichtung_mode()
  BEGIN
  RETURN (Optimierungsrichtung_Mode) xv_get(plan_optimierungsrichtung_mode_choice, PANEL_VALUE);
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

ST_number_Mode get_st_number_mode()
  BEGIN
    RETURN (ST_number_Mode) xv_get(plan_st_number_mode_choice, PANEL_VALUE);
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

Triangulierungs_Mode  get_triangulierungs_mode()
  BEGIN
    RETURN  (Triangulierungs_Mode) xv_get(plan_triangulierungs_mode_choice, PANEL_VALUE);
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

Simplex_Mode  get_simplex_mode()
  BEGIN
    RETURN  (Simplex_Mode) xv_get(plan_simplex_mode_choice, PANEL_VALUE);
  ENDPROC

