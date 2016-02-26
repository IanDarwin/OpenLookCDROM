/* (C) Universitaet Passau 1986-1991 */
/*****************************************************************/
/*         FILE : main.c                                         */
/*        AUTOR : Uwe Schnieders                                 */
/*        UPDATE: 18.09.90                                       */
/*****************************************************************/
/*                                                               */
/*                                                               */
/*                                                               */
/*                                                               */
/*                                                               */
/*                                                               */
/*                                                               */
/*                                                               */
/*                                                               */
/*                                                               */
/*****************************************************************/

/*****************************************************************/
/*       INCLUDE                                                 */
/*****************************************************************/

#include "path.h"

#include STDI
#include SGRAPHI
#include SLISTI
#include GRAPHEDI

#include "listen.h"
#include "listen1.h"
#include "sgraphhi.h"
#include "modula.h"
#include "main.h"
#include "adj.h"
#include "interfac.h"
#include "stnumber.h"
#include "plan_sf.h"
#include "local_sf.h"
#include "algorithms.h"

/****************************************************************************/

        Sgraph                Work_sgraph = empty_sgraph;



/*****************************************************************/
/*                                                               */
/*  FUNCTION :                                                   */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/

char *planar_menu_test_proc(menu,menu_item)
  char *menu, *menu_item;
  BEGIN
    RETURN(call_sgraph_proc(planar_test));
  END

/*****************************************************************/
/*                                                               */
/*  FUNCTION :                                                   */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/

char *planar_menu_draw_proc(menu,menu_item)
  char *menu, *menu_item;
  BEGIN
    call_sgraph_proc(make_Work_sgraph);
    show_plan_subframe ();
    RETURN(NIL);
  END


/*****************************************************************/
/*                                                               */
/*  FUNCTION :                                                   */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/

char *planar_menu_local_proc(menu,menu_item)
          char *menu, *menu_item;
  BEGIN
    show_local_subframe ();
    RETURN(NIL);
  END


/*****************************************************************/
/*                                                               */
/*  FUNCTION :                                                   */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/

void make_Work_sgraph(info)
      Sgraph_proc_info info;
  BEGIN
    Work_sgraph = copy_sgraph(info->sgraph);
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

void remove_Work_sgraph()
  BEGIN
    destroy_graph(&Work_sgraph);
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

void planar_test(info)
  Sgraph_proc_info info;
  BEGIN
        Snode        s, t;
        Sedge        e;

	if (info->sgraph != empty_sgraph && info->sgraph->nodes != empty_node) {
	
		if (!test_sgraph_biconnected (info->sgraph)) {
			error ("Graph is not biconnected\n");
			return;
		}
		
		make_Work_sgraph(info);

		s = first_node_in_graph(Work_sgraph) ;
		e = s -> slist;
		IF !e THEN 
			e = s -> tlist;
			t = e -> snode;
		ELSE
			t = e -> tnode;
		ENDIF;
		undirected_st_number (Work_sgraph,s,t );

		IF  planar(Work_sgraph) THEN
			message("The graph is planar.\n");
		ELSE
			message("The graph is not planar.\n");
		ENDIF;

		remove_Work_sgraph();
	
	}
  END


/*****************************************************************/
/*                                                               */
/*  FUNCTION :                                                   */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/

void planar_draw(info)
  Sgraph_proc_info info;
  BEGIN
    draw_planar(Work_sgraph,info);
    info->repaint  = TRUE;
    info->recenter = TRUE;
  END

/*****************************************************************/
/*                                                               */
/*  FUNCTION :                                                   */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/

void planar_aussen(info)
  Sgraph_proc_info info;
  BEGIN
    setze_aussen_punkte(Work_sgraph,info);
  END

/*****************************************************************/
/*                                                               */
/*  FUNCTION :                                                   */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/

void planar_fix(info)
  Sgraph_proc_info info;
  BEGIN
    call_sgraph_proc(make_Work_sgraph);
    setze_fixe_punkte(Work_sgraph,info);
  END


/*****************************************************************/
/*                                                               */
/*  FUNCTION :                                                   */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/

void planar_local(info)
        Sgraph_proc_info info;
  BEGIN
        Slist list;

    Work_sgraph = copy_sgraph(info->sgraph);
    list = make_slist_of_sgraph (Work_sgraph);
    draw_local(Work_sgraph,list);
    store_xy_back(Work_sgraph,info->sgraph);
    destroy_graph(&Work_sgraph);
    info->repaint  = TRUE;
    info->recenter = TRUE;
  END

/*****************************************************************/
/*                                                               */
/*  FUNCTION :                                                   */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/

void planar_local_group(info)
        Sgraph_proc_info info;
  BEGIN
        Slist list;

    list = empty_slist;
    Work_sgraph = copy_sgraph(info->sgraph);
    IF info->selected == SGRAPH_SELECTED_GROUP THEN
      list = info->selection.group;
    ENDIF;
    IF info->selected == SGRAPH_SELECTED_SNODE THEN
      list = new_slist(make_attr(ATTR_DATA,info->selection.snode));
    ENDIF;
    convert_list_from_graph_to_graph(list,info->sgraph,Work_sgraph);
    draw_local(Work_sgraph,list);
    store_xy_back(Work_sgraph,info->sgraph);
    destroy_graph(&Work_sgraph);
    info->repaint  = TRUE;
    info->recenter = TRUE;
  END


