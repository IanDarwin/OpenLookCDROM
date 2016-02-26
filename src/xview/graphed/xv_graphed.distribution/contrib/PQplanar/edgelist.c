/* (C) Universitaet Passau 1986-1991 */
/*****************************************************************/
/*         FILE : edgelist.c                                     */
/*        AUTOR : Uwe Schnieders                                 */
/*        UPDATE: 23.07.90                                       */
/*****************************************************************/

/*****************************************************************/
/*       INCLUDE                                                 */
/*****************************************************************/

#include "path.h"

#include STDI
#include SGRAPHI

#include "modula.h"
#include "listen.h"
#include "listen1.h"
#include "dfs.h"
#include "plzeich.h"
#include "sgraphhi.h"
#include "simplex.h"
#include "simplexh.h"
#include "edgelist.h"
#include "adj.h"
#include "neck.h"
#include "necklist.h"
#include "interfac.h"
#include <stdio.h>
#include <math.h>


/*****************************************************************/
/*                                                               */
/*  FUNCTION :                                                   */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/

edge_list_Ptr make_edgelist_rec(node,edge)
     Snode node;
     Sedge edge;
  BEGIN
    edge_list_Ptr hilf;

    NEW(hilf);
    hilf->xw   = 0.0;
    hilf->node = node;
    hilf->edge = edge;
    hilf->var  = 0;
    hilf->cores = NIL;
    hilf->full  = FALSE;
    hilf->aussen  = FALSE;
    hilf->reached  = FALSE;
    hilf->fix   = FALSE;
    hilf->fix_xw = 0.0;
    RETURN(hilf);
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

EDGELIST push_edgelist(edge_list,node,edge)
    EDGELIST edge_list;
    Snode     node;
    Sedge     edge;
  BEGIN
    RETURN(dpushlist(edge_list,(char*)make_edgelist_rec(node,edge)));
  END;


/*****************************************************************/
/*                                                               */
/*  FUNCTION :                                                   */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/

EDGELIST queue_edgelist(edge_list,node,edge)
    EDGELIST edge_list;
    Snode     node;
    Sedge     edge;
  BEGIN
    RETURN(dqueuelist(edge_list,(char*)make_edgelist_rec(node,edge)));
  END;



/*****************************************************************/
/*                                                               */
/*  FUNCTION :                                                   */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/

void edgelist_verbinde(rec1,rec2)
        EDGELIST rec1,rec2;
BEGIN
  EDGELIST_CORES(rec1) = rec2;
  EDGELIST_CORES(rec2) = rec1;
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

EDGELIST edgelist_insert_after(List)
        EDGELIST      List;
  BEGIN
    RETURN((EDGELIST)insert_after(List,(char*)make_edgelist_rec(EDGELIST_NODE(List),
                                                                (Sedge)NIL)));
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

EDGELIST edgelist_insert_before(List)
        EDGELIST      List;
  BEGIN
    RETURN((EDGELIST)insert_before(List,
                                   (char*)make_edgelist_rec(EDGELIST_NODE(List),
                                                                   (Sedge)NIL)));
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

void edgelist_insert_sort(node,edge,less)
    Snode     node;
    Sedge     edge;
    BOOLEAN  (*less)();
  BEGIN
    NODE_EDGELIST(node) = 
              dinsertSortlist(NODE_EDGELIST(node),
                              (char*)make_edgelist_rec(node,edge),less);
  ENDPROC
