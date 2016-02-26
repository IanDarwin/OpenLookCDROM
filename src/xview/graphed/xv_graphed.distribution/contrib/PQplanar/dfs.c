/* (C) Universitaet Passau 1986-1991 */
/*****************************************************************/
/*         FILE : dfs.c                                          */
/*        AUTOR : Uwe Schnieders                                 */
/*        UPDATE: 23.07.90                                       */
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

#include "modula.h"
#include "listen.h"
#include "listen1.h"
#include "sgraphhi.h"
#include "adj.h"
#include "dfs.h"
#include "plzeich.h"
#include "interfac.h"
#include "edgelist.h"
#include "stnumber.h"


/*****************************************************************/
/*       DEFINES                                                 */
/*****************************************************************/

#define MARK_OLD(n)          NODE_REACHED(n) = FALSE; 
#define MARK_NEW(n)          NODE_REACHED(n) = TRUE; 
#define MARKED_NEW(n)        NODE_REACHED(n) 

/*****************************************************************/
/*                                                               */
/*  FUNCTION :                                                   */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/


void init_DFS(graph)
        Sgraph         graph;
BEGIN
  Snode           node;

  for_all_nodes(graph,node)
    MARK_NEW(node);
  end_for_all_nodes(graph,node);
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


void init_DFS2(graph)
        Sgraph         graph;
BEGIN
  Snode           node;

  for_all_nodes(graph,node)
    DCLEAR_LIST(NODE_EDGELIST(node));
    NODE_EDGELIST(node) = NODE_ADJ_LIST(node);
    NODE_ADJ_LIST(node) = NIL;
  end_for_all_nodes(graph,node);
END
/*****************************************************************/
/*                                                                 */
/*  FUNCTION :                                                       */
/*                                                                */
/*   INPUT   :                                                     */
/*                                                                */
/*   OUTPUT  :                                                    */
/*                                                                */
/*****************************************************************/

 
void DFS(graph,y)
        Sgraph         graph;
        Snode        y;
BEGIN
  Sedge         edge;
  Snode         v;
  EDGELIST         ecke;

  MARK_OLD(y);
  for_edgelist(y,ecke)
    edge = EDGELIST_EDGE(ecke);
    v = OTHER_NODE(y,edge);
    IF ST_NUMBER(v) > ST_NUMBER(y) THEN
      PUSH_EDGELIST(NODE_EDGELIST(v) , v , edge );
      edgelist_verbinde(ecke,NODE_EDGELIST(v));
      IF MARKED_NEW(v) THEN
        DFS(graph,v);
      END;
    END;
  end_for_edgelist(y);

END

