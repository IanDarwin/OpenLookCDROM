/* (C) Universitaet Passau 1986-1991 */
/* Sammelt die externen Deklarationen der Module */

#include "std.h"
#include "sgraph.h"
#include "slist.h"
#include "graphed.h"
#include <xview/xview.h>
#include <xview/panel.h>
#include <math.h>

typedef enum
  { NO_ERROR,
    NODES_HAVE_SAME_COORDINATES,
    GRAPH_CONTAINS_SELF_LOOPS,
    GRAPH_CONTAINS_MULTIPLE_EDGES,
    NO_NODES_SELECTED,
    GRAPH_NOT_CONNECTED,
    PLAIN_OF_SUBGRAPH_NOT_CONNECTED,
    SUBGRAPH_NOT_CONNECTED,
    RESTGRAPH_NOT_CONNECTED,
    EDGE_OF_RESTGRAPH_INTERSECTS_SUBGRAPH,
    SELECTED_SUBGRAPH_IS_BLOCKED,
    SUBGRAPH_WITH_BORDER_NOT_PLANAR,
    NOT_ENOUGH_MEMORY
  } Error;

typedef enum
  { INNER,
    OUTER,
    ALL
  } WHERE;

typedef struct nodepair
  { Snode snode, tnode } *NODEPAIR;

typedef enum
  { CROSSTYPE1,   /* 2 Kanten schneiden sich             */
    CROSSTYPE2,   /* 1 Knoten liegt auf einer Kante      */
    CROSSTYPE3    /* 2 Kanten haben einen Teil gemeinsam */
  } CROSSTYPE;

typedef struct virtualobject
  { Slist     virtualnodes;
    Slist     oldedges;                         /* Paare von Knoten (NODEPAIR) */
    Slist     virtualedges;                     /* Kanten (Sedge)              */
  } *VIRTUALOBJECT;

typedef struct polygonedge
  { Snode node1, node2;
    Sedge edge;
  } *POLYGONEDGE;
  
typedef enum
  { NODE,
    EDGELIST
  } CYCLETYPE;

typedef struct global_tool_info
  { int                         V;                /* Knotenzahl                          */
    int                         E;                /* Kantenzahl                          */
    Sgraph                      graph;
    Sgraph_selected             selected;
    Sgraph_selection            selection;
    Slist                       innercycle;       /* Kantenliste des inneren Randes      */
    CYCLETYPE                   outercycletype;   /* Kantenliste oder Knoten             */
    Slist                       outercycle;       /* Liste des aeusseren Randes          */
    Slist                       connecting_edges; /* Kantenliste der Verbindungen von    */
                                                  /* innen nach aussen                   */
    Error                       error;
    Slist                       innercrosses;
    Slist                       outercrosses;

    enum {FIXEDPARTS, SUBGRAPH} algor;            /* Optionen fuer Springembedder        */
    Slist                       functions;
    int *  			node_i;           /* Array mit Zeiger auf Knoten i       */
    int *			dij;              /* kuerzester Weg zwischen i und j     */
    int *			lij;              /* optimale Laenge zwischen i und j    */
    double *			kij;              /* Kraft zwischen i und j              */
    int                         edgelength;       /* optimale Laenge einer Kante         */
    float                       springconst;            /* Federnkonstante                     */
    int *			partikel;         /* Array aller vorhandenen Partikel    */

    Slist                       outerpolygon;     /* globale Variable fuer Bewegung      */
    Slist                       innerpolygon;
    float                       min_x, min_y;
    int                         animation;
    int                         intersection;
    Slist			garbage;
  } *GLOBAL_TOOL_INFO;

#define RW_NUMBER_OF_NODES Global_Tool_Info->V
#define RW_NUMBER_OF_EDGES Global_Tool_Info->E
#define RW_GRAPH Global_Tool_Info->graph
#define RW_SELECTED Global_Tool_Info->selected
#define RW_SELECTION Global_Tool_Info->selection
#define RW_ERROR Global_Tool_Info->error
#define RW_INNERCYCLE Global_Tool_Info->innercycle
#define RW_CONNECTING_EDGES Global_Tool_Info->connecting_edges
#define RW_OUTERCYCLETYPE Global_Tool_Info->outercycletype
#define RW_OUTERCYCLE Global_Tool_Info->outercycle
#define RW_INNERCROSSES Global_Tool_Info->innercrosses
#define RW_OUTERCROSSES Global_Tool_Info->outercrosses

#define RW_SPRING_ALGOR Global_Tool_Info->algor
#define RW_SPRING_FUNCTIONS Global_Tool_Info->functions
#define RW_SPRING_EDGELENGTH Global_Tool_Info->edgelength
#define RW_SPRING_CONST Global_Tool_Info->springconst
#define RW_SPRING_NODE Global_Tool_Info->node_i
#define RW_SPRING_NODE_I(i) Global_Tool_Info->node_i[i]
#define RW_SPRING_DIJ Global_Tool_Info->dij
#define RW_SPRING_LIJ Global_Tool_Info->lij
#define RW_SPRING_KIJ Global_Tool_Info->kij
#define RW_SPRING_d(ii,jj) Global_Tool_Info->dij[ii + RW_NUMBER_OF_NODES*jj]
#define RW_SPRING_l(ii,jj) Global_Tool_Info->lij[ii + RW_NUMBER_OF_NODES*jj]
#define RW_SPRING_k(ii,jj) Global_Tool_Info->kij[ii + RW_NUMBER_OF_NODES*jj]
#define RW_SPRING_PARTIKEL Global_Tool_Info->partikel
#define RW_SPRING_PARTIKEL_I(i) ((PARTIKEL)Global_Tool_Info->partikel[i])

#define RW_OUTERPOLYGON Global_Tool_Info->outerpolygon
#define RW_INNERPOLYGON Global_Tool_Info->innerpolygon
#define RW_MIN_X Global_Tool_Info->min_x
#define RW_MIN_Y Global_Tool_Info->min_y
#define RW_ANIMATION Global_Tool_Info->animation
#define RW_INTERSECTION Global_Tool_Info->intersection
#define RW_GARBAGE Global_Tool_Info->garbage

typedef struct edgelistmarks
  { float alpha;
    Sedge edge;
  } *EDGELISTMARKS;

typedef struct nodemarks
  { int visited;
    int selected;
    int number;
    float x;
    float y;
    Slist Listmarks;
    Snode copied_node;       /* fuer planaren Graph mit Rand, zeigt auf entsprechenden Knoten */
  } *NODEMARKS;

#define VISITED(node) (((NODEMARKS)attr_data(node))->visited)
#define SELECTED(node) (((NODEMARKS)attr_data(node))->selected)
#define NUMBER(node) (((NODEMARKS)attr_data(node))->number)
#define X_ORDINATE(node) (((NODEMARKS)attr_data(node))->x)
#define Y_ORDINATE(node) (((NODEMARKS)attr_data(node))->y)
#define LISTMARKS(node) (((NODEMARKS)attr_data(node))->Listmarks)
#define EDGE_OF_LISTMARKS(list) ((Sedge)((EDGELISTMARKS)attr_data(list))->edge)
#define COPIED_NODE(node) (((NODEMARKS)attr_data(node))->copied_node)

typedef struct cross
  { float x, y;    /* Schnittpunkt */
    Sedge edge;    /* Kante, die die urspruengliche Kante schneidet */
    CROSSTYPE crosstype;
  } *CROSS;

typedef struct edgecross
  { float x, y;
    CROSS cross;
  } *EDGECROSS;


/* allgemeine Grundfunktionen */


static NODEMARKS nodemarks;
static GLOBAL_TOOL_INFO Global_Tool_Info;

static void RW_Message_Error ()

  {
   switch(RW_ERROR)
     {
      case NODES_HAVE_SAME_COORDINATES           : error("some nodes have same coordinates\n");
                                                   break;
      case GRAPH_CONTAINS_SELF_LOOPS             : error("the graph contains self loops\n");
                                                   break;
      case GRAPH_CONTAINS_MULTIPLE_EDGES         : error("the graph contains multiple edges\n");
                                                   break;
      case NO_NODES_SELECTED                     : error("there are no nodes selected\n");
                                                   break;
      case PLAIN_OF_SUBGRAPH_NOT_CONNECTED       : error("the plain of the selected subgraph is not connected\n");
                                                   break;
      case GRAPH_NOT_CONNECTED                   : error("the graph is not connected\n");
                                                   break;
      case SUBGRAPH_NOT_CONNECTED                : error("the selected subgraph is not connected\n");
                                                   break;
      case RESTGRAPH_NOT_CONNECTED               : error("the restgraph is not connected\n");
                                                   break;
      case EDGE_OF_RESTGRAPH_INTERSECTS_SUBGRAPH : error("the selected subgraph is intersected\n        by an edge of the restgraph\n");
                                                   break;
      case SELECTED_SUBGRAPH_IS_BLOCKED          : error("the selected subgraph is blocked\n");
                                                   break;
      case SUBGRAPH_WITH_BORDER_NOT_PLANAR       : error("the selected subgraph with\n        its border is not planar\n");
                                                   break;
      case NOT_ENOUGH_MEMORY                     : error("not enough memory\n");
                                                   break;
      default                                    : break;
     }
  } /* RW_Message_Error */
static float Angle (node1,node2)
  Snode node1, node2;

  {
   float dx, dy;
   dx = X_ORDINATE(node2) - X_ORDINATE(node1);
   dy = Y_ORDINATE(node1) - Y_ORDINATE(node2);
   if ((dx > 0) && (dy >= 0))
     return (atan(dy/dx));
   else
     if ((dx <= 0) && (dy > 0))
       return (M_PI_2 + atan((-dx)/dy));
     else
       if ((dx < 0) && (dy <= 0))
         return (M_PI + atan(dy/dx));
       else
         return (M_PI + M_PI_2 + atan(dx/-dy));
  } /* Angle */

static void Make_ordered_edgelist_of_node (node)
  Snode node;

  {
   char *malloc();
   EDGELISTMARKS mark;
   Snode node2;
   Sedge edge;
   Slist edgelist;
   Slist pseudolist2;
   float angle, oldangle;
   EDGELISTMARKS pseudomark1, pseudomark2;
   pseudomark1 = (EDGELISTMARKS)malloc(sizeof(struct edgelistmarks));
   pseudomark2 = (EDGELISTMARKS)malloc(sizeof(struct edgelistmarks));
   pseudomark1->alpha = 0.0;
   pseudomark2->alpha = M_PI + M_PI;
   LISTMARKS(node) = new_slist(make_attr(ATTR_DATA,(char *)pseudomark2));
   pseudolist2 = LISTMARKS(node);
   LISTMARKS(node) = add_to_slist(LISTMARKS(node),make_attr(ATTR_DATA,(char *)pseudomark1));
   for_sourcelist(node,edge)
     {
      if ((node2 = edge->snode) == node)
        node2 = edge->tnode;
      angle = Angle(node,node2);
      edgelist = pseudolist2;
      oldangle = M_PI + M_PI;
      while (angle < oldangle)
        {
         edgelist = edgelist->suc;
         oldangle = ((EDGELISTMARKS)attr_data(edgelist))->alpha;
        }
      mark = (EDGELISTMARKS)malloc(sizeof(struct edgelistmarks));
      LISTMARKS(node) = add_immediately_to_slist(edgelist,make_attr(ATTR_DATA,(char *)mark));
      mark->alpha = angle;
      mark->edge = edge;
     }
   end_for_sourcelist(node,edge);
   if (RW_GRAPH->directed)
     {
      for_targetlist(node,edge)
        {
         if ((node2 = edge->snode) == node)
           node2 = edge->tnode;
         angle = Angle(node,node2);
         edgelist = pseudolist2;
         oldangle = M_PI + M_PI;
         while (angle < oldangle)
           {
            edgelist = edgelist->suc;
            oldangle = ((EDGELISTMARKS)attr_data(edgelist))->alpha;
           }
         mark = (EDGELISTMARKS)malloc(sizeof(struct edgelistmarks));
         LISTMARKS(node) = add_immediately_to_slist(edgelist,make_attr(ATTR_DATA,(char *)mark));
         mark->alpha = angle;
         mark->edge = edge;
        }
      end_for_targetlist(node,edge);
     }
   LISTMARKS(node) = subtract_from_slist(LISTMARKS(node),make_attr(ATTR_DATA,(char *)pseudomark1));
   LISTMARKS(node) = subtract_from_slist(LISTMARKS(node),make_attr(ATTR_DATA,(char *)pseudomark2));
  } /* Make_ordered_edgelist_of_node */

/*##############################################################################################
  #                                                                                            #
  #                           Make_ordered_edgelist                                            #
  #                                                                                            #
  #   Bildet fuer jeden Knoten die im Uhrzeigersinn sortierte Kantenliste, und haengt sie in   #
  #   'Listmarks' ein.
  #                                                                                            #
  ##############################################################################################*/


void Make_ordered_edgelist ()

  {
   Snode node;
   for_all_nodes(RW_GRAPH,node)
     {
      Make_ordered_edgelist_of_node(node);
     }
   end_for_all_nodes(RW_GRAPH,node);
  } /* Make_ordered_edgelist */

static void Dfs (node, where)
  Snode node;
  WHERE where;
  
/* Dfs benutzt VISITED */

  {
   Sedge edge;
   Snode nextnode;
   VISITED(node) = 1;
   for_sourcelist(node,edge)
     {
      if ((nextnode = edge->snode) == node)
        nextnode = edge->tnode;
      if (where == INNER)
        {
         if ((VISITED(nextnode) == 0) && (SELECTED(nextnode) > 0))
           Dfs(nextnode,where);
        }
      else
        {
         if (where == OUTER)
           {
            if ((VISITED(nextnode) == 0) && (SELECTED(nextnode) <= 0))
              Dfs(nextnode,where);
           }
         else
           {
            if (VISITED(nextnode) == 0)
              Dfs(nextnode,where);
           }
        }
     }
   end_for_sourcelist(node,edge);
   if (RW_GRAPH->directed)
     {
      for_targetlist(node,edge)
        {
         if ((nextnode = edge->snode) == node)
           nextnode = edge->tnode;
         if (where == INNER)
           {
            if ((VISITED(nextnode) == 0) && (SELECTED(nextnode) > 0))
              Dfs(nextnode,where);
           }
         else
           {
            if (where == OUTER)
              {
               if ((VISITED(nextnode) == 0) && (SELECTED(nextnode) <= 0))
                 Dfs(nextnode,where);
              }
            else
              {
               if (VISITED(nextnode) == 0)
                 Dfs(nextnode,where);
              }
           }
        }
      end_for_targetlist(node,edge);
     }
  } /* Dfs */

static void Unvisit_graph (graph)
  Sgraph graph;
  
  {
   Snode node;
   for_all_nodes(graph,node)
     {
      VISITED(node) = 0;
     }
   end_for_all_nodes(graph,node);
  } /* Unvisit_graph */

static void Test_connectivity_of_graph ()

  {
   Snode node;
   if (RW_GRAPH->nodes != NULL)
     Dfs(RW_GRAPH->nodes,ALL);
   for_all_nodes(RW_GRAPH,node)
     {
      if (VISITED(node) == 0)
        {
         RW_ERROR = GRAPH_NOT_CONNECTED;
         RW_Message_Error();
         break;
        }
     }
   end_for_all_nodes(RW_GRAPH,node);
   Unvisit_graph(RW_GRAPH);
  } /* Test_connectivity_of_graph */
  
static void Insert_User_Marks ()

  {
   char *malloc();
   Snode node;
   Sedge edge;
   Slist l;
   NODEMARKS mark;
   int i;
   RW_NUMBER_OF_NODES = 0;
   RW_NUMBER_OF_EDGES = 0;
   for_all_nodes(RW_GRAPH,node)
     {
      RW_NUMBER_OF_NODES++;
      for_sourcelist(node,edge)
        {
         set_edgeattrs(edge,make_attr(ATTR_DATA,(char *)NULL));
         RW_NUMBER_OF_EDGES++;
        }
      end_for_sourcelist(node,edge);
     }
   end_for_all_nodes(RW_GRAPH,node);
   nodemarks = (NODEMARKS)malloc((RW_NUMBER_OF_NODES+1)*sizeof(struct nodemarks));
   mark = nodemarks;
   i = 1;
   for_all_nodes(RW_GRAPH,node)
     {
      set_nodeattrs(node,make_attr(ATTR_DATA,(char *)mark));
      VISITED(node) = 0;
      SELECTED(node) = 0;
      NUMBER(node) = i;
      X_ORDINATE(node) = node->x;
      Y_ORDINATE(node) = node->y;
      i++;
      LISTMARKS(node) = empty_slist;
      COPIED_NODE(node) = NULL;
      mark++;
     }
   end_for_all_nodes(RW_GRAPH,node);
  } /* Insert_User_Marks */

static void Delete_ordered_edgelist_of_node (node)
  Snode node;

  {
   Slist l;
   for_slist(LISTMARKS(node),l)
     {
      free((EDGELISTMARKS)attr_data(l));
     }
   end_for_slist(LISTMARKS(node),l);
   free_slist(LISTMARKS(node));
  } /* Delete_ordered_edgelist_of_node */

static void Delete_User_Marks ()

  {
   Snode node;
   for_all_nodes(RW_GRAPH,node)
     {
      Delete_ordered_edgelist_of_node(node);
      free((NODEMARKS)attr_data(node));
     }
   end_for_all_nodes(RW_GRAPH,node);
  } /* Delete_User_Marks */

static void Plain_Edges ()

  {
   Snode node;
   Sedge edge;
   Edgeline line, newline, l1, ltemp;
   for_all_nodes(RW_GRAPH,node)
     {
      for_sourcelist(node,edge)
        {
         line = (Edgeline)edge_get(graphed_edge(edge),EDGE_LINE);
         newline = new_edgeline(edgeline_x(line),edgeline_y(line));
         for_edgeline(line,l1)
           {
            ltemp = l1;
           }
         end_for_edgeline(line,l1);
         add_to_edgeline(newline,edgeline_x(ltemp),edgeline_y(ltemp));
         free_edgeline(line);
         edge_set(graphed_edge(edge),EDGE_LINE,newline, 0 );
        }
      end_for_sourcelist(node,edge);
     }
   end_for_all_nodes(RW_GRAPH,node);
  } /* Plain_Edges */

static void Test_Node_Coordinates ()

/* Es duerfen keine zwei Knoten auf einem Punkt liegen */

  {
   Snode node1, node2;
   for_all_nodes(RW_GRAPH,node1)
     {
      for_all_nodes(RW_GRAPH,node2)
        {
         if ((node1 != node2) &&
             (node1->x == node2->x) &&
             (node1->y == node2->y))
           {
            RW_ERROR = NODES_HAVE_SAME_COORDINATES;
            RW_Message_Error();
            return;
           }
        }
      end_for_all_nodes(RW_GRAPH,node2);
     }
   end_for_all_nodes(RW_GRAPH,node1);
  } /* Test_Node_Coordinates */

static void Test_nodeedgeinterface_self_loops_and_multiple_edges ()

  {
   char *malloc();
   char *nodearray;
   Snode node, node2;
   int i = 1;
   Sedge edge;
   nodearray = malloc(RW_NUMBER_OF_NODES+1);
   for_all_nodes(RW_GRAPH,node)
     {
      VISITED(node) = i++;
     }
   end_for_all_nodes(RW_GRAPH,node);
   for_all_nodes(RW_GRAPH,node)
     {
      for(i = 0; i < RW_NUMBER_OF_NODES+1; i++)
        nodearray[i] = 0;
      nodearray[VISITED(node)] = 1;
      for_sourcelist(node,edge)
        {
         if ((node2 = edge->snode) == node)
           node2 = edge->tnode;
         if (node2 == node)
           {
            RW_ERROR = GRAPH_CONTAINS_SELF_LOOPS;
            RW_Message_Error();
            break;
           }
         if (nodearray[VISITED(node2)])
           {
            RW_ERROR = GRAPH_CONTAINS_MULTIPLE_EDGES;
            RW_Message_Error();
            break;
           }
         else
           nodearray[VISITED(node2)] = 1;
        }
      end_for_sourcelist(node,edge);
      if (RW_ERROR != NO_ERROR)
        break;
      else
        {
         if (RW_GRAPH->directed)
           {
            for_targetlist(node,edge)
              {
               if ((node2 = edge->snode) == node)
                 node2 = edge->tnode;
               if (node2 == node)
                 {
                  RW_ERROR = GRAPH_CONTAINS_SELF_LOOPS;
                  RW_Message_Error();
                  break;
                 }
               if (nodearray[VISITED(node2)])
                 {
                  RW_ERROR = GRAPH_CONTAINS_MULTIPLE_EDGES;
                  RW_Message_Error();
                  break;
                 }
               else
                 nodearray[VISITED(node2)] = 1;
              }
            end_for_targetlist(node,edge);
           }
        }
      if (RW_ERROR != NO_ERROR)
        break;
     }
   end_for_all_nodes(RW_GRAPH,node);
   for_all_nodes(RW_GRAPH,node)
     {
      VISITED(node) = 0;
     }
   end_for_all_nodes(RW_GRAPH,node);
   free(nodearray);
  } /* Test_nodeedgeinterface_self_loops_and_multiple_edges */

static void Init_DefaultOptions ()

  {
   char *malloc();
   Global_Tool_Info = (GLOBAL_TOOL_INFO)malloc(sizeof(struct global_tool_info));
   RW_SPRING_ALGOR = FIXEDPARTS;
   RW_MIN_X = 5;
   RW_MIN_Y = 5;
   RW_ANIMATION = 5;
   RW_INTERSECTION = 0;
   RW_SPRING_EDGELENGTH = 100;
  } /* Init_DefaultOptions */



/* Springembedder */


static int iterations;

static int Compute_iterations ()

  {
   iterations = RW_NUMBER_OF_NODES * 10;
  } /* Compute_iterations */

static double Compute_const ()

  {
   return 1;
  } /* Compute_const */

static void Init_spring_constants ()

  {
   RW_SPRING_CONST = Compute_const();
  } /* Init_spring_constants */

/*###############################################################################################*/

typedef struct partikel
  { Snode node;
    double delta_i;
  } *PARTIKEL;

static double Compute_dE_dxm (m)
  int m;
  
  {
   double sum = 0;
   int i;
   double xm_xi, ym_yi;
   for(i = 1; i <= RW_NUMBER_OF_NODES; i++)
     {
      if (i != m)
        {
         xm_xi = X_ORDINATE((Snode)RW_SPRING_NODE_I(m)) - X_ORDINATE((Snode)RW_SPRING_NODE_I(i));
         ym_yi = Y_ORDINATE((Snode)RW_SPRING_NODE_I(m)) - Y_ORDINATE((Snode)RW_SPRING_NODE_I(i));
         sum = sum + RW_SPRING_k(m,i) * (
                             xm_xi
                           - ((RW_SPRING_l(m,i)*xm_xi)/sqrt(xm_xi*xm_xi + ym_yi*ym_yi)));
        }
     }
   return sum;
  } /* Compute_dE_dxm */
  
static double Compute_dE_dym (m)
  int m;
  
  {
   double sum = 0;
   int i;
   double xm_xi, ym_yi;
   for(i = 1; i <= RW_NUMBER_OF_NODES; i++)
     {
      if (i != m)
        {
         xm_xi = X_ORDINATE((Snode)RW_SPRING_NODE_I(m)) - X_ORDINATE((Snode)RW_SPRING_NODE_I(i));
         ym_yi = Y_ORDINATE((Snode)RW_SPRING_NODE_I(m)) - Y_ORDINATE((Snode)RW_SPRING_NODE_I(i));
         sum = sum + RW_SPRING_k(m,i) * (
                             ym_yi
                           - ((RW_SPRING_l(m,i)*ym_yi)/sqrt(xm_xi*xm_xi + ym_yi*ym_yi)));
        }
     }
   return sum;
  }/* Compute_dE_dym */

static double Compute_delta_i (i)
  int i;
  
  {
   double dE_dxm = Compute_dE_dxm(i),
          dE_dym = Compute_dE_dym(i);
   return (sqrt(dE_dxm*dE_dxm + dE_dym*dE_dym));
  }/* Compute_delta_i */
  
static PARTIKEL Init_delta_i ()

  {
   char *malloc();
   PARTIKEL part, mem_start;
   int i;
   RW_SPRING_PARTIKEL = (int *)malloc((RW_NUMBER_OF_NODES+1) * sizeof(int));
   mem_start = (PARTIKEL)malloc((RW_NUMBER_OF_NODES+1) * sizeof(struct partikel));
   part = mem_start;
   for(i = 1; i <= RW_NUMBER_OF_NODES; i++)
     {
      RW_SPRING_PARTIKEL_I(i) = part;
      part->node = (Snode)RW_SPRING_NODE_I(i);
      part->delta_i = Compute_delta_i(i);
      part++;
     }
   return mem_start;
  } /* Init_delta_i */

static Update_delta_i ()
  
  {
   int i;
   for(i = 1; i<= RW_NUMBER_OF_NODES; i++)
     {
      RW_SPRING_PARTIKEL_I(i)->delta_i = Compute_delta_i(i);
     }
  } /* Update_delta_i */

static void Delete_delta_i (part)

  {
   free(RW_SPRING_PARTIKEL);
   free(part);
  } /* Delete_delta_i */

static PARTIKEL Max_delta_i()

  {
   PARTIKEL part;
   int i;
   part = RW_SPRING_PARTIKEL_I(1);
   for(i = 1; i <= RW_NUMBER_OF_NODES; i++)
     {
      if (part->delta_i < RW_SPRING_PARTIKEL_I(i)->delta_i)
        {
         part = RW_SPRING_PARTIKEL_I(i);
        }
     }
   return part;
  } /* Max_delta_i */

/*###############################################################################################*/

static double Compute_d2E_dxm2 (m)
  int m;
  
  {
   double sum = 0;
   int i;
   double xm_xi, ym_yi;
   for(i = 1; i <= RW_NUMBER_OF_NODES; i++)
     {
      if (i != m)
        {
         xm_xi = X_ORDINATE((Snode)RW_SPRING_NODE_I(m)) - X_ORDINATE((Snode)RW_SPRING_NODE_I(i));
         ym_yi = Y_ORDINATE((Snode)RW_SPRING_NODE_I(m)) - Y_ORDINATE((Snode)RW_SPRING_NODE_I(i));
         sum = sum + RW_SPRING_k(m,i) * (
                          1 - ((RW_SPRING_l(m,i)*ym_yi*ym_yi)/
                                       ((xm_xi*xm_xi + ym_yi*ym_yi)*sqrt(xm_xi*xm_xi + ym_yi*ym_yi))));
        }
     }
   return sum;
  } /* Compute_d2E_dxm2 */

static double Compute_d2E_dxmym (m)
  int m;
  
  {
   double sum = 0;
   int i;
   double xm_xi, ym_yi;
   for(i = 1; i <= RW_NUMBER_OF_NODES; i++)
     {
      if (i != m)
        {
         xm_xi = X_ORDINATE((Snode)RW_SPRING_NODE_I(m)) - X_ORDINATE((Snode)RW_SPRING_NODE_I(i));
         ym_yi = Y_ORDINATE((Snode)RW_SPRING_NODE_I(m)) - Y_ORDINATE((Snode)RW_SPRING_NODE_I(i));
         sum = sum + RW_SPRING_k(m,i) *
                          ((RW_SPRING_l(m,i)*ym_yi*xm_xi)/
                                       ((xm_xi*xm_xi + ym_yi*ym_yi)*sqrt(xm_xi*xm_xi + ym_yi*ym_yi)));
        }
     }
   return sum;
  } /* Compute_d2E_dxmym */

static double Compute_d2E_dym2 (m)
  int m;
  
  {
   double sum = 0;
   int i;
   double xm_xi, ym_yi;
   for(i = 1; i <= RW_NUMBER_OF_NODES; i++)
     {
      if (i != m)
        {
         xm_xi = X_ORDINATE((Snode)RW_SPRING_NODE_I(m)) - X_ORDINATE((Snode)RW_SPRING_NODE_I(i));
         ym_yi = Y_ORDINATE((Snode)RW_SPRING_NODE_I(m)) - Y_ORDINATE((Snode)RW_SPRING_NODE_I(i));
         sum = sum + RW_SPRING_k(m,i) * (
                          1 - ((RW_SPRING_l(m,i)*xm_xi*xm_xi)/
                                       ((xm_xi*xm_xi + ym_yi*ym_yi)*sqrt(xm_xi*xm_xi + ym_yi*ym_yi))));
        }
     }
   return sum;
  } /* Compute_d2E_dym2 */

static void Compute_dx_dy (dx, dy, m)
  double *dx, *dy;
  int    m;
  
  {
   double d2E_dxm2   = Compute_d2E_dxm2(m),
          d2E_dxmdym = Compute_d2E_dxmym(m),
          d2E_dym2   = Compute_d2E_dym2(m),
          dE_dxm     = Compute_dE_dxm(m),
          dE_dym     = Compute_dE_dym(m);
   *dy = (d2E_dxmdym*dE_dxm - d2E_dxm2*dE_dym)/(d2E_dxm2*d2E_dym2 - d2E_dxmdym*d2E_dxmdym);
   *dx = (-dE_dxm - d2E_dxmdym*(*dy))/d2E_dxm2;
  } /* Compute_dx_dy */

static double Energy ()                               /* Nur zur Kontrolle */

  {
   int i,j;
   double sum = 0;
   double xi_xj, yi_yj;
   for (i = 1; i <= RW_NUMBER_OF_NODES-1; i++)
     {
      for (j = i+1; j <= RW_NUMBER_OF_NODES; j++)
        {
         xi_xj = X_ORDINATE((Snode)RW_SPRING_NODE_I(i)) - X_ORDINATE((Snode)RW_SPRING_NODE_I(j));
         yi_yj = Y_ORDINATE((Snode)RW_SPRING_NODE_I(i)) - Y_ORDINATE((Snode)RW_SPRING_NODE_I(j));
         sum = sum + 0.5 * RW_SPRING_k(i,j) *
                 (xi_xj*xi_xj + yi_yj*yi_yj + RW_SPRING_l(i,j)*RW_SPRING_l(i,j)
                   - 2*RW_SPRING_l(i,j)*sqrt(xi_xj*xi_xj + yi_yj*yi_yj));
        }
     }
   return sum;
  } /* Energy */

static void Start_spring ()

  {
   int i;
   Snode node;
   PARTIKEL part, p_mem;
   double dx, dy;
   float  dfx, dfy;
   Slist nodepolygon = empty_slist, l;
   double energy, tempenergy;
   p_mem = Init_delta_i();
   energy = Energy();
   for(i = 1; i <= iterations; i++)
     { part = Max_delta_i();
      node = part->node;
      Compute_dx_dy(&dx,&dy,NUMBER(node));
      dfx = (float)dx;
      dfy = (float)dy;
      Update_delta_i();
      node->x = node->x + (int)dfx;
      node->y = node->y + (int)dfy;
      X_ORDINATE(node) = X_ORDINATE(node) + dfx;
      Y_ORDINATE(node) = Y_ORDINATE(node) + dfy;
      node_set(graphed_node(node),NODE_POSITION,node->x,node->y,0);
      force_repainting();
     }
   Delete_delta_i(p_mem);
  } /* Start_spring */

static void Shortest_path (n)        /* nach R.W. Floyd, "Algoritm 97 : shortest path" */
  int n;                             /*    Comm. of the ACM Vol.5 No.6 P.345 Juni 1962 */
  
  {
   int i,j,k;
   int inf, s;
   Snode node, nextnode;
   Sedge edge;
   Slist l;
   inf = 1000000;
   
   for(i = 1; i <= n; i++)                      /* Initialisierung */
     {
      for(j = 1; j <= n; j++)
        {
         RW_SPRING_d(i,j) = 1000000;
        }
     }
   for(i = 1; i <= n; i++)
     {
      node = (Snode)RW_SPRING_NODE_I(i);
      RW_SPRING_d(i,i) = 0;
      for_slist(LISTMARKS(node),l)
        {
         edge = EDGE_OF_LISTMARKS(l);
         if ((nextnode = edge->snode) == node)
           nextnode = edge->tnode;
         RW_SPRING_d(i,NUMBER(nextnode)) = 1;
        }
      end_for_slist(LISTMARKS(node),l);
     }
     
   for(i = 1; i <= n; i++)                      /* eigentlicher Algorithmus */
     {
      for(j = 1; j <= n; j++)
        {
         if (RW_SPRING_d(j,i) < inf)
           {
            for(k = 1; k <= n; k++)
              {
               if (RW_SPRING_d(i,k) < inf)
                 {
                  s = RW_SPRING_d(j,i) + RW_SPRING_d(i,k);
                  if (s < RW_SPRING_d(j,k))
                    {
                     RW_SPRING_d(j,k) = s;
                    }
                 }
              }
           }
        }
     }
  } /* Shortest_path */
  
static void Compute_dij ()

  {
   Shortest_path(RW_NUMBER_OF_NODES);
  } /* Compute_dij */

static void Compute_lij ()

  {
   int i, j;
   for(i = 1; i <= RW_NUMBER_OF_NODES; i++)
     {
      for(j = 1; j <= RW_NUMBER_OF_NODES; j++)
        {
         RW_SPRING_l(i,j) = RW_SPRING_EDGELENGTH * RW_SPRING_d(i,j);
        }
     }
  } /* Compute_lij */

static void Compute_kij ()

  {
   int i, j;
   for(i = 1; i <= RW_NUMBER_OF_NODES; i++)
     {
      for(j = 1; j <= RW_NUMBER_OF_NODES; j++)
        {
         RW_SPRING_k(i,j) = RW_SPRING_CONST/(double)(RW_SPRING_d(i,j) * RW_SPRING_d(i,j));
        }
     }
  } /* Compute_kij */

static void Delete_spring_marks ()
  
  {
   free(RW_SPRING_NODE);
  } /* Delete_spring_marks */

static void Make_springmarks ()

  {
   char *malloc();
   Snode node;
   RW_SPRING_NODE = (int *)malloc((RW_NUMBER_OF_NODES+1) * sizeof(int));
   for_all_nodes(RW_GRAPH,node)
     {
      RW_SPRING_NODE_I(NUMBER(node)) = (int)node;
     }
   end_for_all_nodes(RW_GRAPH,node);
   RW_SPRING_DIJ = (int *)malloc((RW_NUMBER_OF_NODES+1) * (RW_NUMBER_OF_NODES+1) * sizeof(int));
   RW_SPRING_LIJ = (int *)malloc((RW_NUMBER_OF_NODES+1) * (RW_NUMBER_OF_NODES+1) * sizeof(int));
   RW_SPRING_KIJ = (double *)malloc((RW_NUMBER_OF_NODES+1) * (RW_NUMBER_OF_NODES+1) * sizeof(double));
  } /* Make_springmarks */
  
void Springemb (info)
  Sgraph_proc_info info;

  {
/* Changes by MH 14/10/91 */
   if (info->sgraph == empty_graph) {
      warning ("no graph given\n");
      return;
   }
   dispatch_user_action (UNSELECT);
/* End of Changes */

   Init_DefaultOptions();
   RW_NUMBER_OF_NODES = 0;
   RW_NUMBER_OF_EDGES = 0;
   RW_ERROR = NO_ERROR;
   RW_GRAPH = info->sgraph;
   Insert_User_Marks();
   Test_Node_Coordinates();
   Test_nodeedgeinterface_self_loops_and_multiple_edges();
   Make_ordered_edgelist();
   Plain_Edges();
   Test_connectivity_of_graph();
   if (RW_ERROR == NO_ERROR)
     {
      Make_springmarks();
      Init_spring_constants();
      Compute_dij();
      Compute_lij();
      Compute_kij();
      Compute_iterations();
      Start_spring();
      Delete_spring_marks();
      Delete_User_Marks();
      free(nodemarks);
     }
   info->no_changes = FALSE;
   info->no_structure_changes = FALSE;
   info->recompute = TRUE;
   info->repaint = TRUE;
   info->selected = SGRAPH_SELECTED_NONE;
  } /* Springemb */

void Springembedder (item ,event)
  Panel_item item;
  Event *event;

  {
   call_sgraph_proc(Springemb);
   RW_ERROR = NO_ERROR;
  } /* Springembedder */

void Set_desirable_edgelength (item, value, event)
  Panel_item item;
  int        value;
  Event      *event;
  
  {
   RW_SPRING_EDGELENGTH = value;
  } /* Set_desirable_edgelength */
