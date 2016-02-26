/* (C) Universitaet Passau 1986-1991 */
#include <std.h>
#include <sgraph.h>
#include <slist.h>
#include <graphed.h>
#include <math.h>

#include <xview/xview.h>
#include "springembedder.h"


#define PROC void
#define MAIN void
#define FUNC /**/
#define mark attr_flags

float	springembedder_max_force = 3.0;
float	springembedder_opt_distance = 32.0;
float	springembedder_period = 0.02;
int	springembedder_max_iterations = 1000;
int	springembedder_animation_intervals = 10;

#define AREA 4000      /* nur provisorisch */
#define MAX_FORCE springembedder_max_force
         /* wenn die groesste Kraft, die an allen Knoten wirkt, gleich */
         /* kleiner als  MAX_FORCE ist, terminiert der Algorithmus     */
#define C springembedder_opt_distance
         /* Konstante fuer die Berechnung des opt. Abstandes zweier Knoten */
#define PERIOD springembedder_period

typedef struct 
{       float x, y;
} vektor;


MAIN nature ();
MAIN fast_nature ();




/**********************************************************************/
/*                                                                    */
/*                     Nature-Layout-Algorithmus                      */
/*                                                                    */
/**********************************************************************/

PROC unmark_all_nodes ( graph )
     /*****************************************************************/
     /* setzt die Markierungen aller Knoten von graph auf FALSE       */
     /*****************************************************************/
     Sgraph graph;
{    Snode  node;
     for_all_nodes( graph, node )
     {   mark( node ) = FALSE;
     }   end_for_all_nodes( graph, node ); 
}

PROC mark_connected_nodes ( graph, node )
     /*****************************************************************/
     /* markiert in graph die mit node durch Kanten verbundene Knoten */
     /*****************************************************************/
     Sgraph graph;
     Snode  node;
{    Sedge  edge;
     if ( mark( node ) == FALSE )
     {  mark( node ) = TRUE;
        for_sourcelist( node, edge )
        {   mark_connected_nodes( graph, edge->tnode );
        }   end_for_sourcelist( node, edge );
        if ( graph->directed ) {
           for_targetlist( node, edge )
           {   mark_connected_nodes( graph, edge->snode );
           }   end_for_targetlist( node, edge );
        }     
     }       
}

FUNC bool is_connected ( graph )
     /*****************************************************************/
     /* ueberprueft, ob graph zusammenhaengend ist (oder aus mehreren */
     /* untereinander nicht verbundenen Teilgraphen besteht)          */
     /*****************************************************************/
     Sgraph graph;
{    Snode  node;
     bool   connected;
     unmark_all_nodes( graph );
     mark_connected_nodes( graph, first_node_in_graph( graph ) );
     connected = TRUE;
     for_all_nodes( graph, node )
     {   if ( !mark( node ) )
            connected = FALSE;
     }   end_for_all_nodes( graph, node );
     return connected;
}

FUNC bool multiple_connected_nodes ( graph )
     /*****************************************************************/
     /*  ueberprueft, of graph Knoten besitzt, die durch mehr als     */
     /*  eine Kante verbunden sind                                    */                       
     /*****************************************************************/
     Sgraph graph;
{    Snode  node;
     Sedge  edge;
     bool multiple_connected = FALSE;
     int node_index = 0;
     unmark_all_nodes( graph );
     for_all_nodes( graph, node )
     {  node_index++;
	for_sourcelist( node, edge )
	{  if ( mark( edge->tnode ) == node_index )
	      multiple_connected = TRUE;
           else
	      mark( edge->tnode ) = node_index;
        }  end_for_sourcelist( node, edge );
        node_index ++;
        if (graph->directed) {
	    for_targetlist( node, edge )
	    {  if ( mark( edge->snode ) == node_index )
	          multiple_connected = TRUE;
	       else
	          mark( edge->snode ) = node_index;
	    }  end_for_targetlist( node, edge );
	}
     }  end_for_all_nodes( graph, node );
     return multiple_connected;
}
 
FUNC bool is_correct ( graph )
     /*****************************************************************/
     /* ueberprueft, ob graph in einer korrekten Form vorliegt, um    */   
     /* von dem Nature-Algorithmus bearbeitet zu werden               */ 
     /*****************************************************************/
     Sgraph graph;
{    bool correct;
     correct = FALSE;
     if ( graph == empty_graph || graph->nodes == nil )
        { error ( "No graph selected\n" );
        }
/* Commented out MH 25/9/91
     else if ( graph->directed )
        {    error ( "Graph is directed\n" );
        }
*/
     else if ( !is_connected( graph ) )
        {     error ( "Graph is not connected\n" );
        }  
     else if ( multiple_connected_nodes( graph ) )
        {    error ( "Graph contains a multiple edge\n" );
        }
     else correct = TRUE;
     return correct;
}	 

PROC straighten_edges ( sgraph )
     /*****************************************************************/
     /* "biegt" eventuell vorhandene nicht geradlienig verlaufende    */
     /* Kanten im Graphen gerade, da der Nature-Algorithmus gerad-    */
     /* lienig verlaufende Kanten vorraussetzt                        */
     /*****************************************************************/
     Sgraph sgraph;
{    Snode  snode;
     Sedge  sedge;
     Graphed_edge gedge;
     Edgeline edgeline, straight_edgeline;
     int x1, y1, x2, y2;
     for_all_nodes( sgraph, snode )
     {   
         for_sourcelist( snode, sedge )
         {   gedge = graphed_edge( sedge );
             edgeline = ( Edgeline ) edge_get( gedge, EDGE_LINE );
             if ( !is_single_edgeline( edgeline ) )
             {  /* free_edgeline( edgeline ); */
                x1 = snode_x( snode ); x2 = sedge->tnode->x;
                y1 = snode_y( snode ); y2 = sedge->tnode->y; 
                straight_edgeline = add_to_edgeline( new_edgeline( x1, y1 ), x2, y2 );
                edge_set( graphed_edge( sedge ), EDGE_LINE, straight_edgeline, 0 ); 
             }
         }   end_for_sourcelist( snode, sedge );
         if (sgraph->directed) {
	     for_targetlist( snode, sedge )
	     {   gedge = graphed_edge( sedge );
		 edgeline = ( Edgeline ) edge_get( gedge, EDGE_LINE );
		 if ( !is_single_edgeline( edgeline ) )
		 {  /* free_edgeline( edgeline ); */
		    x1 = snode_x( snode ); x2 = sedge->snode->x;
		    y1 = snode_y( snode ); y2 = sedge->snode->y; 
		    straight_edgeline = add_to_edgeline( new_edgeline( x1, y1 ), x2, y2 );
		    edge_set( graphed_edge( sedge ), EDGE_LINE, straight_edgeline, 0 ); 
		 }
	     }   end_for_targetlist( snode, sedge );
	 }
     }   end_for_all_nodes( sgraph, snode );
}

FUNC int number_of_nodes ( graph )
     /*****************************************************************/
     /* stellt fest, wieviel Knoten graph bestitzt                    */
     /*****************************************************************/
     Sgraph graph;
{    Snode  node;
     int n;
     n = 0;
     for_all_nodes( graph, node )
     {   n++;
     }   end_for_all_nodes( graph, node );
     return n;
}

FUNC vektor *init_node_positions ( graph, N, enough_memory )
     /*****************************************************************/
     /* gibt ein Pointer auf ein Array der Laenge N mit Eintraegen    */
     /* vom Typ vektor zurueck, indem die Koordinaten der Knoten von  */
     /* graph stehen und ordnet jedem 'node' von 'graph' einen Index  */
     /* i zu, so dass gilt : node_positions[i] gehoert zum i-ten      */
     /* 'node' der Knotenliste (wird fuer compute_attractive_forces   */
     /* benoetigt)                                                    */  
     /*****************************************************************/    
     Sgraph graph;
     int    N;
     bool   *enough_memory;
{
	Snode  node;
	vektor *node_positions;
	int    i;
	
	node_positions = (vektor*) malloc( (unsigned) ( N * sizeof( vektor ) ) );
	
	if ( node_positions == nil ) {
		*enough_memory = FALSE;
	} else {
		i = 0;
		for_all_nodes( graph, node ) {
			node_positions[i].x = snode_x( node );
			node_positions[i].y = snode_y( node );
			mark( node ) = i;
			i++; 
		}  end_for_all_nodes( graph, node );
	};
	return node_positions;
}

PROC init_null ( forces, N )
     vektor *forces;
     int N;
{    int i;
      for ( i = 0; i < N; i++ )
      {   forces[i].x = 0.0;
          forces[i].y = 0.0;
      }
}

FUNC vektor *init_forces ( N, enough_memory )
     /*****************************************************************/
     /* gibt ein Pointer auf ein Array der Laenge N mit Eintraegen    */
     /* vom Typ vektor zurueck, die alle Koordinaten den Wert 0       */
     /* haben                                                         */
     /*****************************************************************/
     int N;
     bool *enough_memory;
{    vektor *forces;
     forces = (vektor*) malloc( (unsigned) ( N * sizeof( vektor ) ) );
     if ( forces == nil )
     {  *enough_memory = FALSE;
     }
     else /* else-Zweig nicht unbedingt erforderlich */
        init_null( forces, N );
     return forces;
}

FUNC  float vek_abs ( v )
     /*****************************************************************/
     /* berechnet die euklidische Norm fuer ein Element von Typ       */
     /* vektor, die ja der Laenge des vektors entspricht              */
     /*****************************************************************/
      vektor v;
{     return   hypot( v.x, v.y ); /* hypot( x, y ) = sqrt( x*x + y*y ) */
}

FUNC float compute_max_force ( forces, N )
     vektor *forces;
     int N;
{    float abs_force, max_abs_force;
     int i;
     max_abs_force = 0;
     for ( i = 0; i < N; i++ )
     {   abs_force = vek_abs ( forces[i] );
         if ( abs_force > max_abs_force )
            max_abs_force = abs_force;
     }
     return max_abs_force;
}
    
FUNC vektor add ( v1, v2 )
     /*****************************************************************/
     /* addiert zwei Elemente von Type vektor komponentenweise        */
     /*****************************************************************/
     vektor v1, v2;
{    vektor v;
     v.x = ( v1.x + v2.x );
     v.y = ( v1.y + v2.y );
     return v;
}

FUNC vektor sub ( v1, v2 )
     /*****************************************************************/
     /* substrahiert zwei Elemente von Type vektor komponentenweise   */
     /*****************************************************************/
     vektor v1, v2;
{    vektor v;
     v.x = ( v1.x - v2.x );
     v.y = ( v1.y - v2.y );
     return v;
}

FUNC vektor sca_mult ( scalar, v )
     /*****************************************************************/
     /* multipliziert ein Element vom Typ float mit einem Element    */
     /* vom Typ vektor durch uebliche komponentenweise Multiplikation */
     /*****************************************************************/
     float scalar;
     vektor v;
{    v.x = scalar * v.x;
     v.y = scalar * v.y;
     return v;
}

FUNC vektor sca_diff ( v, scalar )
     /*****************************************************************/
     /* diffidiert ein Element vom Typ vektor mit einem Element vom   */
     /* Typ float durch uebliche komponentenweise Division           */
     /*****************************************************************/
     vektor v;
     float scalar;
{    v.x = v.x / scalar;
     v.y = v.y / scalar;
     return v;
}


FUNC vektor center ( node_positions, N )
     vektor *node_positions;
     int N;
{    vektor center; 
     int i;
     center.x = 0.0;
     center.y = 0.0;
     for ( i = 0; i < N; i++ )
         center = add( center, node_positions[i] );
     return sca_diff( center, ( (float) N ) ); 
}

static	float	optimal_distance; /* Was a function in an earlier incarnation */


/* OBSOLETE */
FUNC float abs_attractive_force ( distance, optimal_distance )
     /*****************************************************************/
     /*  berechnet den Betrag der Kraft, mit der ein Knoten einen     */
     /*  anderen ANZIEHT, wobei der Abstand beider Knoten 'distance'  */
     /*  betraegt und der wuenschenswerte Abstand 'optimal_distance'  */
     /*  ist                                                          */
     /*****************************************************************/
     float distance, optimal_distance;
{
/*   return ( pow( distance, 2.0 )/optimal_distance ); */
     return ( (distance*distance) / optimal_distance );
}

/* OBSOLETE */
FUNC float abs_repulsive_force ( distance, optimal_distance )
     /*****************************************************************/
     /*  berechnet den Betrag der Kraft, mit der ein Knoten einen     */
     /*  anderen ABSTOESST, wobei der Abstand beider Knoten 'distance'*/
     /*  betraegt und der wuenschenswerte Abstand 'optimal_distance'  */
     /*  ist                                                          */
     /*****************************************************************/
     float distance, optimal_distance;
{
/*   return pow( optimal_distance, 2.0 )/distance; */
     return (optimal_distance*optimal_distance) / distance;
}

PROC compute_attractive_forces ( forces, node_positions, N, graph )
	vektor *forces, *node_positions;
	int N;    
	Sgraph graph;
{
	Snode  node;
	Sedge  edge;
	vektor delta, direction, attractive_force;
	float  dist, opt_dis, factor;
	int    i = 0;
	
/* Orig. Code
	for_all_nodes( graph, node ) {
		for_sourcelist( node, edge ) {
			delta    = sub( node_positions[edge->tnode->attrs.flags], node_positions[node->attrs.flags] ) ;
			distance = vek_abs( delta );
			direction        = sca_diff( delta, distance );
			opt_dis          = optimal_distance;
			attractive_force = sca_mult( abs_attractive_force( distance, opt_dis ), direction );
			forces[i] = add( forces[i], attractive_force );
		} end_for_sourcelist( node, edge );
		if (graph->directed) for_targetlist( node, edge ) {
			delta    = sub( node_positions[edge->snode->attrs.flags], node_positions[node->attrs.flags] ) ;
			distance = vek_abs( delta );
			direction        = sca_diff( delta, distance );
			opt_dis          = optimal_distance;
			attractive_force = sca_mult( abs_attractive_force( distance, opt_dis ), direction );
			forces[i] = add( forces[i], attractive_force );
		} end_for_targetlist( node, edge );
		i++ ;
	} end_for_all_nodes( graph, node );
*/

	for_all_nodes( graph, node ) {
		for_sourcelist( node, edge ) {
			delta.x = node_positions[edge->tnode->attrs.flags].x - node_positions[node->attrs.flags].x;
			delta.y = node_positions[edge->tnode->attrs.flags].y - node_positions[node->attrs.flags].y;
			if (delta.x == 0) delta.x = 1; /* Prevent 0 distance, which is considered unnatural */
			if (delta.y == 0) delta.y = 1;
			factor  = hypot (delta.x, delta.y) / optimal_distance;
			forces[i].x += delta.x * factor;
			forces[i].y += delta.y * factor;
		} end_for_sourcelist( node, edge );
		if (graph->directed) for_targetlist( node, edge ) {
			delta.x = node_positions[edge->snode->attrs.flags].x - node_positions[node->attrs.flags].x;
			delta.y = node_positions[edge->snode->attrs.flags].y - node_positions[node->attrs.flags].y;
			if (delta.x == 0) delta.x = 1; /* Prevent 0 distance, which is considered unnatural */
			if (delta.y == 0) delta.y = 1;
			factor  = hypot (delta.x, delta.y) / optimal_distance;
			forces[i].x += delta.x * factor;
			forces[i].y += delta.y * factor;
		} end_for_targetlist( node, edge );
		i++ ;
	} end_for_all_nodes( graph, node );
}


PROC	compute_repulsive_forces ( forces, node_positions, N )
	vektor *forces, *node_positions;
	int N;
{
	int i, j;
	vektor delta, direction, repulsive_force;
	float dist2, opt_dis, factor;

	for ( i = 0; i < N; i++ ) {
		for ( j = i+1; j < N; j++ ) {
/* Orig. Code
			delta           = sub( node_positions[i], node_positions[j] );
			distance        = vek_abs( delta );  
			direction       = sca_diff( delta, distance ); 
			opt_dis         = optimal_distance;
			repulsive_force = sca_mult( abs_repulsive_force( distance, opt_dis ), direction );
			forces[i]       = add( forces[i], repulsive_force );
			forces[j]       = sub( forces[j], repulsive_force );
*/
			delta.x  = node_positions[i].x - node_positions[j].x;
			delta.y  = node_positions[i].y - node_positions[j].y;
			if (delta.x == 0) delta.x = 1; /* Prevent 0 distance, which is considered unnatural */
			if (delta.y == 0) delta.y = 1;
			factor   = optimal_distance * optimal_distance / (delta.x * delta.x + delta.y * delta.y);
			repulsive_force.x = delta.x * factor;
			repulsive_force.y = delta.y * factor;
			forces[i].x += repulsive_force.x;
			forces[i].y += repulsive_force.y;
			forces[j].x -= repulsive_force.x;
			forces[j].y -= repulsive_force.y;
         }                                                                               
     }
}

PROC	compute_new_node_positions ( node_positions, forces, N, temperature )
	vektor *node_positions, *forces;
	int N;
	float temperature;
{
	vektor direction, shift;
	float abs_force;
	int i;
	
	for ( i = 0; i < N; i++ ) {
/* Orig Code
		abs_force         = vek_abs( forces[i] );
		direction         = sca_diff( forces[i], abs_force );
		shift             = sca_mult( minimum( abs_force, temperature ), direction );
		node_positions[i] = add( node_positions[i], shift );
*/
		abs_force = vek_abs (forces[i]);
		if (abs_force < temperature) {
			shift.x = forces[i].x;
			shift.y = forces[i].y;
		} else {
			shift.x = forces[i].x * temperature / abs_force;
			shift.y = forces[i].y * temperature / abs_force;
		}
		node_positions[i].x += shift.x;
		node_positions[i].y += shift.y;
     }
}    

static int   cool_period = 0;
static float cool_max_force_1 = 0.0,
             cool_max_force_2 = 0.0,
             cool_max_force_3 = 0.0,
             cool_max_force_4 = 0.0;

FUNC float cool ( max_force )
     float max_force;
{
     static float temperature;
     cool_max_force_4 = cool_max_force_3;
     cool_max_force_3 = cool_max_force_2;
     cool_max_force_2 = cool_max_force_1;
     cool_max_force_1 = max_force;
     if ( (cool_max_force_4 != 0) &&
          (fabs((cool_max_force_1 - cool_max_force_3) / cool_max_force_1) < PERIOD) &&
          (fabs((cool_max_force_2 - cool_max_force_4) / cool_max_force_2) < PERIOD) )
     {   cool_max_force_1 = 0.0; 
         cool_max_force_2 = 0.0; 
         cool_max_force_3 = 0.0; 
         cool_max_force_4 = 0.0; 
         cool_period++;
     }
     if ( cool_period = 0 )
        temperature = sqrt( max_force );
     else 
        if ( cool_period = 1 )
           temperature = sqrt( max_force ) / 15;
        else
           temperature = temperature / 15;
     return temperature;
}

PROC write_node_positions_back_to_sgraph ( node_positions, shift, graph )
     vektor *node_positions, shift;
     Sgraph graph;
{    Snode  node;
     int i = 0;
     for_all_nodes( graph, node )
     {   node_positions[i] = add( node_positions[i], shift );    
         snode_x( node )   = (int) ( node_positions[i].x + 0.5 );
         snode_y( node )   = (int) ( node_positions[i].y + 0.5 );
         i++;
     }   end_for_all_nodes( graph, node );
}


PROC mh_write_node_positions_back_to_sgraph ( node_positions, old_center, N, graph )
     vektor *node_positions, old_center;
     int N;
     Sgraph graph;
{
     Snode  node;
     int i = 0;
     vektor new_center, shift;
     Slist list_of_nodes = empty_slist;
     Graphed_group g;
     
     new_center = center( node_positions, N );
     shift = sub( old_center, new_center );
           
     for_all_nodes( graph, node )
     {   
	node_set (graphed_node (node), ONLY_SET, NODE_POSITION,
		(int)(node_positions[i].x + shift.x + 0.5),
		(int)(node_positions[i].y + shift.y + 0.5),
		0);
		
         i++;
         list_of_nodes = add_to_slist (list_of_nodes, make_attr (ATTR_DATA, (char *)node));
     }
     end_for_all_nodes( graph, node );
     
     g = create_graphed_group_from_slist (list_of_nodes);
     group_set (g, RESTORE_IT, 0);
     force_repainting();
     
     free_group (g);
     free_slist (list_of_nodes);
}

static	springembedder_animation = FALSE;

MAIN springembedder ( info )
     /*****************************************************************/
     /*                                                               */
     /*                                                               */
     /*****************************************************************/
     Sgraph_proc_info info;
{
	Sgraph graph;
	int	N; /* Anzahl der Knoten des von GraphEd uebergebenen Graphen */
	float	temperature; /* max. Verschiebung eines Knotens in einem Schritt angibt */
	vektor	*node_positions, *forces;
	vektor	old_center, new_center, shift;
	bool	enough_memory;
	int	iteration_count=0;
	int	animation_iteration_count = 0;
	float	max_force;
	
	cool_period = 0;         /* Reset cool variables ; MH 5/9/91 */
	cool_max_force_1 = 0.0;
	cool_max_force_2 = 0.0,
	cool_max_force_3 = 0.0,
	cool_max_force_4 = 0.0;
	
	optimal_distance = (float)springembedder_opt_distance;
	
	graph = info->sgraph;
     
	if (springembedder_animation)
		dispatch_user_action (UNSELECT);
     
	if ( is_correct (graph ) ) {
	
		straighten_edges (graph ); 
		N              = number_of_nodes( graph );
		enough_memory  = TRUE;   
		node_positions = init_node_positions( graph, N, &enough_memory );
		old_center     = center( node_positions, N );
		forces         = init_forces( N, &enough_memory );
        
		if ( !enough_memory ) {
			error ("Springembedder : not enough memory avaiable !\n"); 
		} else {
        
			init_null( forces, N ); 
			compute_attractive_forces( forces, node_positions, N, graph );
			/* forces ist ein VAR-Parameter         */
			compute_repulsive_forces ( forces, node_positions, N );
			/* forces ist ein VAR-Parameter         */
			max_force = compute_max_force( forces, N );
		   
			while (max_force > MAX_FORCE  &&
			       iteration_count < springembedder_max_iterations) {
		   
				init_null( forces, N );
				compute_attractive_forces( forces, node_positions, N, graph );
				/* forces ist ein VAR-Parameter         */
				compute_repulsive_forces ( forces, node_positions, N );
				/* forces ist ein VAR-Parameter         */
				max_force  = compute_max_force( forces, N );
				temperature = cool( max_force );
				compute_new_node_positions( node_positions, forces, N, temperature );
				/* node_positions ist ein VAR-Parameter */
	
				if (springembedder_animation &&
				    animation_iteration_count >= springembedder_animation_intervals) {
					mh_write_node_positions_back_to_sgraph (
						node_positions, old_center, N, graph );
					force_repainting();
					animation_iteration_count = 0;
				}
	
				iteration_count ++;
				animation_iteration_count ++;
			}
			
			new_center = center( node_positions, N );
			shift = sub( old_center, new_center );
			write_node_positions_back_to_sgraph( node_positions, shift, graph );
			free( (char*) node_positions ); free( (char*) forces );
		} 
	}
}


MAIN fast_nature ( info )
     /*****************************************************************/
     /*                                                               */
     /*                                                               */
     /*****************************************************************/
     Sgraph_proc_info info;
{

	springembedder_animation = FALSE;
	springembedder (info);
}


MAIN nature ( info )
     /*****************************************************************/
     /*                                                               */
     /*                                                               */
     /*****************************************************************/
     Sgraph_proc_info info;
{

	springembedder_animation = TRUE;
	springembedder (info);
}
