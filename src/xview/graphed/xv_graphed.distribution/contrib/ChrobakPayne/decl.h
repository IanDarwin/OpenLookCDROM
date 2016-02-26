/* (C) Universitaet Passau 1986-1991 */
/****************************************************************
 **                                                            **
 **     In diesem File werden die benoetigten Fremddateien     **
 **     eingebunden. Sonst enthaelt es saemtliche Prozedur-    **
 **     koepfe des Programms, die Deklarationen der Typen      **
 **     und globalen Variablen.                                **
 **                                                            **
 ****************************************************************/


/*#include <stdio.h>
#include <math.h>*/
#include <string.h>
#include <ctype.h>
#include <std.h>
#include <sgraph.h>
#include <slist.h>
#include <graphed.h>
#include <xview/xview.h>
#include <xview/panel.h>

#define NULL 0
#define ABS(x)	((x) < 0 ? (-(x)) : (x))



/****************************************************************
 **                                                            **
 **     Die Prozedurkoepfe des Programms:                      **
 **                                                            **
 ****************************************************************/
 
struct node *search_node ();
void fill_neighbors ();
extern void add_to_user_menu ();
extern char *cp_menu_call_sgraph_proc ();

void main_proc1 ();
void main_proc2 ();
void main_proc3 ();
void main_proc4 ();
void main_proc5 ();
void main_proc6 ();
void main_proc7 ();
void main_proc8 ();
void main_proc9 ();
void main_proc10 ();
void main_proc11 ();
void main_proc12 ();
void main_proc13 ();
void main_proc14 ();
void main_proc15 ();

int cp_window ();
void chrobak_payne_switch ();
void compression_switch ();
void zoom_switch ();
void hide_switch ();
void cp_case1 ();
void cp_case2 ();
void cp_case3 ();
void cp_case4 ();
void comp_case1 ();
void comp_case2 ();
void comp_case3 ();
void comp_case4 ();
void comp_case5 ();
void comp_case6 ();


void take_graph ();
void sort_neighbors ();
void sort_neighbors2 ();
double angle ();
void sequence_for_placing_nodes ();
int neighboring_edges_test ();

void sort_y_coordinates ();
void regularisation ();
int at_left_of_edge ();
void init_location_list ();
void insert_edges_in_location_list ();
struct location_edge *delete_edges_in_location_list ();
void regular_one_direction ();
void exterior_face_situation ();
int exterior_edge ();

void decomposition ();
void mark_exterior_face ();
void store_polygon ();
struct nlist *left_neigh_at_nei ();
struct nlist *left_neigh_at_nei_2 (); /* Ohne Dekrementierung. */
struct node *min_node ();
struct node *max_node ();
void merge_chains_of_polygon ();
void adjust_grid ();
int adjust_grid_cp ();
void find_origin ();

void triangulation ();
void polygon_triangulation ();
int convex_angle ();


void add_edge ();
void color_switch ();
void initialisation ();
void next_node ();
int is_neighbor ();
void contour_neighbors ();
void canonical_numbering ();
int placing_test ();
void nejia_assila_idea ();
void stretching ();
void accumulate_offsets ();
void chrobak_payne_algorithm ();

double pyth ();
void change_y_coord_with_y ();
void make_row ();
void make_row_2 ();
double bound ();
void compress ();
void compress_normal ();
double test_exterior_nodes ();
void update_exterior_face_situation ();
void change_coordinates ();
void zoom ();
void store_last_graph ();
int test_changes ();
void make_exterior_face_path ();
void make_the_graph ();
void find_grid_origin ();

void free_the_graph ();
void free_neigh ();
void free_polygon ();
void free_polynode ();




/****************************************************************
 **                                                            **
 **     Die Typen des Programms:                               **
 **                                                            **
 ****************************************************************/

struct nlist {
	int nr;
	int visited;      /* Marke fuer Kanten, die bereits fuer ein Polygon abgespeichert wurde. */
	int orig;         /* nr Bildet mit dem adjazenten Knoten eine Kante des Original-Graphen. */
	int exterior2;    /* Marke fuer Kante d. Randes vor der Dreieckszerlegung (exterior2 == 1). */
	double angle;     /* Winkel zum adjazenten Knoten (0 <= angle < 2*M_PI). */
	double angle2;    /* Winkel zur Bestimmung von ein- und ausgehenden Kanten. */
	struct node *node;
	struct nlist *clockw;     /* Im Uhrzeigersinn folgende Nachbarkante. */
	struct nlist *co_clockw;  /* Im Gegenuhrzeigersinn folgende Nachbarkante. */
	struct nlist *s_next; /* Naechster in der zugehoerigen geordnerten Liste (up, dn). */
	struct nlist *next;
};



struct node {
	int number;
	int x;
	int y;
	int degree;               /* Grad des Knotens. */
	int up_degree;            /* Anzahl der Nachbarn ueber dem Knoten. */
	int dn_degree;            /* Anzahl der Nachbarn unter dem Knoten. */
	int kind;                 /* Art des Knotens bzgl. Regularisation. */
	struct nlist *neigh;      /* Nachbarn (Kanten) des Knoten. */
	struct nlist *clockw;     /* Im Uhrzeigersinn geordnete Kanten. */
	struct nlist *co_clockw;  /* Im Gegenuhrzeigersinn geordnete Kanten. */
	struct nlist *up;         /* Liste der Nachbarn ueber dem Knoten (Uhrzeigersinn). */
	struct nlist *dn;         /* Liste der Nachbarn unter dem Knoten (Gegenuhrzeigers.). */
	double dist;              /* Abstand des Knotens zur "Kontur". */
	int label;
	int x_offset;
	int y_coord;
	struct node  *placenext;  /* Naechster Knoten der kanonischen Nummerierung. */
	struct node  *placelast;  /* Letzter Knoten der kanonischen Nummerierung. */
	int processed;            /* Marke der bereits behandelten Knoten in der kan. Numm. */
	int in_cn_list;           /* Marke fuer Elemente der kan. Nummerierungsliste. */
	struct node  *left;
	struct node  *right;
	int exterior;              /* Marke fuer Knoten auf der Huelle (exterior == 1). */
	struct node  *exnext;      /* Naechster Knoten auf der Huelle (Uhrzeigersinn). */
	struct node  *exlast;      /* Letzter Knoten auf der Huelle. */
	int in;                    /* Zeigt die Seite des "Inneren" des Graphen. */
	int top;                   /* Zeigt, ob d. Knoten auf der li. od. re. Haelfte d. Huelle liegt. */
	struct node  *ascend;
	struct node  *descend;
	struct node  *next;
};


struct location_edge {
	struct node      *rel_node;
	struct node          *node1;
	struct node          *node2;
	struct location_edge *next;
	struct location_edge *last;
};


struct polynode {               /* Knoten eines Polygons. */
	struct node       *pnode;
	struct polynode   *next;
	struct polynode   *last;
	int               kind;     /* Gibt die Situation d. Knotens f. die Regularisierung an. */
	struct polynode *ascend;    /* Aufsteigende Y-Liste. */
	struct polynode   *descend; /* Absteigende Y-Liste. */
};


struct polygon {                /* Element der Polygonliste. */
	int             nr;
	struct polynode *firstnode;
	struct polynode *firstasc;
	struct polynode *firstdesc;
	struct polygon  *nextpoly;
};


struct  edge {
	int    node1;
	int    node2;
	int    x1;
	int    x2;
	int    y1;
	int    y2;
	int    length;
	struct edge   *next;
};



/*********************************************************
 **                                                     **
 **    Die globale Variablen des Programms:             **
 *                                                      **
 *********************************************************/
 
struct node *graph, *pgraph, *lastgra,
            *firstdesc, *firstasc, 
            *cn_list,        /* Liste der kanonischen Numerierung. */
            *first, *second, *third, 
            *place, *minele, *help1, *help2,
            *bintree, *htree, *firstcont, *lastcont, *prelast,
            *knode1, *knode2, *fc, *lc, *pl;
 
/*struct nlist *h;             Hilfsliste. */ 
            
int gsize;                  /* Anzahl der Knoten. */
int no_graph;
int grid;                   /* Gitterweite. */
int plan;                   /* Marke zum Pruefen der planaren Einbettung. */
int new_zero_x, new_zero_y; /* Koordinaten des neuen "Nullpunkts". */
int k, l, m, n, count;
int pcounter;

struct edge    *trian, *ht, testedge, minedge;

struct nlist *hneigh, *hneigh1, *hneigh2;

struct location_edge *location_list;

struct polygon *polygonlist, *plist, *exterior;


typedef	struct	{
	int	grid;
	int	grid_defaults;
}
	Chrobak_payne_settings;

extern	Chrobak_payne_settings chrobak_payne_settings;
