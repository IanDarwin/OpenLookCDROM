/* (C) Universitaet Passau 1986-1991 */
/* #include <stdio.h>  */
/* #include <malloc.h> */    /*** in std.h (GraphEd) enthalten ***/
#include <string.h>
#include <strings.h>
#include <ctype.h>
#include <math.h>
#include "std.h"
#include "sgraph.h"
#include "slist.h"
#include "graphed.h"

#include <xview/xview.h>
#include <xview/panel.h>
#include <xview/tty.h>
#include <xview/text.h>
#include <xview/scrollbar.h>
#include <xview/expandname.h>
#include <xview/canvas.h>


extern void add_to_user_menu ();
extern char *menu_call_sgraph_proc ();
extern void node_set ();
extern void edge_set ();

/********************************************/
/* Some names to comment actions in runtime */
/********************************************/
/*#define SHOW*/
/*#define SHOWTREE*/
/*#define DEBUG*/

#define SESSION_RUN_COUNTER 28	/* see also variable current_graph_name in 'main.c' */
#define INT_LENGTH 7		/* max. length of an integer */
#define TEMPORARY_GRAPH_NAME "temporary_petri_net_number_0"
		/* the part 'temporary_petri_net_number_0' must have a length of SESSION_RUN_COUNTER */

#define MAXBUF	10000
#define MAXSTRING 256
#define DOUBLE_MAXBUF 2*MAXBUF
#define MAXNAME 80			/* maximum accepted length of a node label */

#define DEFAULT_ELEMENT_LENGTH 40	/* standard unit length for petri-net elements */
#define DEFAULT_CORX 500		/* x-value of a fixed ancor point on the screen */
#define DEFAULT_CORY 3500		/* y-value of a fixed ancor point on the screen */

#define PLACEX 24
#define PLACEY 24
#define TRANSX 36
#define TRANSY 16

#define DEFAULT_TRANSX_LENGTH 36
#define DEFAULT_TRANSY_LENGTH 16
#define DEFAULT_PLACEX_LENGTH 24
#define DEFAULT_PLACEY_LENGTH DEFAULT_PLACEX_LENGTH

#define DEFAULT_TRANSITION_COLOR	3
#define DEFAULT_PLACE_COLOR		DEFAULT_TRANSITION_COLOR
#define DEFAULT_EDGE_COLOR		14

#define ARROW_LENGTH 7		/* see also P_AL and T_AL */

/* the following P_- and T_-definitions are defaults of the place nodes and
   the transition nodes; usually not all of them will be used (e.g. P_NP/T_NP
   always will be computed exactly for petri-net edges) */
#define P_NSY	PLACEY
#define P_NSX	PLACEX
#define P_NTI	1
#define P_NLP	0
#define P_NFI	0			
#define P_NEI	4
#define P_NLV	0			/* node label is invisibel, but existing */
#define P_NCOL	12 			/* 12=dark pink */
#define P_ECOL	14
#define P_NPY	DEFAULT_CORY
#define P_NPX	DEFAULT_CORX
#define P_ETI	0
#define P_EFI	2
#define P_ELV	0
#define P_AL	7
#define P_AA	0.75			
#define P_EL	add_to_edgeline(new_edgeline(DEFAULT_CORX+15,DEFAULT_CORY),DEFAULT_CORX+100,DEFAULT_CORY+100)
#define P_NP	"1015 1000 1100 1100"
#define P_LABEL	"<place>"

#define T_NSY	TRANSY
#define T_NSX	TRANSX
#define T_NTI	0
#define T_NLP	0
#define T_NFI	0
#define T_NEI	4
#define T_NLV	1
#define T_NCOL	3 			/* 3=blue or 12=dark pink */
#define T_ECOL	14
#define T_NPY	DEFAULT_CORY
#define T_NPX	DEFAULT_CORX
#define T_ETI	0
#define T_EFI	2
#define T_ELV	0
#define T_AL	7
#define T_AA	0.75
#define T_EL	add_to_edgeline(new_edgeline(DEFAULT_CORX+15,DEFAULT_CORY),DEFAULT_CORX+100,DEFAULT_CORY+100)
#define T_NP	"1015 1000 1100 1100"
#define T_LABEL	"<transition>"

#define TG_EDGE_LABEL "<edge>"


#define THETA_HELP_NAME	"<help_node>"	/* for only syntactional reasoned nodes */
#define THETA_HELP_NSX 	40
#define THETA_HELP_NSY 	3
#define P_SEMA_NSX	(P_NSX + 10)
#define P_SEMA_NSY	(P_NSY + 2)
#define SEMA_NAME	"SEMA"
#define SEQUENCE_HELP_TRANSITION_NAME	"@SEQ"

#define DEFAULT_AGENT_FILE 		"AGENT"
#define TERMGRAPH_PARAMETER_FILE 	".termgraph"
#define TERM_END_CHR 			'.'
#define AGENT_SKIP_NAME 		"skip"

typedef char string[MAXBUF];
typedef char double_string[DOUBLE_MAXBUF];

typedef enum { INIT_NORTH, INIT_WEST, INIT_SOUTH, INIT_EAST } Termgraph_orientation;

#define DEFAULT_ORIENTATION INIT_NORTH

typedef struct pnodestruct {
	char			name[MAXNAME];	/* Knotenoperator bzw. -name */
	int 			cardinal; /* current number for GraphEd */
	int 			x, y;	/* Bildkoordinatenfelder, relativ */
	int			height, width;	/* reservierte Rahmengroesse des Teilbaumes */
	int 			activ;	/* temp. Zustand, fuer Algorithmen */
	int			istarget;	/* zeigt an, ob ein Knoten erreicht wird */
	int			isfixed;	/* zeigt an, ob ein Teilbaum noch vertauschbar ist */
	struct pnodestruct	*pred;	/* Vorgaengerknoten */
	struct pnodestruct	*succ;	/* successor of a 'sheet' */
	int 			lbranch;/* interne Baumorientierung */
	struct pnodestruct	*left;	/* linker Nachfolger im Binaerbaum */
	struct pnodestruct	*right;	/* rechter     "      "      "     */
	Snode			Snod;		/* GraphEd-Entsprechung */
} pnode;

static char opchar[5] ={
	'$', '|', '%', ';', ':'
};


/**************************************************************************/
/****    Bezeichner in der Sicherungsdatei TERMGRAPH_PARAMETER_FILE:    ****/
/**************************************************************************/
static char *name[] = {
		"TG_ONLY_DEFAULTS",
		"TG_BASIC_LENGTH",
		"TG_TRANS_X",
		"TG_TRANS_Y",
		"TG_PLACE_X",
		"TG_PLACE_Y",
		"TG_TRANS_COL",
		"TG_PLACE_COL",
		"TG_EDGE_COL",
		"TG_TRANS_FONT",
		"TG_PLACE_FONT",
		"TG_EDGE_FONT",
		"TG_TRANS_NLV",
		"TG_PLACE_NLV",
		"TG_EDGE_NLV",
		"TG_ARROW_LENGTH",
		"TG_ARROW_ANGLE",
		"TG_INITIAL_X",
		"TG_INITIAL_Y",
		"TG_AGENT_FILE",
		"TG_EDGELINE_LAYOUT"
};


/*****************************/
/****    Makrobefehle:    ****/
/*****************************/

#define ABS(x) (((x)<0)?(-(x)):(x))
#define VAL(p,x) (((p)==NULL)?0:(p->x))
#define VALa(p,a,x) (((p)==NULL)?(a):(p->x))
#define ALTERNODE(p) ((p->lbranch==0)?(p->pred->left):(p->pred->right))
#define LIFT_NODE(n) ( ELEMENT_LENGTH/2 + ( MAX(3, (n)->activ) * ELEMENT_LENGTH)/6 )/*6war2,21.11.*/

extern	int	name_char();
#define NAMENODE(p) ( name_char(VALa((p)->right,'|',name[0]))?name_char(VALa((p)->left,'|',name[0]))\
			?(p)->left:(p)->right:(p)->left )
#define SETY_NODE(a,b) ((a)<(b) ? (a)+ELEMENT_LENGTH : (a)-ELEMENT_LENGTH)    /* (((a)+(b))/2) */

#define _itoa(n,s,i) {i = 0;do {s[i] = n % 10 + '0';i++;} while (( n /= 10 ) > 0);\
					s[i] = '\0';reverse(s);}
					

