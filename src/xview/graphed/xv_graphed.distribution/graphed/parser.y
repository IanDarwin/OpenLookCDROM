/* (C) Universitaet Passau 1986-1991 */
%{/* GraphEd Source, 1986-1991 by Michael Himsolt	*/
/**********************************************************************/
/*									*/
/*				parser.y				*/
/*									*/
/************************************************************************/
/*									*/
/*	Dieses Modul enthaelt den Parser fuer GraphEd.			*/
/*	Geparst werden muessen :					*/
/*	- Graphen (inklusive GraphEd-interner Information)		*/
/*	- GRAPHED_INITIALISATION_FILE					*/
/*									*/
/*	Der eigentliche C-Sourcecode (der den Parser yyparse() enhaelt)	*/
/*	wird von dem Parsergenerator yacc erzeuggt.			*/
/*									*/
/************************************************************************/


#include "misc.h"
#include "graph.h"
#include "load.h"
#include "user.h"
#include "find.h"
#include "group.h"
#include "graphed_subwindows.h"

/************************************************************************/
/*									*/
/*			GLOBALE FUNKTIONEN				*/
/*									*/
/************************************************************************/
/*									*/
/*	int	yyparse ()		[Nach Durchlauf von YACC]	*/
/*									*/
/************************************************************************/


/************************************************************************/
/*									*/
/*		LOKALE FUNKTIONEN UND PROZEDUREN			*/
/*									*/
/************************************************************************/


static	int	yyerror   ();
static	int	yywarning ();

static	void	start_load_graph                 ();
static	void	end_load_graph                   ();


/************************************************************************/
/*									*/
/*			LOKALE VARIABLEN				*/
/*									*/
/************************************************************************/


static	Graph		current_graph      = empty_graph;    /*		*/
static	Node		current_node       = empty_node;     /* werden	*/
static	Edge		current_edge       = empty_edge;     /* gerade	*/
static	Edgeline	current_edgeline   = (Edgeline)NULL; /* geladen	*/
static	Edgeline	current_el_head    = (Edgeline)NULL; /*		*/
static	Node_attributes	current_node_attributes;
static	Edge_attributes	current_edge_attributes;

static	Group		group_of_current_graph;

/* Umrechnungstabellen fuer Indices (Noetig, da in der Datei die	*/
/* Reihenfolge ganz anders sein kann als in GraphEd)			*/

static	int	fontindex_translation     [MAX_FONTS]    = 0;
static	int	nodetypeindex_translation [MAX_NODETYPE] = 0;
static	int	edgetypeindex_translation [MAX_EDGETYPE] = 0;

/* Zaehler : wie viele ... wurden geladen (zur Konsistenzpruefung)	*/

static	int	font_count     = 0;
static	int	nodetype_count = 0;
static	int	edgetype_count = 0;

/* Wurde ueberhaupt ein ... erfolgreich geladen ?			*/

static	int	any_font_successfully_loaded     = FALSE;
static	int	any_nodetype_successfully_loaded = FALSE;
static	int	any_edgetype_successfully_loaded = FALSE;

/* Makros, Range-Checking	*/

/*
#define	is_legal_coordinate(x,y)   (is_positive(x) && is_positive(y))
/*
/* Changed MH 25/9/91 to avoid flames */
#define	is_legal_coordinate(x,y)   TRUE

#define	is_legal_size(x,y)         (((x)>=0) && ((y)>=0))
#define is_legal_nodetype_index(i) (((i)>=0) && ((i)<nodetype_count))
#define is_legal_edgetype_index(i) (((i)>=0) && ((i)<edgetype_count))
#define is_legal_font_index(i)     (((i)>=0) && ((i)<font_count))
#define is_legal_color(i)          (((i)>=0) && ((i)<GRAPHED_COLORMAPSIZE))
#define is_legal_gragra_type(i)    (((i)>=0) && ((i)<NUMBER_OF_GRAGRA_TYPES))
#define	is_positive(x)             ((x) >= 0)

/* Falls graph_loading == TRUE, so wird gerade ein Graph geladen	*/
/* Wird gebraucht, um im Fehlerfall Konsistenz wiederherzustellen	*/
 
static	int	graph_loading = FALSE;

/* Stackgroesse fuer yacc : Maximal < 10000 Knoten zulaessig		*/
/* IST DIE YACC-STACKGROESSE NICHT AUSREICHEND, FOLGENDEN WERT ERHOEHEN	*/

#define	YYMAXDEPTH 10000

/* Das folgende Makro wird verwendet, um yyerror aufzurufen und dann	*/
/* den Parser abzubrechen (yyerror loescht den Graphen)			*/

#define	yacc_error(s)	{ yyerror(s); YYABORT; }

/************************************************************************/
/*									*/
/*			KONVENTIONEN					*/
/*									*/
/************************************************************************/
/*									*/
/*	Da Graphen auch von anderen Programmen erzeugt werden koennen,	*/
/*	wird im Parser mit Hilfe der obigen is_legel_... - Makros ein	*/
/*	Konsistenzcheck auf den Knoten- und Kantenattributen		*/
/*	vorgenommen. Nach Moeglichkeit werden Attribute, die "illegal"	*/
/*	sind, durch Defaultwerte ersetzt und mit yywarning eine Meldung	*/
/*	abgegeben.							*/
/*									*/
/*======================================================================*/
/*									*/
/*	Bei Syntax - und sonstigen schweren Fehlern, die einen Abbruch	*/
/*	des Parsens erzwingen (yacc_error), muss leider der bisher	*/
/*	eingegebene Graph aus Gruenden der Konsistenzerhaltung		*/
/*	(wegen der "vorwaerts" erzeugten Knoten, die keine Attribute	*/
/*	enthalten, was beim Zeichnen zum Absturz fuehren kann)		*/
/*	in yyerror geloescht werden.					*/
/*									*/
/*======================================================================*/
/*									*/
/*	yacc_error (string) ruft yyerror auf und bricht das Parsen	*/
/*	ab.								*/
/*									*/
/************************************************************************/
%}
%union	{	/* Stack fuer Parser	*/
	int	nr;	/* Integerwert		*/
	char	*text;	/* Stringwert		*/
	Group	group;	/* Stringwert		*/
}
	
%token		GRAPH '=' ';' ',' END GRAPHS INIT
%token		BEGIN_GRAPH_INTERNALS END_GRAPH_INTERNALS
%token		DIR DIRECTED UNDIRECTED
%token		NODESTYLE EDGESTYLE

%token		COLOR

%token		NODEPLACE NODESIZE NODEFONT_INDEX NODETYPE_INDEX
%token		NODELABEL_PLACEMENT NODE_EDGE_INTERFACE NODELABEL_VISIBILITY
%token		NODECOLOR

%token		EDGETYPE_INDEX EDGEFONT_INDEX EDGELINE_POINTS EDGELABEL_SIZE
%token		ARROWLENGTH ARROWANGLE EDGELABEL_VISIBILITY EDGECOLOR

%token		GRAGRA GLOBAL_EMBEDDING EMBED_MATCH_ATTR
%token		WORKING_AREA_SIZE SCROLL_OFFSET GRIDWIDTH WORKING_AREA_WINDOW

%token		SELECT_IF_MULTIPLE_PICKED PROD LEFT RIGHT EMBED MAPSTO_IN MAPSTO_OUT

%token		UNKNOWN_INTERNAL
%token	<nr>	NUMBER
%token	<text>	IDENTIFIER

%start		parser_start
%%
parser_start :		  plain_graph
			{
				/* Hier besteht die Datei aus einem	*/
				/* einzelnen Graphen			*/
				
				if (!all_nodes_complete()) {
					yacc_error ("Not all Nodes specified\n");
				}
			}
			| graphs
			{
				/* Datei besteht aus einer Liste von	*/
				/* Graphen				*/
				
				if (!all_nodes_complete()) {
					yacc_error ("Not all Nodes specified\n");
				}
			}
			| initialisation
			| error
			{
				/* Syntax Error	*/
				YYABORT;
			}
			;



plain_graph :		/* REAL GRAPH	*/
			{
				start_load_graph ();
			}
			GRAPH IDENTIFIER '='
			{
				current_graph = create_graph (load_buffer);
				set_graph_label (current_graph, $3);
				current_graph->is_production = FALSE;
			}
			directedness
			graph_internals
			list_of_nodes
			END
			{
				end_load_graph ();
			}

			|

			/* GRAGRA PRODUCTION	*/
			{
				start_load_graph ();
			}
			PROD IDENTIFIER '='
			{
				current_graph = create_graph (load_buffer);
				set_graph_label (current_graph, $3);
				current_graph->is_production = TRUE;
				current_graph->is_production = TRUE;
			}
			gragra_type
			directedness
			graph_internals
			list_of_nodes
			maybe_prod
			END
			{
				end_load_graph ();
			}

			;

graph :			/* REAL GRAPH	*/
			{
				start_load_graph ();
			}
			GRAPH IDENTIFIER '='
			{
				current_graph = create_graph (load_buffer);
				set_graph_label (current_graph, $3);
				current_graph->is_production = FALSE;
			}
			directedness
			list_of_nodes
			END
			{
				end_load_graph ();
			}

			|

			/* GRAGRA PRODUCTION	*/
			{
				start_load_graph ();
			}
			PROD IDENTIFIER '='
			{
				current_graph = create_graph (load_buffer);
				set_graph_label (current_graph, $3);
				current_graph->is_production = TRUE;
			}
			gragra_type
			directedness
			list_of_nodes
			maybe_prod
			END
			{
				end_load_graph ();
			}

			;


directedness :		  /* nothing or ... */
			{
				set_graph_directedness (current_graph, TRUE);
			}
			| DIRECTED
			{
				set_graph_directedness (current_graph, TRUE);
			}
			| UNDIRECTED
			{
				set_graph_directedness (current_graph, FALSE);
			}
			;


gragra_type :		  /* nothing or ... */
			{
				current_graph->gra.type = ENCE_1;
			}
			| GRAGRA NUMBER
			{
				if (is_legal_gragra_type ($2)) {
					current_graph->gra.type = $2;
				} else {
					yywarning ("Illegal graph grammar type");
				}
			}
			;


graphs :		GRAPHS graph_internals list_of_graphs END
			;

list_of_graphs :	  /* nothing */
			| graph list_of_graphs
			;

graph_internals :	  /* nothing or ... */
			{
				font_count     = 1; /* Einer ist immer	*/
				nodetype_count = 1; /* ins System	*/
				edgetype_count = 1; /* geladen		*/
				
				/* Initialisiere Translationstabellen;	*/
				/* ab jetzt duerfen nur Graphen mit	*/
				/* ..._index == 0 geladen werden.	*/
				
				fontindex_translation[0] = -1;
				/* ==> nehme current_nodefont_index	*/
				/* oder current_edgefont_index		*/
				
				nodetypeindex_translation[0] =
					current_nodetype_index;
				edgetypeindex_translation[0] =
					current_edgetype_index;
			}
			| BEGIN_GRAPH_INTERNALS
			  {
			  	font_count     = 0;
			  	nodetype_count = 0;
			  	edgetype_count = 0;
			  	any_font_successfully_loaded     = FALSE;
			  	any_nodetype_successfully_loaded = FALSE;
			  	any_edgetype_successfully_loaded = FALSE;
			  }
			  fontlist           ';'
			  nodetypelist       ';'
			  edgetypelist       ';'
			  graph_state_list   ';'
			  graphed_state_list
			  END_GRAPH_INTERNALS
			;



list_of_nodes :		  /* nothing or ... */
			| node list_of_nodes
			;

node :			node_nr
			node_internals
			nodelabel
			{
				current_node->loaded = TRUE;
			}
			list_of_edges
			{
				current_node = empty_node;
			}
			;

node_nr :		NUMBER
			{
				current_node = get_node_with_number(current_graph, $1);
				group_of_current_graph = add_to_group (group_of_current_graph, current_node);
			}
			;

nodelabel :		IDENTIFIER
			{
				if (strcmp ($1, "")) {
					node_set (current_node, ONLY_SET, NODE_LABEL, $1, 0);
				}
			}
			;


node_internals :	BEGIN_GRAPH_INTERNALS
			node_attributes
			END_GRAPH_INTERNALS
			{
				node_set (current_node, ONLY_SET,
					SET_NODE_ATTRIBUTES (current_node_attributes),
					NODE_POSITION, current_node_attributes.x,
					               current_node_attributes.y,
					0);
			}
			|
			/* Kurze Version	*/
			BEGIN_GRAPH_INTERNALS
			NUMBER NUMBER
			END_GRAPH_INTERNALS
			{
				if (is_legal_coordinate ($2, $3))
					node_set (current_node, ONLY_SET,
						SET_NODE_ATTRIBUTES (get_node_style (NORMAL_NODE_STYLE)),
						NODE_POSITION, $2, $3,
						0);
				else {
					yacc_error ("Illegal node position");
				}

			}
			;

any_node_attribute :	  NODESIZE             nodesize
			| NODETYPE_INDEX       nodetype_index
			| NODEFONT_INDEX       nodelabel_font_index
			| NODELABEL_PLACEMENT  nodelabel_placement
			| NODE_EDGE_INTERFACE  node_edge_interface
			| NODELABEL_VISIBILITY nodelabel_visibility
			| NODEPLACE            nodeplace
			| COLOR                nodecolor
			| UNKNOWN_INTERNAL     unknown_internal
			;


any_old_node_attribute :
			  NODESIZE             nodesize
			| NODETYPE_INDEX       nodetype_index
			| NODEFONT_INDEX       nodelabel_font_index
			| NODELABEL_PLACEMENT  nodelabel_placement
			| NODE_EDGE_INTERFACE  node_edge_interface
			| NODELABEL_VISIBILITY nodelabel_visibility
			| NODEPLACE            nodeplace
			;

list_of_node_attributes :	
			| any_node_attribute list_of_node_attributes
			;

node_attributes :	{
				current_node_attributes = get_node_style (NORMAL_NODE_STYLE);
			}
			list_of_node_attributes
			;


nodeplace :		NUMBER NUMBER
			{
				if (is_legal_coordinate ($1, $2)) {
					current_node_attributes.x = $1;
					current_node_attributes.y = $2;
				} else {
					yacc_error ("Illegal nodeplace");
				}
			}
			;

nodesize :		NUMBER NUMBER
			{
				if (is_legal_size($1,$2)) {
					current_node_attributes.width  = $1;
					current_node_attributes.height = $2;
				} else {
					yywarning ("Illegal nodesize");
				}
			}
			;

nodetype_index :	NUMBER
			{
				if (is_legal_nodetype_index($1)) {
					current_node_attributes.type_index  = $1;
				} else {
					yywarning ("Illegal nodetype_index");
				}
			}
			;

nodelabel_font_index :	NUMBER
			{
				if (is_legal_font_index($1)) {
					current_node_attributes.font_index  = $1;
				} else {
					yywarning ("Illegal font_index");
				}
			}
			;

nodelabel_placement :	NUMBER
			{
				if (is_legal_nodelabel_placement($1)) {
					current_node_attributes.nodelabel_placement = 
						(Nodelabel_placement)$1;
				} else {
					yywarning ("Illegal nodelabel_placement");
				}
			}
			;

node_edge_interface :	NUMBER
			{
				if (is_legal_node_edge_interface($1)) {
					current_node_attributes.node_edge_interface = 
						(Node_edge_interface)$1;
				} else {
					yywarning ("Illegal node_edge_interface");
				}
			}
			;

nodelabel_visibility :	NUMBER
			{
				current_node_attributes.label_visibility = $1;
			}

nodecolor :		NUMBER
			{
				if (is_legal_color($1)) {
					current_node_attributes.color = $1;
				}
			}


list_of_edges :		 ';'
			| edge list_of_edges
			;

edge :			targetnode_nr
			edge_internals
			edgelabel
			{
				current_edge = empty_edge;
			}
			;

targetnode_nr :		NUMBER
			{
				current_edge = create_edge (current_node,
					get_node_with_number (current_graph, $1));
			}
			;

edgelabel :		IDENTIFIER
			{
				if (strcmp ($1, "")) {
					edge_set (current_edge, ONLY_SET, EDGE_LABEL, $1, 0);
				}
			}
			;


edge_internals:		/* nothing or ... */
			{
				current_el_head = new_edgeline(
					node_x(current_edge->source),
					node_y(current_edge->source));
				current_edgeline = add_to_edgeline(current_el_head,
					node_x(current_edge->target),
					node_y(current_edge->target));
				
			  	edge_set (current_edge, ONLY_SET,
			  		SET_EDGE_ATTRIBUTES (get_edge_style (NORMAL_EDGE_STYLE)),
			  		EDGE_LINE, current_el_head,
			  		0);
			}
			| BEGIN_GRAPH_INTERNALS
			  edge_attributes
			  END_GRAPH_INTERNALS
			{
			  	edge_set (current_edge, ONLY_SET,
			  		SET_EDGE_ATTRIBUTES (current_edge_attributes),
			  		EDGE_LINE, current_el_head,
			  		0);
			}
			;

list_of_edge_attributes :
			| any_edge_attribute list_of_edge_attributes
			;

any_edge_attribute :	  EDGETYPE_INDEX       edgetype_index
			| EDGEFONT_INDEX       edgelabel_font_index
			| EDGELINE_POINTS      edgeline
			| EDGELABEL_VISIBILITY edgelabel_visibility
			| ARROWLENGTH          arrowlength
			| ARROWANGLE           arrowangle
			| COLOR                edgecolor
			| UNKNOWN_INTERNAL     unknown_internal
			;

any_old_edge_attribute :
			  EDGETYPE_INDEX       edgetype_index
			| EDGEFONT_INDEX       edgelabel_font_index
			| EDGELINE_POINTS      edgeline
			| EDGELABEL_VISIBILITY edgelabel_visibility
			| EDGELABEL_SIZE       current_edgelabel_size
			| ARROWLENGTH          arrowlength
			| ARROWANGLE           arrowangle
			;

current_edgelabel_size:	NUMBER NUMBER
			{
				if (is_legal_size ($1, $2))
					set_current_edgelabelsize ($1, $2);
				else {
					yywarning ("Illegal current_edgelabelsize");
				}
			}
			;

edge_attributes :	{
				current_edge_attributes = get_edge_style (NORMAL_EDGE_STYLE);
			}
			list_of_edge_attributes
			;
	
	
edgetype_index :	NUMBER
			{
				if (is_legal_edgetype_index($1)) {
					current_edge_attributes.type_index = $1;
				} else {
					yywarning ("Illegal edgetype_index");
				}
			}
			;

edgelabel_font_index :	NUMBER
			{
				if (is_legal_font_index($1)) {
					current_edge_attributes.font_index = $1;
				} else {
				    yywarning ("Illegal font_index");
				}
			}
			;

edgelabel_visibility :	NUMBER
			{
				current_edge_attributes.label_visibility = $1;
			}

arrowlength :		NUMBER
			{
				if (is_positive ($1))
					current_edge_attributes.arrow_length = $1;
				else {
					yywarning ("Illegal arrowlength");
				}
			}

arrowangle :		NUMBER
			{
				current_edge_attributes.arrow_angle = deg_to_rad ($1);
			}

edgecolor :		NUMBER
			{
				if (is_legal_color ($1)) {
					current_edge_attributes.color = $1;
				}
			}


edgeline :		{
				current_edgeline = current_el_head = (Edgeline)NULL;
			}
			edgeline_points
			;

edgeline_points :	  edgeline_point
			| edgeline_point edgeline_points
			;

edgeline_point :	NUMBER NUMBER
			{
				if (current_edgeline == (Edgeline)NULL)
					current_edgeline = current_el_head  = new_edgeline ($1, $2);
				else
					current_edgeline = add_to_edgeline (current_edgeline, $1, $2);
			}
			;

fontlist :		  /* nothing or ... */
			| font fontlist
			;

font :			IDENTIFIER IDENTIFIER
			{
			    int	 i;
			    char buffer[FILENAMESIZE];
			
			    if ((i = add_font($1,$2)) != -1) {
				fontindex_translation [font_count] = i;
				any_font_successfully_loaded = TRUE;
			    } else {
				sprintf (buffer, "Can't get font %s\n", $1);
				yywarning (buffer);
				fontindex_translation [font_count] = 0;
			    }
			    font_count++;
			}
			;




nodetypelist :		  /* nothing or ... */
			| nodetype nodetypelist
			;

nodetype :		IDENTIFIER
			{
			    int i;
			    char buffer[FILENAMESIZE];
			
			    if ((i = add_nodetype($1)) != -1) {
			        nodetypeindex_translation [nodetype_count] = i;
				any_nodetype_successfully_loaded = TRUE;
			    } else {
				sprintf (buffer, "Can't get nodetype %s\n", $1);
			        yywarning (buffer);
			        nodetypeindex_translation [nodetype_count] = 0;
			    }
			    nodetype_count++;
			}
			;


edgetypelist :		  /* nothing or ... */
			| edgetype edgetypelist
			;

edgetype :		IDENTIFIER
			{
			    int  i;
			    char buffer[FILENAMESIZE];
			
			    if ((i = add_edgetype($1)) != -1) {
			        edgetypeindex_translation [edgetype_count] = i;
				any_edgetype_successfully_loaded = TRUE;
			    } else {
				sprintf (buffer, "Can't get edgetype %s", $1);
			        yywarning (buffer);
			        edgetypeindex_translation [edgetype_count] = 0;
			    }
			    edgetype_count++;
			}
			;



graph_state_list :	  nodestyles
			  edgestyles
			  any_graph_states
			|
			  old_node_and_edge_attributes
			  {
				if (overwrite_state) {
				
					set_node_style (NORMAL_NODE_STYLE, current_node_attributes);
				
					set_current_nodesize (current_node_attributes.width,
						current_node_attributes.height);
					set_current_nodetype (current_node_attributes.type_index);
					set_current_nodefont (current_node_attributes.font_index);
					set_current_nodelabel_placement  (
						current_node_attributes.nodelabel_placement);
					set_current_node_edge_interface  (
						current_node_attributes.node_edge_interface);
					set_current_nodelabel_visibility (
						current_node_attributes.label_visibility);
					set_current_nodecolor (current_node_attributes.color);
					
					set_edge_style (NORMAL_EDGE_STYLE, current_edge_attributes);
							
					set_current_arrowlength (current_edge_attributes.arrow_length);
					set_current_arrowangle  (current_edge_attributes.arrow_angle);
					set_current_edgetype    (current_edge_attributes.type_index);
					set_current_edgefont    (current_edge_attributes.font_index);
					set_current_edgelabel_visibility (current_edge_attributes.label_visibility);
					set_current_edgecolor   (current_edge_attributes.color);
				}
			  }
			  any_old_graph_states
			{
				/* graphed_state may be empty since the old ... */
				/* fields may be empty				*/
				/* declaring graphed_state as empty | ...	*/
				/* yields a reduce/reduce - conflict		*/
			}
			;

any_graph_states :	  /* nothing or ... */
			| any_graph_state any_graph_states
			;

any_graph_state :	  DIR                  current_directedness
			| GRAGRA               gragra
			| GLOBAL_EMBEDDING     global_embedding
			| EMBED_MATCH_ATTR     embed_match_attributes
			| EDGELABEL_SIZE       current_edgelabel_size
			| UNKNOWN_INTERNAL     unknown_internal
			;

any_old_graph_states :	  /* nothing or ... */
			| any_old_graph_state any_old_graph_states
			;

any_old_graph_state :	  DIR                  current_directedness
			| GRAGRA               gragra
			| GLOBAL_EMBEDDING     global_embedding
			| EMBED_MATCH_ATTR     embed_match_attributes
			| UNKNOWN_INTERNAL     unknown_internal
			;

current_directedness :	NUMBER
			{
				if (overwrite_state)
					set_current_directedness (int_to_bool($1));
			}


gragra :		NUMBER IDENTIFIER IDENTIFIER
			{
				if (is_legal_gragra_type ($1)) {
					set_current_gragra_type ($1);
				} else {
					yywarning ("Illegal graph grammar type");
				}
				
				set_current_gragra_nonterminals ($2);
				set_current_gragra_terminals    ($3);
			}

global_embedding :	IDENTIFIER
			{
				set_global_embedding_name ($1);
			}

embed_match_attributes : NUMBER
			{
				set_embed_match_attributes ((unsigned)$1);
			}

nodestyles :		nodestyle
			{
				if (overwrite_state) {
					set_node_style (NORMAL_NODE_STYLE, current_node_attributes);
				
					set_current_nodesize (current_node_attributes.width,
						current_node_attributes.height);
					set_current_nodetype (current_node_attributes.type_index);
					set_current_nodefont (current_node_attributes.font_index);
					set_current_nodelabel_placement  (
						current_node_attributes.nodelabel_placement);
					set_current_node_edge_interface  (
						current_node_attributes.node_edge_interface);
					set_current_nodelabel_visibility (
						current_node_attributes.label_visibility);
					set_current_nodecolor (current_node_attributes.color);
				}
			}
			nodestyle
			{
				if (overwrite_state) {
					set_node_style (LEFT_SIDE_NODE_STYLE, current_node_attributes);
				}
			}	
			nodestyle
			{
				if (overwrite_state) {
					set_node_style (EMBED_NODE_STYLE, current_node_attributes);
				}
			}
			;

nodestyle :		NODESTYLE IDENTIFIER node_attributes ','


edgestyles :		edgestyle
			{
				if (overwrite_state) {
					set_edge_style (NORMAL_EDGE_STYLE, current_edge_attributes);
							
					set_current_arrowlength (current_edge_attributes.arrow_length);
					set_current_arrowangle  (current_edge_attributes.arrow_angle);
					set_current_edgetype    (current_edge_attributes.type_index);
					set_current_edgefont    (current_edge_attributes.font_index);
					set_current_edgelabel_visibility (current_edge_attributes.label_visibility);
					set_current_edgecolor   (current_edge_attributes.color);
				}
			}
			edgestyle
			{
				if (overwrite_state) {
					set_edge_style (EMBED_EDGE_STYLE, current_edge_attributes);
				}
			}
			;

edgestyle :		EDGESTYLE IDENTIFIER edge_attributes ','


old_node_and_edge_attributes :
			list_of_old_node_and_edge_attributes
			;

list_of_old_node_and_edge_attributes :	
			| any_old_node_attribute list_of_old_node_and_edge_attributes
			| any_old_edge_attribute list_of_old_node_and_edge_attributes
			;

initialisation :	INIT
			{
				font_count     = 0;
				nodetype_count = 0;
				edgetype_count = 0;
			  	any_font_successfully_loaded     = FALSE;
			  	any_nodetype_successfully_loaded = FALSE;
			  	any_edgetype_successfully_loaded = FALSE;
			}
			fontlist ';'
			{
				if (!any_font_successfully_loaded) {
					yacc_error ("No initial fonts found");
				}
			}
			nodetypelist ';'
			{
				if (!any_nodetype_successfully_loaded) {
					yacc_error ("No initial nodetypes found");
				}
			}
			edgetypelist ';'
			{
				if (!any_edgetype_successfully_loaded) {
					yacc_error ("No initial edgetypes found");
				}
			}
			graph_state_list ';'
			graphed_state_list
			;

graphed_state_list :	
			| any_graphed_state graphed_state_list
			;



any_graphed_state :	  WORKING_AREA_SIZE         working_area_size
			| WORKING_AREA_WINDOW       working_area_window_size
			| SCROLL_OFFSET             scroll_offset
			| GRIDWIDTH                 gridwidth
			| SELECT_IF_MULTIPLE_PICKED select_if_multiple_picked
			| UNKNOWN_INTERNAL          unknown_internal;
			;

working_area_size :	NUMBER NUMBER
			{
				if (is_positive($1) && is_positive ($2)) {
					set_working_area_size ($1,$2);
					if (graphed_state.startup) {
						graphed_state.default_working_area_canvas_width  = $1;
						graphed_state.default_working_area_canvas_height = $2;
					}
				} else {
					yywarning ("Illegal working_area_size");
				}
			}
			;

working_area_window_size :	NUMBER NUMBER NUMBER NUMBER
			{
				if (is_positive($1) && is_positive ($2) && is_positive($3) && is_positive ($4)) {
					set_canvas_window (load_buffer, $1,$2, $3,$4);
					if (graphed_state.startup) {
						graphed_state.default_working_area_window_width  = $3;
						graphed_state.default_working_area_window_height = $4;
					}

				} else {
					yywarning ("Illegal working_area_window");
				}
			}
			;

scroll_offset :		NUMBER NUMBER
			{
				if (is_positive($1) && is_positive ($2))
					scroll_working_area ($1,$2);
				else
					yywarning ("Illegal scroll_offset");
			}
			;

gridwidth :		NUMBER
			{
				if (is_positive($1))
					show_grid ($1);
				else
					yywarning ("Illegal gridwidth");
			}
			;


select_if_multiple_picked :	NUMBER
			{
				/* Obsolete, included for backwards	*/
				/* compatibility			*/
			}
			;



unknown_internal :	numbers_and_strings
			{
				yywarning ("Unknown internal");
			}
			;

numbers_and_strings :	  /* nothing */
			| number_or_string numbers_and_strings
			;

number_or_string :	  NUMBER
			| IDENTIFIER
			;



maybe_prod :		/* nothing or ...	*/
			|
			prod
			;

prod :			left_side
			right_side
			embedding
			{	/* this and the following items are	*/
				/* included for backwards compatibility	*/
				/* only					*/
			}
			;

left_side :		LEFT  node_group ';'
			;
right_side :		RIGHT node_group ';'
			;
embedding :		EMBED embed_in '|' embed_out END
			;
node_group :		/* nothing */
			| node_group NUMBER
			;

embed_in :		NUMBER
			embed_in_list
			;
embed_out :		NUMBER
			embed_out_list
			;
embed_in_list :		  /* nothing */
			| NUMBER MAPSTO_IN node_group ';'
			  embed_in_list
			;
embed_out_list :	  /* nothing */
			| NUMBER MAPSTO_OUT node_group ';'
			  embed_out_list
			;



%%/**********************************************************************/
/*									*/
/*			YACC-FEHLERMELDUNGEN				*/
/*									*/
/************************************************************************/
/*									*/
/*	int	yyerror (s)						*/
/*									*/
/*	Gibt ueber error die Fehlermeldung aus s mit Angabe der		*/
/*	Zeilennummer aus.						*/
/*	Der bisher eingelesene Graph muss leider wieder geloescht	*/
/*	werden, da bei den Knoten, die als Folge von Vorwaerts-		*/
/*	referenzen in der Liste der Kanten "zu frueh" erzeugt wurden,	*/
/*	keine Attribute gesetzt wurden (-> Inkonsistenzen !).		*/
/*									*/
/*----------------------------------------------------------------------*/
/*									*/
/*	int	yywarning (s)						*/
/*									*/
/*	Bibt ueber warning die Warnung aus s mit Angabe der Zeilen-	*/
/*	nummer aus.							*/
/*	Nach yywarning ist Weiterarbeiten moeglich.			*/
/*									*/
/************************************************************************/


static	int	yyerror (string)
char	*string;
{
	error ("line %d :\n%s\n", lex_input_file_linenumber, string);
	
	if (graph_loading) {
		/* These lines have to be consistent with end_load_graph */
		graph_loading = FALSE;
	}
	
	delete_graphs_in_buffer (load_buffer);
	buffers[load_buffer].changed = FALSE;
}



static	int	yywarning (string)
char	*string;
{
	warning ("line %d :\n%s\n", lex_input_file_linenumber, string);
}


/************************************************************************/
/*									*/
/*			HILFSPROZEDUREN					*/
/*									*/
/************************************************************************/


static	void	start_load_graph ()
{
	graph_loading = TRUE;
	group_of_current_graph = empty_group;
	
	current_graph = empty_graph;
}


static	void	end_load_graph ()
{
	Node	node;
	Edge	edge;

	group_set  (group_of_current_graph, RESTORE_IT, 0);
	free_group (group_of_current_graph);
	
	reset_graph_has_changed (current_graph);
	
	graph_loading = FALSE;
}
