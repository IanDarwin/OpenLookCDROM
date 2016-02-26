/* (C) Universitaet Passau 1986-1991 */
/*****************************************************************************************/
/**											**/
/**				Modul termgraph_main.h					**/
/**											**/
/*****************************************************************************************/
/**	ERSTELLUNG:	Rudolf Seisenberger,	1990					**/
/**	Enthaelt globale Variablendefinitionen mit Initialisierung.			**/
/** 	Konstanten sind in termgraph_decl.h definiet.					**/
/*****************************************************************************************/

/* node size parameters */
Termgraph_orientation termgraph_orientation = DEFAULT_ORIENTATION;
int termgraph_transx_length = DEFAULT_TRANSX_LENGTH;
int termgraph_transy_length = DEFAULT_TRANSY_LENGTH;
int termgraph_placex_length = DEFAULT_PLACEX_LENGTH;
int termgraph_placey_length = DEFAULT_PLACEY_LENGTH;


/* miscelous parameters */
int termgraph_transition_color = T_NCOL;
int termgraph_place_color = P_NCOL;
int termgraph_edge_color = P_ECOL;
int termgraph_transition_font = T_NFI;
int termgraph_place_font = P_NFI;
int termgraph_edge_font = P_EFI;
int termgraph_transition_nlv = T_NLV;
int termgraph_place_nlv = P_NLV;
int termgraph_edge_elv = P_ELV;
int termgraph_arrow_length = P_AL;
float termgraph_arrow_angle = (float)P_AA;
int termgraph_initial_x = DEFAULT_CORX;
int termgraph_initial_y = DEFAULT_CORY;

int termgraph_misc_item_value = 0;
int termgraph_only_defaults = TRUE;

