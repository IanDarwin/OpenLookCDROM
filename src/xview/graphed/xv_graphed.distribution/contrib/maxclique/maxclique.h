/* (C) Universitaet Passau 1986-1991 */
/************************************************************************
 * File    : maxclique.h						*
 * Aufgabe : Umwandlung von Slist in L_node_list Datenstruktur		*
 *           Stellt die Funktionen zur Verfuegung, die fuer die Berech-	*
 *           nung der Clique benoetigt werden.				*
 *									*
 * Autor   : Torsten Bachmann						*
 * Datum   : 21.8.90							*
 ************************************************************************/
 
Sedge	M_check_double_edges      ( /* Sgraph graph */ );
Slist	M_find_max_clique_in_list ( /* Slist  list  */ );
Slist	M_find_max_clique         ( /* Sgraph graph */ );
