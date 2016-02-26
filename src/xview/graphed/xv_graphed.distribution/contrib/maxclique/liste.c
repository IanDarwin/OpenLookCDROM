/* (C) Universitaet Passau 1986-1991 */
/************************************************************************
 * File    : liste.c							*
 * Aufgabe : Verwaltung der eigenen Zustandsmenge			*
 *									*
 * Autor   : Torsten Bachmann						*
 * Datum   : 21.10.89							*
 ************************************************************************/

#include <string.h>

#include <std.h>
#include <sgraph.h>
#include "liste.h"
#include "gfunc.h"


L_node_list L_create_list (size)
int size;
	/*
	Erzeugt eine neue Liste fuer Knoteneintraege. Speicher wird fuer size
	Knoten reserviert.
	*/
	{	L_node_list lhilf;
		lhilf = (L_node_list) malloc(sizeof(struct l_node_list));
		G_Test_Stack_Overflow(lhilf);
		lhilf->curlen = 0;
		if (size<1)  size = 1;
		lhilf->maxlen = size;
		lhilf->nodes = (Snode *) calloc(size, sizeof(Snode));
		G_Test_Stack_Overflow(lhilf->nodes);
		return lhilf;
	}


void L_append(list, node)
L_node_list list;
Snode node;
	/*
	Haengt den Knoten node an die Liste list an. Falls fuer das Array nicht
	genug Speicher reserviert ist, wird mittels realloc noch weiterer
	dazugeholt.
	*/
	{       if (list->curlen >= list->maxlen)
		{       list->maxlen += 4; /* Jeweils 4er Schritte */
			list->nodes= (Snode *)	realloc(list->nodes,
						list->maxlen*sizeof(Snode));
			G_Test_Stack_Overflow(list->nodes);
		}
		list->nodes [list->curlen++] = node;
	}


void L_delete_entry(list, nr)
L_node_list list;
int nr;
	/*
	Entfernt den nr-ten Knoten aus der Knotenliste von list. Falls diese
	Funktion in einer L_for_node_list - Schleife benutzt wird, muss an-
	schliessend die Laufvariable um eins herabgesetzt werden, um noch alle
	Eintraege zu erreichen. Speicherplatz wird hierbei nicht freigegeben.
	*/
	{       if ( nr < --(list->curlen))
		{	memcpy(&L_list_entry(list, nr),
				&L_list_entry(list, nr+1),
				(list->curlen - nr) * sizeof(Snode));
		}
	}


void L_move(sourcelist,destlist,nr)
L_node_list sourcelist;
L_node_list destlist;
int nr;
	/*
	Kopiert den nr-ten Eintrag der Liste sourcelist in die Liste destlist.
	Aus sourcelist wird dieser Eintrag entfernt. Falls die Elemente von
	sourcelist mit einer L_for_node_list - schleife durchlaufen werden,
	muss anschliessend die Laufvariable um 1 erniedrigt werden.
	*/
	{	L_append(destlist, L_list_entry(sourcelist,nr));
		L_delete_entry(sourcelist,nr);
	}


void L_close_list (list)
L_node_list list;
	/*
	Gibt saemtlichen Speicher fuer die Liste list wieder frei.
	*/
	{	if (list != NULL)
		{	free(list->nodes);
			free(list);
		}
	}


void L_minimize_mem(list)
L_node_list list;
	/*
	Gibt allen, zur Speicherung der Liste nicht benoetigten Speicher
	wieder frei.
	*/
	{       list->maxlen = list->curlen>0 ? list->curlen : 1 ;
		list->nodes = (Snode *)	realloc(list->nodes,
					list->maxlen*sizeof(Snode));
	}


