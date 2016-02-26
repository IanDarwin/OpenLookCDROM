/* (C) Universitaet Passau 1986-1991 */
/************************************************************************
 * File    : Sgragra.c                                      Version 1.0 *
 * Aufgabe : Datenstruktur fuer Graphgrammatiken			*
 *									*
 * Autor   : Torsten Bachmann						*
 * Datum   : 17.12.90							*
 ************************************************************************/

#include <string.h>		/* for strdup */

#include "std.h"
#include "slist.h"
#include "sgraph.h"
#include "sgragra.h"

extern	Sgragra	make_sgragra     ()
	/*
	Reserviert Speicher fuer den Kopf einer Graphgrammatik.
	*/
	{	Sgragra gg = (Sgragra) mymalloc (sizeof(struct sgragra));
		gg->class		= S_UNDEFINED;
		gg->tv = gg->te		= empty_slist;
		gg->nv = gg->ne		= empty_slist;
		gg->productions		= empty_sprod;
		gg->global_embeddings	= empty_sglobalembed;
		gg->startnode		= empty_node;
		gg->label		= NULL;
		gg->attrs		= make_attr(ATTR_DATA, NULL);
		return gg;
	}


extern	void	remove_sgragra   ( gragra )
Sgragra gragra;
	/*
	Loescht saemtlichen durch die Graphgrammatik reservierten Speicher.
	Dabei wird auch der Speicher saemtlicher Produktionen, Alphabete und
	des Labels freigegeben. Attribute bleiben unberuecksichtigt.
	gragra muss beim Aufruf != NULL sein.
	*/
	{       
		while (gragra->productions != empty_sprod)
			remove_sprod  ( gragra->productions );
		if (gragra->startnode != empty_node)
			remove_node  ( gragra->startnode );
		S_alphabet_remove(gragra->tv);
		S_alphabet_remove(gragra->te);
		S_alphabet_remove(gragra->nv);
		S_alphabet_remove(gragra->ne);
		if (gragra->label != NULL)
			myfree(gragra->label);
		
		myfree(gragra);
	}


extern	void	set_sgragralabel ( gragra, label )
Sgragra	gragra;
char	*label;
	{       
		if (gragra != empty_sgragra)
		{
			if (gragra->label != NULL)
			{
				myfree(gragra->label);
				gragra->label = NULL;
			}
			if (label != NULL)
				gragra->label = strdup(label);
		}
	}
