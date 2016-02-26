/* (C) Universitaet Passau 1986-1991 */
/************************************************************************
 * File    : Sprod.c                                        Version 1.0 *
 * Aufgabe : Datenstruktur fuer Produktionen von Graphgrammatiken	*
 *									*
 * Autor   : Torsten Bachmann						*
 * Datum   : 19.11.90							*
 ************************************************************************/

#include <string.h>	/* for strdup */

#include "std.h"
#include "slist.h"
#include "sgraph.h"
#include "sgragra.h"


extern	Sprod	make_sprod ( gragra )
Sgragra gragra;
	/*
	Reserviert Speicher fuer eine neue Produktion in der Graphgramatik
	gragra. Die neue Produktion wird am Anfang der Produktionsliste
	eingefuegt, und kann ueber gragra->productions angesprochen werden.
	Werte fuer die Produktion muessen nach dem Einfuegen eingetragen
	werden.
	*/
	{	Sprod new = (Sprod) mymalloc(sizeof(struct sprod));
		if (gragra->productions == empty_sprod)
		{	new->pre = new;
			new->suc = new;
                        gragra->productions = new;
		}
		else
		{	new->pre = gragra->productions;
			new->suc = gragra->productions->suc;
			gragra->productions->suc->pre = new;
			gragra->productions->suc = new;
			gragra->productions = new;
		}
		new->gragra = gragra;
		new->left = NULL;
		new->right = empty_graph;
		new->embedding = empty_sembed;
		new->label = NULL;
		new->attrs = make_attr(ATTR_DATA, NULL);
		return new;
	}

extern	void	remove_sprod ( prod )
Sprod prod;
	/*
	Entfernt die Produktion prod aus der Liste der Produktionen. Es
	wird auch der Speicher fuer die Einbettungsregeln und das label
	freigeben. Speicher fuer Attribute wird NICHT wieder freigegeben.
	*/
	{       
		while (prod->embedding != empty_sembed)
			remove_sembed ( prod->embedding );
		if (prod->right != empty_graph)
			remove_graph ( prod->right );
		if (prod->gragra->productions == prod)
		{	if (prod->suc == prod)
			{	prod->gragra->productions = empty_sprod;
			}
			else
			{	prod->suc->pre = prod->pre;
				prod->pre->suc = prod->suc;
				prod->gragra->productions = prod->suc;
			}
		}
		else
		{	prod->suc->pre = prod->pre;
			prod->pre->suc = prod->suc;
		}
		if (prod->label != NULL)
			myfree(prod->label);
		myfree(prod);
	}



extern	void	set_sprodlabel ( prod, label )
Sprod	prod;
char	*label;
	{       
		if (prod != empty_sprod)
		{
			if (prod->label != NULL)
			{
				myfree(prod->label);
				prod->label = NULL;
			}
			if (label != NULL)
				prod->label = strdup(label);
				prod->label = strdup(label);
		}
	}
