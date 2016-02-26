/* (C) Universitaet Passau 1986-1991 */
/************************************************************************
 * File    : Sembed.c                                       Version 1.0 *
 * Aufgabe : Datenstruktur fuer Einbettungen und globale Einbettungen	*
 *           von Graphgrammatiken					*
 *									*
 * Autor   : Torsten Bachmann						*
 * Datum   : 19.11.90							*
 ************************************************************************/

#include "std.h"
#include "slist.h"
#include "sgraph.h"
#include "sgragra.h"


Sembed	make_sembed ( prod, node )
Sprod prod;
Snode node;
	/*
	prod gibt die Produktion an, zu der die neue Einbettungsregel ge-
	hoert; node gibt einen Knoten aus dem Graphen prod->right der
	rechten Seite der Produktion an.
	Reserviert Speicher fuer den Eintrag einer Einbettung. Eingetragen
	wird dieser in die Produktion-Einbettungsliste, sowie in die Kno-
	ten-Einbettungsliste.
	Im Anschluss an die make_embed Operation zeigt prod->embedding auf
	die neu eingefuege Einbettung. Ein Zeiger auf diese Struktur wird
	zusaetzlich als Funktionsergebnis zurueckgegeben.
	*/
	{	Sembed new = (Sembed) mymalloc(sizeof(struct sembed));
		/* Einfuegen in Produktionen-Einbettungsliste */
		if (prod->embedding == empty_sembed)
		{	new->pre = new;
			new->suc = new;
                        prod->embedding = new;
		}
		else
		{	new->pre = prod->embedding;
			new->suc = prod->embedding->suc;
			prod->embedding->suc->pre = new;
			prod->embedding->suc = new;
			prod->embedding = new;
		}

		/* Einfuegen in Knoten-Einbettungsliste */
		if ((Sembed) node->embedding == empty_sembed)
		{	node->embedding = (char *) new;
			new->npre = new;
			new->nsuc = new;
		}
		else
		{	new->npre = (Sembed) node->embedding;
			new->nsuc = ((Sembed) node->embedding)->nsuc;
			((Sembed) node->embedding)->nsuc->npre = new;
			((Sembed) node->embedding)->nsuc = new;
			node->embedding = (char *) new;
		}
		new->node_right = node;

		new->prod = prod;
		new->node_embed = NULL;
		new->oldedge = NULL;
		new->olddir  = S_out;
		new->newedge = NULL;
		new->newdir  = S_out;
		new->attrs   = make_attr(ATTR_DATA, NULL);
		return new;
	}


void	remove_sembed ( embed )
Sembed embed;
	/*
	Loescht die Einbettungsregel embed aus der Produktionen-Einbettungs-
	liste und aus der Knoten-Einbettungsliste. Speicher, der fuer Attri-
	bute belegt wird, wird NICHT wieder freigegeben.
	*/
	{       /* Loeschen aus Produktionen-Einbettungsliste */
		if (embed->prod->embedding == embed)
		{	if (embed->suc == embed)
			{	embed->prod->embedding = empty_sembed;
			}
			else
			{	embed->suc->pre = embed->pre;
				embed->pre->suc = embed->suc;
				embed->prod->embedding = embed->suc;
			}
		}
		else
		{	embed->suc->pre = embed->pre;
			embed->pre->suc = embed->suc;
		}

		/* Loeschen aus Knoten-Einbettungsliste */
		if (((Sembed) embed->node_right->embedding) == embed)
		{	if (embed->nsuc == embed)
			{	((Sembed) embed->node_right->embedding) = empty_sembed;
			}
			else
			{	embed->nsuc->npre = embed->npre;
				embed->npre->nsuc = embed->nsuc;
				embed->node_right->embedding = (char *) embed->nsuc;
			}
		}
		else
		{	embed->nsuc->npre = embed->npre;
			embed->npre->nsuc = embed->nsuc;
		}

		myfree(embed);
	}




/*************************** Global Embeddings **************************/




Sglobalembed	make_sglobalembed ( gragra )
Sgragra	gragra;
	/*
	reserves memory for a global embedding. the new embedding is 
	inserted into the global_embeddings list from gragra.
	*/
	{	Sglobalembed new = (Sglobalembed) mymalloc(sizeof(struct sglobalembed));
		if (gragra->global_embeddings == empty_sglobalembed)
		{	new->pre = new;
			new->suc = new;
                        gragra->global_embeddings = new;
		}
		else
		{	new->pre = gragra->global_embeddings;
			new->suc = gragra->global_embeddings->suc;
			gragra->global_embeddings->suc->pre = new;
			gragra->global_embeddings->suc = new;
			gragra->global_embeddings = new;
		}
		
		new->gragra  = gragra;

		new->node_right = NULL;
		new->node_embed = NULL;

		new->oldedge = NULL;
		new->olddir  = S_out;
		
		new->newedge = NULL;
		new->newdir  = S_out;
		new->attrs   = make_attr(ATTR_DATA, NULL);
		return new;
	}


void	remove_sglobalembed ( gembed )
Sglobalembed gembed;
	/*
	detaches gembed from gragra->global_embeddings and free's memory.
	*/
	{       if (gembed->gragra->global_embeddings == gembed)
		{	if (gembed->suc == gembed)
			{	gembed->gragra->global_embeddings = empty_sglobalembed;
			}
			else
			{	gembed->suc->pre = gembed->pre;
				gembed->pre->suc = gembed->suc;
				gembed->gragra->global_embeddings = gembed->suc;
			}
		}
		else
		{	gembed->suc->pre = gembed->pre;
			gembed->pre->suc = gembed->suc;
		}
		myfree(gembed);
	}






