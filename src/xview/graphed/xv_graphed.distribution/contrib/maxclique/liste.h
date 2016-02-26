/* (C) Universitaet Passau 1986-1991 */
/************************************************************************
 * File    : liste.h							*
 * Aufgabe : Verwaltung der eigenen Zustandsmenge			*
 *									*
 * Autor   : Torsten Bachmann						*
 * Datum   : 18.11.89							*
 ************************************************************************/

typedef struct l_node_list
{       int	curlen;
	int	maxlen;
	Snode	*nodes;
}
	*L_node_list;


#define L_list_entry(list,nr) ((list->nodes)[nr])

#define L_for_node_list(list, nr)\
			if (list->curlen>0)\
			{       nr=-1; while(++nr < list->curlen) {
#define L_end_for_node_list(list, nr) }}


L_node_list	L_create_list ();
void		L_append ();
void		L_delete_entry ();
void		L_move ();
void		L_close_list ();
void		L_minimize_mem ();
