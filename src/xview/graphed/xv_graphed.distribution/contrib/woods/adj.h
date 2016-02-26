/* (C) Universitaet Passau 1986-1991 */

#include "../PQplanar/adj.h"

typedef		adj_rec	*adj_ptr;

#define ADJ_LIST(node)		((adj_ptr)((node)->attrs.data))->adj_list

#define ADJ_DIRECTION(node)	((adj_ptr)((node)->attrs.data))->direction

#define ADJ_DIRECT_NODE(node)	((adj_ptr)((node)->attrs.data))->direct_node
