/* (C) Universitaet Passau 1986-1991 */
/* GraphEd Source, 1986-1991 by Michael Himsolt	*/
/************************************************************************/
/*									*/
/*			GraphEd User Commands				*/
/*									*/
/************************************************************************/


#include "misc.h"
#include "graph.h"
#include "graphed_subwindows.h"
#include "user_header.h"


#define INCLUDE_SGRAPH
#ifdef INCLUDE_SGRAPH
#include "sgraph/std.h"
#include "sgraph/sgraph.h"
#include "sgraph/slist.h"
#include "sgraph/graphed.h"
#include "graphed_sgraph_interface.h"


static	char		*dummy_sgraph_proc (info)
Sgraph_proc_info	info;
{
	return NULL;
}


static	char	*dummy (menu, menu_item)
char		*menu, *menu_item;
{
	return call_sgraph_proc (dummy_sgraph_proc);
}


void	init_user_menu ()
{
	add_to_user_menu ("User-defined extensions may be included here", dummy);
}


main(argc, argv)
int	argc;
char	**argv;
{
	graphed_main (argc, argv);
}


#endif

