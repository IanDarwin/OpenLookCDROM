/* (C) Universitaet Passau 1986-1991 */

/* test.c  Beispielprogramm zum Test von connect1 */




#include <std.h>
#include <slist.h>
#include <sgraph.h> 
#include <graphed.h> 
#include <algorithms.h>
 

void	call_check_connectivity (sgraph_info) 
Sgraph_proc_info sgraph_info; 
{ 
	if  (test_sgraph_connected(sgraph_info->sgraph))
		message("The graph is connected.\n");
	else
		message("The graph is not connected.\n");
} 

char*	call_menu_check_connectivity (menu, menu_item)
char	*menu, menu_item; 
{ 
	return call_sgraph_proc (call_check_connectivity); 
} 



 
 
void	call_check_strong_connectivity (sgraph_info) 
Sgraph_proc_info sgraph_info; 
{ 
	if  (test_sgraph_strongly_connected(sgraph_info->sgraph))
		message("The graph is strongly connected.\n");
	else
		message("The graph is not strongly connected.\n");
} 

char*	call_menu_check_strong_connectivity (menu, menu_item) 
char	*menu, menu_item; 
{ 
	return call_sgraph_proc (call_check_strong_connectivity); 
} 
 


void	call_check_biconnectivity (sgraph_info) 
Sgraph_proc_info sgraph_info; 
{ 
	if  (test_sgraph_biconnected(sgraph_info->sgraph))
		message("The graph is biconnected.\n");
	else
		message("The graph is not biconnected.\n");
} 

char*	call_menu_check_biconnectivity (menu, menu_item) 
char	*menu, menu_item; 
{ 
	return call_sgraph_proc (call_check_biconnectivity); 
} 
 
 

void	call_check_strong_biconnectivity (sgraph_info) 
Sgraph_proc_info sgraph_info; 
{ 
	if  (test_sgraph_strongly_biconnected(sgraph_info->sgraph))
		message("The graph is strongly biconnected.\n");
	else
		message("The graph is not strongly biconnected.\n");
} 

char*	call_menu_check_strong_biconnectivity (menu, menu_item) 
char	*menu, menu_item; 
{ 
	return call_sgraph_proc (call_check_strong_biconnectivity); 
} 
