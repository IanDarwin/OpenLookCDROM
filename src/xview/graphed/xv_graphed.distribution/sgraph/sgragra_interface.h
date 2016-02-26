/* (C) Universitaet Passau 1986-1991 */
/* Sgraph Source, 1988-1991 by Torsten Bachmann / Michael Himsolt */

#include <xview/rect.h>

#ifndef SGRAGRA_INTERFACE_H
#define SGRAGRA_INTERFACE_H

/************************** Sgragra-interface **************************/
#ifdef SGRAGRA_HEADER

typedef enum
{
	SGG_UNDEFINED,
	SGG_NO_PRODUCTION,
	SGG_CURRENT_PROD_WINDOW,
	SGG_ACTIVE_WINDOW,
	SGG_ALL_WINDOWS
}
	Sgragra_create_mode;



typedef	struct	sgragra_proc_info
{
	Sgragra_create_mode	create_mode;
	Sgragra			sgragra;
	Sprod			current_production;
	
}
	*Sgragra_proc_info;


extern	Sgragra_proc_info	init_sgragra_from_graphed_gragra();
extern	void			exit_sgragra_from_graphed_gragra();
	

#endif 
/****************** End ********* Sgragra-interface ******************/


#endif
