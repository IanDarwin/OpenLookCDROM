/*  -------------------------------------------------------------------

This program is the property of:

                             Cornell University 
                        Center for Applied Mathematics 
                              Ithaca, NY 14853

and may be used, modified and distributed freely, subject to the 
following restrictions:

       Any product which incorporates source code from the dstool
       program or utilities, in whole or in part, is distributed
       with a copy of that source code, including this notice. You
       must give the recipients all the rights that you have with
       respect to the use of this software. Modifications of the
       software must carry prominent notices stating who changed
       the files and the date of any change.

DsTool is distributed in the hope that it will be useful, but 
WITHOUT ANY WARRANTY; without even the implied warranty of FITNESS 
FOR A PARTICULAR PURPOSE.  The software is provided as is without 
any obligation on the part of Cornell faculty, staff or students to 
assist in its use, correction, modification or enhancement.

  -----------------------------------------------------------------  */
#include <stdio.h>
#include <sys/param.h>
#include <sys/types.h>
#include <xview/xview.h>
#include <xview/panel.h>
#include <xview/textsw.h>
#include <xview/xv_xrect.h>
#include <memory.h>
#include <constants.h>
#include "autoif_cui.h"
#include "autoif.h"

void	coartype_notify();
void	coarmode_notify();
void	coarsh_notify();
void	coarftype_notify();
void	coaraatm_notify();

autoif_coautord_objects	*Autoif_coautord;
extern struct Autoif_Ds                autoif_ds;

/*
 * Notify callback function for `coarinfo'.
 */
void
coarinfo_notify(item, event)
	Panel_item	item;
	Event		*event;
{
	int	status = NO_ERROR, count=0;
	autoif_coautord_objects *ip = (autoif_coautord_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);

	open_auto_file();

        while( status != AUTO_EOF )
	   {
	    status = auto_rdblock();
	    if( status != AUTO_EOF ) ++count;
           }
        autoif_ds.Total_Blocks = count;
	close_auto_file();

	if( count>0 )
	   {
	    open_auto_file();
	    status = auto_rdblock();
            autoif_ds.Cur_Block = 1;
	    close_auto_file();
           }
        else
	   {
            autoif_ds.Cur_Block = 0;
	    autoif_init();
	   }

	autoif_data_refresh();
	autoif_dump_blk();
	
}

/*
 * Notify callback function for `coaradd'.
 */
void
coaradd_notify(item, event)
	Panel_item	item;
	Event		*event;
{
	autoif_coautord_objects *ip = (autoif_coautord_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);

	autoif_init_mem();
	autoif_add_pt();
	autoif_data_refresh();
}

/*
 * Notify callback function for `coargo'.
 */
void
coargo_notify(item, event)
	Panel_item	item;
	Event		*event;
{
	int	block_number, status=NULL;

	autoif_coautord_objects *ip = (autoif_coautord_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);

	autoif_rd_curblk();
	autoif_data_refresh();
	autoif_init_mem();

	if( autoif_ds.Cur_Block == autoif_ds.Block_Index )	/* set the starting block index number */
	    if( autoif_ds.Block_Index == 0 )
	       block_number = 1;
	    else block_number = autoif_ds.Block_Index + 1;
        else block_number = autoif_ds.Cur_Block;

	if( autoif_ds.Read_Mode == READ_ALL )
          while( status == NULL )
	    {
	     status = auto_rdblock_id( block_number );
	     if( autoif_ds.Aatm == TRUE ) autoif_add_pt();
	     ++block_number;
            }
        else
	  {
	   status = auto_rdblock_id( block_number );
	   if( autoif_ds.Aatm == TRUE ) autoif_add_pt();
          }

	autoif_ds.Cur_Block = autoif_ds.Block_Index = block_number;

	autoif_data_refresh();

	if( autoif_ds.Save_Type == TRAJ | autoif_ds.Save_Type == TRAJ_and_CONT )
           mem_all_win(autoif_ds.Traj_Mem);
	if( autoif_ds.Save_Type == CONT | autoif_ds.Save_Type == TRAJ_and_CONT )
           mem_all_win(autoif_ds.Cont_Mem);

	autoif_dump_blk();

}

/*
 * Notify callback function for `coartype'.
 */
void
autoif_coautord_coartype_notify_callback(item, value, event)
	Panel_item	item;
	unsigned int	value;
	Event		*event;
{
	autoif_coautord_objects *ip = (autoif_coautord_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);

	autoif_data_refresh();
}


/*
 * Notify callback function for `coarmode'.
 */
void
autoif_coautord_coarmode_notify_callback(item, value, event)
	Panel_item	item;
	int		value;
	Event		*event;
{
	autoif_coautord_objects *ip = (autoif_coautord_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	
	autoif_data_refresh();
	
}


/*
 * Notify callback function for `coarcurblk'.
 */
Panel_setting
coarrw(item, event)
	Panel_item	item;
	Event		*event;
{
	autoif_coautord_objects *ip = (autoif_coautord_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	char *	value = (char *) xv_get(item, PANEL_VALUE);
	
	autoif_data_refresh();
	return panel_text_notify(item, event);
}

/*
 * Notify callback function for `coarsh'.
 */
void
autoif_coautord_coarsh_notify_callback(item, value, event)
	Panel_item	item;
	int		value;
	Event		*event;
{
	autoif_coautord_objects *ip = (autoif_coautord_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	
	if (value == 0)
           autoif_ds.Show_Flag = FALSE;
        else
	   autoif_ds.Show_Flag = TRUE;
	
	autoif_data_refresh();
}


/*
 * Notify callback function for `coarftype'.
 */
void
autoif_coautord_coarftype_notify_callback(item, value, event)
	Panel_item	item;
	int		value;
	Event		*event;
{
	autoif_coautord_objects *ip = (autoif_coautord_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	
	autoif_data_refresh();
}

/*
 * Notify callback function for `coaraatm'.
 */
void
autoif_coautord_coaraatm_notify_callback(item, value, event)
	Panel_item	item;
	int		value;
	Event		*event;
{
	autoif_coautord_objects *ip = (autoif_coautord_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	
	autoif_data_refresh();

}

/* callback for FRAME_DONE */
void
    autoif_done_proc(frame)
Frame
    frame;
{
    autoif_close();
}	
