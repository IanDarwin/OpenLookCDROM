#ifndef lint
static char *RCSid = "$Header: /n/homeserver/i/gounares/pu/apex/src/apex/RCS/s_pad.c,v 1.1 93/01/06 03:27:49 gounares Exp Locker: gounares $";
#endif

/*
 * $Log:	s_pad.c,v $
 * Revision 1.1  93/01/06  03:27:49  gounares
 * Initial revision
 * 
 */

/*
 * s_pad.c
 * 
 * the scratch pad library
 * 
 * written by alexander gounares
 */
/*
 * Copyright 1993 Alexander Gounares
 * 
 * This source is covered by the 
<!@ 1 COPYING 1 #Link to the GNU General Public License
#when run this description will be passed to the shell
cat COPYING

 @!>
GNU General Public License Version 
<!@ 1 COPYING 1 #Link to the GNU General Public License
#when run this description will be passed to the shell
cat COPYING

 @!>
2
 * 
 * see the apeX manual for more details
 */

#include <xview/xview.h>
#include <xview/frame.h>
#include <xview/panel.h>
#include <xview/textsw.h>
#include "editor.h"
#include "s_pad.h"
#include "apex_options.h"

extern int      CLIENT_DATA_KEY;

/*
 * sp_done_proc -- called when the sratch pad is dismissed
 */
void 
sp_done_proc(frame)
	Frame           frame;
{
	SP             *psp = (SP *) xv_get(frame, XV_KEY_DATA, CLIENT_DATA_KEY);

	if (psp->ped) {
		xv_set(psp->frame, XV_SHOW, FALSE, NULL);
	} else {
		xv_destroy_safe(psp->frame);
		free(psp);
	}
}

/*
 * create_sp -- create a sratch pad structure/window
 */
SP             *
create_sp(owner, ped)
	Frame           owner;
	Editor         *ped;	/* we'll use just frames to make this more
				 * generic */
{
	SP             *psp;
	Panel           panel;

	psp = (SP *) acalloc(1, sizeof(SP));

	psp->frame = (Frame) xv_create(owner, FRAME_CMD,
				       FRAME_LABEL, "apeX Scratch Pad",
				       FRAME_SHOW_RESIZE_CORNER, TRUE,
				       FRAME_DONE_PROC, sp_done_proc,
				       XV_KEY_DATA, CLIENT_DATA_KEY, psp,
						XV_WIDTH, 100, XV_HEIGHT, 100,
				       NULL);

	panel = (Panel) xv_get(psp->frame, FRAME_CMD_PANEL);
	xv_destroy_safe(panel);

	psp->textsw = (Textsw) xv_create(psp->frame, TEXTSW,
					XV_X, 0, XV_Y, 0,
					 TEXTSW_TAB_WIDTH, get_tabsize(),
					 WIN_COLUMNS, 80,
					 WIN_ROWS, 10,
					 NULL);

	window_fit(psp->frame);

	psp->ped = (void *)ped;

	return psp;
}

/*
 * show_sp_proc -- display a sratch pad -- assumes that an Editor structure
 * has been stored as the client data -- this function is called from the
 * toolbar
 */
void 
show_sp_proc(item, event)
	Panel_item      item;
	Event          *event;
{
	Editor         *ped = (Editor *) xv_get(item, XV_KEY_DATA, CLIENT_DATA_KEY);
	SP *psp;

	psp = create_sp(ped->frame, NULL);

	xv_set(psp->frame, XV_SHOW, TRUE, NULL);

}

/*
 * clear_sp -- reset the data in an sp
 */
void 
clear_sp(psp)
	SP             *psp;
{
	textsw_reset(psp->textsw, 0, 0);
}

/*
 * set_sp -- set the data in an sp
 */
void 
set_sp(psp, szBuf)
	SP             *psp;
	char           *szBuf;
{
	/* maybe we want to check to see if user put text in here */

	clear_sp(psp);

	xv_set(psp->textsw, TEXTSW_CONTENTS, szBuf, NULL);
}

/*
 * set_sp -- set sp from a ped
 */
void 
set_sp_ped(szBuf, ped)
	char           *szBuf;
	Editor         *ped;
{
	if (!ped->psp)
		ped->psp = create_sp(ped->frame, ped);

	xv_set(ped->psp->frame, XV_SHOW, TRUE, NULL);
	set_sp(ped->psp, szBuf);
}
