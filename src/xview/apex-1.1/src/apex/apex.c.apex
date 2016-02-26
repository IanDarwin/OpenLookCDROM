#ifndef lint
static char *RCSid = "$Header: /n/homeserver/i/gounares/pu/apex/src/apex/RCS/apex.c,v 1.1 93/01/06 03:27:11 gounares Exp Locker: gounares $";
#endif

/*
 * $Log:	apex.c,v $
 * Revision 1.1  93/01/06  03:27:11  gounares
 * Initial revision
 * 
 */

/*
 * apeX: Advanced Programming Environment for X-Window Systems
 * 
 * written by:
 * 
 * Alexander Gounares 9/18/92
 */
/*
 * Copyright 1993 Alexander Gounares
 * 
 * This source is covered by the 
<!@ 3 COPYING 3 #Link to the GNU General Public License
#when run this description will be passed to the shell
cat COPYING
 @!>
GNU Public License Version 
<!@ 3 COPYING 3 #Link to the GNU General Public License
#when run this description will be passed to the shell
cat COPYING
 @!>
2
 * 
 * see the apeX manual for more details
 */

#include <xview/xview.h>
#include <xview/frame.h>
#include <xview/textsw.h>
#include <xview/panel.h>
#include "editor.h"

main( argc, argv)
	int argc;
	char **argv;
{
	Editor         *editor;

	xv_init(XV_INIT_ARGC_PTR_ARGV, &argc, argv, NULL);

	if( argc > 1 )
		while( --argc ) 
			editor = new_editor( argv[argc], 0, EDITOR_MAIN);
	else
			editor = new_editor( NULL, 0, EDITOR_MAIN );

	xv_main_loop(editor->frame);
	return 0;
}
