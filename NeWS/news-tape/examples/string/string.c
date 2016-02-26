/*  string.c - a simple NeWS client */

/*
 * Copyright (C) 1988 by Martha Zimet. All rights reserved.
 * This program is provided for unrestricted use, provided that this 
 * copyright message is preserved. There is no warranty, and no author 
 * or distributer accepts responsibility for any damage caused by this 
 * program. 
 */

#include <stdio.h>
#include "string.h"

main()
{    
    float fillgray = .75;
    char h_string[100];

    if (ps_open_PostScript() == 0 ) {
	printf(stderr,"Cannot connect to NeWS server");
	exit(1);
    }
    
    initialize();
    
    while ( !psio_error(PostScriptInput) ) {
	if      (get_gray(&fillgray)) 	call_paint_client();
	else if (get_paint_client()) 	paint_client(fillgray);
	else if (get_str(h_string))	printf("This is it: %s\n", h_string);
	else if (get_done())		{printf ("Done!\n");break;}
	else if (psio_eof(PostScriptInput))	break;
	else				{printf ("Strange Stuff!\n");break;}
    }
    
    ps_close_PostScript();
}

