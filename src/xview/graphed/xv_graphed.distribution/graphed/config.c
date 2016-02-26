/* (C) Universitaet Passau 1986-1991 */
/* GraphEd Source, 1986-1991 by Michael Himsolt	*/
#include "misc.h"

char	graphed_default_initialisation_file [FILENAMESIZE];
char	graphed_default_types_directory     [FILENAMESIZE];
char	graphed_default_fonts_directory     [FILENAMESIZE];
char	graphed_library_directory           [FILENAMESIZE];

/* from postscri.c */

char	graphed_postscript_header[] = "graphed.header.ps" ;
char	graphed_postscript_tail[]   = "graphed.tail.ps";


void	init_config ()
{
	strcpy (graphed_library_directory, LIBDIR);
	strcat (strcat (strcpy (graphed_default_types_directory,
	                        graphed_library_directory), "/"), "types");
	strcat (strcat (strcpy (graphed_default_fonts_directory,
	                        graphed_library_directory), "/"), "fonts");
	strcat (strcat (strcpy (graphed_default_initialisation_file,
	                        graphed_library_directory), "/"), ".graphed");
}
