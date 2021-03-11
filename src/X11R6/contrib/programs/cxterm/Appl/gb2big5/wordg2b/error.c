#include <stdio.h>

void    FatalError( cpMsg )
        char *  cpMsg ;
{
        fprintf( stderr, "FATAL: %s\n", cpMsg );
        exit( -1 );
} /* FatalError */

