#include <stdio.h>
#include <malloc.h>

#define SIZE 131072
#define REPS 1000

void
main( argc, argv )
    int argc;
    char* argv[];
    {
    unsigned long* mem;
    unsigned long* mem2;
    int i;
    register unsigned long* f;
    register unsigned long* t;
    register unsigned long* f2;
    char* usage = "usage:  %s memcpy|loop|Memcpy-reverse|Loop-reverse|non-overlapping-memcpy\n";

    if ( argc != 2 || ( argv[1][0] != 'm' &&
			argv[1][0] != 'l' &&
                        argv[1][0] != 'M' &&
                        argv[1][0] != 'L' &&
			argv[1][0] != 'n' ) )
	{
	(void) fprintf( stderr, usage, argv[0] );
	exit( 1 );
	}

    mem = (unsigned long*) malloc( SIZE + 4 );
    if ( mem == (unsigned long*) 0 )
	{
	(void) fprintf( stderr, "%s: malloc failed\n", argv[0] );
	exit( 1 );
	}
    if ( argv[1][0] == 'n' )
	{
	mem2 = (unsigned long*) malloc( SIZE );
	if ( mem2 == (unsigned long*) 0 )
	    {
	    (void) fprintf( stderr, "%s: malloc failed\n", argv[0] );
	    exit( 1 );
	    }
	}

    for ( i = 0; i < REPS; ++i )
	{
	switch ( argv[1][0] )
	    {
	    case 'm':
	    memcpy( (char*) mem, ( (char*) mem ) + 4, SIZE );
	    break;

	    case 'l':
	    t = mem;
	    f = t + 1;
	    f2 = f + SIZE / sizeof(unsigned long);
	    while ( f != f2 )
		{
		*t = *f;
		++f;
		++t;
		}
	    break;

	    case 'M':
	    memcpy( ( (char*) mem ) + 4, (char*) mem, SIZE );
	    break;

	    case 'L':
	    t = mem + SIZE / sizeof(unsigned long);
	    f = t - 1;
	    f2 = mem - 1;
	    while ( f != f2 )
		{
		*t = *f;
		--f;
		--t;
		}
	    break;

	    case 'n':
	    memcpy( (char*) mem2, (char*) mem, SIZE );
	    break;
	    }
	}

    exit( 0 );
    }
