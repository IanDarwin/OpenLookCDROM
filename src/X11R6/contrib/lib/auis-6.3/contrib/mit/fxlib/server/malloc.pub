#ifndef _malloc_
#define _malloc_

#define New(TYPE) ((TYPE *)malloc(sizeof(TYPE)))
malloc_init ( /* start */ );
int m_blocksize( /* a_block */ );
char * malloc ( /* n */ );		/* get a block */
free ( /* mem */ );
char * realloc ( /* mem, n */ );
struct mstats_value
  {
    int blocksize;
    int nfree;
    int nused;
  };
struct mstats_value malloc_stats ( /* size */ );
get_lim_data ( /*  */ );
get_lim_data ( /*  */ );
get_lim_data ( /*  */ );
#endif _malloc_
