#include <X11/Wc/COPY>

/*
* SCCS_data: @(#) MapAg.c 1.6 92/10/21 06:59:22
*
* Mapping Agent - MapAg.c
*
* This module implements an agent for associating arbitrary data.  In all
* cases, the data consists of char* (pointers to anything).
*
* An agent has 2 states: Initial and Normal.  The states provide the following
* behaviors:
*
*	Initial:
*		MapAg_Define_Initial,
*		MapAg_Find_Initial, MapAg_FindMap_Initial,
*		MapAg_Forget_Initial, MapAg_Resize_Initial
*	Normal:
*		MapAg_Define_Normal,
*		MapAg_Find_Normal, MapAg_FindMap_Normal,
*		MapAg_Forget_Normal, MapAg_Resize_Normal
*
* The hashing algorithm is tailored  based on the initial a,b,c passed in to 
* Map_Ag_Define_Initial.  The hashing algorithm may change at resize time
* if too many collisions are occuring.
*
* Also, if the agent is dynamically allocated (using MapAg_Create) then
* the free method is MapAg_Free_Dynamic.  If the agent is statically
* allocated, then the free method is MapAg_Free_Static.  The free methods
* are never changed.
*
* State Transitions:
*
* Creating a new agent results in an agent in the Initial state.
*
* Defining a new mapping to an Initial agent causes the transition to Normal
* if sucessful (i.e., all the allocations succeed).  If any malloc fails, then
* XtError is called.
* 
* From the Normal state, Forgeting the last mapping causes the agent to
* revert to the initial state.
*
*******************************************************************************
*/

#include <stdio.h>
#include <X11/Intrinsic.h>	/* only need XtMalloc etc */
#include <X11/Wc/MapAg.h>


/* -- Hash Method - Find Bucket
================================================
    The algorithm REQUIRE that hashSize be a power of 2!
    Mask is all ones: (some power of 2) - 1
    Shifts are determined based on initial a,b,c passed to MapAg_Define_Initial
*/
#define MapAg_HashBucket(ag,a,b,c) \
  ( ( ( (int)(a) >> (ag)->shiftA ) +  \
      ( (int)(b) >> (ag)->shiftB ) +  \
      ( (int)(c) >> (ag)->shiftC ) ) & (ag)->mask )

void MapAg_FirstGuessAtHashShifts( ag, a, b, c )
    MapAg	ag;
    char	*a, *b, *c;
{
    if ( a == (char*)0 )
    {
	ag->shiftA = 0;
    }
    else
    {
	/* Find the number of least significant 0 bits in a
	*/
	int i = (int)a;
	for ( ag->shiftA = 1 ; ag->shiftA < 8 * (sizeof(char*)) ; ag->shiftA++ )
	{
	    /* shift right some bits, then left again, see if we lose any
	    */
	    if ( i != (( i >> ag->shiftA ) << ag->shiftA ) )
	    {
		ag->shiftA--;	/* cannot mask so many bits */
		break;		/* out of for look */
	    }
	}
    }

    if ( b == (char*)0 )
    {
	ag->shiftB = 0;
    }
    else
    {
	int i = (int)b;
	for ( ag->shiftB = 1 ; ag->shiftB < 8 * (sizeof(char*)) ; ag->shiftB++ )
	{
	    if ( i != (( i >> ag->shiftB ) << ag->shiftB ) )
	    {
		ag->shiftB--;
		break;
	    }
	}
    }

    if ( c == (char*)0 )
    {
	ag->shiftC = 0;
    }
    else
    {
	int i = (int)c;
	for ( ag->shiftC = 1 ; ag->shiftC < 8 * (sizeof(char*)) ; ag->shiftC++ )
	{
	    if ( i != (( i >> ag->shiftC ) << ag->shiftC ) )
	    {
		ag->shiftC-- ;
		break;
	    }
	}
    }
}


/* -- MapAg Sanity Checks
==========================
    Each check is an individual if statement so its easier to see the problem
*/

int MapAg_InInitialState( agent )
    MapAg	agent;
{
    if ( agent->table   != (Map*)0		)	return 0;
    if ( agent->mask    != 0			)	return 0;
    if ( agent->numMaps != 0			)	return 0;
    if ( agent->Define  != MapAg_Define_Initial	)	return 0;
    if ( agent->Find    != MapAg_Find_Initial   )	return 0;
    if ( agent->FindMap != MapAg_FindMap_Initial)	return 0;
    if ( agent->Forget  != MapAg_Forget_Initial	)	return 0;
    if ( agent->Resize  != MapAg_Resize_Initial	)	return 0;
    if ( agent->Free    != MapAg_Free_Static 
      && agent->Free    != MapAg_Free_Dynamic	) 	return 0;
    if ( agent->shiftA  != 0			)	return 0;
    if ( agent->shiftB  != 0			)	return 0;
    if ( agent->shiftC  != 0			)	return 0;
    return 1;
}

int MapAg_InNormalState( agent )
    MapAg	agent;
{
    if ( agent->table   == (Map*)0		)	return 0;
    if ( 0 != (agent->mask & (agent->mask + 1)) )	return 0;
    if ( agent->numMaps <=  0			)	return 0;
    if ( agent->Define  != MapAg_Define_Normal 	)	return 0;
    if ( agent->Find    != MapAg_Find_Normal	)	return 0;
    if ( agent->FindMap != MapAg_FindMap_Normal	)	return 0;
    if ( agent->Forget  != MapAg_Forget_Normal	)	return 0;
    if ( agent->Resize  != MapAg_Resize_Normal	)	return 0;
    if ( agent->Free    != MapAg_Free_Static 
      && agent->Free    != MapAg_Free_Dynamic	)	return 0;
    return 1;
}

void MapAg_AssertLooksOk( agent, file, line )
    MapAg	agent;
    char*	file;
    int		line;
{
    if (   !MapAg_InNormalState(  agent )
	&& !MapAg_InInitialState( agent ) )
    {
	fprintf(stderr, "Assertion failed: file \"%s\", line %d\n", file, line);
	abort();
	exit(1);
    }
}

/* -- Transition to Initial State
=================================
*/
void MapAg_SetInitialBehaviors( agent )
    MapAg agent;
{
    agent->Define  = MapAg_Define_Initial;
    agent->Find    = MapAg_Find_Initial;
    agent->FindMap = MapAg_FindMap_Initial;
    agent->Forget  = MapAg_Forget_Initial;
    agent->Resize  = MapAg_Resize_Initial;
    agent->shiftA = agent->shiftB = agent->shiftC = 0;
}

/* -- Transition to Normal State
================================
   Look at initial a,b,c and determine a good hashing function.
*/
void MapAg_SetNormalBehaviors( agent, a, b, c )
    MapAg agent;
    char  *a, *b, *c;
{
    MapAg_FirstGuessAtHashShifts( agent, a, b, c );

    agent->Define  = MapAg_Define_Normal;
    agent->Find    = MapAg_Find_Normal;
    agent->FindMap = MapAg_FindMap_Normal;
    agent->Forget  = MapAg_Forget_Normal;
    agent->Resize  = MapAg_Resize_Normal;
}

/* -- MapAg "Find" Methods
==========================
*/

/*ARGSUSED*/
char* MapAg_Find_Initial( agent, a, b, c )
    MapAg	agent;
    char	*a, *b, *c;
{
    return (char*)0;
}

char* MapAg_Find_Normal( agent, a, b, c )
    MapAg	agent;
    char	*a, *b, *c;
{
    Map map = agent->table[ MapAg_HashBucket( agent, a, b, c ) ];

    for ( ; (Map)0 != map ; map = map->next )
	if ( (map->a == a) && (map->b == b) && (map->c == c) )
	    return map->data;

    return (char*)0;
}

/* -- MapAg "FindMap" Methods
=============================
*/

/*ARGSUSED*/
Map MapAg_FindMap_Initial( agent, a, b, c )
    MapAg	agent;
    char	*a, *b, *c;
{
    return (Map)0;
}

Map MapAg_FindMap_Normal( agent, a, b, c )
    MapAg	agent;
    char	*a, *b, *c;
{
    Map map = agent->table[ MapAg_HashBucket( agent, a, b, c ) ];

    for ( ; (Map)0 != map ; map = map->next )
	if ( (map->a == a) && (map->b == b) && (map->c == c) )
	    return map;

    return (Map)0;
}

/*  -- Make new definition of a,b,c to d
========================================================
    Might need to grow hash table.  
    New map is head of chain if hash collision.
*/

void MapAg_Define_Initial( agent, a, b, c, data )
    MapAg agent;
    char  *a, *b, *c, *data;
{
    Map newMap = (Map)XtMalloc( sizeof(MapRec) );

    /* Do initial allocation of table
    */
    agent->Resize(agent);

    newMap->a = a; newMap->b = b; newMap->c = c; newMap->data = data;
    newMap->next = (Map)0;

    /* Change behaviors to normal behaviors, determine hashing function.
    */
    MapAg_SetNormalBehaviors( agent, a, b, c );

    agent->table[ MapAg_HashBucket( agent, a, b, c ) ] = newMap;

    agent->numMaps = 1;

}

void MapAg_Define_Normal( agent, a, b, c, data )
    MapAg agent;
    char  *a, *b, *c, *data;
{
    Map map;
    int bkt;

    bkt = MapAg_HashBucket( agent, a, b, c );

    for ( map = agent->table[ bkt ] ; (Map)0 != map ; map = map->next )
    {
	if ( (map->a == a) && (map->b == b) && (map->c == c) )
	{
	    map->data = data;
	    return;
	}
    }

    /* No map at this bucket, or no map at this bucket with same a,b,c
    ** Make a new map, put it at head of chain from its bucket.
    */

    map = (Map)XtMalloc( sizeof(MapRec) );

    /* Possibilities for refinement here - perhaps only grow when
    ** "too many" collisions, perhaps first try a different hash
    ** and simply re-distribute before re-size, ...
    ** If the resize suceeds, we need to re-compute the hash bucket.
    */
    if ( agent->mask < ++(agent->numMaps) )
    {
	agent->Resize(agent);
	bkt = MapAg_HashBucket( agent, a, b, c );
    }

    map->a = a; map->b = b; map->c = c; map->data = data;
    map->next = agent->table[bkt];
    agent->table[bkt] = map;
}

/*  -- MapAg Resize Methods
===============================================================================
    When nothing is in the MapAg, MapAg_Resize_Initial is the Resize method
    of the MapAg.  This initial method allocates the hash table.  The table
    size must ALWAYS be a power of two, as we use a mask in the hashing
    function which must be all ones.

    MapAg_Resize_Normal doubles the size of the hash table and re-distributes
    the entries.
*/

#define MAPDB_InitTableSize 1024

void MapAg_Resize_Initial(agent)
    MapAg agent;
{
    agent->table	= (Map*)XtCalloc( MAPDB_InitTableSize, sizeof(Map) );

    agent->mask		= MAPDB_InitTableSize-1;
}

/* We should check the number of collisions, and change the hashing function
 * if too many collisions.  Remember, we made initial "guess" of a good hashing
 * function based only on the initial a,b,c passed to MapAg_Define_Initial().
 */
void MapAg_Resize_Normal(agent)
    MapAg	agent;
{
    int		oldBkt, newBkt;
    int		oldSize	 = agent->mask + 1;
    int		newSize  = oldSize + oldSize;
    Map*	newTable = (Map*)XtCalloc(newSize, sizeof(Map));
    Map*	oldTable = agent->table;

    agent->table = newTable;
    agent->mask  = newSize-1;

    for ( oldBkt = 0 ; oldBkt < oldSize ; oldBkt++ ) 
    {
	/* Each non-empty bucket points to a Map, each Map is a link
	** in a chain.  Need to go down chain and re-hash each Map.
	*/
	Map map, next;
	for ( map = oldTable[oldBkt] ; (Map)0 != map ; map = next )
	{
	    /* 'map' gets pointed to by bucket in new hash table, what
	    ** was in that bucket becomes the next ptr of 'map'.
	    */
	    newBkt = MapAg_HashBucket( agent, map->a, map->b, map->c );
	    next = map->next;
	    map->next = agent->table[newBkt];
	    agent->table[newBkt] = map;
	}
    }
    XtFree( (char*)oldTable );
}

/*  -- Drop Map from MapAg
==========================
*/

/*ARGSUSED*/
void MapAg_Forget_Initial( agent, a, b, c )
    MapAg agent;
    char  *a, *b, *c;
{
    return;
}

void MapAg_Forget_Normal( agent, a, b, c )
    MapAg agent;
    char  *a, *b, *c;
{
    int bkt  = MapAg_HashBucket( agent, a, b, c );
    Map map  = agent->table[bkt];
    Map prev = (Map)0;

    for ( ; map ; prev = map, map = map->next )
    {
	if ( (map->a == a) && (map->b == b) && (map->c == c) )
	{
	    if ((Map)0 == prev)
		agent->table[bkt] = map->next;
	    else
		prev->next = map->next;
	    XtFree( (char*)map );
	    if ( --(agent->numMaps) == 0 )
	    {
		XtFree( (char*)agent->table );
		agent->table = (Map*)0;
		agent->mask  = 0;
		MapAg_SetInitialBehaviors( agent );
	    }
	}
    }
}

/*  -- Private: Free all maps, free the hash table
--------------------------------------------------
*/

static void MapAg_FreeTable( agent )
    MapAg agent;
{
    int bkt;

    for ( bkt = 0 ; bkt <= agent->mask ; bkt++ )
    {
	Map map, next;
	for ( map = agent->table[bkt] ; map ; map = next )
	{
	    next = map->next;
	    XtFree( (char*)map );
	}
    }
    XtFree( (char*)agent->table );
}

/*  -- Free Mapping Agent Methods
=================================
    Statically created agents can only free the table,
    dynamically created agents also free the agent struct.
*/

void MapAg_Free_Static( agent )
    MapAg agent;
{
    if (agent->table != (Map*)0)
	MapAg_FreeTable( agent );
}

void MapAg_Free_Dynamic( agent )
    MapAg agent;
{
    if (agent->table != (Map*)0)
	MapAg_FreeTable( agent );
    XtFree( (char*)agent );
}


/*  -- Mapping Agent Public Functions
*******************************************************************************
*/

/*  -- Create a new Mapping Agent
=======================================================================
    This function must be coordinated with the MapAg_STATIC macro so they
    both do the same type of initialization.
*/
MapAg MapAg_New()
{
    MapAg agent = (MapAg)XtCalloc( sizeof(MapAgRec), 1 );

    agent->Free = MapAg_Free_Dynamic;

    MapAg_SetInitialBehaviors( agent );

    MapAg_AssertLooksOk( agent, __FILE__, __LINE__ );
    return agent;
}
