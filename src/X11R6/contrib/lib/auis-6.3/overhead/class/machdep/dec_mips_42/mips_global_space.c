/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */

/* Filler to pad out global area so dynamic loader can allocate variables
 * there.
 */
/* MIPS_GLOBAL_SPACE_SIZE is set to zero on first link to tell how much */
 /* space is left in GP area. */
#if MIPS_GLOBAL_SPACE_SIZE != 0
static char mips_GlobalPool[MIPS_GLOBAL_SPACE_SIZE];
char *mips_GlobalArea = mips_GlobalPool;
int mips_GlobalSize = sizeof(mips_GlobalPool);
#else
char *mips_GlobalArea = 0;
int mips_GlobalSize = 0;
#endif
