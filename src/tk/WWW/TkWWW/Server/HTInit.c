/*		Configuration-specific Initialialization	HTInit.c
**		----------------------------------------
*/

/*	Define a basic set of suffixes and presentations
**	------------------------------------------------
*/

/* Implements:
*/

#include "HTInit.h"

PUBLIC void HTFormatInit NOARGS
{
}



/*	Define a basic set of suffixes
**	------------------------------
**
**	The LAST suffix for a type is that used for temporary files
**	of that type.
**	The quality is an apriori bias as to whether the file should be
**	used.  Not that different suffixes can be used to represent files
**	which are of the same format but are originals or regenerated,
**	with different values.
*/

#ifndef NO_INIT
PUBLIC void HTFileInit NOARGS
{
}
#endif /* NO_INIT */

