/*****************************************************************************/
/**       Copyright 1988 by Evans & Sutherland Computer Corporation,        **/
/**                          Salt Lake City, Utah                           **/
/**  Portions Copyright 1989 by the Massachusetts Institute of Technology   **/
/**                        Cambridge, Massachusetts                         **/
/**                                                                         **/
/**                           All Rights Reserved                           **/
/**                                                                         **/
/**    Permission to use, copy, modify, and distribute this software and    **/
/**    its documentation  for  any  purpose  and  without  fee is hereby    **/
/**    granted, provided that the above copyright notice appear  in  all    **/
/**    copies and that both  that  copyright  notice  and  this  permis-    **/
/**    sion  notice appear in supporting  documentation,  and  that  the    **/
/**    names of Evans & Sutherland and M.I.T. not be used in advertising    **/
/**    in publicity pertaining to distribution of the  software  without    **/
/**    specific, written prior permission.                                  **/
/**                                                                         **/
/**    EVANS & SUTHERLAND AND M.I.T. DISCLAIM ALL WARRANTIES WITH REGARD    **/
/**    TO THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES  OF  MERCHANT-    **/
/**    ABILITY  AND  FITNESS,  IN  NO  EVENT SHALL EVANS & SUTHERLAND OR    **/
/**    M.I.T. BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL  DAM-    **/
/**    AGES OR  ANY DAMAGES WHATSOEVER  RESULTING FROM LOSS OF USE, DATA    **/
/**    OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER    **/
/**    TORTIOUS ACTION, ARISING OUT OF OR IN  CONNECTION  WITH  THE  USE    **/
/**    OR PERFORMANCE OF THIS SOFTWARE.                                     **/
/*****************************************************************************/
/* 
 *  [ ctwm ]
 *
 *  Copyright 1992 Claude Lecommandeur.
 *            
 * Permission to use, copy, modify  and distribute this software  [ctwm] and
 * its documentation for any purpose is hereby granted without fee, provided
 * that the above  copyright notice appear  in all copies and that both that
 * copyright notice and this permission notice appear in supporting documen-
 * tation, and that the name of  Claude Lecommandeur not be used in adverti-
 * sing or  publicity  pertaining to  distribution of  the software  without
 * specific, written prior permission. Claude Lecommandeur make no represen-
 * tations  about the suitability  of this software  for any purpose.  It is
 * provided "as is" without express or implied warranty.
 *
 * Claude Lecommandeur DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL  IMPLIED WARRANTIES OF  MERCHANTABILITY AND FITNESS.  IN NO
 * EVENT SHALL  Claude Lecommandeur  BE LIABLE FOR ANY SPECIAL,  INDIRECT OR
 * CONSEQUENTIAL  DAMAGES OR ANY  DAMAGES WHATSOEVER  RESULTING FROM LOSS OF
 * USE, DATA  OR PROFITS,  WHETHER IN AN ACTION  OF CONTRACT,  NEGLIGENCE OR
 * OTHER  TORTIOUS ACTION,  ARISING OUT OF OR IN  CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE.
 *
 * Author:  Claude Lecommandeur [ lecom@sic.epfl.ch ][ April 1992 ]
 */


/**********************************************************************
 *
 * $XConsortium: list.c,v 1.20 91/01/09 17:13:30 rws Exp $
 *
 * TWM code to deal with the name lists for the NoTitle list and
 * the AutoRaise list
 *
 * 11-Apr-88 Tom LaStrange        Initial Version.
 *
 * Do the necessary modification to be integrated in ctwm.
 * Can no longer be used for the standard twm.
 *
 * 22-April-92 Claude Lecommandeur.
 *
 *
 **********************************************************************/

#include <stdio.h>
#ifdef VMS
#include <string.h>
#endif
#include "twm.h"
#include "screen.h"
#include "gram.h"

/***********************************************************************
 *
 *  Procedure:
 *	AddToList - add a window name to the appropriate list
 *
 *  Inputs:
 *	list	- the address of the pointer to the head of a list
 *	name	- a pointer to the name of the window 
 *	ptr	- pointer to list dependent data
 *
 *  Special Considerations
 *	If the list does not use the ptr value, a non-null value 
 *	should be placed in it.  LookInList returns this ptr value
 *	and procedures calling LookInList will check for a non-null 
 *	return value as an indication of success.
 *
 ***********************************************************************
 */

int match ();
int is_pattern ();

void
AddToList(list_head, name, ptr)
name_list **list_head;
char *name;
char *ptr;
{
    name_list *nptr;

    if (!list_head) return;	/* ignore empty inserts */

    nptr = (name_list *)malloc(sizeof(name_list));
    if (nptr == NULL)
    {
	twmrc_error_prefix();
	fprintf (stderr, "unable to allocate %d bytes for name_list\n",
		 sizeof(name_list));
	Done();
    }

    nptr->next = *list_head;
#ifdef VMS
    {
        char *ftemp;
        ftemp = (char *) malloc((strlen(name)+1)*sizeof(char));
        nptr->name = strcpy (ftemp,name);
    }
#else
    nptr->name = (char*) strdup (name);
#endif
    nptr->ptr = (ptr == NULL) ? (char *)TRUE : ptr;
    *list_head = nptr;
}    

/***********************************************************************
 *
 *  Procedure:
 *	LookInList - look through a list for a window name, or class
 *
 *  Returned Value:
 *	the ptr field of the list structure or NULL if the name 
 *	or class was not found in the list
 *
 *  Inputs:
 *	list	- a pointer to the head of a list
 *	name	- a pointer to the name to look for
 *	class	- a pointer to the class to look for
 *
 ***********************************************************************
 */

char *
LookInList(list_head, name, class)
name_list *list_head;
char *name;
XClassHint *class;
{
    name_list *nptr;

    /* look for the name first */
    for (nptr = list_head; nptr != NULL; nptr = nptr->next)
	if (match (nptr->name, name))
	    return (nptr->ptr);

    if (class)
    {
	/* look for the res_name next */
	for (nptr = list_head; nptr != NULL; nptr = nptr->next)
	    if (match (nptr->name, class->res_name))
		return (nptr->ptr);

	/* finally look for the res_class */
	for (nptr = list_head; nptr != NULL; nptr = nptr->next)
	    if (match (nptr->name, class->res_class))
		return (nptr->ptr);
    }
    return (NULL);
}

char *
LookInNameList(list_head, name)
name_list *list_head;
char *name;
{
    return (LookInList(list_head, name, NULL));
}

char *
LookPatternInList(list_head, name, class)
name_list *list_head;
char *name;
XClassHint *class;
{
    name_list *nptr;

    for (nptr = list_head; nptr != NULL; nptr = nptr->next)
	if (match (nptr->name, name))
	    return (nptr->name);

    if (class)
    {
	for (nptr = list_head; nptr != NULL; nptr = nptr->next)
	    if (match (nptr->name, class->res_name))
		return (nptr->name);

	for (nptr = list_head; nptr != NULL; nptr = nptr->next)
	    if (match (nptr->name, class->res_class))
		return (nptr->name);
    }
    return (NULL);
}

char *
LookPatternInNameList (list_head, name)
name_list *list_head;
char *name;
{
    return (LookPatternInList(list_head, name, NULL));
}

/***********************************************************************
 *
 *  Procedure:
 *	GetColorFromList - look through a list for a window name, or class
 *
 *  Returned Value:
 *	TRUE if the name was found
 *	FALSE if the name was not found
 *
 *  Inputs:
 *	list	- a pointer to the head of a list
 *	name	- a pointer to the name to look for
 *	class	- a pointer to the class to look for
 *
 *  Outputs:
 *	ptr	- fill in the list value if the name was found
 *
 ***********************************************************************
 */

int GetColorFromList(list_head, name, class, ptr)
name_list *list_head;
char *name;
XClassHint *class;
Pixel *ptr;
{
    int save;
    name_list *nptr;

    for (nptr = list_head; nptr != NULL; nptr = nptr->next)
	if (match (nptr->name, name))
	{
	    save = Scr->FirstTime;
	    Scr->FirstTime = TRUE;
	    GetColor(Scr->Monochrome, ptr, nptr->ptr);
	    Scr->FirstTime = save;
	    return (TRUE);
	}

    if (class)
    {
	for (nptr = list_head; nptr != NULL; nptr = nptr->next)
	    if (match (nptr->name, class->res_name))
	    {
		save = Scr->FirstTime;
		Scr->FirstTime = TRUE;
		GetColor(Scr->Monochrome, ptr, nptr->ptr);
		Scr->FirstTime = save;
		return (TRUE);
	    }

	for (nptr = list_head; nptr != NULL; nptr = nptr->next)
	    if (match (nptr->name, class->res_class))
	    {
		save = Scr->FirstTime;
		Scr->FirstTime = TRUE;
		GetColor(Scr->Monochrome, ptr, nptr->ptr);
		Scr->FirstTime = save;
		return (TRUE);
	    }
    }
    return (FALSE);
}

/***********************************************************************
 *
 *  Procedure:
 *	FreeList - free up a list
 *
 ***********************************************************************
 */

void FreeList(list)
name_list **list;
{
    name_list *nptr;
    name_list *tmp;

    for (nptr = *list; nptr != NULL; )
    {
	tmp = nptr->next;
	free((char *) nptr);
	nptr = tmp;
    }
    *list = NULL;
}

int regex_match_after_star ();

int is_pattern (p)
char *p;
{
    while ( *p ) {
	switch ( *p++ ) {
	    case '?':
	    case '*':
	    case '[':
		return TRUE;
	    case '\\':
		if ( !*p++ ) return FALSE;
	}
    }
    return FALSE;
}

#define ABORT 2

int regex_match (p, t)
char *p, *t;
{
    register char range_start, range_end;
    int invert;
    int member_match;
    int loop;

    for ( ; *p; p++, t++ ) {
	if (!*t)  return ( *p == '*' && *++p == '\0' ) ? TRUE : ABORT;
	switch ( *p ) {
	    case '?':
		break;
	    case '*':
		return regex_match_after_star (p, t);
	    case '[': {
		p++;
		invert = FALSE;
		if ( *p == '!' || *p == '^') {
		    invert = TRUE;
		    p++;
		}
		if ( *p == ']' ) return ABORT;
		member_match = FALSE;
		loop = TRUE;
		while ( loop ) {
		    if (*p == ']') {
			loop = FALSE;
			continue;
		    }
		    if (*p == '\\') range_start = range_end = *++p;
		    else range_start = range_end = *p;
		    if (!range_start) return ABORT;
		    if (*++p == '-') {
			range_end = *++p;
			if (range_end == '\0' || range_end == ']') return ABORT;
			if (range_end == '\\') range_end = *++p;
			p++;
		    }
		    if ( range_start < range_end  ) {
			if (*t >= range_start && *t <= range_end) {
			    member_match = TRUE;
			    loop = FALSE;
			}
		    }
		    else {
			if (*t >= range_end && *t <= range_start) {
			    member_match = TRUE;
			    loop = FALSE;
			}
		    }
		}
		if ((invert && member_match) || !(invert || member_match)) return (FALSE);
		if (member_match) {
		    while (*p != ']') {
			if (!*p) return (ABORT);
			if (*p == '\\')  p++;
			p++;
		    }
		}
		break;
	    }
	    case '\\':
		p++;

	    default:
		if (*p != *t) return (FALSE);
	}
    }
    return (!*t);
}

int regex_match_after_star (p, t)
char *p, *t;
{
    register int match;
    register nextp;

    while ((*p == '?') || (*p == '*')) {
	if (*p == '?') {
	    if ( !*t++ ) return ABORT;
	}
	p++;
    }
    if ( !*p ) return TRUE;

    nextp = *p;
    if (nextp == '\\') nextp = p[1];

    match = FALSE;
    while (match == FALSE) {
	if ( nextp == *t || nextp == '[' ) match = regex_match(p, t);
	if ( !*t++ ) match = ABORT;
    }
    return (match);
}

int match (p, t)
char *p, *t;
{
    if ((p == NULL) || (t == NULL)) return (FALSE);
    return ((regex_match (p,t) == TRUE) ? TRUE : FALSE);
}
