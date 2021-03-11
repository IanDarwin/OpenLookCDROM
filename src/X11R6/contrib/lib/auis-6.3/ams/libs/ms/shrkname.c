/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */

/*
	$Disclaimer: 
 * Permission to use, copy, modify, and distribute this software and its 
 * documentation for any purpose is hereby granted without fee, 
 * provided that the above copyright notice appear in all copies and that 
 * both that copyright notice, this permission notice, and the following 
 * disclaimer appear in supporting documentation, and that the names of 
 * IBM, Carnegie Mellon University, and other copyright holders, not be 
 * used in advertising or publicity pertaining to distribution of the software 
 * without specific, written prior permission.
 * 
 * IBM, CARNEGIE MELLON UNIVERSITY, AND THE OTHER COPYRIGHT HOLDERS 
 * DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT 
 * SHALL IBM, CARNEGIE MELLON UNIVERSITY, OR ANY OTHER COPYRIGHT HOLDER 
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
 * DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, 
 * WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS 
 * ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
 * OF THIS SOFTWARE.
 *  $
*/

#ifndef NORCSID
#define NORCSID
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/libs/ms/RCS/shrkname.c,v 2.12 1992/12/15 21:21:37 rr2b R6tape $";
#endif

#include <andrewos.h>
#include <stdio.h>
#include <ms.h>
#include <ctype.h>

extern char *StripWhiteEnds();

#define MAXPARTS 10

ShrinkName(longname, shortname, limit)
char *longname, *shortname;
int limit;
{
    int i, len, numparts = 0, ComponentLengths[MAXPARTS], commalen = 0, looking;
    char *space, *comma, *Components[MAXPARTS], *lastpart, *tmp;

    debug(1, ("Shrinking name %s\n", longname));
    ReduceWhiteSpace(longname);
    len = strlen(longname);
    /* We check for foo!bar even before the overall length, because they
	just look better without them */
    lastpart = strrchr(longname, '!');
    if (lastpart) {
	/* This thing has an old-style UNIX foo!bar!baz name. */
	longname = ++lastpart;
	len = strlen(longname);
	if (len <= limit && len > 1) {
	    strcpy(shortname, longname);
	    return;
	}
    }
    if (len <= limit) {
	strcpy(shortname, longname);
	return;
    }
    /* Next we strip out the leading/trailing double quotes, if any */
    debug(16, ("Checking for double quotes\n"));
    if (*longname == '"') {
	++longname;
	lastpart = longname + strlen(longname) - 1;
	if (*lastpart == '"') *lastpart = '\0';
	longname = StripWhiteEnds(longname);
	debug(16, ("Double quote removal leaves %s\n", longname));
    }
    lastpart = strrchr(longname, '/');
    if (lastpart) {
	/* This stupid thing has a file name!  Could be an Andrew dist list, or heaven knows what else.  We ignore the leading path name and anything before it.  */
	longname = ++lastpart;
	len = strlen(longname);
	if (len <= limit) {
	    strcpy(shortname, longname);
	    return;
	}
    }
    comma = strrchr(longname, ',');
    if (comma) {
	*comma++ = '\0';
	StripWhiteEnds(longname);
	comma = StripWhiteEnds(comma);
	commalen = strlen(comma);
	len = strlen(longname) + commalen + 2;
    }
    space = longname;
    while (space && *space && numparts < MAXPARTS) {
	Components[numparts++] = space;
	space = strchr(space, ' ');
	if (space) *space++ = '\0';
    }
    for(i=0; i<numparts; ++i) {
	ComponentLengths[i] = strlen(Components[i]);
    }
    while (len > limit && numparts > 0) {
	looking = 1;
	for (i=0; i<numparts; ++i) {
	    tmp = strchr(Components[i], '%');
	    if (tmp) {
		ComponentLengths[i] -= strlen(tmp);
		len -= strlen(tmp);
		*tmp = '\0';
		looking = 0;
	    }
	}
	if (!looking) continue;
	for (i=0; i<numparts; ++i) {
	    if (!strcmp(Components[i], "at")) {
		int j;
		for (j=i; j<numparts; ++j) {
		    len -= ComponentLengths[j];
		}
		numparts = i;
		looking = 0;
		break;
	    }
	}
	if (!looking) continue;
	for (i=0; i<numparts; ++i) {
	    if (Components[i][0] == '"') {
		for (tmp=Components[i]; *tmp; ++tmp) {
		    *tmp = *(tmp+1);
		}
		ComponentLengths[i] -= 1;
		--len;
		tmp -= 2;
		if (*tmp == '"') {
		    *tmp = '\0';
		    ComponentLengths[i] -= 1;
		    --len;
		}
		looking = 0;
	    }
	}
	if (!looking) continue;
	for (tmp = Components[numparts-1]; *tmp && !isalpha(*tmp); ++tmp) {;}
	if (!*tmp) { /* We can trash the whole last component! */
	    --numparts;
	    len -= ComponentLengths[numparts];
	    continue;
	}
	/* Next shrink all the middle names to initials */
	for (i=numparts-2; looking && i>0; --i) {
	    if (ComponentLengths[i] > 2) {
		Components[i][1] = '.';
		Components[i][2] = '\0';
		len -= (ComponentLengths[i] - 2);
		ComponentLengths[i] = 2;
		looking = 0;
		break;
	    }
	}
	if (!looking) continue;
	/* Next get rid of ", Jr." suffixes */
	if (comma) {
	    comma = NULL;
	    len -= (commalen + 2);
	    continue;
	}
	/* Next get rid of middle initials */
	for (i=1; i<(numparts-1); ++i) {
	    if (ComponentLengths[i] > 0) {
		Components[i][0] = '\0';
		len -= ComponentLengths[i];
		--len;
		ComponentLengths[i] = 0;
		looking = 0;
		break;
	    }
	}
	if (!looking) continue;
	if (numparts < 2) {
	    /* This used to happen without the above test, but "Nath Borenstein" was deemed bogus. */
	    /* But if there's only one name, this is still the thing to do.   */
	    /* Next shrink the first name slowly into oblivion */
	    if (ComponentLengths[0] > 0) {
		Components[0][ComponentLengths[0] - 1] = '\0';
		--len;
		--ComponentLengths[0];
		if (ComponentLengths[0] <= 0) {
		    /* Now they are all gone, note the vanishing space, too */
		    --len;
		}
		continue;
	    }
	}
	/* Next shrink the first name to an initial */
	if (ComponentLengths[0] > 2) {
		Components[0][1] = '.';
		Components[0][2] = '\0';
		len -= (ComponentLengths[0] - 2);
		ComponentLengths[0] = 2;
		continue;
	}
	/* Next get rid of the stupid first name entirely */
	if (ComponentLengths[0] > 0) {
	    Components[0][0] = '\0';
	    len -= ComponentLengths[0];
	    --len;
	    ComponentLengths[0] = 0;
	}
	break; /* Give up; no shrinking left to do -- all we have is one last name */
    }
    if (len > limit) {
	for (i=0; i<numparts; ++i) {
	    if (ComponentLengths[i]) {
		strncpy(shortname, Components[i], limit-1);
		shortname[limit-1] = '\0';
		return;
	    }
	}
	/* Should never be reached, but be safe */
	strncpy(shortname, longname, limit-1);
	shortname[limit-1] = '\0';
    } else {
	int hasadded = 0;

	*shortname = '\0';
	for (i=0; i<numparts; ++i) {
	    if (ComponentLengths[i] > 0) {
		if (hasadded) {
		    strcat(shortname, " ");
		}
		strcat(shortname, Components[i]);
		++hasadded;
	    }
	}
	if (comma) {
	    strcat(shortname, ", ");
	    strcat(shortname, comma);
	}
    }
}
