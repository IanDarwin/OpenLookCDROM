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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/libs/ms/RCS/bldpvmap.c,v 2.11 1992/12/15 21:17:22 rr2b R6tape $";
#endif

#include <andrewos.h>
#include <stdio.h>
#include <ms.h>

#define MAXPUBLICLINE (MAXPATHLEN + 150)

extern int NumSubsInUse;
extern struct SubscriptionProfile **SubsInPathOrder;

BuildPrivateSubscriptionMap(public, private, Root)
FILE *public, *private;
char *Root;
{
    char   *name,
           *s,
	    Fname[1+MAXPATHLEN],
            Buf[MAXPUBLICLINE];
    int     status,
	    subsindex,
	    pathindex,
    	    oldsubsindex,
	    code;
    int     rootLen=strlen(Root);

    debug(1,("BuildPrivateSubscriptionMap\n"));
    if (MakeSubsListInPathOrder() != 0) {
	return(-1); /* error code set */
    }

    for(pathindex = 0; TRUE; ++pathindex) {
	int eltLen;

	if (MS_GetSearchPathEntry(pathindex, Fname, MAXPATHLEN)) {
	    if (AMS_ERRNO == EINVAL) {
		pathindex = -1;
		break;
	    }
	    return(mserrcode);
	}

	eltLen=strlen(Fname);
	if ((eltLen==rootLen || (eltLen<rootLen && Root[eltLen]=='/')) &&
	    strncmp(Fname, Root, eltLen)==0)
	    break;
    }

    debug(16, ("Path index is %d\n", pathindex));
    for (subsindex = 0; subsindex<NumSubsInUse && SubsInPathOrder[subsindex]->pathelt != pathindex; ++subsindex) {
	;
    }
    debug(16, ("Initial subs index is %d\n", subsindex));
    while (fgets(Buf, MAXPUBLICLINE, public) != NULL) {
	if (BadSubMapLine(Buf)) continue;
	name = Buf;
	Buf[strlen(Buf) - 1] = '\0';
	for (s = name; *s; ++s) {
	    if (*s == ':')
		name = s + 1;
	}
	if (*name == '*')
	    ++name;
tryagain:
	debug(16,("Looking for %s in subscription cache\n", name));
	if (subsindex >= NumSubsInUse || SubsInPathOrder[subsindex]->pathelt != pathindex) {
	    debug(16,("Did not find it\n"));
	    status = AMS_UNSUBSCRIBED;
	}
	else {
	    code = PreorderSubscriptionStrcmp(SubsInPathOrder[subsindex]->key, name);
	    if (code == 0) {
		debug(16,("found it!\n"));
		status = SubsInPathOrder[subsindex++]->status;
	    } else if (code > 0) {
		debug(16, ("It is not the same as %s\n", SubsInPathOrder[subsindex]->key));
		status = AMS_UNSUBSCRIBED;
	    } else {
		oldsubsindex = subsindex;
                while ((subsindex < NumSubsInUse)
                        && (PreorderSubscriptionStrcmp(SubsInPathOrder[subsindex]->key, name) < 0)
                        && (SubsInPathOrder[subsindex]->pathelt == pathindex)) {
		    debug(16, ("Skipping over my subs entry %s\n", SubsInPathOrder[subsindex]->key));
		    if (SubsInPathOrder[subsindex]->status != AMS_UNSUBSCRIBED) { 
			/* This check is necessary because the subscription entry might refer to a no-longer-existent folder */
			fprintf(private, "%s:%s %d\n", SubsInPathOrder[subsindex]->sname, SubsInPathOrder[subsindex]->key, SubsInPathOrder[subsindex]->status);
		    } 
		    ++subsindex;
		}
		if (oldsubsindex == subsindex) {
		    AMS_RETURN_ERRCODE(EMSBOGUS, EIN_PARAMCHECK, EVIA_NAMESUBSCRIPTIONMAP);
		}
		goto tryagain;
	    }
	}
/*	if (status != AMS_UNSUBSCRIBED) { */
	    fprintf(private, "%s %d\n", Buf, status);
/*	} */
    }
    return(0);
}
