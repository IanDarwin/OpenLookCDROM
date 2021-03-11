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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/support/RCS/viewref.c,v 2.7 1992/12/15 21:42:39 rr2b R6tape $";
#endif


 

#include <class.h>
#include <dataobj.ih>
#include <viewref.eh>

static long viewID = 0;

boolean viewref__InitializeObject(classID, self)
struct classheader *classID;
struct viewref *self;
{
    self->viewType = NULL;
    self->viewID = viewID++;
    self->dataObject = NULL;
    self->desw = self->desh = 0;
    return TRUE;
}

void viewref__FinalizeObject(classID, self)
struct classheader *classID;
struct viewref *self;
{
    if (self->viewType != NULL) {
	free(self->viewType);
	self->viewType=NULL;
    }
    
    if(self->dataObject) {
	dataobject_Destroy(self->dataObject);
	self->dataObject=NULL;
    }
}

struct viewref *viewref__Create(classID, viewType, dataObject)
struct classheader *classID;
char *viewType;
struct dataobject *dataObject;
{
    struct viewref *newvr;
    
    if ((newvr = viewref_New()))  {
	if ((newvr->viewType = (char *) malloc(strlen(viewType)+1)))  {
	    strcpy(newvr->viewType, viewType);
	    newvr->dataObject = dataObject;
	    dataobject_Reference(dataObject);
	    return newvr;
	}
    }
    fprintf(stderr, "Could not allocate viewref structure.\n");
    return NULL;
}
