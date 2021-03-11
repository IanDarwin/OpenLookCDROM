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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/adew/RCS/arbiter.c,v 2.8 1993/01/08 16:30:59 rr2b R6tape $";
#endif


 

#include <class.h>
#include <cel.ih>
#include <arbiter.eh>
static struct arbiter *master;

struct arbiter *arbiter__GetMaster(classID)
struct classheader *classID;
{
    return master;
}
struct arbiter *arbiter__SetMaster(classID,newmaster)
struct classheader *classID;
struct arbiter *newmaster;
{
    struct arbiter *oldmaster;
    oldmaster = master;
    master = newmaster;
    return oldmaster;
}
boolean arbiter__InitializeClass(classID)
struct classheader *classID;
{
    master = NULL;
    return TRUE;
}

long arbiter__Read(self, file, id)
struct arbiter *self;
FILE *file;
long id;
{
    long result; /* set self as master so that as child cells are read, they will
		   call declare read on self, thus the arbiter will find all
		   of it's children */
    struct arbiter *lastmaster;
    lastmaster = arbiter_SetMaster(self);
    arbiter_SetArbiter(self,lastmaster);
    self->first = (struct cel *)self;
    result = super_Read(self, file, id);
    arbiter_SetMaster(lastmaster);
    arbiter_SetArbiter(self,lastmaster);
    arbiter_ReadObjects(self);
    return result;
}
long arbiter__ReadFile(self,thisFile)
struct arbiter *self;
FILE *thisFile;
{  
    long result; /* set self as master so that as child cells are read, they will
		   call declare read on self, thus the arbiter will find all
		   of it's children */
    struct arbiter *lastmaster;
    lastmaster = arbiter_SetMaster(self);
    arbiter_SetArbiter(self,lastmaster);
    self->first = (struct cel *)self;
    result = super_ReadFile(self, thisFile);
    arbiter_SetMaster(lastmaster);
    arbiter_SetArbiter(self,lastmaster);
    arbiter_ReadObjects(self);
    return result;
}
FILE *arbiter__DeclareRead(self,cel)
struct arbiter *self;
struct cel *cel;
{
#ifdef DEBUG
    fprintf(stdout,"declaring %d(%s) to %d(%s)\n",cel,cel_GetRefName(cel),self,arbiter_GetRefName(self));fflush(stdout);
#endif /* DEBUG */
    if(cel != (struct cel *) self){
	cel->chain = self->first;
	self->first = cel;
    }
    else if(arbiter_GetArbiter(self) && arbiter_GetArbiter(self) != self)
	    arbiter_DeclareRead(arbiter_GetArbiter(self),(struct cel *)self);

    return NULL;
}
struct cel *arbiter__FindChildCelByName(self,name)
struct arbiter *self;
char *name;
{
    struct cel *cel,*ncel;
    char *tname;
    cel = arbiter_GetFirst(self);
    while(cel != (struct cel *) self){
	if((tname = cel_GetRefName(cel)) != NULL &&
	   strcmp(tname,name) == 0)
	    return cel;
	ncel = cel_GetNextChain(cel);
	if(ncel == cel) return NULL; /* shouldn't happen */
	cel = ncel;
    }
    return NULL;
}
struct dataobject *arbiter__FindChildObjectByName(self,name)
struct arbiter *self;
char *name;
{
    struct cel *cel;
    cel = arbiter_FindChildCelByName(self,name);
    if(cel != NULL) return cel_GetObject(cel);
    return NULL;
}
void arbiter__ReadObjects(self)
struct arbiter *self;
{
/* for subclass to use */
}
boolean arbiter__InitializeObject(classID, self)
struct classheader *classID;
struct arbiter *self;
{
self->first = (struct cel *)self;
return TRUE;
}
