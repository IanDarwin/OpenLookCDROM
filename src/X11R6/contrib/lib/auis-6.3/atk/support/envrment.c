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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/support/RCS/envrment.c,v 2.15 1992/12/15 21:41:42 rr2b R6tape $";
#endif


 

#include <class.h>
#include <envrment.eh>
#include <tree23.ih>
#include <viewref.ih>

/* Crank out structs in 4k blocks. */
#define DESIREDBLOCKSIZE 4096
/* Number of marks per block */
#define NUMPERBLOCK DESIREDBLOCKSIZE / sizeof(struct environment)
#define BLOCKSIZE NUMPERBLOCK * sizeof(struct environment)

static struct environment *freeList = NULL;
static struct environment *lastBlock = NULL;

struct environment *environment__Allocate(classID)
struct classheader *classID;
{

    static int lastIndex = NUMPERBLOCK; /* Force a block malloc on first call. */

    if (freeList) {
        struct environment *temp = freeList;
        freeList = (struct environment *) freeList->header.environment_methods;
        return temp;
    }
    if (lastIndex >= NUMPERBLOCK) {
        lastBlock = (struct environment *) malloc(BLOCKSIZE);
        lastIndex = 0;
    }
    return &lastBlock[lastIndex++];
}

void environment__Deallocate(classID, self)
    struct classheader *classID;
    struct environment *self;
{
    self->header.environment_methods = (struct basicobject_methods *) freeList;
    freeList = self;
}

boolean environment__InitializeObject(classID, self)
    struct classheader *classID;
    struct environment *self;
{
    self->type = environment_None;
    self->data.style = NULL;

    return TRUE;
}

void environment__FinalizeObject(classID, self)
    struct classheader *classID;
    struct environment *self;
{
    switch (self->type)  {
	case environment_View:
	    if(self->data.viewref != NULL)
		viewref_Destroy(self->data.viewref);
	    break;
    }
}
	
struct environment *environment__NewButSimilar(self)
struct environment *self;
{
    struct environment *sib=super_NewButSimilar(self);
    sib->type=self->type;
    sib->data.style=self->data.style;
    return sib;
}

struct environment *environment__Wrap(self, pos, length, type, data)
    struct environment *self;
    long pos;
    long length;
    enum environmenttype type;
    union environmentcontents data;
{
    struct environment *newenv, *nm1, *nm2, *cp;

    nm1 = environment_GetInnerMost(self, pos);
    nm2 = environment_GetInnerMost(self, pos + length - 1);
    cp = environment_GetCommonParent(nm1, nm2);

    if (cp->type == environment_View) {
        long oldpos, oldlen;
        union environmentcontents olddata;

        /* Hack to prevent non-view envs from get inside view envs; */
        /* removes the view env, adds new env, adds back view env. */

        oldpos = environment_Eval(cp);
        oldlen = environment_GetLength(cp);
        olddata = cp->data;

        /* Null out cp->data so environment_Delete leaves viewref alone! */
        cp->data.viewref = NULL;
        environment_Delete(cp);

        if ((newenv = environment_Add(self, pos, length)) != NULL) {
            newenv->type = type;
            newenv->data = data;
            if (type == environment_View)
                environment_SetStyle(newenv, FALSE, FALSE);
        }

        cp = environment_Add(self, oldpos, oldlen);
        if (cp != NULL) {
            cp->type = environment_View;
            cp->data = olddata;
            environment_SetStyle(cp, FALSE, FALSE);
        }
    } else if ((newenv = environment_Add(self, pos, length)) != NULL) {
	newenv->type = type;
	newenv->data = data;
        if (type == environment_View)
            environment_SetStyle(newenv, FALSE, FALSE);
    }
    return newenv;
}

struct environment *environment__WrapStyle(self, pos, length, style)
struct environment *self;
long pos;
long length;
struct style *style;
{

    union environmentcontents data;

    data.style = style;
    return environment_Wrap(self, pos, length, environment_Style, data);
}

struct environment *environment__WrapView(self, pos, length, viewref)
struct environment *self;
long pos;
long length;
struct viewref *viewref;
{

    union environmentcontents data;

    data.viewref = viewref;
    return environment_Wrap(self, pos, length, environment_View, data);
}

struct environment *environment__Insert(self, rpos, type, data, doinsert)
struct environment *self;
long rpos;			/* relative position of the environment */
enum environmenttype type;
union environmentcontents data;
boolean doinsert;
{
    struct environment *newenv;
    
    newenv = environment_NewFromObject(self);
    newenv->header.nestedmark.parent = (struct nestedmark *) self;
    newenv->type = type;
    newenv->data = data;
    if (type == environment_View)
        environment_SetStyle(newenv, FALSE, FALSE);
    if (doinsert) {
	if (self->header.nestedmark.children == NULL)
	    self->header.nestedmark.children = tree23int_New();
	newenv->header.nestedmark.position = (struct tree23int *) tree23int_Insert(self->header.nestedmark.children, rpos, (long) newenv);
    }
    return newenv;
}

struct environment *environment__InsertStyle(self, rpos, style, doinsert)
struct environment *self;
long rpos;			/* relative position of the environment */
struct style *style;
boolean doinsert;
{

    union environmentcontents data;

    data.style = style;

    return environment_Insert(self, rpos, environment_Style, data, doinsert);
}

struct environment *environment__InsertView(self, rpos, viewref, doinsert)
struct environment *self;
long rpos;			/* relative position of the environment */
struct viewref *viewref;
boolean doinsert;
{

    union environmentcontents data;

    data.viewref = viewref;

    return environment_Insert(self, rpos, environment_View, data, doinsert);
}


struct environment *environment__GetRootEnvironment(classID)
struct classheader *classID;
{
    struct environment *newenv;
    
    newenv = environment_New();
    newenv->header.nestedmark.length = 2;	/* Probably should be proper setting of includebeginning and ending */
    return newenv;
}

struct removestruct {
    long pos;
    long length;
    enum environmenttype type;
    boolean deleteall;
    boolean anyChange;
};

static long AlterEnvironmentSize(self, data)
    struct environment *self;
    struct removestruct *data;
{
    long pos;
    if (self->type != data->type && data->type != environment_Any)
	return 0;
    
    pos = environment_Eval(self);
    if (pos < data->pos + data->length && pos + self->header.nestedmark.length > data->pos)  {
	data->anyChange = TRUE;
	if (pos < data->pos)  {
	    /* Delete end of the environment */
	    struct environment *endPart;

	    if(data->pos + data->length < pos + self->header.nestedmark.length) environment_Split(self, data->pos - pos + data->length);
	    endPart=environment_Split(self,data->pos-pos);

	    /* Delete child environments if necessary. */
	    if (data->deleteall && endPart->header.nestedmark.children != NULL) 
		tree23int_Enumerate(endPart->header.nestedmark.children, (procedure) AlterEnvironmentSize, (char *) data);

	    environment_Delete(endPart);
	}
	else if (pos + self->header.nestedmark.length > data->pos + data->length)  {
	    struct environment *env;
	    /* Delete beginning of the environment */
	    /* Is this split position corect? */
	    (void)environment_Split(self, data->pos - pos + data->length);
            if (data->deleteall && self->header.nestedmark.children != NULL)
                tree23int_Enumerate(self->header.nestedmark.children, (procedure) AlterEnvironmentSize, (char *) data);

            environment_Delete(self);
	}
	else  {
	    /* Delete the entire environment */

	    if (data->deleteall && self->header.nestedmark.children != NULL)
		tree23int_Enumerate(self->header.nestedmark.children, (procedure) AlterEnvironmentSize, (char *) data);
	    environment_Delete(self);
	}
    }
    return 0;
}

boolean environment__Remove(self, pos, length, type, deleteall)
    struct environment *self;
    long pos;
    long length;
    enum environmenttype type;
    boolean deleteall;
{
    struct environment *beginenv, *endenv;
    struct environment *parentenv;
    struct environment *env, *bottomenv;
    struct removestruct procdata;
    long envpos;

    procdata.anyChange = FALSE;
    beginenv = environment_GetInnerMost(self, pos);
    endenv = environment_GetInnerMost(self, pos + length - 1);

    parentenv = environment_GetCommonParent(beginenv, endenv);

    for (envpos = environment_Eval(parentenv); (pos == envpos || pos + length == envpos + parentenv->header.nestedmark.length); envpos = environment_Eval(parentenv))  {
	if (parentenv->header.nestedmark.parent == NULL) break;
	parentenv = (struct environment *) parentenv->header.nestedmark.parent;
	if (! deleteall) break;
    }

    if (parentenv->header.nestedmark.children != NULL)  {
	procdata.pos = pos;
	procdata.length = length;
	procdata.deleteall = deleteall;
	procdata.type = type;
	tree23int_Enumerate(parentenv->header.nestedmark.children, (procedure) AlterEnvironmentSize, (char *) &procdata);
	if(procdata.anyChange && !deleteall)
	    return TRUE;
	if(parentenv->header.nestedmark.parent!=NULL) {
	    AlterEnvironmentSize(parentenv,&procdata);
	    if(procdata.anyChange && !deleteall)
		return TRUE;
	}
    }

    /* no child envs were modified, so apply to parent instead */

    if(parentenv->header.nestedmark.parent==NULL)
	return procdata.anyChange;	/* can't plainer top environment */

    bottomenv=parentenv;

    do{
	env=parentenv;
	parentenv=(struct environment *)parentenv->header.nestedmark.parent;
    }while(parentenv->header.nestedmark.parent!=NULL && deleteall);


    procdata.pos = pos;
    procdata.length = length;
    procdata.deleteall = deleteall;
    procdata.type = type;
    AlterEnvironmentSize(env, &procdata);

    return procdata.anyChange;
}

void environment__Dump(self, level)
    struct environment *self;
    int level;
{
    struct nestedmark *nself = (struct nestedmark *) self;
    register int i = level;

    while (i-- > 0)
	printf("    ");
    printf("Env %x (%x ^) position: %d length: %d ib: %c ie: %c type: %d value %d\n", nself, nself->parent, environment_Eval(self), nself->length, (nself->includeBeginning) ? 'T' : 'F', (nself->includeEnding) ? 'T' : 'F', self->type, self->data.style);
    if (nself->children != NULL)
        tree23int_Enumerate(nself->children, (procedure) environment__Dump, (char *) level + 1);
}
