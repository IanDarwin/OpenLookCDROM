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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/support/RCS/tree23.c,v 2.10 1992/12/15 21:42:39 rr2b R6tape $";
#endif

 
 

#include <class.h>
#include <tree23.eh>

/* Crank out structs in 4k blocks. */
#define DESIREDBLOCKSIZE 4096
/* Number of marks per block */
#define NUMPERBLOCK DESIREDBLOCKSIZE / sizeof(struct tree23int)
#define BLOCKSIZE NUMPERBLOCK * sizeof(struct tree23int)

static struct tree23int *freeList = NULL;
static struct tree23int *lastBlock = NULL;

struct tree23int *tree23int__Allocate(classID)
struct classheader *classID;
{

    static int lastIndex = NUMPERBLOCK; /* Force a block malloc on first call. */

    if (freeList) {
        struct tree23int *temp = freeList;
        freeList = (struct tree23int *) freeList->header.tree23int_methods;
        return temp;
    }
    if (lastIndex >= NUMPERBLOCK) {
        lastBlock = (struct tree23int *) malloc(BLOCKSIZE);
        lastIndex = 0;
    }
    return &lastBlock[lastIndex++];
}

void tree23int__Deallocate(classID, self)
struct classheader *classID;
    struct tree23int *self;
{
    self->header.tree23int_methods = (struct basicobject_methods *) freeList;
    freeList = self;
}

boolean tree23int__InitializeObject(classID, self)
struct classheader *classID;
struct tree23int *self;  {
    self->leaf = FALSE;
    self->bump = 0;
    self->data = 0;
    self->parent = NULL;
    self->nKids = 0;
    return TRUE;
}
struct tree23int *tree23int__Delete(self)
struct tree23int *self;  {
    struct tree23int *parent = self->parent;

    if (parent == NULL) return  self;
    return tree23int_Remove(parent, self);
}
long tree23int__Eval(self)
struct tree23int *self;  {
    register int i;
    
    for (i = 0; self != NULL; i+= self->bump, self =  self->parent);
    return i;
}
struct tree23int *tree23int__Insert(self, key, data)
struct tree23int *self;
long key;
long data;  {
    struct tree23int *newnode;
    
    newnode = tree23int_New();
    newnode->leaf = TRUE;
    newnode->data = data;
    newnode->bump = key;
    tree23int_AddIn(self, 0, newnode);
    return newnode;
}

void tree23int__AddIn(self, offset, newnode)
struct tree23int *self;
long offset;
struct tree23int *newnode;  {
    long value;
    long i;

    if(self->leaf) {
    	tree23int_Jam(self->parent, newnode);
	return;
	}

    offset += self->bump;
    newnode->bump -= self->bump;
    value = newnode->bump;
/*     leaf already checked above
 */    if (self->nKids >= 3 && self->kid[2]->bump <= value)  { 
	tree23int_AddIn(self->kid[2],offset,newnode);
	return;
    }
    if (self->nKids >= 2 && self->kid[1]->bump <= value)  {
	tree23int_AddIn(self->kid[1],offset,newnode);
	return;
    }
    if (self->nKids >= 1 && self->kid[0]->bump <= value)  {
	tree23int_AddIn(self->kid[0],offset,newnode);
	return;
    }

/*     If we make it this far, there's no real home for the new item->
    We either add it as the new leftmost child (if there's room for 1 more)
    or we add it to the old leftmost child, which may result in a split
    further down the line.
 */   
    if (self->nKids < 3)  {
/* 	there's room in this node
	use slot 0; we're adding this as the leftmost child
 */	i = self->nKids;
	while (i>0)  {
	    self->kid[i] = self->kid[i-1];
	    i--;
	}
	self->kid[0] =  newnode;
	newnode->parent = self;
	self->nKids++;
	tree23int_Twiddle(self, newnode);
    }
    else  {
	/* otherwise we add it to the bottom */
	tree23int_AddIn(self->kid[0],offset,newnode);
    }
}

void tree23int__Apply(self, proc)
struct tree23int *self;
procedure proc;  {
    if(self->leaf){ 
	(*proc)(self->data); 
	return;
    }
    if (self->nKids >= 1) tree23int_Apply(self->kid[0],proc);
    if (self->nKids >= 2) tree23int_Apply(self->kid[1],proc);
    if (self->nKids >= 3) tree23int_Apply(self->kid[2],proc);
}

void tree23int__Filter(self, offset, left, right, lowval, highval, proc, procdata)
struct tree23int *self;
long offset;
struct tree23int *left;
struct tree23int *right;
long lowval;
long highval;
procedure proc;
char *procdata;
  {
    offset += self->bump;

    if(self->leaf ){
	struct tree23int *newnode;
    	if (offset >= lowval && offset < highval)  {
		newnode = tree23int_Insert(right, offset, self->data);
		(*proc)(self->data, procdata, newnode, right);
    	}
    	else  {
		newnode = tree23int_Insert(left, offset, self->data);
		(*proc)(self->data, procdata, newnode, left);
    	}
    	return;
    }
    if (self->nKids >= 1) tree23int_Filter(self->kid[0],offset,left,right,lowval,highval, proc, procdata);
    if (self->nKids >= 2) tree23int_Filter(self->kid[1],offset,left,right,lowval,highval, proc, procdata);
    if (self->nKids >= 3) tree23int_Filter(self->kid[2],offset,left,right,lowval,highval, proc, procdata);
}

long tree23int__FindL(self, offset, key)
struct tree23int *self;
long offset;
long key;  {
    register int nKids;
    if(self->leaf ) return self->data;
    while (self != NULL)  {
	if (self->leaf) return self->data;
	offset += self->bump;
	if ((nKids=self->nKids) >= 3 && offset+self->kid[2]->bump <= key) 
	    self = self->kid[2];
	else if (nKids >= 2 && offset+self->kid[1]->bump <= key) 
	    self = self->kid[1];
	else if (nKids >= 1 && offset+self->kid[0]->bump <= key) 
	    self = self->kid[0];
	else return 0;
    }
    return 0;		/* there's no other possibility */
}

long tree23int__FindR(self, offset, key)
struct tree23int *self;
long offset;
long key;  {
    register struct tree23int *lastright;
    register int lastrightx = 0;
    if(self->leaf ) return 0;

    lastright = NULL;
    while (self != NULL)  {
	offset += self->bump;
	if (self->leaf) break;
	if (self->nKids >= 3 && offset+self->kid[2]->bump <= key)
	    self = self->kid[2];
	else if (self->nKids >= 2 && offset+self->kid[1]->bump <= key)  {
	    if (self->nKids >= 3)  {
		lastright = self;
		lastrightx = 2;
	    }
	    self = self->kid[1];
	}
	else if (self->nKids >= 1 && offset+self->kid[0]->bump <= key)  {
	    if (self->nKids >= 2)  {
		lastright = self;
		lastrightx = 1;
	    }
	    self = self->kid[0];
	}
	else if (self->nKids >= 1)  {
	    lastright = self;
	    lastrightx = 0;
	    break;
	}
	else break;
    }
    if (lastright != NULL)
	return tree23int_FindL(lastright->kid[lastrightx], 0, lastright->kid[lastrightx]->bump);
    return 0;
}

void tree23int__Free(self)
struct tree23int *self;  {
    if (self->nKids >= 1) tree23int_Free(self->kid[0]);
    if (self->nKids >= 2) tree23int_Free(self->kid[1]);
    if (self->nKids >= 3) tree23int_Free(self->kid[2]);
    tree23int_Destroy(self);
}

void tree23int__Merge(self, ancestor, offset, proc, procdata)
struct tree23int *self;
struct tree23int *ancestor;
long offset;
procedure proc;
char *procdata;
{
    offset = offset+self->bump;
    /*     recurse down the tree, adding stuff to the root */
    if(self->leaf) {
	struct tree23int *tp;
	tp = tree23int_Insert(ancestor,offset,self->data);
	(*proc)(self->data,procdata,tp,ancestor);
	return;
    }
    if (self->nKids >= 1)
	tree23int_Merge(self->kid[0], ancestor, offset, proc, procdata);
    if (self->nKids >= 2)
	tree23int_Merge(self->kid[1], ancestor, offset, proc, procdata);
    if (self->nKids >= 3)
	tree23int_Merge(self->kid[2], ancestor, offset, proc, procdata);
}

void tree23int__Update(self, pos, size)
struct tree23int *self;
long pos;
long size;  {
    int i;
    struct tree23int *tp;
    boolean flag;

    pos -= self->bump;
    if (size > 0)  {
/* 	inserting chars
 */
	if (pos <= 0)  {
	    self->bump += size;
	    if (self->parent != NULL)
		tree23int_Twiddle(self->parent, self);
	    return;
	}
	if(self->leaf) return;
/* 	otherwise handle things recursively
 */	i = self->nKids-1;	/* right to left to avoid interaction with twiddle */
	flag = FALSE;
	while (i >= 0)  {
	    tp = self->kid[i];
	    if (pos >= tp->bump) flag = TRUE;
/* 	    only i==0 call can change tp->bump, and we've already checked it.
 */	    tree23int_Update(tp,pos,size);
	    if (flag) return;
	    i--;
	}
    }
    else {
/* 	deleting characters, size is negative
 */
	if (pos < 0)  {
/* 	    delete the stuff to the left
 */	    if (pos > size) i = pos;
	    else i = size;
	    self->bump +=  i;
	    if (self->parent != NULL)
		tree23int_Twiddle(self->parent, self);
	    if(self->leaf) return;
	    pos -= i;
	    size -= i;
	}
	else	if(self->leaf) return;
	if (size == 0) return;
	i = self->nKids - 1;
	flag = FALSE;
	while (i >= 0)  {
	    tp = self->kid[i];
	    if (pos >= tp->bump) flag = TRUE;
/* 	    once again, only i==0 call can change tp->bump
 */	    tree23int_Update(tp,pos,size);
	    if (flag) return;
	    i--;
	}
    }
}

void tree23int__Jam(self, newnode)
struct tree23int *self;
struct tree23int *newnode;  {
    register struct tree23int *tp, *up;
    struct tree23int *nroot;
    register int i;
    int slot, value;
/*     called to jam a new leaf node under a parent (self)
 */
    value = newnode->bump;
/*     there are, as usual, two choices: 3 nodes or less than three
 */    if (self->nKids >= 3 && value > self->kid[2]->bump) slot = 3;
    else if (self->nKids >= 2 && value > self->kid[1]->bump) slot = 2;
    else if (self->nKids >= 1 && value > self->kid[0]->bump) slot = 1;
    else slot = 0;
    if (self->nKids < 3) {
/* 	add the new node into the right spot.  If this is slot 0, be sure
	to call tree23int_Twiddle when you're done
 */	i = self->nKids;
	while (i>slot)  {
	    self->kid[i] = self->kid[i-1];
	    i--;
	}
	self->kid[slot] =  newnode;
	self->nKids += 1;
	newnode->parent = self;
	if (slot == 0) tree23int_Twiddle(newnode->parent, newnode);
    }
    else  {
/* 	otherwise we do a split
 */	tp = tree23int_New();
	up = tree23int_New();
	tp->bump = self->bump;
	up->bump = self->bump;
	tp->parent = NULL;	/* just for a little while */
	up->parent = NULL;
	tp->nKids = 2;
	up->nKids = 2;

/* 	put the value in the tree
 */	if (slot > 0) tp->kid[0] = self->kid[0];
	if (slot > 1) tp->kid[1] = self->kid[1];
	else tp->kid[1] = self->kid[0];
	if (slot > 2) up->kid[0] = self->kid[2];
	else up->kid[0] = self->kid[1];
	up->kid[1] = self->kid[2];
	if (slot == 0) tp->kid[0] = newnode;
	else if (slot == 1) tp->kid[1] = newnode;
	else if (slot == 2) up->kid[0] =  newnode;
	else up->kid[1] =  newnode;

	tp->kid[0]->parent = tp;
	tp->kid[1]->parent = tp;
	up->kid[0]->parent = up;
	up->kid[1]->parent = up;
	tree23int_Twiddle(tp->kid[0]->parent, tp->kid[0]);	/* note that tp and up ->parent is 0 */
	tree23int_Twiddle(up->kid[0]->parent, up->kid[0]);

	if ((nroot = self->parent) != NULL)
	    value = tree23int_Eval(nroot);	/* where is self, originally */
	else
	    value = 0;
	nroot = tree23int_Delete(self);		/* delete the self from its parent */
	if (nroot == self)  {
/* 	    create a new self  */
	    nroot->bump = 0;
	    nroot->parent = NULL;
	    nroot->nKids = 0;
	}
	else tree23int_Destroy(self);
	tp->bump += value - tree23int_Eval(nroot);
	tree23int_Jam(nroot,tp);		/* note this can't cause a split, but can change nroot->bump */
	up->bump +=value - tree23int_Eval(nroot);
	tree23int_Jam(nroot,up);		/* this one *CAN* cause a split */
	tree23int_Twiddle(tp->parent, tp);
    }
}

struct tree23int *tree23int__Remove(self, child)
struct tree23int *self;
struct tree23int *child;  {
    int i;
    int j;
    struct tree23int *parent;
    struct tree23int *newParent;
    
    i=0;
    while (i < self->nKids)  {
	if (self->kid[i] ==  child)  {
/* 	    found the dude to zap
 */	    j = --(self->nKids);
	    while (i < j) {
		self->kid[i]=self->kid[i+1];
		i++;
	    }
	    if (self->nKids > 0)
		tree23int_Twiddle(self->kid[0]->parent, self->kid[0]);
	    else if ((parent = self->parent) != NULL)  {
/* 		if no parent, then we would delete a root of a 2/3 tree,
		    which is probably pointed to by other stuff.  We must delete zero-child
		    nodes, otherwise FindR might attempt to go down one of these null
		    paths in order to find the dude just to the right of the value it is given.
		    This results in an incorrect return value of 0 from FindR.
 */		
		newParent = tree23int_Remove(parent, self);
		tree23int_Destroy(self);
		return newParent;
	    }
	    return self;
	}
	i++;
    }
}

void tree23int__Twiddle(self, child)
struct tree23int *self;
struct tree23int *child;  {
    long t;
    
    while (child->bump != 0 && self != NULL && child ==  self->kid[0])  {
	t = child->bump;
	self->bump += t;
	child->bump = 0;
	if (self->nKids >= 2) self->kid[1]->bump -= t;
	if (self->nKids >= 3) self->kid[2]->bump -= t;
	child = self;
	self = self->parent;
    }
}

struct tree23int *tree23int__GetLeftMostNode(self)
    struct tree23int *self;
{
    while (! self->leaf)  {
	if (self->nKids == 0) return NULL;
	self = self->kid[0];
    }
    return  self;
}

struct tree23int *tree23int__GetNextNode(self,node)
    struct tree23int *self;
    struct tree23int *node;
{
    struct tree23int *parent;
    
    while (node != self)  {
	parent = node->parent;
	if ( node == parent->kid[0])  {
	    if (parent->nKids >= 2)
		return tree23int_GetLeftMostNode(parent->kid[1]);
	}
	else if (parent->nKids == 3 &&  node == parent->kid[1])  {
	    return tree23int_GetLeftMostNode(parent->kid[2]);
	}
	node =  parent;
    }
    return NULL;
}

struct tree23int *tree23int__GetRightMostNode(self)
    struct tree23int *self;
{
    while (! self->leaf)  {
	if (self->nKids == 0) return NULL;
	self = self->kid[self->nKids - 1];
    }
    return  self;
}

struct tree23int *tree23int__GetPreviousNode(self,node)
    struct tree23int *self;
    struct tree23int *node;
{
    struct tree23int *parent;
    
    while (node != self)  {
	parent = node->parent;
	if ( node == parent->kid[0])  {
	    node = parent;
	}
	else if (node == parent->kid[1]) {
	    return tree23int_GetRightMostNode(parent->kid[0]);
	}
	else if (node == parent->kid[2]) {
	    return tree23int_GetRightMostNode(parent->kid[1]);
	}
    }

    return NULL;
}

long tree23int__Enumerate(self, proc, procdata)
    struct tree23int *self;
    procedure proc;
    char *procdata;
{
    struct tree23int *node;
    struct tree23int *nextnode;
    if(self->leaf) {
    	if ((*proc)(self->data, procdata)) return self->data;
    	return 0;
    }
    node = tree23int_GetLeftMostNode(self);
    while (node != NULL)  {
	nextnode = tree23int_GetNextNode(self,node);
	if ((*proc)(node->data, procdata)) return node->data;
	node = nextnode;
    }
    return 0;
}

long tree23int__NumberOfLeaves(self)
    struct tree23int *self;
{
    long count = 0;
    if(self->leaf) return 1;
    if (self->nKids > 0)
	count += tree23int_NumberOfLeaves(self->kid[0]);
    if (self->nKids > 1)
	count += tree23int_NumberOfLeaves(self->kid[1]);
    if (self->nKids > 2)
	count += tree23int_NumberOfLeaves(self->kid[2]);
    return count;
}
void tree23int__Dump(self, offset)
struct tree23int *self;
long offset;  {
    register int i;

    printf("%8x (%8x^): ",self,self->parent);
    if(self->leaf) {
    	printf("(LEAF %x %x)\n",self->bump+offset, self->data);
	return;
    }
    printf("(INT (%d) %x)\n",self->nKids,self->bump+offset);
    i = 0;
    while (i<self->nKids)  {
	tree23int_Dump(self->kid[i],self->bump+offset);
	i++;
    }
    printf("End of %x\n",self);
}


