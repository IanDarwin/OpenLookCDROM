/* $Author: rr2b $ */

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


 

/* ************************************************************
 *  Copyright (C) 1989, 1990, 1991
 *  by the Massachusetts Institute of Technology
 *  For full copyright information see:'mit-copyright.h'     *
 *************************************************************/

#include <mit-copyright.h>

/* This is the sorting algorithm for the list of papers */
/* June 21 1990 -njw */

/* Version 1: Treesort */
/* The idea is, we build a sorted binary tree with the data
 * which is, I think O(n.log n) and then with the resultant tree,
 * go back through the list, rechaining it. (Using left-first traversal 
 * of the tree.)
 * On the second run thru, where the original list is reordered, 
 * the tree is freed from memory on the fly.
 */

typedef struct node * Tree;

struct node {
    Tree left;
    Tree right;
    Tree parent;
    Paperlist data;
    int count; /* Used for the traversal of the tree */
};


void
add_node(item, tree, data)
Paperlist item;
Tree *tree;
int data;
{
    Tree n = *tree;
    Tree oldnode = NULL;
    int direction;
    Tree newnode;

    if (n != NULL) {
	while (n != NULL) {
	    oldnode = n;
	    direction = compare(n->data, item, data);
	    if (direction <= 0)
		n = n->left;
	    else
		n = n->right;
	}
	newnode = (Tree) malloc(sizeof(struct node));
	/* What if malloc fails? XXX */
	newnode->data = item;
	newnode->left = newnode->right = NULL;
	newnode->parent = oldnode;
	if (direction <= 0)
	    oldnode->left = newnode;
	else
	    oldnode->right = newnode;
	newnode->count = 0;
    } else {
	*tree = (Tree) malloc(sizeof(struct node));
	(*tree)->data = item;
	(*tree)->left = (*tree)->right = (*tree)->parent = NULL;
	(*tree)->count = 0;
    }
}

void
sort(papers, data)
Paperlist *papers;
int data;
{
    Paperlist item;
    Tree tree = NULL;
    Tree tmpnode = NULL;
    Tree nextnode = NULL;
    int direction = 0; /* 0=left, 1=right */
    Paperlist *newlist = papers; /* This is the resorted list */
    int firstitem = 1; /* Boolean flag to show when at top of list */

    if (!papers || !*papers)
	return;

    for (item = *papers; item != NULL; item = item->next)
	add_node(item, &tree, data);

    /* We now do a left-first tree traversal, reordering the initial linked list to follow the order of the tree. A node is only ever looked at going down, so whenever there is a move up, the tree node is free'd */
    tmpnode = tree;
    while (tmpnode != NULL) {
	tmpnode->count++;
	if (tmpnode->count == 2) {
	    /* This is where we reorder the real list */
	    if (!firstitem)
		(*newlist)->next = tmpnode->data;
	    else {
		*papers = tmpnode->data;
		firstitem = 0;
	    }
	    newlist = &(tmpnode->data);
	}
	if (tmpnode->count == 3) {
	    nextnode = tmpnode->parent;
	    free(tmpnode); /* We destroy the tree as we scan it */
	    tmpnode = nextnode;
	} else {
	    if (direction == 0 /* left */) {
		nextnode = tmpnode->left;
	    } else
		nextnode = tmpnode->right;
	    if (nextnode) {
		tmpnode = nextnode;
		direction = 0 /* always try to go left */; 
	    } else
		if (direction == 1 /* right */) {
		    nextnode = tmpnode->parent;
		    free(tmpnode); /* We destroy the tree as we scan it */
		    tmpnode = nextnode;
		} else
		    direction = 1 /* right */;
	}
    }
    /* Close the end of the list */
    (*newlist)->next = NULL;
}
