/*
Post A Note V3.0
Copyright (c) 1993, Jeffrey W. Bailey
All rights reserved.

Permission is granted to distribute this program in exact, complete
source form, which includes this copyright notice, as long as no fee
other than media and distribution cost is charged.

This program may not be used in whole, or in part, in any other manner
without prior written permission from the author.

This program may not be distributed in modified form without prior
written permission from the author.  In other words, patches may be
distributed, but modified source may not be distributed.

If there are any questions, comments or suggestions, the author may be
contacted at:

    jeff@rd1.interlan.com

    or

    Jeffrey Bailey
    Racal-Datacom, Inc.
    Mail Stop E-110
    1601 N. Harrison Parkway
    Sunrise, FL  33323-2899
*/

#include <stdio.h>
#include "pan.h"

LLM_init(root, size)
    struct LLM_root *root;
    int  size;
    {
    if(root == NULL)
        {
        return(NULL);
        }
    root->First   = NULL;
    root->Last    = NULL;
    root->Current = NULL;
    root->Size    = size + sizeof(struct LLM_node);
    root->Init    = 1;
    return(1);
    }

char *LLM_add(root)
    struct LLM_root *root;
    {
    struct LLM_node *hold, *temp;

    if(root == NULL)
        {
        return(NULL);
        }
    if(root->Init != 1)
        {
        return(NULL);
        }
    hold = (struct LLM_node *) malloc(root->Size);
    if(hold == NULL)
        {
        return(NULL);
        }
    if(root->First == NULL)
        {
        root->First = hold;
        root->Last = hold;
        root->Current = hold;
        hold->Next = NULL;
        hold->Prev = NULL;
        }
    else
        {
        temp = root->Last;
        temp->Next = hold;
        hold->Next = NULL;
        hold->Prev = temp;
        root->Last = hold;
        }
    return( (char *) (hold + 1));
    }

char *LLM_first(root)
    struct LLM_root *root;
    {
    if(root == NULL)
        {
        return(NULL);
        }
    if(root->Init != 1)
        {
        return(NULL);
        }
    if(root->First == NULL)
        {
        return(NULL);
        }
    root->Current = root->First;
    return((char *) (root->Current + 1));
    }

char *LLM_last(root)
    struct LLM_root *root;
    {
    if(root == NULL)
        {
        return(NULL);
        }
    if(root->Init != 1)
        {
        return(NULL);
        }
    if(root->Last == NULL)
        {
        return(NULL);
        }
    root->Current = root->Last;
    return((char *) (root->Current + 1));
    }

char *LLM_next(root)
    struct LLM_root *root;
    {
    if(root == NULL)
        {
        return(NULL);
        }
    if(root->Init != 1)
        {
        return(NULL);
        }
    if(root->First == NULL)
        {
        return(NULL);
        }
    if(root->Current == NULL)
        {
        return(NULL);
        }
    if(root->Current == root->Last)
        {
        return(NULL);
        }
    root->Current = root->Current->Next;
    return((char *) (root->Current + 1));
    }

char *LLM_previous(root)
    struct LLM_root *root;
    {
    if(root == NULL)
        {
        return(NULL);
        }
    if(root->Init != 1)
        {
        return(NULL);
        }
    if(root->First == NULL)
        {
        return(NULL);
        }
    if(root->Current == NULL)
        {
        return(NULL);
        }
    if(root->Current == root->First)
        {
        return(NULL);
        }
    root->Current = root->Current->Prev;
    return((char *) (root->Current + 1));
    }

LLM_free(root)
    struct LLM_root *root;
    {
    struct LLM_node *temp, *hold;

    if(root == NULL)
        {
        return(NULL);
        }
    if(root->Init != 1)
        {
        return(NULL);
        }
    temp = root->First;
    while(temp != NULL)   /* Loop until we're at the end. */
        {
        hold = temp;
        temp = temp->Next;
        (void)free((char *)hold);       /* Free up our current node. */
        }
    root->First   = NULL;
    root->Last    = NULL;
    root->Current = NULL;
    return(1);
    }

LLM_delete(root, node)
    struct LLM_root *root;
    char *node;
    {
    struct LLM_node *temp;

    if(root == NULL)
        {
        return(NULL);
        }
    if(root->Init != 1)
        {
        return(NULL);
        }
    if(node == NULL)
        {
        return(NULL);
        }
    temp = (struct LLM_node *) node;
    temp--;

    if(temp->Prev != NULL)
        temp->Prev->Next = temp->Next;
    else
        root->First = temp->Next;

    if(temp->Next != NULL)
        temp->Next->Prev = temp->Prev;
    else
        root->Last = temp->Prev;

    if(temp == root->Current) root->Current = root->First;

    (void)free((char *)temp);
    return(1);
    }

char *LLM_insert(root, node)
    struct LLM_root *root;
    char *node;
    {
    struct LLM_node *temp, *hold;

    if(root == NULL)
        {
        return(NULL);
        }
    if(root->Init != 1)
        {
        return(NULL);
        }
    if(node == NULL)
        {
        return(NULL);
        }
    temp = (struct LLM_node *) node;
    temp--;
    hold = (struct LLM_node *) malloc(root->Size);
    if(hold == NULL)
        {
        return(NULL);
        }
    if(temp == root->First)
        {
        hold->Next = root->First;
        hold->Next->Prev = hold;
        root->First = hold;
        hold->Prev = NULL;
        return( (char *) (hold + 1));
        }
    hold->Next = temp;
    hold->Prev = temp->Prev;
    temp->Prev = hold;
    hold->Prev->Next = hold;

    return( (char *) (hold + 1));
    }

LLM_position(root, node)
    struct LLM_root *root;
    char *node;
    {
    struct LLM_node *temp;

    if(root == NULL)
        {
        return(NULL);
        }
    if(node == NULL)
        {
        return(NULL);
        }
    if(root->Init != 1)
        {
        return(NULL);
        }
    temp = (struct LLM_node *) node;
    temp--;
    root->Current = temp;
    return(1);
    }

char *LLM_current(root)
    struct LLM_root *root;
    {
    if(root == NULL)
        {
        return(NULL);
        }
    if(root->Init != 1)
        {
        return(NULL);
        }
    if(root->Current == NULL)
        {
        return(NULL);
        }
    return((char *) (root->Current + 1));
    }

LLM_unlink(root, node)
    struct LLM_root *root;
    char *node;
    {
    struct LLM_node *temp;

    if(root == NULL)
        {
        return(NULL);
        }
    if(root->Init != 1)
        {
        return(NULL);
        }
    if(node == NULL)
        {
        return(NULL);
        }
    temp = (struct LLM_node *) node;
    temp--;

    if(temp->Prev != NULL)
        temp->Prev->Next = temp->Next;
    else
        root->First = temp->Next;

    if(temp->Next != NULL)
        temp->Next->Prev = temp->Prev;
    else
        root->Last = temp->Prev;

    if(temp == root->Current) root->Current = root->First;

    return(1);
    }

LLM_link(root, node, newnode)
    struct LLM_root *root;
    char *node;
    char *newnode;
    {
    struct LLM_node *temp, *hold;

    if(root == NULL)
        {
        return(NULL);
        }
    if(root->Init != 1)
        {
        return(NULL);
        }
    if(newnode == NULL)
        {
        return(NULL);
        }
    hold = (struct LLM_node *) newnode;
    hold--;
    if(node == NULL && root->First != NULL) /* link at end */
        {
        temp = root->Last;
        temp->Next = hold;
        hold->Next = NULL;
        hold->Prev = temp;
        root->Last = hold;
        return(1);
        }
    if(node == NULL && root->First == NULL) /* first node */
        {
        root->First = hold;
        root->Last = hold;
        root->Current = hold;
        hold->Next = NULL;
        hold->Prev = NULL;
        return(1);
        }
    temp = (struct LLM_node *) node;
    temp--;
    if(temp == root->First)
        {
        hold->Next = root->First;
        hold->Next->Prev = hold;
        root->First = hold;
        hold->Prev = NULL;
        return(1);
        }
    hold->Next = temp;
    hold->Prev = temp->Prev;
    temp->Prev = hold;
    hold->Prev->Next = hold;

    return(1);
    }
