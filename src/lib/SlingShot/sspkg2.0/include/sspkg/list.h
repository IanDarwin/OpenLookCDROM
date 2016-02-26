/*
 *	(c) Copyright 1991 Sun Microsystems, Inc.  All rights reserved.
 *	See LEGAL_NOTICE file for terms and restrictions.
 */

#ifndef _LIST_H
#define _LIST_H

/* @(#) list.h 1.2 92/05/28  */

#include <xview/xv_c_types.h>
#include <xview/base.h>

typedef struct _node {
	struct _node   *prev;
	struct _node   *next;
	void           *handle;
}               Listnode;

#define list_next(node)		(node ? ((Listnode*)node)->next : NULL)
#define list_prev(node)		(node ? ((Listnode*)node)->prev : NULL)
#define list_handle(node)	(node ? ((Listnode*)node)->handle : NULL)

#define list_for(node)		for(node = list_first(node); 	\
 				    node; 			\
 				    node = list_next(node))

#define list_rof(node)		for(node = list_last(node); 	\
 				    node; 			\
 				    node = list_prev(node))

#define list_copy(node)		list_dup(node, sizeof(Listnode))
#define list_alloc_node()	(Listnode*)calloc(1, sizeof(Listnode))

EXTERN_FUNCTION( Listnode *list_insert_after, (Listnode*, Listnode*));
EXTERN_FUNCTION( Listnode *list_insert_before, (Listnode*, Listnode*));
EXTERN_FUNCTION( void     list_destroy, (Listnode*));
EXTERN_FUNCTION( Listnode *list_first, (Listnode*));
EXTERN_FUNCTION( Listnode *list_last, (Listnode*));
EXTERN_FUNCTION( Listnode *list_unlink_node, (Listnode*));
EXTERN_FUNCTION( Listnode *list_delete_node, (Listnode*));
EXTERN_FUNCTION( Listnode *list_dup, (Listnode*, int));
EXTERN_FUNCTION( Listnode *list_concat, (Listnode*, Listnode*));
EXTERN_FUNCTION( Listnode *list_find, (Listnode*, void*));
EXTERN_FUNCTION( void     list_traverse, (Listnode*, int(*)(), void*));

#endif
